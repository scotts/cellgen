/*
 * Scott Schneider
 *
 * Cellgen: pseudo-openmp support for the Cell.
 */

#include <cassert>
#include <iostream>
#include <fstream>
#include <sstream>
#include <list>
using namespace std;

#include <boost/program_options.hpp>
#include <boost/regex.hpp>
using namespace boost;
using namespace boost::program_options;

#include "cellgen_grammar.h"
#include "variable.h"
#include "spe_region.h"
#include "streamops.h"
#include "utility.h"

const string pass_struct_oname		= "pass_struct.h";

// TEMPLATE_DIR is fed from the makefile
const string path			= TEMPLATE_DIR;
const string ppe_fork_iname		= path + "ppe_fork.c";
const string ppe_prolouge_iname		= path + "ppe_prolouge.c";
const string spe_declarations_iname	= path + "spe_prolouge.c";
const string spe_main_iname		= path + "spe_epilouge.c";
const string pass_struct_iname		= path + pass_struct_oname;
const string cellstrider_dma_iname	= path + "cellstrider_dma.h";

const string mmgp_h_oname		= "MMGP.h";
const string mmgp_c_oname		= "MMGP.c";
const string mmgp_spu_h_oname		= "spu/MMGP_spu.h";
const string mmgp_h_iname		= path + "MMGP/" + mmgp_h_oname;
const string mmgp_c_iname		= path + "MMGP/" + mmgp_c_oname;
const string mmgp_spu_h_iname		= path + "MMGP/MMGP_spu.h";

const string spe_profiler		= path + "cellgen_timer.h " + path + "cellgen_timer.c " + path + "spu_flih.S";

const string loop_hook			= "LOOP_ID";
const string pass_struct_hook		= "STRUCT_PASS_VARIABLE";
const string pass_assign_hook		= "PASS_ASSIGNMENT";
const string program_name_hook		= "PROGRAM_NAME";
const string num_kernels_hook		= "NUM_KERNELS";
const string case_hook			= "CASES";
const string var_hook			= "VAR";
const string op_hook			= "OP";
const string num_threads_hook		= "NUM_THREADS_HOOK";

const string struct_pass_var		= "((struct pass_t *)" + pass_var + "[__i" + loop_hook + "])->";
const string mmgp_spe_stop		= "MMGP_SPE_stop(";
const string mmgp_wait			= "MMGP_wait_SPE(" + loop_hook + ");\n";
const string mmgp_reduction		= "MMGP_reduction(" + var_hook + "," + op_hook + ","; // + loop_hook + ");\n";

bool print_ast = false;
int num_threads = 6;
string inc_name = "";

void file_to_stream(stringstream& in, istream& file) 
{
	while (!file.eof()) {
		int i = file.get();
		if (i != EOF) {
			in << static_cast<char>(i);
		}
	}
}

template <class T>
void open_check(const T& s, const string& name)
{
	if (!s) {
		cerr << "error: cannot open " << name << endl;
		exit(1);
	}
}

class codeout {
	ostream& out;

public:
	codeout(ostream& o): out(o) {}
	void operator()(const ast_node& node)
	{
		out	<< inv_accumulate_all(node.value.xformations, string(node.value.begin(), node.value.end()))
			<< " ";
		for_all(node.children, this);
	}
};

class case_sig {
	stringstream& file;
	stringstream& casest;

public:
	case_sig(stringstream& f, stringstream& c): file(f), casest(c) {}
	void operator()(const region_variable* v)
	{
		casest	<< v->actual() << ",";
		file	<< v->formal() << ",";
	}
};


string reduction_writeback(string val, const region_variable* v)
{
	return val + "signal.result = " + pass_var + "." + v->name() + ";\n";
}

class print_region {
	ostream& file;
	sslist& cases;
	int loop_num;

public:
	print_region(ostream& f, sslist& c): file(f), cases(c), loop_num(1) {}
	void operator()(spe_region* region)
	{
		stringstream* casest = new stringstream;
		*casest << "\t\t\tcase " << loop_num << ": " 
			<< "loop" << loop_num << "(" << loop_num <<", "; 
		file	<< "void loop" << loop_num << "(int fn_id, ";

		stringstream f;
		stringstream c;
		for_all(region->priv(), case_sig(f, c));
		for_all(region->shared(), case_sig(f, c));
		for_all(region->reductions(), case_sig(f, c));

		string writebacks = accumulate_all(region->reductions(), string(""), reduction_writeback);

		*casest	<< c.str().replace(c.str().find_last_of(","), strlen(","), "") << "); " << endl
			<< writebacks << endl
			<< mmgp_spe_stop << loop_num << "); " << "break;" << endl;
		cases.push_back(casest);

		file << f.str().replace(f.str().find_last_of(","), strlen(","), "");
		file << ") " << endl;

		for_all(region->ast_root()->children, codeout(file));

		++loop_num;
	}
};

void parse_generic(stringstream& stream, const string& str)
{
	ifstream ifile(str.c_str());
	open_check(ifile, str);
	file_to_stream(stream, ifile);
}

void replace_n_print_file(const string& ifile_name, const string& ofile_name, const string& hook, const string& replace)
{
	ifstream ifile(ifile_name.c_str());
	open_check(ifile, ifile_name);

	stringstream ss;
	file_to_stream(ss, ifile);

	ofstream ofile(ofile_name.c_str());
	open_check(ofile, ofile_name);
	ofile << regex_replace(regex_replace(ss.str(), regex(hook), replace),
			regex(num_threads_hook), to_string(num_threads));
}

class define_pass_struct {
	stringstream& ss;
public:
	define_pass_struct(stringstream& s): ss(s) {}
	void operator()(const region_variable* v)
	{
		if (v->name() != "SPE_start" && v->name() != "SPE_stop") {
			ss << "\t" << v->declare() << ";" << endl;
		}
	}

	void operator()(spe_region* region)
	{
		for_all(region->priv(), this);
		for_all(region->shared(), this);
		for_all(region->reductions(), this);
	}
};

class define_buffers {
	ostream& out;
public:
	define_buffers(ostream& o): out(o) {}
	void operator()(const shared_variable* v)
	{
		out << buffer_adaptor(v).declare() << ";" << endl;
	}
};

class define_in_and_out_buffers {
	stringstream& out;
public:
	define_in_and_out_buffers(stringstream& o): out(o) {}
	void operator()(spe_region* region)
	{
		for_all(region->in(), define_buffers(out));
		for_all(region->out(), define_buffers(out));
	}
};

class define_inout_buffers {
	stringstream& out;
public:
	define_inout_buffers(stringstream& o): out(o) {}
	void operator()(spe_region* region)
	{
		for_all(region->inout(), define_buffers(out));
	}

};

class define_private_buffers {
	ostream& out;
public:
	define_private_buffers(ostream& o): out(o) {}
	void operator()(const private_variable* v)
	{
		if (v->depth() > 0) {
			out << buffer_adaptor(v).declare() << ";" << endl;
		}
	}
	void operator()(spe_region* region)
	{
		for_all(region->priv(), this);
	}
};

class define_buff_size {
	ostream& out;
	const string& induction;
	const int unroll;
	const int buffer;
public:
	define_buff_size(ostream& o, const string& i, const int u, const int b):
		out(o), induction(i), unroll(u), buffer(b) {}
	void operator()(const shared_variable* v)
	{
		if (v->depth() > 0 ) {
			string def;

			// User specified buffer overrides fitting buffer to unrolling
			if (buffer) {
				def = "(" + to_string(buffer) + ")";
			}
			else if (unroll) {
				def = "(" + to_string(unroll) + v->math().factor(induction) + ")";
			}
			else {
				def = default_buff_size;
			}
			out << pound_define(buffer_adaptor(v).size(), def).define();
		}
	}
	void operator()(const private_variable* v)
	{
		if (v->depth() > 0 ) {
			out << pound_define(buffer_adaptor(v).size(), default_buff_size).define();
		}
	}

};

class define_region_buff_sizes {
	ostream& out;
public:
	define_region_buff_sizes(ostream& o): out(o) {}
	void operator()(spe_region* region)
	{
		for_all(region->priv(), define_buff_size(out, region->induction(), region->unroll(), region->buffer()));
		for_all(region->shared(), define_buff_size(out, region->induction(), region->unroll(), region->buffer()));
	}
};

class pass_assign {
	stringstream& out;
public:
	pass_assign(stringstream& o): out(o) {}
	void operator()(const region_variable* v)
	{
		out	<< struct_pass_var << v->name() 
			<< " = " << v->definition() << ";" 
			<< endl;
	}
};

void print_ppe(const string& name, sslist& blocks, stringstream& pro, stringstream& ppe_fork, spelist& regions)
{
	ofstream file(name.c_str());
	open_check(file, name);
	
	file << pro.str();

	// After every block of ppe code, we print a fork of spe code, except 
	// for the final block of ppe code, which is the final code section of 
	// the source file. This works because the definition of ppe code is 
	// all code before the #pragma cell section.
	int loop_num = 1;
	sslist::iterator b = blocks.begin();
	spelist::iterator r = regions.begin();
	while (b != blocks.end()) {

		file << (*b)->str();
		if (++b != blocks.end()) {

			string id = to_string(loop_num++);

			stringstream passes;
			for_all((*r)->priv(), pass_assign(passes));
			for_all((*r)->shared(), pass_assign(passes));
			for_all((*r)->reductions(), pass_assign(passes));

			string fork_str = regex_replace(ppe_fork.str(), regex(loop_hook), id);
			string passes_str = regex_replace(passes.str(), regex(loop_hook), id);
			file << regex_replace(fork_str, regex(pass_assign_hook), passes_str);

			if ((*r)->reduction_op() != "") {
				string str = regex_replace(
						mmgp_reduction, 
						regex(var_hook), 
						"&" + (*((*r)->reductions().begin()))->definition());
                                str = str + string(id) + ");\n";
				file << regex_replace(str, regex(op_hook), (*r)->reduction_op());
			}
			else {
				file << regex_replace(mmgp_wait, regex(loop_hook), id);
			}

			++r;
		}
	}
}

void print_pass_struct(spelist& regions)
{	
	ofstream out(pass_struct_oname.c_str());
	open_check(out, pass_struct_oname);

	stringstream pass;
	parse_generic(pass, pass_struct_iname);

	stringstream vars;
	for_all(regions, define_pass_struct(vars));

	out << regex_replace(pass.str(), regex(pass_struct_hook), vars.str());
}

void print_spe(const string& name, stringstream& spe_dec, stringstream& spe_main, spelist& regions)
{
	ofstream file(name.c_str());
	open_check(file, name);

	stringstream ss;
	ss << spe_dec.str() << endl;

	/*
	for_all(regions, define_region_buff_sizes(ss));
	for_all(regions, define_in_and_out_buffers(ss));
	for_all(regions, define_inout_buffers(ss));
	for_all(regions, define_private_buffers(ss));
	*/

	sslist cases;
	for_all(regions, print_region(ss, cases));

	stringstream cases_ss;
	for_all(cases, sstream_out(cases_ss));
	ss << regex_replace(spe_main.str(), regex(case_hook), cases_ss.str());

	file << ss.str();
}

string parse_command_line(int argc, char* argv[])
{
	string src_name;

	try {
		options_description desc("Allowed options");
		desc.add_options()
		("help,h", "print usage message")
		("infile,i", value<string>(&src_name), "input filename")
		("astout,a", value<bool>(&print_ast)->zero_tokens()->default_value(false), "print the ast")
		("speinclude,s", value<string>(&inc_name), "filename to include in spe source")
		("num_threads,n", value<int>(&num_threads)->default_value(6), "set number of SPE threads");

		positional_options_description p;
		p.add("infile", -1);

		variables_map vm;
		store(command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
		notify(vm);
	
		if (vm.count("help")) {  
			cout << desc << "\n";
			exit(0);
		}
	} 

	catch (exception& e) {
		cerr << "options related exception: " << e.what() << endl;
		exit(1);
	}

	return src_name;
}

int main(int argc, char* argv[])
{
	const string src_name = parse_command_line(argc, argv);

	// Do all of the input and parsing.
	sslist ppe_src_blocks;
	spelist spe_regions;
	parse_src(src_name, ppe_src_blocks, spe_regions);

	stringstream ppe_fork;
	stringstream ppe_prolouge;
	stringstream spe_declarations;
	stringstream spe_main;
	stringstream mmgp_c;

	parse_generic(ppe_fork, ppe_fork_iname);
	parse_generic(ppe_prolouge, ppe_prolouge_iname);
	parse_generic(spe_declarations, spe_declarations_iname);
        if (inc_name != "") {
		parse_generic(spe_declarations, inc_name);
	}
	parse_generic(spe_main, spe_main_iname);
	parse_generic(mmgp_c, mmgp_c_iname);

	// Do all of the output and code generation.
	string no_dot = src_name;
	size_t pos;
	if ((pos = no_dot.find(".cellgen")) == string::npos) {
		cerr << "error: file " << src_name << " is not a cellgen source." << endl;
		exit(1);
	}
	no_dot.replace(pos, strlen(".cellgen"), "");
	const string ppe_oname = no_dot + ".c";
	const string spe_oname = "spu/" + no_dot + "_spe.c";

	string num_kernels = to_string<size_t>(spe_regions.size());
	replace_n_print_file(mmgp_h_iname, mmgp_h_oname, num_kernels_hook, num_kernels);
	replace_n_print_file(mmgp_c_iname, mmgp_c_oname, program_name_hook, no_dot);
	replace_n_print_file(mmgp_spu_h_iname, mmgp_spu_h_oname, num_kernels_hook, num_kernels);

	print_pass_struct(spe_regions);
	print_ppe(ppe_oname, ppe_src_blocks, ppe_prolouge, ppe_fork, spe_regions);

	print_spe(spe_oname, spe_declarations, spe_main, spe_regions);
	
	system(string("cp " + spe_profiler + " spu/").c_str());
	system(string("cp " + cellstrider_dma_iname + " spu/").c_str());

	system(string("indent " + ppe_oname + " " + spe_oname).c_str());
	system("rm *~ spu/*~");

	return 0;
}

