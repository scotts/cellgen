/*
 * Scott Schneider
 *
 * Cellgen: pseudo-openmp support for the Cell.
 * http://www.cs.vt.edu/~scschnei/cellgen
 *
 * This code is under GPL 2.0. See the file COPYING.
 */

#include <cassert>
#include <iostream>
#include <fstream>
#include <sstream>
#include <list>
using namespace std;

#include <boost/program_options.hpp>
#include <boost/regex.hpp>
using namespace boost::program_options;

#include "cellgen_grammar.h"
#include "variable.h"
#include "spe_region.h"
#include "streamops.h"
#include "utility.h"

const string pass_struct_oname		= "pass_struct.h";

// TEMPLATE_DIR is fed from the makefile
const string path			= TEMPLATE_DIR;
const string cellgen_lib_c		= path + "cellgen.c";
const string cellgen_lib_h		= path + "cellgen.h";
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
const string spe_stop_fn		= "spe_stop(";
const string wait_for_spes		= "wait_for_spes(" + loop_hook + ");\n";
const string spe_reduction		= "spe_reduction(" + var_hook + "," + op_hook + ","; // + loop_hook + ");\n";

struct cmdline_options {
	string src_name;
	bool print_pt;
	int num_threads;
	string inc_name;
	string spe_inc_name;

	cmdline_options(): print_pt(false), num_threads(0) {}
};

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
		throw user_error("cannot open " + name);
	}
}

class codeout {
	ostream& out;

public:
	codeout(ostream& o): out(o) {}
	void operator()(const pt_node& node)
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
	void operator()(const reduction_variable* v)
	{
		casest	<< "(" << v->type() << "*)" << v->actual() << ",";
		file	<< v->formal() << ",";
	}

	void operator()(const region_variable* v)
	{
		casest	<< v->actual() << ",";
		file	<< v->formal() << ",";
	}
};

string reduction_writeback(string val, const region_variable* v)
{
	return val + "sig.result = " + pass_var + "." + v->unique_name() + ";\n";
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
			<< spe_stop_fn << loop_num << "); " << "break;" << endl;
		cases.push_back(casest);

		file << f.str().replace(f.str().find_last_of(","), strlen(","), "");
		file << ") " << endl;

		for_all(region->pt_root()->children, codeout(file));

		++loop_num;
	}
};

void parse_generic(stringstream& stream, const string& str)
{
	ifstream ifile(str.c_str());
	open_check(ifile, str);
	file_to_stream(stream, ifile);
}

void replace_print_file(const string& ifile_name, const string& ofile_name, const string& hook, const string& replace, const int num_threads)
{
	ifstream ifile(ifile_name.c_str());
	open_check(ifile, ifile_name);

	stringstream ss;
	file_to_stream(ss, ifile);

	ofstream ofile(ofile_name.c_str());
	open_check(ofile, ofile_name);
	ofile << regex_replace(regex_replace(ss.str(), regex(hook), replace), regex(num_threads_hook), to_string(num_threads));
}

class define_pass_struct {
	stringstream& ss;
public:
	define_pass_struct(stringstream& s): ss(s) {}
	void operator()(const region_variable* v)
	{
		ss << "\t" << v->unique_declare() << ";" << endl;
	}

	void operator()(spe_region* region)
	{
		for_all(region->priv(), this);
		for_all(region->shared(), this);
		for_all(region->reductions(), this);
	}
};

class pass_assign {
	stringstream& out;
public:
	pass_assign(stringstream& o): out(o) {}
	void operator()(const region_variable* v)
	{
		out	<< struct_pass_var << v->unique_name() 
			<< " = (" << v->type() << ")" << v->definition() << ";" 
			<< endl;
	}
};

void print_ppe(const string& name, sslist& blocks, stringstream& pro, stringstream& ppe_fork, spelist& regions, const string& inc_name)
{
	ofstream file(name.c_str());
	open_check(file, name);
	
	if (inc_name != "") {
		file << "#include \"" << inc_name << "\" \n" << endl;
	}
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

			string id = to_string(loop_num);

			//file << (*r)->estimate() << loop_num << ");" << endl;

			stringstream passes;
			for_all((*r)->priv(), pass_assign(passes));
			for_all((*r)->shared(), pass_assign(passes));
			for_all((*r)->reductions(), pass_assign(passes));

			string fork_str = regex_replace(ppe_fork.str(), regex(loop_hook), id);
			string passes_str = regex_replace(passes.str(), regex(loop_hook), id);
			file << regex_replace(fork_str, regex(pass_assign_hook), passes_str);

			if ((*r)->reduction_op() != "") {
				string str = regex_replace(
						spe_reduction, 
						regex(var_hook), 
						"&" + (*((*r)->reductions().begin()))->definition());
                                str = str + string(id) + ");\n";
				file << regex_replace(str, regex(op_hook), (*r)->reduction_op());
			}
			else {
				file << regex_replace(wait_for_spes, regex(loop_hook), id);
			}

			++r;
			++loop_num;
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

void print_spe(const string& name, stringstream& spe_dec, stringstream& spe_main, spelist& regions, const string& inc_name, const string& spe_inc_name)
{
	ofstream file(name.c_str());
	open_check(file, name);

	stringstream ss;
	if (inc_name != "") {
		ss << "#include \"../" << inc_name << "\"" << endl;
	}
	if (spe_inc_name != "") {
		ss << "#include \"../" << spe_inc_name << "\"" << endl;
	}
	ss << spe_dec.str() << endl;

	sslist cases;
	for_all(regions, print_region(ss, cases));

	stringstream cases_ss;
	for_all(cases, sstream_out(cases_ss));
	ss << regex_replace(spe_main.str(), regex(case_hook), cases_ss.str());

	file << ss.str();
}

cmdline_options parse_command_line(int argc, char* argv[])
{
	cmdline_options options;

	try {
		options_description desc("Allowed options");
		desc.add_options()
		("help,h", "print usage message")
		("infile,i", value<string>(&options.src_name), "input filename")
		("ptout,p", value<bool>(&options.print_pt)->zero_tokens()->default_value(false), "print the parse tree")
		("Include,I", value<string>(&options.inc_name), "filename to include in ppe and spe source")
		("spe-include,s", value<string>(&options.spe_inc_name), "filename to include in spe source only")
		("num_threads,n", value<int>(&options.num_threads)->default_value(0), "set number of SPE threads; default is all physical");

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
		throw user_error(string("bad options, ") + e.what());
	}

	return options;
}

int main(int argc, char* argv[])
{
	try {
		const cmdline_options opts = parse_command_line(argc, argv);

		// Do all of the input and parsing.
		sslist ppe_src_blocks;
		spelist spe_regions;

		parse_src(opts.src_name, ppe_src_blocks, spe_regions, opts.print_pt);

		stringstream ppe_fork;
		stringstream ppe_prolouge;
		stringstream spe_declarations;
		stringstream spe_main;
		stringstream mmgp_c;

		parse_generic(ppe_fork, ppe_fork_iname);
		parse_generic(ppe_prolouge, ppe_prolouge_iname);
		parse_generic(spe_declarations, spe_declarations_iname);
		parse_generic(spe_main, spe_main_iname);
		parse_generic(mmgp_c, mmgp_c_iname);

		// Do all of the output and code generation.
		string no_dot = opts.src_name;
		size_t pos;
		if ((pos = no_dot.find(".cellgen")) == string::npos) {
			throw user_error("file " + opts.src_name + " is not a cellgen source.");
		}
		no_dot.replace(pos, strlen(".cellgen"), "");
		const string ppe_oname = no_dot + ".c";
		const string spe_oname = "spu/" + no_dot + "_spe.c";

		string num_kernels = to_string<size_t>(spe_regions.size());
		replace_print_file(mmgp_h_iname, mmgp_h_oname, num_kernels_hook, num_kernels, opts.num_threads);
		replace_print_file(mmgp_c_iname, mmgp_c_oname, program_name_hook, no_dot, opts.num_threads);
		replace_print_file(mmgp_spu_h_iname, mmgp_spu_h_oname, num_kernels_hook, num_kernels, opts.num_threads);

		print_pass_struct(spe_regions);
		print_ppe(ppe_oname, ppe_src_blocks, ppe_prolouge, ppe_fork, spe_regions, opts.inc_name);

		print_spe(spe_oname, spe_declarations, spe_main, spe_regions, opts.inc_name, opts.spe_inc_name);
	
		system(string("cp " + spe_profiler + " spu/").c_str());
		system(string("cp " + cellstrider_dma_iname + " spu/").c_str());
		system(string("cp " + cellgen_lib_c + " .").c_str());
		system(string("cp " + cellgen_lib_h + " .").c_str());

		system(string("indent " + ppe_oname + " " + spe_oname).c_str());
		system("rm *~ spu/*~");
	}
	catch (user_error e) {
		cerr << "error: " << e.err << endl;
		exit(1);
	}

	return 0;
}

