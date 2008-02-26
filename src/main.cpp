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

#include <boost/program_options.hpp>
#include <boost/regex.hpp>

using namespace std;
using namespace boost;
using namespace boost::program_options;

#include "cellgen_grammar.h"
#include "variable.h"
#include "spe_region.h"
#include "streamops.h"

const string path			= "/home/scschnei/code/cellgen/template_code/";
const string ppe_fork_iname		= path + "ppe_fork.c";
const string ppe_prolouge_iname		= path + "ppe_prolouge.c";
const string spe_declarations_iname	= path + "spe_prolouge.c";
const string spe_main_iname		= path + "spe_epilouge.c";
const string pass_struct_oname		= "pass_struct.h";
const string pass_struct_iname		= path + pass_struct_oname;

const string mmgp_h_oname		= "MMGP.h";
const string mmgp_c_oname		= "MMGP.c";
const string mmgp_h_iname		= path + "MMGP/" + mmgp_h_oname;
const string mmgp_c_iname		= path + "MMGP/" + mmgp_c_oname;
const string mmgp_spu_h_iname		= path + "MMGP/MMGP_spu.h";

const string loop_hook			= "LOOP_ID";
const string pass_struct_hook		= "STRUCT_PASS_VARIABLE";
const string pass_assign_hook		= "PASS_ASSIGNMENT";
const string program_name_hook		= "PROGRAM_NAME";
const string buffer_hook		= "BUFFER_SIZE";
const string case_hook			= "CASES";
const string var_hook			= "VAR";
const string op_hook			= "OP";

const string pass_assign		= "((struct pass_t *)" + pass_var + "[__i" + loop_hook + "])->";
const string compute_bounds		= "compute_bounds(&pass.SPE_start, &pass.SPE_stop);";
const string mmgp_init			= "MMGP_init(6);";
const string mmgp_create_threads	= "MMGP_create_threads();";
const string mmgp_spe_stop		= "MMGP_SPE_stop();";
const string mmgp_wait			= "MMGP_wait_SPE(" + loop_hook + ");\n";
const string mmgp_reduction		= "MMGP_reduction(" + var_hook + "," + op_hook + ");\n";

bool print_ast = false;

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

// XFORM CHANGE
class codeout {
	ostream& out;

public:
	codeout(ostream& o): out(o) {}
	void operator()(const tree_node_t& node)
	{
		string str;
		if (node.value.value() == "") {
			str = string(node.value.begin(), node.value.end());
		}
		else {
			str = node.value.value();
		}

		out << str << " ";
		for_all(node.children, this);
	}
};

/*
class codeout {
	ostream& out;

public:
	codeout(ostream& o): out(o) {}
	void operator()(const tree_node_t& node)
	{
		string init = string(node.value.begin(), node.value.end());
		init = inv_accumulate_all(node.value.value(), init, node);

		out << init << " ";
		for_all(node.children, this);
	}
};
*/

class make_case_sig {
	stringstream& file;
	stringstream& casest;

public:
	make_case_sig(stringstream& f, stringstream& c): file(f), casest(c) {}
	void operator()(const region_variable* v)
	{
		casest	<< v->actual() << ",";
		file	<< v->formal() << ",";
	}
};


string make_writebacks(string val, const region_variable* v)
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
			<< compute_bounds << "loop" << loop_num << "(" ; 
		file	<< "void loop" << loop_num << "(";
		++loop_num;

		stringstream f;
		stringstream c;
		for_all(region->priv(), make_case_sig(f, c));
		for_all(region->shared(), make_case_sig(f, c));
		for_all(region->reductions(), make_case_sig(f, c));

		string writebacks = accumulate_all(region->reductions(), string(""), make_writebacks);

		*casest	<< c.str().replace(c.str().find_last_of(","), strlen(","), "") << "); " << endl
			<< writebacks << endl
			<< mmgp_spe_stop << "break;" << endl;
		cases.push_back(casest);

		file << f.str().replace(f.str().find_last_of(","), strlen(","), "");
		file << ")" << endl;

		for_all(region->ast()->children, codeout(file));
	}
};

void parse_generic(stringstream& stream, const string& str)
{
	ifstream ifile(str.c_str());
	open_check(ifile, str);
	file_to_stream(stream, ifile);
}

void print_file(const string& ifile_name, const string& ofile_name, const string& program_name)
{
	ifstream ifile(ifile_name.c_str());
	open_check(ifile, ifile_name);

	stringstream ss;
	file_to_stream(ss, ifile);

	ofstream ofile(ofile_name.c_str());
	open_check(ofile, ofile_name);
	ofile << regex_replace(ss.str(), regex(program_name_hook), program_name);
}

void print_mmgp_c(stringstream& mmgp_c, const string& ofile_name, const string& program_name)
{
	ofstream ofile(ofile_name.c_str());
	open_check(ofile, ofile_name);
	ofile << regex_replace(mmgp_c.str(), regex(program_name_hook), program_name);
}

class unique_variables {
	symtbl& unique;
public:
	unique_variables(symtbl& u): unique(u) {}
	void operator()(region_variable* v)
	{
		if (unique.find(v->name()) == unique.end()) {
			unique[v->name()] = v;
		}
	}
};

class all_unique_variables {
	symtbl& unique;
public:
	all_unique_variables(symtbl& u): unique(u) {}
	void operator()(const spe_region* region)
	{
		for_all(region->priv(), unique_variables(unique));
		for_all(region->shared(), unique_variables(unique));
		for_all(region->reductions(), unique_variables(unique));
	}
};

class in_and_out_unique_variables {
	symtbl& unique;
public:
	in_and_out_unique_variables(symtbl& u): unique(u) {}
	void operator()(spe_region* region)
	{
		for_all(region->in(), unique_variables(unique));
		for_all(region->out(), unique_variables(unique));
	}
};

class inout_unique_variables {
	symtbl& unique;
public:
	inout_unique_variables(symtbl& u): unique(u) {}
	void operator()(spe_region* region)
	{
		for_all(region->inout(), unique_variables(unique));
	}
};

class make_pass_struct {
	stringstream& ss;
public:
	make_pass_struct(stringstream& s): ss(s) {}
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

class make_buffers {
	ostream& out;
	const size_t depth;
public:
	make_buffers(ostream& o, size_t d): out(o), depth(d) {}
	void operator()(const region_variable* v)
	{
		out << buffer_adaptor(v, depth).declare() << ";" << endl;
	}
};

class make_in_and_out_buffers {
	stringstream& out;
public:
	make_in_and_out_buffers(stringstream& o): out(o) {}
	void operator()(spe_region* region)
	{
		for_all(region->in(), make_buffers(out, buffer_adaptor::dbl));
		for_all(region->out(), make_buffers(out, buffer_adaptor::dbl));
	}
};

class make_inout_buffers {
	stringstream& out;
public:
	make_inout_buffers(stringstream& o): out(o) {}
	void operator()(spe_region* region)
	{
		for_all(region->inout(), make_buffers(out, buffer_adaptor::triple));
	}

};

class make_private_buffers {
	ostream& out;
public:
	make_private_buffers(ostream& o): out(o) {}
	void operator()(const region_variable* v)
	{
		if (v->is_pointer()) {
			out << buffer_adaptor(v, buffer_adaptor::single).declare() << ";" << endl;
		}
	}
	void operator()(const spe_region* region)
	{
		for_all(region->priv(), this);
	}
};

class make_pass_assign {
	stringstream& out;
public:
	make_pass_assign(stringstream& o): out(o) {}
	void operator()(const region_variable* v)
	{
		out	<< pass_assign << v->name() 
			<< " = " << v->alias() << ";" 
			<< endl;
	}
};

void print_ppe(const string& name, sslist& blocks, stringstream& pro, stringstream& ppe_fork,
		spelist& regions)
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

			stringstream id_ss;
			id_ss << loop_num++;

			stringstream passes;
			for_all((*r)->priv(), make_pass_assign(passes));
			for_all((*r)->shared(), make_pass_assign(passes));
			for_all((*r)->reductions(), make_pass_assign(passes));

			string fork_str = regex_replace(ppe_fork.str(), regex(loop_hook), id_ss.str());
			string passes_str = regex_replace(passes.str(), regex(loop_hook), id_ss.str());
			file << regex_replace(fork_str, regex(pass_assign_hook), passes_str);

			if ((*r)->reduction_op() != "") {
				string str = regex_replace(
						mmgp_reduction, 
						regex(var_hook), 
						"&" + (*((*r)->reductions()->begin()))->alias());
				file << regex_replace(str, regex(op_hook), (*r)->reduction_op());
			}
			else {
				file << regex_replace(mmgp_wait, regex(loop_hook), id_ss.str());
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

	/*
	symtbl unique;
	for_all(regions, all_unique_variables(unique));
	*/

	stringstream vars;
	for_all(regions, make_pass_struct(vars));

	out << regex_replace(pass.str(), regex(pass_struct_hook), vars.str());
}

void print_spe(const string& name, stringstream& spe_dec, stringstream& spe_main, spelist& regions)
{
	ofstream file(name.c_str());
	open_check(file, name);

	stringstream ss;
	ss << regex_replace(spe_dec.str(), regex(buffer_hook), buff_size.declare());

	/*
	symtbl in_and_out_unique;
	for_all(regions, in_and_out_unique_variables(in_and_out_unique));

	symtbl inout_unique;
	for_all(regions, inout_unique_variables(inout_unique));

	symtbl in_and_out_unique_final;
	set_difference(in_and_out_unique.begin(), in_and_out_unique.end(),
			inout_unique.begin(), inout_unique.end(),
			inserter(in_and_out_unique_final, in_and_out_unique_final.begin()));
	*/

	for_all(regions, make_in_and_out_buffers(ss));
	for_all(regions, make_inout_buffers(ss));
	for_all(regions, make_private_buffers(ss));

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
		("astout,a", value<bool>(&print_ast)->zero_tokens()->default_value(false), "print the ast");

		positional_options_description p;
		p.add("infile", 1);

		variables_map vm;
		store(command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
		notify(vm);
	
		if (vm.count("help")) {  
			cout << desc << "\n";
			return 0;
		}
	} 

	catch(exception& e) {
		cerr << "error: exception " << e.what() << endl;
		exit(1);
	}

	return src_name;
}

int main(int argc, char* argv[])
{
	string src_name = parse_command_line(argc, argv);

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

	print_file(mmgp_h_iname, mmgp_h_oname, no_dot);
	print_file(mmgp_c_iname, mmgp_c_oname, no_dot);

	print_pass_struct(spe_regions);
	print_ppe(no_dot + "_ppe.c", ppe_src_blocks, ppe_prolouge, ppe_fork, 
			spe_regions);
	print_spe("spu/" + no_dot + "_spe.c", spe_declarations, spe_main, 
			spe_regions);
	
	system(string("cp " + mmgp_spu_h_iname + " spu/").c_str());

	return 0;
}

