/*
 * Scott Schneider
 *
 * TODO:	1. identify multiplicative indices
 * 		2. output final DMA	
 */
#include <cassert>
#include <iostream>
#include <fstream>
#include <iterator>
#include <sstream>
#include <list>

#include <boost/program_options.hpp>
#include <boost/regex.hpp>

#include <boost/spirit/core.hpp>
#include <boost/spirit/core/composite/directives.hpp>
#include <boost/spirit/symbols.hpp>
#include <boost/spirit/utility/confix.hpp>
#include <boost/spirit/iterator/file_iterator.hpp>

using namespace std;
using namespace boost;
using namespace boost::program_options;
using namespace boost::spirit;

#include "c_grammar.h"
#include "parse_tree.h"
#include "variable.h"
#include "spe_region.h"
#include "streamops.h"
#include "skip.h"

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

typedef list<stringstream*>	sslist;

const c_compound_grammar c_compound;
const skip_grammar skip;

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
		fmap(this, node.children);
	}
};

class make_case_sig {
	stringstream& file;
	stringstream& casest;

public:
	make_case_sig(stringstream& f, stringstream& c): file(f), casest(c) {}
	void operator()(const c_variable* cv)
	{
		casest	<< cv->actual() << ",";
		file	<< cv->formal() << ",";
	}
};

class make_writebacks {
	stringstream& w;

public:
	make_writebacks(stringstream& _w): w(_w) {}
	void operator()(const c_variable* cv)
	{
		w << "signal.result = " << pass_var << "." << cv->name() << ";" << endl;
	}
};

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
		fmap(make_case_sig(f, c), region->priv());
		fmap(make_case_sig(f, c), region->shared());
		fmap(make_case_sig(f, c), region->reductions());

		stringstream writebacks;
		fmap(make_writebacks(writebacks), region->reductions());

		*casest	<< c.str().replace(c.str().find_last_of(","), strlen(","), "") << "); " << endl
			<< writebacks.str() << endl
			<< mmgp_spe_stop << "break;" << endl;
		cases.push_back(casest);

		file << f.str().replace(f.str().find_last_of(","), strlen(","), "");
		file << ")" << endl;

		fmap(codeout(file), region->ast()->children);
	}
};

template <class T>
class vars_op {
	cvarlist*& vars;
	mutable symtbl& tbl;

public:
	vars_op(cvarlist*& v, symtbl& t): vars(v), tbl(t)
	{
		vars = new cvarlist;	
	}

	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}

		string type, local, equals, alias;
		ss >> type >> local >> equals >> alias;

		const T* cv = new T(type, local, alias);
		vars->push_back(cv);
		tbl[local] = cv;
	}

	cvarlist* pickup()
	{
		cvarlist* temp = vars;
		vars = new cvarlist;
		return temp;
	}
};

struct op_op {
	string& op;

	op_op(string& s): op(s) {}
	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}
		op = ss.str();
	}
};

template <class T>
class push_back_op {
	T& container;
	
public:
	push_back_op(T& c): container(c) {}
	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream* ss = new stringstream;
		while (first != last) {
			*ss << *first++;
		}
		container.push_back(ss);
	}
};

struct sstream_op {
	stringstream*& pickup;

	sstream_op(stringstream*& p): pickup(p) {}
	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream* ss = new stringstream;
		while (first != last) {
			*ss << *first++;
		}
		pickup = ss;
	}
};

class assign_cvar {
	cvarlist*& vars;
	string local;

public:
	assign_cvar(cvarlist*& v, string l): vars(v), local(l) {}
	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}

		vars->push_back(new c_variable("int", local, ss.str()));
	}
};

class create_spe_region {
	spelist&			regions;
	sstream_op&			loop_op;
	assign_cvar&			start_op;
	assign_cvar&			stop_op;
	vars_op<c_variable>&		priv_op;
	vars_op<shared_variable>&	shared_op;
	vars_op<reduction_variable>&	reduce_def_op;
	op_op&				reduce_op_op;

public:
	create_spe_region(
			spelist& r, 
			sstream_op& l, 
			assign_cvar& start, 
			assign_cvar& stop, 
			vars_op<c_variable>& p, 
			vars_op<shared_variable>& s, 
			vars_op<reduction_variable>& rd,
			op_op& ro):
		regions(r), loop_op(l), start_op(start), stop_op(stop),
		priv_op(p), shared_op(s), reduce_def_op(rd), reduce_op_op(ro)
		{}
	void operator()(fileiter first, const fileiter& last) const
	{
		spe_region* r = new spe_region(	priv_op.pickup(),
						shared_op.pickup(),
						loop_op.pickup,
						reduce_def_op.pickup(),
						reduce_op_op.op);
		regions.push_back(r);
	}
};

/*
 * 3: Allow program to start with cell region.
 */
struct cellgen_grammar: public grammar<cellgen_grammar> {
	push_back_op<sslist>		ppe_op;
	sstream_op			loop_op;
	assign_cvar			start_op;
	assign_cvar			stop_op;
	vars_op<c_variable>		priv_op;
	vars_op<shared_variable>	shared_op;
	vars_op<reduction_variable>	reduce_def_op;
	op_op				reduce_op_op;
	create_spe_region		region_op;

	stringstream*	loop_pickup;
	cvarlist*	priv_vars;
	cvarlist*	shared_vars;
	cvarlist*	reduce_vars;
	string		op;

	cellgen_grammar(sslist& ppe, spelist& regions, symtbl& s, symtbl& p, symtbl& r):
		ppe_op(ppe),
		loop_op(loop_pickup),
		start_op(priv_vars, "SPE_start"),
		stop_op(priv_vars, "SPE_stop"),
		priv_op(priv_vars, p),
		shared_op(shared_vars, s),
		reduce_def_op(reduce_vars, r),
		reduce_op_op(op),
		region_op(regions,
			loop_op, 
			start_op, 
			stop_op, 
			priv_op, 
			shared_op,
			reduce_def_op,
			reduce_op_op
			)
		{}

	template <typename ScannerT>
	struct definition {
		rule<ScannerT> 
			program, options, ppe_code, spe_code,
			reduction, operation, reduce_list, reduce_dec, reduce_dec_f,
			start_code, start_priv, stop_code, stop_priv,
			priv_code, priv_list, priv_dec, priv_dec_f,
			shared_code, shared_list, shared_dec, shared_dec_f;

		rule<ScannerT, parser_tag<ids::cell_region> >
			pragma_code;

		const rule<ScannerT>& start() const { return program; }

		definition(const cellgen_grammar& self)
		{
			program = 
				ppe_code >> pragma_code >> ppe_code 
				>> *(pragma_code >> ppe_code);

			ppe_code = no_node_d[
					lexeme_d[ *(anychar_p - "#pragma cell") ] 
					[self.ppe_op]
				];

			pragma_code = root_node_d[
					(no_node_d[strlit<>("#pragma cell")] >> *options >> spe_code)
					[self.region_op]
				];

			spe_code = c_compound[self.loop_op];

			options = 
				priv_code 
			| 	start_code
			| 	stop_code
			|	reduction
			| 	shared_code;

			start_code = 
				no_node_d[strlit<>("SPE_start(")] >> start_priv >> no_node_d[chlit<>(')')];

			start_priv = no_node_d[
					lexeme_d[ (*(anychar_p - ')')) ]
					[self.start_op]
				];

			stop_code =
				no_node_d[strlit<>("SPE_stop(")] >> stop_priv >> no_node_d[chlit<>(')')];

			stop_priv = no_node_d[
					lexeme_d[ (*(anychar_p - ')')) ]
					[self.stop_op]
				];

			priv_code =
				no_node_d[strlit<>("private(")] >> priv_list >> no_node_d[chlit<>(')')];

			priv_list =
				priv_dec >> no_node_d[chlit<>(',')] >> priv_list 
			| 	priv_dec_f;

			priv_dec = no_node_d[
					lexeme_d[ (*(anychar_p - ',' - ')')) ]
					[self.priv_op]
				];

			priv_dec_f = no_node_d[
					lexeme_d[ *(anychar_p - ',' - ')') ] 
				];

			reduction = 
				no_node_d[strlit<>("reduction(")] 
					>> operation[self.reduce_op_op] 
					>> no_node_d[strlit<>(":")] 
					>> reduce_list 
				>> no_node_d[strlit<>(")")];

			operation = no_node_d[
				chlit<>('+')
			|	chlit<>('-')
			|	chlit<>('*')
			|	chlit<>('&')
			|	chlit<>('|')
			|	strlit<>("&&")
			|	strlit<>("||")
			];

			reduce_list =
				reduce_dec >> no_node_d[chlit<>(',')] >> reduce_list
			|	reduce_dec_f;

			reduce_dec = no_node_d[
					lexeme_d[ (*(anychar_p - ',' - ')')) ]
					[self.reduce_def_op]
				];

			reduce_dec_f = no_node_d[
					lexeme_d[ *(anychar_p - ',' - ')')]
				];

			shared_code =
				no_node_d[strlit<>("shared(")] >> shared_list >> no_node_d[chlit<>(')')];

			shared_list =
				shared_dec >> no_node_d[chlit<>(',')] >> shared_list 
			| 	shared_dec_f;

			shared_dec = no_node_d[
					lexeme_d[ (*(anychar_p - ',' - ')')) ]
					[self.shared_op]
				];

			shared_dec_f = no_node_d[
					lexeme_d[ *(anychar_p - ',' - ')') ]
				];
		}
	};
};

class astout {
	int level;
	string tabs;

public:
	astout(int l, string t): level(l), tabs(t) {}
	void operator()(const tree_node_t& node)
	{
		string str;
		if (node.value.value() == string()) {
			str = string(node.value.begin(), node.value.end());
		}
		else {
			str = node.value.value();
		}
		string rule = "";

		if (     node.value.id() == ids::for_loop) { 
			rule = "for_loop"; 
		}
		else if (node.value.id() == ids::identifier) { 
			rule = "IDENTIFIER"; 
		}
		else if (node.value.id() == ids::array_index) { 
			rule = "array_index"; 
		}
		else if (node.value.id() == ids::declaration) { 
			rule = "declaration"; 
		}
		else if (node.value.id() == ids::cell_region) { 
			rule = "cell_region"; 
		}
		else if (node.value.id() == ids::compound) { 
			rule = "compound"; 
		}
		else if (node.value.id() == ids::postfix_expression) { 
			rule = "postfix_expression"; 
		}
		else if (node.value.id() == ids::expression_statement) { 
			rule = "expression_statement"; 
		}
		else if (node.value.id() == ids::expression) { 
			rule = "expression"; 
		}
		else if (node.value.id() == ids::statement) { 
			rule = "statement"; 
		}
		else if (node.value.id() == ids::declaration_list) { 
			rule = "declaration_list"; 
		}
		else if (node.value.id() == ids::expression_helper) { 
			rule = "expression_helper"; 
		}
		else if (node.value.id() == ids::relational_expression) { 
			rule = "relational_expression"; 
		}
		else if (node.value.id() == ids::argument_expression_list) { 
			rule = "argument_expression_list"; 
		}
		else if (node.value.id() == ids::postfix_expression_helper) { 
			rule = "postfix_expression_helper"; 
		}
		else if (node.value.id() == ids::int_constant_dec) { 
			rule = "int_constant_dec"; 
		}
		else if (node.value.id() == ids::multiplicative_expression) {
			rule = "multiplicative_expression";
		}
		else if (node.value.id() == ids::multiplicative_expression_helper) {
			rule = "multiplicative_expression_helper";
		}
		else {
			stringstream ss;
			ss << node.value.id();
			rule = ss.str();
		}

		cout << tabs << level << ":" << str << " [" << rule << "]" <<  endl;

		fmap(astout(level + 1, tabs + "  "), node.children);
	}
};

void parse_src(const string& src_name, sslist& ppe_blocks, spelist& spe_regions, 
		symtbl& shared_tbl, symtbl& private_tbl, symtbl& reduce_tbl)
{
	fileiter first(src_name);
	if (!first) {
		cout << "Unable to open " << src_name << endl;
		exit(-1);
	}
	fileiter last = first.make_end();

	cellgen_grammar cg(ppe_blocks, spe_regions, shared_tbl, private_tbl, reduce_tbl);
	tree_parse_info_t* p = new tree_parse_info_t(); // need to make sure the ast persists
	*p = ast_parse<string_factory>(first, last, cg, skip);

	if (!p->full) {
		cerr << "error: parse of " << src_name << " failed." << endl;
		exit(-1);
	}

	root_eval(p->trees, shared_tbl, spe_regions);

	spelist::iterator s = spe_regions.begin();
	for (tree_iterator_t a = (*p->trees.begin()).children.begin(); 
			a != (*p->trees.begin()).children.end(); 
			++a) {
		if ((*a).value.id() == ids::cell_region) {
			(*s)->ast(&(*a));	
			++s;
		}
	}

	if (print_ast) {
		fmap(astout(0, ""), p->trees);
	}
}

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

class make_pass_struct {
	stringstream& ss;

public:
	make_pass_struct(stringstream& s): ss(s) {}

	void operator()(const c_variable* cv)
	{
		ss	<< "\t" 
			<< cv->declare() << ";" << endl;
	}

	void operator()(const spe_region* r)
	{
		fmap(this, r->priv());
		fmap(this, r->shared());
		fmap(this, r->reductions());
	}
};

class make_buffers {
	ostream& out;
	const size_t depth;

public:
	make_buffers(ostream& o): out(o), depth(0) {}
	make_buffers(ostream& o, size_t d): out(o), depth(d) {}

	void operator()(const c_variable* cv)
	{
		out << buffer_variable(cv, depth).declare() << ";" << endl;
	}
	
	void operator()(spe_region* region)
	{
		fmap(make_buffers(out, 2), region->in());
		fmap(make_buffers(out, 2), region->out());
		fmap(make_buffers(out, 3), region->inout());
	}
};

class make_private_buffers {
	ostream& out;

public:
	make_private_buffers(ostream& o): out(o) {}

	void operator()(const c_variable* cv)
	{
		if (cv->is_pointer()) {
			out << buffer_variable(cv, 1).declare() << ";" << endl;
		}
	}

	void operator()(const spe_region* region)
	{
		fmap(this, region->priv());
	}
};

class make_pass_assign {
	stringstream& out;
public:
	make_pass_assign(stringstream& o): out(o) {}

	void operator()(const c_variable* cv)
	{
		out	<< pass_assign << cv->name() 
			<< " = " << cv->alias() << ";" 
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

		/*
		if (loop_num == 1) {
			file << mmgp_init << endl;
			file << mmgp_create_threads << endl;
		}
		*/

		if (++b != blocks.end()) {

			stringstream id_ss;
			id_ss << loop_num++;

			stringstream passes;
			fmap(make_pass_assign(passes), (*r)->priv());
			fmap(make_pass_assign(passes), (*r)->shared());
			fmap(make_pass_assign(passes), (*r)->reductions());

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

	stringstream vars;
	fmap(make_pass_struct(vars), regions);
	out << regex_replace(pass.str(), regex(pass_struct_hook), vars.str());
}

void print_spe(const string& name, stringstream& spe_dec, stringstream& spe_main, spelist& regions)
{
	ofstream file(name.c_str());
	open_check(file, name);

	stringstream ss;
	ss << regex_replace(spe_dec.str(), regex(buffer_hook), buff_size.declare());
	fmap(make_buffers(ss), regions);
	fmap(make_private_buffers(ss), regions);

	sslist cases;
	fmap(print_region(ss, cases), regions);

	stringstream cases_ss;
	fmap(sstream_out(cases_ss), cases);
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
	symtbl shared_tbl;
	symtbl private_tbl;
	symtbl reduce_tbl;
	parse_src(	src_name, 
			ppe_src_blocks, 
			spe_regions, 
			shared_tbl, 
			private_tbl,
			reduce_tbl);

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

