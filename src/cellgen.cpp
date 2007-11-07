/*
 * Scott Schneider
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

const string spe_main_switch		= "switch (received) {";
const string pass_assign		= "((struct pass_t *)pass[__iLOOP_ID])->";
const string compute_bounds		= "compute_bounds(&pass.SPE_start, &pass.SPE_stop);";
const string mmgp_init			= "MMGP_init(6);";
const string mmgp_create_threads	= "MMGP_create_threads();";
const string mmgp_spe_stop		= "MMGP_SPE_stop();";
const string loop_hook			= "LOOP_ID";
const string mmgp_wait			= "MMGP_wait_SPE(" + loop_hook + ");\n";
const string mmgp_reduction		= "MMGP_reduction(VAR, " + loop_hook + ");\n";
const string pass_struct_hook		= "STRUCT_PASS_VARIABLE";
const string pass_assign_hook		= "PASS_ASSIGNMENT";
const string program_name_hook		= "PROGRAM_NAME";
const string buffer_hook		= "BUFFER_SIZE";

#include "c_grammar.h"
#include "parse_tree.h"
#include "variable.h"
#include "streamops.h"
#include "skip.h"

class spe_region;
typedef list<spe_region*>	spelist_t;
typedef list<stringstream*>	sslist_t;

const c_compound_grammar c_compound;
const skip_grammar skip;

class spe_region {
	cvarlist_t* _priv;
	sharedlist_t* _shared;
	stringstream* _text;
	tree_node_t* _ast;

public:
	spe_region(cvarlist_t* v, sharedlist_t* d, stringstream* t):
		_priv(v), _shared(d), _text(t) {}

	cvarlist_t* priv() const
	{
		assert(_priv);
		return _priv;
	}

	sharedlist_t* shared() const
	{
		assert(_shared);
		return _shared;
	}

	stringstream* text() const
	{
		assert(_text);
		return _text;
	}

	void ast(tree_node_t* a)
	{
		_ast = a;
	}

	tree_node_t* ast() const
	{
		assert(_ast);
		return _ast;
	}
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
		cerr << "error: cannot open " << name << endl;
		exit(1);
	}
}

struct make_buffer_declarations {
	stringstream& ss;
	make_buffer_declarations(stringstream& s): ss(s) {}
	void operator()(const shared_variable* sv)
	{
		ss 	<< sv->orig_declare() << ";" << endl
			<< sv->index_declare() << " = 0;" << endl
			<< "mfc_get(" 
				<< sv->buff_name() << "[" << sv->index_name() << "], " 
				<< sv->name() << " + SPE_start,"
				<< "sizeof(" << sv->buff_type() << ")*" << buff_size.actual() << ", "
				<< sv->index_name() << ", 0, 0); \n" 
			<< sv->orig_name() << " = " << sv->buff_name() << "[" << sv->index_name() << "];" << endl;
	}
};

struct i_dma {
	string& str;
	i_dma(string& s): str(s) {}
	void operator()(const shared_variable* sv)
	{
		str = str +
			"if(!(__i__ %" + buff_size.actual() + ")) {\n" +
				"__dblbf = !__dblbf; \n" + 
				"mfc_get(" + 
				sv->buff_name() + "[__dblbf], " +
				sv->name() + buff_size.actual() + ", " +
				"__dblbf, 0, 0); \n" +
				"MMGP_SPE_dma_wait(!__dblbf); \n" +
				sv->orig_name() + " = " + sv->buff_name() + "[!__dblbf]; \n" +
			"} \n";
	}
};

struct add_declarations {
	const string& declarations;
	add_declarations(const string& d): declarations(d) {}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::compound) {
			tree_node_t& lparen = *node.children.begin();
			lparen.value.value("{" + declarations);
		}
	}
};

struct codeout {
	ostream& out;
	codeout(ostream& o): out(o) {}
	void operator()(const tree_node_t& node)
	{
		string str;
		if (node.value.value() == string()) {
			str = string(node.value.begin(), node.value.end());
		}
		else {
			str = node.value.value();
		}

		out << str << " ";
		fmap(*this, node.children);
	}
};

void print_loops(ostream& out, spelist_t& regions, sslist_t& cases)
{
	int loop_num = 1;
	for (spelist_t::iterator r = regions.begin(); r != regions.end(); ++r) {

		stringstream* case_ss = new stringstream;
		*case_ss << "\t\t\tcase " << loop_num << ": " 
			<< compute_bounds << "loop" << loop_num << "(" ; 
		out << "void loop" << loop_num << "(";
		++loop_num;

		// Can this be done more elegantly?
		cvarlist_t::iterator v = (*r)->priv()->begin();
		while (v != (*r)->priv()->end()) {
			*case_ss << "pass." << (*v)->name();
			out << (*v)->declare();

			if (++v != (*r)->priv()->end()) {
				out << ", ";
				*case_ss << ", ";
			}
		}
		sharedlist_t::iterator d = (*r)->shared()->begin();
		while (d != (*r)->shared()->end()) {
			if (d == (*r)->shared()->begin()) {
				out << ", ";
				*case_ss << ", ";
			}
			*case_ss << "pass." << (*d)->name();
			out << (*d)->declare();
			if (++d != (*r)->shared()->end()) {
				out << ", ";
				*case_ss << ", ";
			}
		}

		out << ")" << endl;
		stringstream decs;
		fmap(make_buffer_declarations(decs), (*r)->shared());
		fmap(add_declarations(decs.str()), (*r)->ast()->children);
		fmap(codeout(out), (*r)->ast()->children);

		string temp;
		fmap(i_dma(temp), (*r)->shared());

		*case_ss << "); " << mmgp_spe_stop << "break;" << endl;
		cases.push_back(case_ss);
	}
}

void print_cases(ofstream& out, sslist_t& list)
{
	fmap(sstream_out(out), list);
}

template <class T>
struct vars_op {
	list<const T*>*& vars;
	const symbols<const T*>& tbl;

	vars_op(list<const T*>*& v, const symbols<const T*>& t): vars(v), tbl(t)
	{
		vars = new list<const T*>;	
	}

	void operator()(fileiter_t first, const fileiter_t& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}

		string type, local, equals, actual;
		ss >> type >> local >> equals >> actual;

		const T* cv = new T(type, local, actual);
		vars->push_back(cv);
		tbl.add(local.c_str(), cv);
	}

	list<const T*>* pickup()
	{
		list<const T*>* temp = vars;
		vars = new list<const T*>;
		return temp;
	}
};

template <class T>
struct push_back_op {
	T& container;

	push_back_op(T& c): container(c) {}
	void operator()(fileiter_t first, const fileiter_t& last) const
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
	void operator()(fileiter_t first, const fileiter_t& last) const
	{
		stringstream* ss = new stringstream;
		while (first != last) {
			*ss << *first++;
		}
		pickup = ss;
	}
};

struct assign_cvar {
	cvarlist_t*& vars;
	string local;

	assign_cvar(cvarlist_t*& v, string l): vars(v), local(l) {}
	void operator()(fileiter_t first, const fileiter_t& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}

		vars->push_back(new c_variable("int", local, ss.str()));
	}
};

class create_spe_region {
	spelist_t&			regions;
	sstream_op&			loop_op;
	assign_cvar&			start_op;
	assign_cvar&			stop_op;
	vars_op<c_variable>&		priv_op;
	vars_op<shared_variable>&	shared_op;

public:
	create_spe_region(spelist_t& r, sstream_op& l, assign_cvar& start, 
			assign_cvar& stop, vars_op<c_variable>& p, 
			vars_op<shared_variable>& s):
		regions(r), loop_op(l), start_op(start), stop_op(stop),
		priv_op(p), shared_op(s)
	{}
	void operator()(fileiter_t first, const fileiter_t& last) const
	{
		spe_region* r = new spe_region(	priv_op.pickup(),
						shared_op.pickup(),
						loop_op.pickup);
		regions.push_back(r);
	}
};

/*
 * 3: Allow program to start with cell region.
 */
struct cellgen_grammar: public grammar<cellgen_grammar> {
	push_back_op<sslist_t>		ppe_op;
	sstream_op			loop_op;
	assign_cvar			start_op;
	assign_cvar			stop_op;
	vars_op<shared_variable>	shared_op;
	vars_op<c_variable>		priv_op;
	create_spe_region		region_op;

	stringstream*	loop_pickup;
	cvarlist_t*	priv_vars;
	sharedlist_t*	shared_vars;

	cellgen_grammar(sslist_t& ppe, spelist_t& regions, const sharedtbl_t& s, const symtbl_t& p):
		ppe_op(ppe),
		loop_op(loop_pickup),
		start_op(priv_vars, "SPE_start"),
		stop_op(priv_vars, "SPE_stop"),
		shared_op(shared_vars, s),
		priv_op(priv_vars, p),
		region_op(regions,
			loop_op, 
			start_op, 
			stop_op, 
			priv_op, 
			shared_op)
		{}

	template <typename ScannerT>
	struct definition {
		rule<ScannerT> 
			program, options, ppe_code, spe_code,
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

class spe_main_grammar: public grammar<spe_main_grammar> {
	stream_writer prolouge_sw;
	stream_writer epilouge_sw;

public:
	spe_main_grammar(stringstream& pro, stringstream& ep):
		prolouge_sw(pro), epilouge_sw(ep) {}

	template <typename ScannerT>
	struct definition {
		rule<ScannerT> program, prolouge, epilouge;

		const rule<ScannerT>& start() const { return program; }

		definition(const spe_main_grammar& self)
		{
			program	=	prolouge >> spe_main_switch.c_str() >> epilouge;
			prolouge =	(*(anychar_p - spe_main_switch.c_str()))
					[self.prolouge_sw];
			epilouge =	(*anychar_p)
					[self.epilouge_sw];
		}
	};
};

struct astout {
	int level;
	string tabs;
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
		else {
			stringstream ss;
			ss << node.value.id();
			rule = ss.str();
		}

		cout << tabs << level << ":" << str << " [" << rule << "]" <<  endl;

		fmap(astout(level + 1, tabs + "  "), node.children);
	}
};

void parse_src(const string& src_name, sslist_t& ppe_blocks, spelist_t& spe_regions, 
		const sharedtbl_t& shared_tbl, const symtbl_t& private_tbl)
{
	fileiter_t first(src_name);
	if (!first) {
		cout << "Unable to open " << src_name << endl;
		exit(-1);
	}
	fileiter_t last = first.make_end();

	cellgen_grammar cg(ppe_blocks, spe_regions, shared_tbl, private_tbl);
	tree_parse_info_t* p = new tree_parse_info_t(); // need to make sure the ast persists
	*p = ast_parse<string_factory_t>(first, last, cg, skip);

	if (!p->full) {
		cerr << "error: parse of " << src_name << " failed." << endl;
		exit(-1);
	}

	root_eval(p->trees, shared_tbl);

	spelist_t::iterator s = spe_regions.begin();
	for (tree_iterator_t a = (*p->trees.begin()).children.begin(); 
			a != (*p->trees.begin()).children.end(); 
			++a) {
		if ((*a).value.id() == ids::cell_region) {
			(*s)->ast(&(*a));	
			++s;
		}
	}

	//fmap(astout(0, ""), p->trees);
}

void parse_spe(stringstream& declarations, 
		stringstream& main_prolouge, stringstream& main_epilouge)
{
	ifstream ifile_declarations(spe_declarations_iname.c_str());
	file_to_stream(declarations, ifile_declarations);

	fileiter_t first_main(spe_main_iname);
	open_check(first_main, spe_main_iname);
	fileiter_t last_main = first_main.make_end();

	spe_main_grammar smg(main_prolouge, main_epilouge);
	parse(first_main, last_main, smg);
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

class make_pass_struct {
	string& str;

public:
	make_pass_struct(string& s): str(s) {}

	void operator()(const c_variable* cv)
	{
		// SPE_start and SPE_stop are keywords 
		// that already exist in the template code.
		if (	cv->name() != "SPE_start" && 
			cv->name() != "SPE_stop") {
			stringstream svar;
			svar	<< "\t" 
				<< cv->declare() << ";" << endl 
				<< pass_struct_hook;
			string temp = regex_replace(str, regex(pass_struct_hook), svar.str());
			str = temp;
		}
	}

	void operator()(const spe_region* r)
	{
		fmap(*this, r->priv());
		fmap(*this, r->shared());
	}
};

class make_buffer {
	ostream& out;

public:
	make_buffer(ostream& o): out(o) {}
	void operator()(const shared_variable* sv)
	{
		out << sv->buff_declare() << ";" << endl;
	}

	void operator()(const spe_region* r)
	{
		fmap(*this, r->shared());
	}
};

class make_pass_assign {
	stringstream& out;
public:
	make_pass_assign(stringstream& o): out(o) {}
	void operator()(const c_variable* cv)
	{
		out	<< pass_assign << cv->name() 
			<< " = " << cv->actual() << ";" 
			<< endl;
	}
};

void print_ppe(const string& name, sslist_t& blocks, stringstream& pro, stringstream& ppe_fork,
		spelist_t& regions)
{
	ofstream file(name.c_str());
	open_check(file, name);
	
	file << pro.str();

	// After every block of ppe code, we print a fork of spe code, except 
	// for the final block of ppe code, which is the final code section of 
	// the source file. This works because the definition of ppe code is 
	// all code before the #pragma cell section.
	int loop_num = 1;
	sslist_t::iterator b = blocks.begin();
	spelist_t::iterator r = regions.begin();
	while (b != blocks.end()) {
		file << (*b)->str();

		if (loop_num == 1) {
			file << mmgp_init << endl;
			file << mmgp_create_threads << endl;
		}

		if (++b != blocks.end()) {

			stringstream id_ss;
			id_ss << loop_num++;

			stringstream passes;
			fmap(make_pass_assign(passes), (*r)->priv());
			fmap(make_pass_assign(passes), (*r)->shared());

			string fork_str = regex_replace(ppe_fork.str(), regex(loop_hook), id_ss.str());
			string passes_str = regex_replace(passes.str(), regex(loop_hook), id_ss.str());
			file << regex_replace(fork_str, regex(pass_assign_hook), passes_str);

			file << regex_replace(mmgp_wait, regex(loop_hook), id_ss.str());

			++r;
		}
	}
}

void print_buffers(ostream& out, spelist_t& regions)
{
	fmap(make_buffer(out), regions);
}

void print_pass_struct(spelist_t& regions)
{	
	ofstream out(pass_struct_oname.c_str());
	open_check(out, pass_struct_oname);

	stringstream pass;
	parse_generic(pass, pass_struct_iname);

	string str = pass.str();
	fmap(make_pass_struct(str), regions);
	string final_pass = regex_replace(str, regex(pass_struct_hook), "");

	out << final_pass;
}

void print_spe(const string& name, stringstream& spe_dec, stringstream& main_pro, 
		stringstream& main_epi, spelist_t& regions)
{
	ofstream file(name.c_str());
	open_check(file, name);

	string str = regex_replace(spe_dec.str(), regex(buffer_hook), buff_size.declare());
	file << str;
	print_buffers(file, regions);	

	sslist_t cases;
	print_loops(file, regions, cases);

	file << main_pro.str() << spe_main_switch << endl;
	print_cases(file, cases);
	file << main_epi.str();
}

string parse_command_line(int argc, char* argv[])
{
	string src_name;

	try {
		options_description desc("Allowed options");
		desc.add_options()
		("help,h", "print usage message")
		("infile,i", "input filename");

		positional_options_description p;
		p.add("infile", 1);

		variables_map vm;
		store(command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
		notify(vm);
	
		if (vm.count("help")) {  
			cout << desc << "\n";
			return 0;
		}

		if (vm.count("infile")) {
			src_name = vm["infile"].as<string>();
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
	sslist_t ppe_src_blocks;
	spelist_t spe_regions;
	sharedtbl_t shared_tbl;
	symtbl_t private_tbl;
	parse_src(	src_name, 
			ppe_src_blocks, 
			spe_regions, 
			shared_tbl, 
			private_tbl);

	stringstream ppe_fork;
	parse_generic(ppe_fork, ppe_fork_iname);

	stringstream ppe_prolouge;
	parse_generic(ppe_prolouge, ppe_prolouge_iname);

	stringstream spe_declarations;
	stringstream spe_main_prolouge;
	stringstream spe_main_epilouge;
	parse_spe(spe_declarations, spe_main_prolouge, spe_main_epilouge);

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
	print_spe("spu/" + no_dot + "_spe.c", spe_declarations, 
			spe_main_prolouge, spe_main_epilouge, spe_regions);
	
	system(string("cp " + mmgp_spu_h_iname + " spu/").c_str());

	return 0;
}

