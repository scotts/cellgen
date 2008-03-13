#include <list>
#include <iostream>
#include <sstream>
using namespace std;

#undef BOOST_SPIRIT_THREADSAFE // performance optimization
#include <boost/spirit/core.hpp>
#include <boost/spirit/symbols.hpp>
#include <boost/spirit/utility/confix.hpp>
#include <boost/spirit/iterator/file_iterator.hpp>
using namespace boost::spirit;

#include "cellgen_grammar.h"
#include "c_grammar.h"
#include "parse_tree.h"
#include "variable.h"
#include "skip.h"
#include "utility.h"

const c_compound_grammar c_compound;
const skip_grammar skip;

class astout {
	int level;
	string tabs;

public:
	astout(int l, string t): level(l), tabs(t) {}
	void operator()(const ast_node& node)
	{
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

		cout << tabs << level	<< ":" 
					<< string(node.value.begin(), node.value.end()) 
					<< " [" << rule << "]" 
					<<  endl;

		for_all(node.children, astout(level + 1, tabs + "  "));
	}
};

template <class T>
class vars_op {
	varset*& vars;
	symtbl*& tbl;
	spelist& regions;

public:
	vars_op(varset*& v, symtbl*& t, spelist& r): vars(v), tbl(t), regions(r)
	{
		vars = new varset;
		tbl = new symtbl;
	}

	varset* pickup_vars()
	{
		varset* temp = vars;
		vars = new varset;
		return temp;
	}

	symtbl* pickup_symbols()
	{
		symtbl* temp = tbl;
		tbl = new symtbl;
		return temp;
	}

	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}

		string type, local, equals, definition;
		ss >> type >> local >> equals >> definition;

		T* v = new T(type, local, definition, regions.size() + 1);
		vars->insert(v);
		(*tbl)[local] = v;
	}
};

template <class T>
class scalar_op {
	T& scalar;

public:
	scalar_op(T& s): scalar(s) {}
	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}
		ss >> scalar;
	}

	T pickup()
	{
		T temp = scalar;
		scalar = T();
		return temp;
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

class sstream_op {
	stringstream*& pickup;

public:
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

class assign_var {
	varset*& vars;
	string local;

public:
	assign_var(varset*& v, string l): vars(v), local(l) {}
	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}

		vars->insert(new region_variable("int", local, ss.str()));
	}
};

struct create_spe_region {
	spelist&			regions;
	assign_var&			start_op;
	assign_var&			stop_op;
	vars_op<private_variable>&	priv_op;
	vars_op<shared_variable>&	shared_op;
	vars_op<reduction_variable>&	reduce_def_op;
	scalar_op<string>&		reduce_op_op;
	scalar_op<int>&			unroll_op;

	create_spe_region(spelist& r, assign_var& start, assign_var& stop, vars_op<private_variable>& p, 
			vars_op<shared_variable>& s, vars_op<reduction_variable>& rd, 
			scalar_op<string>& ro, scalar_op<int>& uo):
		regions(r), start_op(start), stop_op(stop), priv_op(p), shared_op(s), 
		reduce_def_op(rd), reduce_op_op(ro), unroll_op(uo)
		{}
	void operator()(fileiter first, const fileiter& last) const
	{
		spe_region* r = new spe_region(	priv_op.pickup_vars(),
						shared_op.pickup_vars(),
						reduce_def_op.pickup_vars(),
						reduce_op_op.pickup(),
						shared_op.pickup_symbols(),
						unroll_op.pickup());
		regions.push_back(r);
	}
};

/*
 * 3: Allow program to start with cell region.
 */
struct cellgen_grammar: public grammar<cellgen_grammar> {
	push_back_op<sslist>		ppe_op;
	assign_var			start_op;
	assign_var			stop_op;
	vars_op<private_variable>	priv_op;
	vars_op<shared_variable>	shared_op;
	vars_op<reduction_variable>	reduce_def_op;
	scalar_op<string>		reduce_op_op;
	scalar_op<int>			unroll_op;
	create_spe_region		region_op;

	stringstream*	loop_pickup;
	varset*	priv_vars;
	varset*	shared_vars;
	varset*	reduce_vars;
	symtbl*		shared_tbl;
	symtbl*		private_tbl;
	symtbl*		reduction_tbl;
	string		op;
	int		unroll;
	
	cellgen_grammar(sslist& ppe, spelist& regions):
		ppe_op(ppe),
		start_op(priv_vars, "SPE_start"),
		stop_op(priv_vars, "SPE_stop"),
		priv_op(priv_vars, private_tbl, regions),
		shared_op(shared_vars, shared_tbl, regions),
		reduce_def_op(reduce_vars, reduction_tbl, regions),
		reduce_op_op(op),
		unroll_op(unroll),
		region_op(regions,
			start_op, 
			stop_op, 
			priv_op, 
			shared_op,
			reduce_def_op,
			reduce_op_op,
			unroll_op),
		unroll(0)
		{}

	template <typename ScannerT>
	struct definition {
		rule<ScannerT> 
			program, options, ppe_code, spe_code,
			reduction, operation, reduce_list, reduce_dec, reduce_dec_f,
			start_code, start_priv, stop_code, stop_priv,
			priv_code, priv_list, priv_dec, priv_dec_f,
			shared_code, shared_list, shared_dec, shared_dec_f,
			unrolled_code, unroll_num;

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

			spe_code = c_compound;

			options = 
				priv_code 
			| 	start_code
			| 	stop_code
			|	reduction
			| 	shared_code
			|	unrolled_code;

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

			unrolled_code = 
				no_node_d[strlit<>("unroll(")] >> unroll_num[self.unroll_op] >> no_node_d[chlit<>(')')];

			unroll_num = no_node_d[
				int_p
				];
		}
	};
};

void parse_src(const string& src_name, sslist& ppe_blocks, spelist& spe_regions)
{
	fileiter first(src_name);
	if (!first) {
		cout << "Unable to open " << src_name << endl;
		exit(-1);
	}
	fileiter last = first.make_end();

	cellgen_grammar cg(ppe_blocks, spe_regions);
	ast_parse_info* p = new ast_parse_info(); // need to make sure the ast persists
	*p = ast_parse<xformer_factory>(first, last, cg, skip);

	if (!p->full) {
		cerr << "error: parse of " << src_name << " failed." << endl;
		exit(-1);
	}

	traverse_ast(p->trees, spe_regions);

	spelist::iterator s = spe_regions.begin();
	for (ast_iterator a = (*p->trees.begin()).children.begin(); a != (*p->trees.begin()).children.end(); ++a) {
		if ((*a).value.id() == ids::cell_region) {
			(*s)->ast_root(&(*a));	
			++s;
		}
	}

	if (print_ast) {
		for_all(p->trees, astout(0, ""));
	}
}

