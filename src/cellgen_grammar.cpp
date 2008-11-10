#include <list>
#include <iostream>
#include <sstream>
#include <typeinfo>
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

void print_xform_names(xformer* x)
{
	cout << x->class_name() << " ";
}

class astout {
	int level;
	string tabs;

public:
	astout(int l, string t): level(l), tabs(t) {}
	void operator()(const ast_node& node)
	{
		cout << tabs << level	<< ":" 
					<< string(node.value.begin(), node.value.end()) 
					<< " [" << ids::rule_string(node.value.id()) << "]";
		cout << " ( ";
		for_all(node.value.xformations, print_xform_names);
		cout << ")" << endl;

		for_all(node.children, astout(level + 1, tabs + "  "));
	}
};

template <class T>
class vars_op_base {
protected:
	set<T*>& vars;
	spelist& regions;

	void assign(fileiter first, const fileiter& last, string& t, string& l, string& d) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}

		string equals;
		ss >> t >> l >> equals >> d;
	}

public:
	vars_op_base(set<T*>& v, spelist& r): vars(v), regions(r) {}

	set<T*> pickup_vars()
	{
		set<T*> temp = vars;
		vars.clear();
		return temp;
	}

	set<T*>& current_set()
	{
		return vars;
	}
};

template <class T>
class vars_op: public vars_op_base<T> {
public:
	vars_op(set<T*>& v, spelist& r): vars_op_base<T>(v, r) {}

	void operator()(fileiter first, const fileiter& last) const
	{
		string type, local, definition;
		this->assign(first, last, type, local, definition);

		T* v = new T(type, local, definition, this->regions.size() + 1);
		this->vars.insert(v);
	}
};

template <>
class vars_op<shared_variable>: public vars_op_base<shared_variable> {
	symtbl& tbl;
public:
	vars_op(sharedset& v, symtbl& t, spelist& r): vars_op_base<shared_variable>(v, r), tbl(t) {}

	symtbl pickup_symbols()
	{
		symtbl temp = tbl;
		tbl.clear();
		return temp;
	}

	void operator()(fileiter first, const fileiter& last) const
	{
		string type, local, definition;
		this->assign(first, last, type, local, definition);

		// Check if we point to a multidimensional array.
		//
		// The below should work. And it does! On another machine. I suspect 
		// the version of boost installed on monza is broken. But Centos
		// doesn't have any new boost packages, and far too many people 
		// depend on monza right now for me to wipe Centos and put on Fedora.
		// So I have to put up with this ugly state machine. Sigh.
		//
		//sregex_token_iterator a(definition.begin(), definition.end(), regex("\\d+"));
		//sregex_token_iterator z;
		//copy(a, z, back_inserter(dimensions));

		list<string> dimensions;
		if (definition.find("[") != string::npos) {
			string dim;
			bool capture = false;
			for (string::const_iterator c = definition.begin(); c != definition.end(); ++c) {
				if (*c == ']') {
					capture = false;
					dimensions.push_back(dim);
					dim = "";
				}

				if (capture) {
					dim += *c;
				}

				if (*c == '[') {
					capture = true;
				}
			}

			definition = definition.substr(0, definition.find("["));
		}

		shared_variable* v = new shared_variable(type, local, definition, dimensions, this->regions.size() + 1);
		this->vars.insert(v);
		tbl[local] = v;
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

template <class T>
class assign_var {
	vars_op<T>& op;
	string local;

public:
	assign_var(vars_op<T>& o, string l): op(o), local(l) {}
	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}

		op.current_set().insert(new T("int", local, ss.str()));
	}
};

struct create_spe_region {
	spelist&			regions;
	vars_op<private_variable>&	priv_op;
	vars_op<shared_variable>&	shared_op;
	vars_op<reduction_variable>&	reduce_def_op;
	scalar_op<string>&		reduce_op_op;
	scalar_op<int>&			unroll_op;
	scalar_op<int>&			buffer_op;
	scalar_op<bool>&		dma_unroll_op;

	create_spe_region(spelist& r, vars_op<private_variable>& p, 
			vars_op<shared_variable>& s, vars_op<reduction_variable>& rd, 
			scalar_op<string>& ro, scalar_op<int>& uo, scalar_op<int>& bo, scalar_op<bool>& d):
		regions(r), priv_op(p), shared_op(s), 
		reduce_def_op(rd), reduce_op_op(ro), unroll_op(uo), buffer_op(bo), dma_unroll_op(d)
		{}
	void operator()(fileiter first, const fileiter& last) const
	{
		spe_region* r = new spe_region(	priv_op.pickup_vars(),
						shared_op.pickup_vars(),
						reduce_def_op.pickup_vars(),
						reduce_op_op.pickup(),
						shared_op.pickup_symbols(),
						unroll_op.pickup(),
						buffer_op.pickup(),
						dma_unroll_op.pickup()
						);
		regions.push_back(r);
	}
};

/*
 * 3: Allow program to start with cell region.
 */
struct cellgen_grammar: public grammar<cellgen_grammar> {
	privset		privs;
	sharedset	shared;
	reduceset	reduces;
	symtbl		symbols;
	string		op;
	int		unroll;
	int		buffer;
	bool		dma_unroll;

	push_back_op<sslist>		ppe_op;
	vars_op<private_variable>	priv_op;
	assign_var<private_variable>	start_op;
	assign_var<private_variable>	stop_op;
	vars_op<shared_variable>	shared_op;
	vars_op<reduction_variable>	reduce_def_op;
	scalar_op<string>		reduce_op_op;
	scalar_op<int>			unroll_op;
	scalar_op<int>			buffer_op;
	scalar_op<bool>			dma_unroll_op;
	create_spe_region		region_op;

	cellgen_grammar(sslist& ppe, spelist& regions):
		unroll(0), buffer(0), dma_unroll(false),
		ppe_op(ppe),
		priv_op(privs, regions),
		start_op(priv_op, "SPE_start"),
		stop_op(priv_op, "SPE_stop"),
		shared_op(shared, symbols, regions),
		reduce_def_op(reduces, regions),
		reduce_op_op(op),
		unroll_op(unroll),
		buffer_op(buffer),
		dma_unroll_op(dma_unroll),
		region_op(regions,
			priv_op, 
			shared_op,
			reduce_def_op,
			reduce_op_op,
			unroll_op,
			buffer_op,
			dma_unroll_op)
		{}

	template <typename ScannerT>
	struct definition {
		rule<ScannerT> 
			program, options, ppe_code, spe_code,
			reduction, operation, reduce_list, reduce_dec, reduce_dec_f,
			start_code, start_priv, stop_code, stop_priv,
			priv_code, priv_list, priv_dec, priv_dec_f,
			shared_code, shared_list, shared_dec, shared_dec_f,
			unrolled_code, unroll_num,
			buffer_code, buffer_num,
			dma_unroll_code, dma_val;

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
			|	unrolled_code
			|	buffer_code
			|	dma_unroll_code
			;

			start_code = 
				no_node_d[strlit<>("SPE_start(")] >> start_priv >> no_node_d[chlit<>(')')];

			start_priv = no_node_d[
					lexeme_d[ (*(anychar_p - ')')) ]
					[self.start_op]
				];

			stop_code = no_node_d[strlit<>("SPE_stop(")] >> stop_priv >> no_node_d[chlit<>(')')];

			stop_priv = no_node_d[
					lexeme_d[ (*(anychar_p - ')')) ]
					[self.stop_op]
				];

			priv_code = no_node_d[strlit<>("private(")] >> priv_list >> no_node_d[chlit<>(')')];

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

			reduction = no_node_d[strlit<>("reduction(")] 
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

			shared_code = no_node_d[strlit<>("shared(")] >> shared_list >> no_node_d[chlit<>(')')];

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

			unrolled_code = no_node_d[strlit<>("unroll(")] >> unroll_num[self.unroll_op] >> no_node_d[chlit<>(')')];

			unroll_num = no_node_d[int_p];

			buffer_code = no_node_d[strlit<>("buffer(")] >> buffer_num[self.buffer_op] >> no_node_d[chlit<>(')')];

			buffer_num = no_node_d[int_p];

			dma_unroll_code = no_node_d[strlit<>("dma_unroll(")] >> dma_val[self.dma_unroll_op] >> no_node_d[chlit<>(')')];

			dma_val = no_node_d[int_p];
		}
	};
};

void parse_src(const string& src_name, sslist& ppe_blocks, spelist& spe_regions)
{
	fileiter first(src_name);
	if (!first) {
		cerr << "Unable to open " << src_name << endl;
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

	if (print_ast) {
		for_all(p->trees, astout(0, ""));
	}

	spelist::iterator s = spe_regions.begin();
	for (ast_iterator a = (*p->trees.begin()).children.begin(); a != (*p->trees.begin()).children.end(); ++a) {
		if ((*a).value.id() == ids::cell_region) {
			(*s)->ast_root(&(*a));	
			++s;
		}
	}
}

