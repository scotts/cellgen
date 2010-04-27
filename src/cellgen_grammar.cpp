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

void print_xform_names(xformer* x)
{
	cout << x->class_name() << " ";
}

class ptout {
	int level;
	string tabs;

public:
	ptout(int l, string t): level(l), tabs(t) {}
	void operator()(const pt_node& node)
	{
		cout << tabs << level	<< ":" 
					<< string(node.value.begin(), node.value.end()) 
					<< " [" << ids::rule_string(node.value.id()) << "]";
		cout << " ( ";
		for_all(node.value.xformations, print_xform_names);
		cout << ")" << endl;

		for_all(node.children, ptout(level + 1, tabs + "  "));
	}
};

template <class T>
class vars_op_base {
protected:
	set<T*>& vars;
	map<string, T*>& symbols;
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
	vars_op_base(set<T*>& v, map<string, T*>& s, spelist& r): vars(v), symbols(s), regions(r) {}

	set<T*> pickup_vars()
	{
		set<T*> temp = vars;
		vars.clear();
		return temp;
	}

	map<string, T*> pickup_symbols()
	{
		map<string, T*> temp = symbols;
		symbols.clear();
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
	vars_op(set<T*>& v, map<string, T*>& s, spelist& r): vars_op_base<T>(v, s, r) {}

	void operator()(fileiter first, const fileiter& last) const
	{
		string type, local, definition;
		this->assign(first, last, type, local, definition);

		T* v = new T(type, local, definition, this->regions.size() + 1);
		this->vars.insert(v);
		this->symbols[local] = v;	
	}
};

template <>
class vars_op<shared_variable>: public vars_op_base<shared_variable> {
	privset& privs;
	priv_symtbl& priv_symbols;
public:
	vars_op(sharedset& v, shared_symtbl& s, privset& p, priv_symtbl& ps, spelist& r): 
		vars_op_base<shared_variable>(v, s, r), privs(p), priv_symbols(ps)
	{}

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
					dimensions.push_back(dim);

					// Implicitly pass the dimensions as private ints.
					if (priv_symbols.find(dim) == priv_symbols.end()) {
						private_variable* p = new private_variable("unsigned int", dim, dim, this->regions.size() + 1);
						privs.insert(p);
						priv_symbols[dim] = p;
					}

					capture = false;
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
		symbols[local] = v;
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

template <>
class scalar_op<bool> {
	bool& scalar;

public:
	scalar_op(bool& s): scalar(s) {}

	void operator()(fileiter first, const fileiter& last) const
	{
		scalar = true;
	}

	bool pickup()
	{
		bool temp = scalar;
		scalar = false;
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
class special_assign {
	vars_op<T>& op;
	map<string, T*>& symtbl;
	string local;
	spelist& regions;

public:
	special_assign(vars_op<T>& o, map<string, T*>& s, string l, spelist& r): op(o), symtbl(s), local(l), regions(r) {}
	void operator()(fileiter first, const fileiter& last) const
	{
		stringstream ss;
		while (first != last) {
			ss << *first++;
		}
		T* v = new T("int", local, ss.str(), regions.size() + 1);
		symtbl[local] = v;
		op.current_set().insert(v);
	}
};

struct create_spe_region {
	spelist&			regions;
	vars_op<private_variable>&	priv_op;
	vars_op<shared_variable>&	shared_op;
	vars_op<reduction_variable>&	reduce_def_op;
	scalar_op<string>&		reduce_op_op;
	scalar_op<string>&		buffer_op;

	create_spe_region(spelist& r, vars_op<private_variable>& p, 
			vars_op<shared_variable>& s, vars_op<reduction_variable>& rd, 
			scalar_op<string>& ro, scalar_op<string>& bo):
		regions(r), priv_op(p), shared_op(s), 
		reduce_def_op(rd), reduce_op_op(ro), buffer_op(bo)
		{}
	void operator()(fileiter first, const fileiter& last) const
	{
		spe_region* r = new spe_region(	priv_op.pickup_vars(),
						shared_op.pickup_vars(),
						reduce_def_op.pickup_vars(),
						reduce_op_op.pickup(),
						shared_op.pickup_symbols(),
						priv_op.pickup_symbols(),
						buffer_op.pickup()
						);
		regions.push_back(r);
	}
};

struct cellgen_grammar: public grammar<cellgen_grammar> {
	privset privs;
	sharedset shared;
	reduceset reduces;
	shared_symtbl shared_symbols;
	priv_symtbl private_symbols;
	reduc_symtbl reduction_symbols;
	string op;
	string buffer;
	push_back_op<sslist> ppe_op;
	vars_op<private_variable> priv_op;
	vars_op<shared_variable> shared_op;
	vars_op<reduction_variable> reduce_def_op;
	scalar_op<string> reduce_op_op;
	scalar_op<string> buffer_op;
	create_spe_region region_op;

	cellgen_grammar(sslist& ppe, spelist& regions):
		ppe_op(ppe),
		priv_op(privs, private_symbols, regions),
		shared_op(shared, shared_symbols, privs, private_symbols, regions),
		reduce_def_op(reduces, reduction_symbols, regions),
		reduce_op_op(op),
		buffer_op(buffer),
		region_op(regions,
			priv_op, 
			shared_op,
			reduce_def_op,
			reduce_op_op,
			buffer_op
			)
		{}

	template <typename ScannerT>
	struct definition {
		rule<ScannerT> 
			program, options, ppe_code, spe_code,
			reduction, operation, reduce_list, reduce_dec, reduce_dec_f,
			priv_code, priv_list, priv_dec, priv_dec_f,
			shared_code, shared_list, shared_dec, shared_dec_f,
			buffer_code, buffer_num;

		rule<ScannerT, parser_tag<ids::cell_region> >
			pragma_code;

		const rule<ScannerT>& start() const { return program; }

		definition(const cellgen_grammar& self)
		{
			program = 
				ppe_code >> pragma_code >> ppe_code 
				>> *(pragma_code >> ppe_code);

			// FIXME: remove lexeme_d so I can comment out pragmas.
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
			|	reduction
			| 	shared_code
			|	buffer_code
			;

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

			buffer_code = no_node_d[strlit<>("buffer(")] >> buffer_num[self.buffer_op] >> no_node_d[chlit<>(')')];

			buffer_num = no_node_d[*(anychar_p - ',' - ')')];
		}
	};
};

void parse_src(const string& src_name, sslist& ppe_blocks, spelist& spe_regions, bool print_pt)
{
	fileiter first(src_name);
	if (!first) {
		throw user_error(string("unable to open ") + src_name);
	}
	fileiter last = first.make_end();

	cellgen_grammar cg(ppe_blocks, spe_regions);
	pt_parse_file* parse = new pt_parse_file(); // need to make sure the ast persists
	*parse = ast_parse<xformer_factory>(first, last, cg, skip);

	if (!parse->full) {
		throw user_error(string("parse of ") + src_name + " failed.");
	}

	traverse_pt(parse->trees, spe_regions);

	if (print_pt) {
		for_all(parse->trees, ptout(0, ""));
	}

	spelist::iterator s = spe_regions.begin();
	for (pt_iterator a = (*parse->trees.begin()).children.begin(); a != (*parse->trees.begin()).children.end(); ++a) {
		if ((*a).value.id() == ids::cell_region) {
			(*s)->pt_root(&(*a));	
			++s;
		}
	}
}

