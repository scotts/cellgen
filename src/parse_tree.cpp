#include <string>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <sstream>
#include <iterator>
#include <map>
using namespace std;

#include <boost/function.hpp>
#include <boost/regex.hpp>
using namespace boost;

#include "parse_tree.h"
#include "ids.h"
#include "variable.h"
#include "spe_region.h"
#include "utility.h"

xformerlist_data::xformerlist_data(const xformerlist_data& o): char_data(o)
{
	for (xformerlist::const_iterator i = o.xformations.begin(); i != o.xformations.end(); ++i) {
		xformations.push_back((*i)->clone());
	}
}

xformerlist_data& xformerlist_data::operator=(const xformerlist_data& rhs)
{
	char_data::operator=(rhs);

	xformations.clear();
	for (xformerlist::const_iterator i = rhs.xformations.begin(); i != rhs.xformations.end(); ++i) {
		xformations.push_back((*i)->clone());
	}

	return *this;
}

bool is_constant(const ast_node& node)
{
	return node.value.id() == ids::int_constant_dec;
}

bool is_declaration(const ast_node& node)
{
	return node.value.id() == ids::declaration;
}

bool is_bracket(const string& s)
{
	return s == ")" || s == "(" || s == "{" || s == "}" || s == "<" || s == ">" || s == "[" || s == "]";
}

bool is_multop(const string& s)
{
	return s == "*" || s == "/" || s == "%";
}

bool is_addop(const string& s)
{
	return s == "+" || s == "-";
}

bool is_ident_or_const(const ast_node& node)
{
	return node.value.id() == ids::identifier || node.value.id() == ids::int_constant_dec;
}

bool is_equals(const ast_node& node)
{
	string str(node.value.begin(), node.value.end());
	return str.find("=") != string::npos;
}

bool is_type_specifier(const ast_node& node)
{
	string s(node.value.begin(), node.value.end());
	return	s == "void" || s == "char" || s == "short" || s == "int" || s == "long" ||
		s == "float" || s == "double" || s == "signed" || s == "unsigned";
}

add_expr make_add_expr(const list<string>& dimensions, const list<add_expr>& indices)
{
	if (indices.size() > 1) {
		// Combine dimensions and indices to get string versions of:
		// 	staq = sum(product(dimensions), indices)
		// Where sum goes from 0 to indices.size()-1 and 
		// product goes from 0 to dimensions.size()-1.
		list<string>::const_iterator n = dimensions.begin();
		list<add_expr>::const_iterator i = indices.begin();

		add_expr staq(indices.front());
		for (++n, ++i; n != dimensions.end() && i != indices.end(); ++n, ++i) {
			staq = add_expr(mult_expr(paren_expr(new add_expr(staq)), "*", *n), "+", mult_expr(i->str()));
		}

		return staq;
	}
	else {
		return indices.front();
	}
}

template <class F>
struct descend {
	F f;
	descend() {}
	descend(F f): f(f) {}
	void operator()(ast_node& node)
	{
		f(node);
		for_all(node.children, this);
	}
};

template <class F>
descend<F> make_descend(F f)
{
	return descend<F>(f);
}

class to_buffer_space: public xformer {
	const shared_variable* v;
	add_expr add;
public:
	to_buffer_space(const shared_variable* v, add_expr a): v(v), add(a) {}
	string operator()(const string& old)
	{
		return old + 
			orig_adaptor(v).name() + 
			"[(" + add.str() + ")%" + buffer_adaptor(v).size() + "]";
	}

	xformer* clone() const { return new to_buffer_space(*this); }
	string class_name() const { return "to_buffer_space"; }
};

struct mult_op {
	const symset& inductions;
	bool& found_induction;
	mult_expr& mult;
	string& id;
	bool found_op;

	mult_op(const symset& ind, bool& f, mult_expr& m, string& i):
		inductions(ind), found_induction(f), mult(m), id(i), found_op(false)
		{}
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node.value.id() == ids::multiplicative_expression) {
			mult_expr m;
			mult_op o(inductions, found_induction, m, id);
			for_all(node.children, &o);

			paren_expr p(new add_expr(m));
			if (!found_op) {
				mult.lhs(p);
			}
			else {
				mult.rhs(p);
			}
		}
		else if (is_multop(val)) {
			mult.op(val);
			found_op = true;
		}
		else if (node.value.id() == ids::identifier || is_constant(node)) {
			if (node.value.id() == ids::identifier) {
				if (inductions.find(val) != inductions.end()) {
					id = val;
					found_induction = true;
				}
			}

			if (!found_op) {
				mult.build_lhs(val);
			}
			else {
				mult.build_rhs(val);
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct add_op {
	const symset& inductions;
	bool& found_induction;
	add_expr& add;
	bool found_op;

	add_op(const symset& i, bool& f, add_expr& a):
		inductions(i), found_induction(f), add(a), found_op(false)
		{}
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node.value.id() == ids::multiplicative_expression) {
			mult_expr mult;
			string id;
			mult_op o(inductions, found_induction, mult, id);
			for_all(node.children, &o);

			if (!found_op) {
				add.lhs(mult);
			}
			else {
				add.rhs(mult);
			}
		}
		else if (is_ident_or_const(node)) {
			mult_expr mult;
			mult.lhs(val);

			if (!found_op) {
				add.lhs(mult);
			}
			else {
				add.rhs(mult);
			}
		}
		else if (is_addop(val)) {
			add.op(val);
			found_op = true;
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct array_op {
	const symset& inductions;
	bool& found_induction;
	add_expr& add;
	mult_expr lmult;

	array_op(const symset& i, bool& f, add_expr& a):
		inductions(i), found_induction(f), add(a)
		{}
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node.value.id() == ids::identifier) {
			// FIXME: do we need this or not?
			if (inductions.find(val) != inductions.end()) {
				lmult.lhs(val);
				add.lhs(lmult);

				found_induction = true;
			}
		}
		else if (node.value.id() == ids::additive_expression) {
			add_op o(inductions, found_induction, add);
			for_all(node.children, &o);
		}
		else if (node.value.id() == ids::multiplicative_expression) {
			string id;
			mult_op o(inductions, found_induction, lmult, id);
			for_all(node.children, &o);

			add.lhs(lmult);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct postfix_op {
	const symtbl& shared;
	const symset& inductions;
	sharedset& lst;
	bool found_induction;
	bool found_shared;
	shared_variable* var;
	list<add_expr> accesses;
	
	postfix_op(const symtbl& s, const symset& i, sharedset& l):
		shared(s), inductions(i), lst(l), found_induction(false), found_shared(false)
		{}
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());
		if (node.value.id() == ids::identifier) {
			if (!found_shared) {
				symtbl::const_iterator i = shared.find(val);
				if (i != shared.end()) {
					var = i->second;
					lst.insert(var);
					found_shared = true;
				}
			}
		}
		else if (node.value.id() == ids::array_index) {
			add_expr a;
			array_op o(inductions, found_induction, a);
			for_all(node.children, &o);

			accesses.push_back(a);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct handness {
	const symtbl& shared;
	const symset& inductions;
	sharedset& vars;
	handness(const symtbl& s, const symset& i, sharedset& l):
		shared(s), inductions(i), vars(l)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::postfix_expression) {
			postfix_op o(shared, inductions, vars);
			for_all(node.children, &o);

			if (o.found_shared && o.found_induction) {
				add_expr add = make_add_expr(o.var->dimensions(), o.accesses);
				o.var->math(add);
				node.value.xformations.push_back(new to_buffer_space(o.var, add)); 
				
				// If we don't do this, redundant printing. The to_buffer_space xformer
				// is a reduction of all the children.
				node.children.clear(); 
			}
		}
		else {
			for_all(node.children, this);
		}
	};
};

struct assignment_search {
	const symtbl& shared;
	const symset& inductions;
	sharedset& out;
	sharedset in;
	assignment_search(const symtbl& s, const symset& i, sharedset& o):
		shared(s), inductions(i), out(o)
		{}
	void operator()(ast_node& node)
	{
		// Needs to work for non-assignment expressions.
		if (node.value.id() == ids::assignment_expression) {
			ast_iterator eqs = find_if_all(node.children, is_equals);

			for_each(node.children.begin(), eqs, handness(shared, inductions, out));
			for_each(eqs, node.children.end(), handness(shared, inductions, in));
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct gen_in: public unrollable_xformer {
	shared_variable* v;

	gen_in(shared_variable* v, const string& i): unrollable_xformer(i), v(v) {}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		string if_statement;
		string wait;

		if (!unroll) {
			if_statement = "if (!((" + v->math().lhs().str() + ")%" + buff.size() + "))"; 
		}

		if (v->depth() == 3) {
			wait = "MMGP_SPE_dma_wait(" + next.name() + ");";
		}

		return old + if_statement + "{\n" +
				prev.name() + "=" + next.name() + ";\n" +
				next.name() + "= (" + next.name() + "+1)%" + buff.depth() + ";\n" + 
				wait +
				"mfc_get(" + 
					buff.name() + "[" + next.name() + "]," 
					"(unsigned long)(" + v->name() + "+((" + v->math().lhs().str() + ")+" + buff.size() + "))," 
					"sizeof(" + buff.type() + ") *" + buff.size() + "," +
					next.name() + ", 0, 0);\n" +
				orig.name() + "=" + buff.name() + "[" + prev.name() + "];\n"
				"MMGP_SPE_dma_wait(" + prev.name() + ");\n"
			"}\n";
	}

	xformer* clone() const { return new gen_in(*this); }
	string class_name() const { return "gen_in"; }
};

struct less_gen_in {
	bool operator()(const gen_in* a, const gen_in* b)
	{
		return (a->v) < (b->v);
	}
};
typedef map<gen_in*, ast_node*, less_gen_in> bind_gen_in;

template <class T>
struct erase_from_set: unary_function<T, void> {
	set<T>& container;
	erase_from_set(set<T>& c): container(c) {}
	void operator()(T value)
	{
		container.erase(value);
	}
};

struct serial_for_op {
	const symtbl& shared;
	const string& par_induction;
	symset ser_inductions;
	sharedset in;
	sharedset out;
	sharedset inout;

	serial_for_op(const symtbl& s, const string& par):
		shared(s), par_induction(par) {}
	serial_for_op(const symtbl& s, const string& par, const symset& ser):
		shared(s), par_induction(par), ser_inductions(ser) {}

	void merge_inout(sharedset& g_in, sharedset& g_out, sharedset& g_inout)
	{
		// g_inout += inout + intersection(in + g_in, out + g_out)
		g_in.insert(in.begin(), in.end());
		g_out.insert(out.begin(), out.end());
		g_inout.insert(inout.begin(), inout.end());
		set_intersection_all(g_in, g_out, inserter(g_inout, g_inout.begin()));

		// g_in -= g_inout
		for_all(g_inout, erase_from_set<shared_variable*>(g_in));

		// g_out -= g_inout
		for_all(g_inout, erase_from_set<shared_variable*>(g_out));
	}

	void operator()(ast_node& node);
};

void print_variable(const variable* v)
{
	cout << v->name() << "(" << v << ") ";
}

pair<ast_iterator, ast_node*> find_equals(ast_node& node)
{
	for (ast_iterator i = node.children.begin(); i != node.children.end(); ++i) {
		if (is_equals(*i)) {
			return make_pair(i, &node);
		}

		pair<ast_iterator, ast_node*> p = find_equals(*i);
		if (p.first != i->children.end()) {
			return p;
		}
	}

	return make_pair(node.children.end(), &node);
}

struct declaration_op {
	const symtbl& shared;
	const symset& inductions;
	sharedset in;
	
	declaration_op(const symtbl& s, const symset& ind):
		shared(s), inductions(ind) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::init_declarator) {
			pair<ast_iterator, ast_node*> p = find_equals(node);
			for_each(p.first, p.second->children.end(), handness(shared, inductions, in)); 
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct for_compound_op {
	const symtbl& shared; 
	const string& par_induction;
	const symset& ser_inductions;
	symset inductions;
	bind_gen_in& lazy_in;
	sharedset& pre_out;
	sharedset& in;
	sharedset& out;
	sharedset& inout;
	int unroll;

	for_compound_op(const symtbl& s, const string& par, const symset& ser, bind_gen_in& li, 
			sharedset& po, sharedset& i, sharedset& o, sharedset& io, int u): 
		shared(s), par_induction(par), ser_inductions(ser), inductions(ser), lazy_in(li), 
		pre_out(po), in(i), out(o), inout(io), unroll(u)
	{
		inductions.insert(par_induction);	
	}

	void operator()(ast_node& node)
	{
		static sharedset seen;

		if (node.value.id() == ids::declaration) {
			declaration_op o(shared, inductions);
			for_all(node.children, &o);

			// We need the indirection of o.in because in might already have variables,
			// and we don't want to generate gen_in xformations for them.
			for (sharedset::iterator i = o.in.begin(); i != o.in.end(); ++i) {
				if (seen.find(*i) == seen.end()) {
					node.value.xformations.push_back(new gen_in(*i, par_induction));
					in.insert(*i);
					seen.insert(*i);
				}
			}
		}
		else if (node.value.id() == ids::expression_statement || node.value.id() == ids::selection_statement) {
			assignment_search o(shared, inductions, pre_out);
			for_all(node.children, &o);

			// Store the function object somewhere, and call it later once I know 
			// which variables are in, which are out, and which are inout
			for (sharedset::iterator i = o.in.begin(); i != o.in.end(); ++i) {
				if (seen.find(*i) == seen.end()) {
					lazy_in.insert(bind_gen_in::value_type(new gen_in(*i, par_induction), &node));
					seen.insert(*i);
				}
			}
		}
		else if (node.value.id() == ids::for_loop) {
			serial_for_op o(shared, par_induction, ser_inductions);
			for_all(node.children, &o);
			o.merge_inout(in, out, inout);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct gen_out: public unrollable_xformer {
	const shared_variable* v;

	gen_out(const shared_variable* v, const string& i): unrollable_xformer(i), v(v) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);
		string var_switch;
		string if_statement;

		if (v->depth() < 3) {
			var_switch =	next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; \n" +
					orig.name() + "=" + buff.name() + "[" + next.name() + "]; \n" +
					"MMGP_SPE_dma_wait(" + next.name() + ");";
		}

		if (!unroll) {
			if_statement = "if (!(" + v->math().next_iteration(induction) + "%" + buff.size() + "))";
		}

		return if_statement + "{\n" +
					"mfc_put(" + orig.name() + "," + "(unsigned long)(" + v->name() + 
							"+" + v->math().next_iteration(induction) + "-" + buff.size() + "),"
						"sizeof(" + buff.type() + ")*" + buff.size() + "," +
						next.name() + ", 0, 0); \n" +
					var_switch +
			"} \n" + old;
	}

	xformer* clone() const { return new gen_out(*this); }
	string class_name() const { return "gen_out"; }
};

struct gen_final_out: public unrollable_xformer {
	const shared_variable* v;

	gen_final_out(const shared_variable* v, const string& i): unrollable_xformer(i), v(v) {}
	string operator()(const string& old)
	{
		string ret;

		if (!unroll) {
			buffer_adaptor buff(v);
			orig_adaptor orig(v);
			next_adaptor next(v);

			ret = "if ((((SPE_stop - SPE_start)" + v->math().factor(induction) + ")%" + buff.size() + ")) {" +
					prev.name() + "=" + next.name() + ";" +
					next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; \n" +
					"mfc_put(" + orig.name() + "," 
						"(unsigned long)(" + v->name() +
							"+(SPE_stop" + v->math().factor(induction) + ")-(((SPE_stop-SPE_start)" + 
							v->math().factor(induction) + ")%" + 
							buff.size() + ")),"
						"sizeof(" + buff.type() + ")*(((SPE_stop-SPE_start)" + v->math().factor(induction) +
						")%" + buff.size() + ")," +
						next.name() + ", 0, 0"
					"); \n" +
					"MMGP_SPE_dma_wait(" + prev.name() + "); \n" +
					"MMGP_SPE_dma_wait(" + next.name() + "); \n }";
		}

		return old + ret;
	}

	xformer* clone() const { return new gen_final_out(*this); }
	string class_name() const { return "gen_final_out"; }
};

class reduction_declare: public xformer {
	const reduction_variable* v;
public:
	reduction_declare(const reduction_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return old + orig_adaptor(v).declare() + "= *" + v->name() + "; \n";
	}

	xformer* clone() const { return new reduction_declare(*this); }
	string class_name() const { return "reduction_declare"; }
};

template <class X, class V>
struct make_xformer: public unary_function<const V*, xformer*> {
	xformer* operator()(const V* v)
	{
		return new X(v);
	}
};

/*
 * Doubles as make_unrollabe.
 */
template <class X, class V>
struct make_induction: public unary_function<const V*, xformer*> {
	const string& inductions;
	make_induction(const string& i): inductions(i) {}
	xformer* operator()(const V* v)
	{
		return new X(v, inductions);
	}
};

class variable_name: public xformer {
	const variable v;
public:
	variable_name(const variable v): v(v) {}
	string operator()(const string&)
	{
		return v.name();
	}

	xformer* clone() const { return new variable_name(*this); }
	string class_name() const { return "variable_name"; }
};

class assign_depth {
	int d;
public:
	assign_depth(int d): d(d) {}
	void operator()(region_variable* v)
	{
		if (v->is_non_scalar()) {
			v->depth(d);
		}
	}
};

struct bind_gen_in_void_less {
	bool operator()(const void* v, const bind_gen_in::value_type& p)
	{
		return v < p.first->v;
	}

	bool operator()(const bind_gen_in::value_type& p, const void* v)
	{
		return p.first->v < v;
	}

	bool operator()(const bind_gen_in::value_type& a, const bind_gen_in::value_type& b)
	{
		return a.first->v < b.first->v;
	}
};

void serial_for_op::operator()(ast_node& node)
{
	if (node.value.id() == ids::identifier) {
		// Note that ser_inductions is local to serial_for_op; the induction 
		// variables of enclosing scopes were copied in, yet those scopes 
		// will remain ignorant of our induction variables.
		ser_inductions.insert(string(node.value.begin(), node.value.end()));
	}
	else if (node.value.id() == ids::compound) {
		bind_gen_in lazy_in;
		sharedset pre_out;
		for_compound_op o(shared, par_induction, ser_inductions, lazy_in, pre_out, in, out, inout, NO_UNROLL);
		for_all(node.children, &o);

		// lazy_inout = intersection(lazy_in, pre_out)
		bind_gen_in lazy_inout;
		set_intersection_all(lazy_in, pre_out, 
				inserter(lazy_inout, lazy_inout.begin()), 
				bind_gen_in_void_less());

		// out = pre_out - lazy_inout
		set_difference_all(pre_out, lazy_inout, 
				inserter(out, out.begin()), 
				bind_gen_in_void_less());

		// diff_in = lazy_in - lazy_inout
		bind_gen_in diff_in;
		set_difference_all(lazy_in, lazy_inout, 
				inserter(diff_in, diff_in.begin()), 
				bind_gen_in_void_less());

		// Lazily call gen_in on both in and inout variables.
		for (bind_gen_in::iterator i = diff_in.begin(); i != diff_in.end(); ++i) {
			i->second->value.xformations.push_back(i->first);
			in.insert(i->first->v);
		}

		for (bind_gen_in::iterator i = lazy_inout.begin(); i != lazy_inout.end(); ++i) {
			i->second->value.xformations.push_back(i->first);
			inout.insert(i->first->v);
		}
	}
	else {
		for_all(node.children, this);
	}
}

struct multiple_parallel_induction_variables {
	string old;
	string attempt;
	multiple_parallel_induction_variables(const string& o, const string& a): old(o), attempt(a) {}
};

struct parallel_for_op {
	const symtbl& shared;
	string& par_induction;
	sharedset& in;
	sharedset& out;
	sharedset& inout;
	int unroll;
	parallel_for_op(const symtbl& s, string& par, sharedset& i, sharedset& o, sharedset& io, int u): 
		shared(s), par_induction(par), in(i), out(o), inout(io), unroll(u) {}
	void operator()(ast_node& node)
	{
		// if unroll, need to change iteration parameters
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			if (ident != "SPE_start" && ident != "SPE_stop") {
				if (par_induction != "" && par_induction != ident) {
					throw multiple_parallel_induction_variables(par_induction, ident);
				}
				par_induction = ident;
			}

			if (unroll) {
				if (ident == "SPE_start") {
					node.value.xformations.push_back(new variable_name(unrolled));
				}
				else if (ident == "SPE_stop") {
					node.value.xformations.push_back(new variable_name(epilouge));
				}
			}
		}
		else if (node.value.id() == ids::compound) {
			serial_for_op o(shared, par_induction);
			o(node);
			o.merge_inout(in, out, inout);

			// Right now, we only apply out xformations at the end of a parallel for loop. This is 
			// not entirely correct. Serial for loops should have them if the stopping condition 
			// matches the size of the dimension it is iterating over. (?)
			xformerlist& xformations = node.children.back().value.xformations;
			append(xformations, fmap(make_induction<gen_out, shared_variable>(par_induction), out));
			append(xformations, fmap(make_induction<gen_out, shared_variable>(par_induction), inout));
			append(xformations, fmap(make_induction<gen_final_out, shared_variable>(par_induction), out));
			append(xformations, fmap(make_induction<gen_final_out, shared_variable>(par_induction), inout));
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct unroll_single {
	const int unroll;
	unroll_single(const int u): unroll(u) {}
	void operator()(xformer* xformer)
	{
		xformer->unroll_me(unroll);
	}
};

struct unroll_all {
	const int unroll;
	unroll_all(const int u): unroll(u) {}
	void operator()(ast_node& node)
	{
		for_all(node.value.xformations, unroll_single(unroll));
		for_all(node.children, this);
	}
};

bool has_declaration(const ast_node& node)
{
	if (node.value.id() == ids::declaration) {
		return true;
	}
	else {
		for (ast_node::const_tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
			if (has_declaration(*i)) {
				return true;
			}
		}
		return false;
	}
}

template <class X>
struct remove_xforms {
	void operator()(ast_node& node)
	{
		node.value.xformations.remove_if(is_type<X, xformer>);
	}
};

struct nop: public xformer {
	string operator()(const string&)
	{
		return "";
	}
	xformer* clone() const { return new nop; }
	string class_name() const { return "nop"; }
};

struct match_identifier {
	const string& to_replace;
	xformer* x;
	match_identifier(const string& t, xformer* x): 
		to_replace(t), x(x)
	{
		assert(x);	
	}

	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::identifier) {
			if (to_replace == string(node.value.begin(), node.value.end())) {
				node.value.xformations.push_back(x);
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct wipeout_identifier {
	const string& ident;
	ast_node& root;
	wipeout_identifier(const string& i, ast_node& r):
		ident(i), root(r) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::identifier) {
			if (ident == string(node.value.begin(), node.value.end())) {
				root.value.xformations.push_back(new nop);
				root.children.clear();
			}
		}
	}
};

struct variable_increment: public xformer {
	const string var;
	variable_increment(const string& v): var(v) {}
	string operator()(const string& old)
	{
		return old + "++" + var + ";";
	}

	xformer* clone() const { return new variable_increment(*this); }
	string class_name() const { return "variable_increment"; }
};

struct wipeout_declarations {
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::declaration) {
			node.value.xformations.push_back(new nop);
			node.children.clear();
		}
	}
};

struct init_declarations_to_expressions {
	void operator()(ast_node& node)
	{
		if (	node.value.id() == ids::declaration_specifiers || 
			node.value.id() == ids::pointer || is_type_specifier(node))
		{
			node.value.xformations.push_back(new nop);
			node.children.clear();
		}
		else if (node.value.id() != ids::compound) { // new scopes can keep their declarations
			for_all(node.children, this);
		}
	}
};

bool is_const_declaration(const ast_node& node)
{
	if (node.value.id() == ids::declaration_specifiers) {
		if (node.children.empty()) {
			return false;
		}
		const ast_node& front = node.children.front();
		return "const" == string(front.value.begin(), front.value.end());
	}
	else {
		for (ast_node::const_tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
			if (is_const_declaration(*i)) {
				return true;
			}
		}
		return false;
	}
}

bool is_pure_declaration(const ast_node& node)
{
	if (node.value.id() == ids::init_declarator) {
		return false;
	}
	else {
		for (ast_node::const_tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
			if (!is_pure_declaration(*i)) {
				return false;
			}
		}
		return true;
	}
}

struct wipeout_const_and_pure_declarations {
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::declaration) {
			if (is_const_declaration(node) || is_pure_declaration(node)) {
				node.value.xformations.push_back(new nop);
				node.children.clear();
			}

		}
		else if (node.value.id() != ids::compound) { // new scopes can keep their declarations
			for_all(node.children, this);
		}
	}
};

struct unroll_for_op {
	const symtbl& shared;
	const string& induction;
	const int unroll;
	const bool dma_unroll;
	unroll_for_op(const symtbl& s, const string& i, const int u, const bool d): 
		shared(s), induction(i), unroll(u), dma_unroll(d) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::expression_statement) {
			// Yeah, kinda odd. We changed the original for_loop in place when we knew 
			// we were unrolling, so we're going to remove those, then add the new ones.
			descend< remove_xforms<variable_name> >()(node);
			for_all(node.children, match_identifier("SPE_stop", new variable_name(unrolled)));
		}
		else if (node.value.id() == ids::postfix_expression || node.value.id() == ids::unary_expression) {
			for_all(node.children, make_descend(wipeout_identifier(induction, node)));
		}
		else if (node.value.id() == ids::compound) {
			for_all(node.children, unroll_all(unroll));

			list<ast_node> to_copy;
			copy_all(node.children, back_inserter(to_copy));

			// First iteration doesn't need sending out stuff.
			for_all(node.children, remove_xforms<gen_out>());
			for_all(node.children, remove_xforms<gen_final_out>());

			if (!to_copy.empty()) {
				// front or back?
				to_copy.front().value.xformations.push_back(new variable_increment(induction));
			}

			// We copy it unroll-1 times because the first iteration
			// already exists.
			for (int i = 0; i < unroll - 1; ++i) {
				for (list<ast_node>::iterator j = to_copy.begin(); j != to_copy.end(); ++j) {
					ast_node copy = *j;
					for_all(copy.children, wipeout_const_and_pure_declarations());
					for_all(copy.children, init_declarations_to_expressions());

					// All "inner" iterations don't need any in/out xformations, 
					// but the final iteration needs out xformations. The final node 
					// of the final iteration needs an increment so the induction 
					// variable is correct for the next iteration.
					if (!dma_unroll) {
						descend< remove_xforms<gen_in> >()(copy);
					}
					if (i < unroll - 2) {
						if (!dma_unroll) {
							remove_xforms<gen_out>()(copy);
						}
						remove_xforms<gen_final_out>()(copy);
					}
					else if (next(j) == to_copy.end()) {
						copy.value.xformations.push_front(new variable_increment(induction));
					}

					node.children.insert(node.children.end() - 1, copy);
				}
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

class compute_bounds: public xformer {
	const shared_variable* v;
public:
	compute_bounds(const shared_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string size;
		if (v) { // TODO: I don't like this NULL check. I'd rather it be a default value. Not sure how.
			size = buffer_adaptor(v).size();
		}
		else {
			size = default_buff_size;
		}
		return old + "compute_bounds(&SPE_start, &SPE_stop," + size + ");";
	}

	xformer* clone() const { return new compute_bounds(*this); }
	string class_name() const { return "compute_bounds"; }
};

struct compound {
	const symtbl& shared;
	string& par_induction;
	sharedset& in;
	sharedset& out;
	sharedset& inout;
	int unroll;
	compound(const symtbl& s, string& par, sharedset& i, sharedset& o, sharedset& io, int u): 
		shared(s), par_induction(par), in(i), out(o), inout(io), unroll(u) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::for_loop) {
			parallel_for_op o(shared, par_induction, in, out, inout, unroll);
			try {
				for_all(node.children, &o);
			} catch (multiple_parallel_induction_variables e) {
				cerr	<< "error: attempt to define multiple parallel induction variables" << endl 
					<< "\told: " << e.old << " new: " << e.attempt << endl;
				exit(1);
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

bool is_for_loop(ast_node& node)
{
	return node.value.id() == ids::for_loop;
}

struct reduction_assign: public xformer {
	const reduction_variable* v;
	reduction_assign(const reduction_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return "*" + v->name() + "=" + orig_adaptor(v).name() + "; \n" + old;
	}

	xformer* clone() const { return new reduction_assign(*this); }
	string class_name() const { return "reduction_assign"; }
};

class init_buffers: public xformer {
	const shared_variable* v;

public:
	init_buffers(const shared_variable* _v): v(_v) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);
		orig_adaptor orig(v);

		return	old + 
			orig.declare() + "; \n" +
			next.declare() + " = 0; \n" +
			orig.name() + " = " + buff.name() + "[" + next.name() + "]; \n";
	}

	xformer* clone() const { return new init_buffers(*this); }
	string class_name() const { return "init_buffers"; }
};
	
class in_init_buffers: public induction_xformer {
	const shared_variable* v;

public:
	in_init_buffers(const shared_variable* _v, const string& i): induction_xformer(i), v(_v) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return	init_buffers(v)(old) +
			"mfc_get(" +
				buff.name() + "[" + next.name() + "], " 
				"(unsigned long)(" + v->name() + " + (SPE_start" + v->math().factor(induction) + ")),"
				"sizeof(" + buff.type() + ")*" + buff.size() + ", " +
				next.name() + ", 0, 0); \n";
	}

	xformer* clone() const { return new in_init_buffers(*this); }
	string class_name() const { return "in_init_buffers"; }
};

class init_private_buffers: public xformer {
	const private_variable* v;

public:
	init_private_buffers(const private_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string ret;
		if (v->is_non_scalar()) {
			buffer_adaptor buff(v);
			orig_adaptor orig(v);

			ret =	"mfc_get(" +
					buff.name() + "," +
					"(unsigned long)" + orig.name() + "," +
					"sizeof(" + buff.type() + ")*" + buff.size() + ","
					"3, 0, 0); \n" + 
				orig.name() + "=" + buff.name() + ";"
				"MMGP_SPE_dma_wait(3); \n";
		}

		return old + ret;
	}

	xformer* clone() const { return new init_private_buffers(*this); }
	string class_name() const { return "init_private_buffers"; }
};

class unroll_boundaries: public xformer {
	const int unroll;
public:
	unroll_boundaries(const int u): unroll(u) {}
	string operator()(const string& old)
	{
		stringstream ss;
		ss << unroll;

		return 	old + 
			const_variable("int", "unroll_factor", ss.str()).define() + ";" +
			unrolled.define() + ";" + 
			epilouge.define() + ";";
	}

	xformer* clone() const { return new unroll_boundaries(*this); }
	string class_name() const { return "unroll_boundaries"; }
};

string remove_multop(const string& str)
{
	size_t pos = str.find_first_of("*/%");
	if (pos != string::npos) {
		return str.substr(pos + 1, str.size() - 1);
	}
	return str;
}

struct max_buffer: unary_function<const region_variable*, void> {
	const string& induction;
	shared_variable* max;
	max_buffer(const string& i): induction(i), max(NULL) {}
	void operator()(shared_variable* v)
	{
		if (max) {
			if (	from_string<int>(remove_multop(max->math().factor(induction))) < 
				from_string<int>(remove_multop(v->math().factor(induction)))
			) {
				max = v;
			}
		}
		else {
			max = v;
		}
	}
};

template <class Pred>
ast_node::tree_iterator find_and_duplicate(ast_node& node, Pred p)
{
	ast_node::tree_iterator i = find_if_all(node.children, p);
	if (i != node.children.end()) {
		return node.children.insert(i, *i);
	}
	else {
		for (i = node.children.begin(); i != node.children.end(); ++i) {
			ast_node::tree_iterator j = find_and_duplicate(*i, p);
			if (j != i->children.end()) {
				return j;
			}
		}
	}

	return node.children.end();
}

struct declare_prev: public xformer {
	string operator()(const string& old)
	{
		return old + prev.define() + ";";
	}

	xformer* clone() const { return new declare_prev(*this); }
	string class_name() const { return "declare_prev"; }
};

struct cell_region {
	spelist::iterator region;

	cell_region(spelist::iterator r): region(r) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::compound) {

			// Assumption: one parallel induction variable.
			string par_induction;
			compound o((*region)->symbols(), par_induction,
					(*region)->in(), (*region)->out(), (*region)->inout(), 
					(*region)->unroll());

			for_all(node.children, &o);

			for_all((*region)->priv(), assign_depth(1));
			for_all((*region)->in(), assign_depth(2));
			for_all((*region)->out(), assign_depth(2));
			for_all((*region)->inout(), assign_depth(3));

			if ((*region)->unroll()) {
				ast_node::tree_iterator unroll_pos = find_and_duplicate(node, is_for_loop);
				assert(unroll_pos != node.children.end());
				unroll_for_op((*region)->symbols(), par_induction, (*region)->unroll(), (*region)->dma_unroll())(*unroll_pos);
			}

			xformerlist& front_xforms = node.children.front().value.xformations;
			front_xforms.push_back(new declare_prev());
			front_xforms.push_back(new compute_bounds((for_all((*region)->shared(), max_buffer(o.par_induction)).max)));
			append(front_xforms, fmap(make_xformer<init_buffers, shared_variable>(), (*region)->out()));
			append(front_xforms, fmap(make_induction<in_init_buffers, shared_variable>(par_induction), (*region)->in()));
			append(front_xforms, fmap(make_induction<in_init_buffers, shared_variable>(par_induction), (*region)->inout()));
			append(front_xforms, fmap(make_xformer<init_private_buffers, private_variable>(), (*region)->priv()));
			append(front_xforms, fmap(make_xformer<reduction_declare, reduction_variable>(), (*region)->reductions()));

			// Order matters; unroll_boundaries generates code that depends on 
			// code generated from compute_bounds.
			if ((*region)->unroll()) {
				front_xforms.push_back(new unroll_boundaries((*region)->unroll()));
			}

			xformerlist& back_xforms = node.children.back().value.xformations;
			append(back_xforms, fmap(make_xformer<reduction_assign, reduction_variable>(), (*region)->reductions()));

			(*region)->induction(par_induction);
			++region;
		}
		else {
			for_all(node.children, this);
		}
	}
};

void traverse_ast(ast& trees, spelist& regions)
{
	for_all((*trees.begin()).children, cell_region(regions.begin()));
}

