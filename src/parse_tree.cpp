#include <string>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <sstream>
#include <iterator>
#include <map>
using namespace std;

#include "c_grammar.h"
#include "skip.h"
#include "parse_tree.h"
#include "ids.h"
#include "variable.h"
#include "spe_region.h"
#include "operations.h"
#include "utility.h"

typedef map<xformer*, pt_node*> bind_xformer;
void add_xformer_to_node(bind_xformer::value_type pair)
{
	assert(pair.first && pair.second);
	pair.second->value.xformations.push_back(pair.first);
}

xformerlist_data::xformerlist_data(const xformerlist_data& o): char_data(o)
{
	xformations = fmap(mem_fn(&xformer::clone), o.xformations);
}

xformerlist_data& xformerlist_data::operator=(const xformerlist_data& rhs)
{
	char_data::operator=(rhs);
	xformations = fmap(mem_fn(&xformer::clone), rhs.xformations);
	return *this;
}

// FIXME: For some of these identities, I should use named constants, not string.

template <class Node>
bool node_is(const Node& node, const int id)
{
	return node.value.id() == id;
}

template <class Node>
bool node_is(const Node& node, const int id1, const int id2)
{
	return node.value.id() == id1 || node.value.id() == id2;
}

template <class Node>
bool node_is(const Node& node, const int id1, const int id2, const int id3)
{
	return node.value.id() == id1 || node.value.id() == id2 || node.value.id() == id3;
}

template <class Node>
bool node_is(const Node& node, const int id1, const int id2, const int id3, const int id4)
{
	return node.value.id() == id1 || node.value.id() == id2 || node.value.id() == id3 || node.value.id() == id4;
}

template <class Node>
bool node_is(const Node& node, const int id1, const int id2, const int id3, const int id4, const int id5)
{
	return node.value.id() == id1 || node.value.id() == id2 || node.value.id() == id3 || node.value.id() == id4 || node.value.id() == id5;
}

bool is_int(const string& str)
{
	return str.find("int") != string::npos || str.find("long") != string::npos;
}

bool is_float(const string& str)
{
	return str.find("float") != string::npos;
}

bool is_double(const string& str)
{
	return str.find("double") != string::npos;
}

bool is_int_constant(const pt_node& node)
{
	return node.value.id() == ids::int_constant_dec;
}

bool is_float_constant(const pt_node& node)
{
	return node_is(node, ids::float_constant_1, ids::float_constant_2, ids::float_constant_3);
}

bool is_constant(const pt_node& node)
{
	return is_int_constant(node) || is_float_constant(node);
}

bool is_declaration(const pt_node& node)
{
	return node_is(node, ids::declaration);
}

bool is_bracket(const string& s)
{
	return s == ")" || s == "(" || s == "{" || s == "}" || s == "<" || s == ">" || s == "[" || s == "]";
}

bool is_kind_of_mul(const string& s)
{
	return s == "*" || s == "/" || s == "%";
}

bool is_kind_of_mul(const pt_node& node)
{
	return is_kind_of_mul(string(node.value.begin(), node.value.end()));
}

bool is_kind_of_add(const string& s)
{
	return s == "+" || s == "-";
}

bool is_kind_of_add(const pt_node& node)
{
	return is_kind_of_add(string(node.value.begin(), node.value.end()));
}

bool is_operation(const string& s)
{
	return is_kind_of_add(s) || is_kind_of_mul(s);
}

bool is_ident(const pt_node& node)
{
	return node_is(node, ids::identifier);
}

bool is_ident_or_constant(const pt_node& node)
{
	return is_ident(node) || is_constant(node);
}

bool is_equals(const pt_node& node)
{
	string str(node.value.begin(), node.value.end());
	return str.find("=") != string::npos;
}

bool is_relational(const pt_node& node)
{
	string str(node.value.begin(), node.value.end());
	return str.find(">") != string::npos || str.find("<") != string::npos || str.find("==") != string::npos;
}

bool is_conditional_operator(const pt_node& node)
{
	return is_relational(node) || is_equals(node);
}

bool is_type_specifier(const pt_node& node)
{
	string s(node.value.begin(), node.value.end());
	return	s == "void" || s == "char" || s == "short" || s == "int" || s == "long" ||
		s == "float" || s == "double" || s == "signed" || s == "unsigned";
}

bool is_struct_access(const pt_node& node)
{
	return node_is(node, ids::dot, ids::ptr_op);
}

template <class Node>
bool is_statement(const Node& node)
{
	return node.value.id() == ids::compound || 
		node.value.id() == ids::expression_statement || 
		node.value.id() == ids::selection_statement;
}

bool is_expression(const pt_node& node)
{
	return node.value.id() == ids::expression ||
		node.value.id() == ids::unary_expression ||
		node.value.id() == ids::relational_expression ||
		node.value.id() == ids::assignment_expression ||
		node.value.id() == ids::postfix_expression ||
		node.value.id() == ids::expression_statement ||
		node.value.id() == ids::multiplicative_expression ||
		node.value.id() == ids::additive_expression;
}

bool is_math_expression(const pt_node& node)
{
	return node.value.id() == ids::multiplicative_expression ||
		node.value.id() == ids::additive_expression;
}

bool is_for_loop(pt_node& node)
{
	return node_is(node, ids::for_loop);
}

bool is_compound_expression(pt_node& node)
{
	return node_is(node, ids::compound);
}

add_expr construct_access_formula(const list<string>& dimensions, const list<add_expr>& indices)
{
	if (indices.size() > 1) {
		// Combine dimensions and indices to get string versions of:
		// 	staq = sum(product(dimensions), indices)
		// Where sum goes from 0 to indices.size()-1 and 
		// product goes from 0 to dimensions.size()-1.
		list<string>::const_iterator n = dimensions.begin();
		list<add_expr>::const_iterator i = indices.begin();
		list<string> str_indices;

		add_expr staq(indices.front());
		str_indices.push_back(indices.front().str());
		for (++n, ++i; n != dimensions.end() && i != indices.end(); ++n, ++i) {
			staq = add_expr(mult_expr(paren_expr(new add_expr(staq)), "*", *n), "+", mult_expr(i->str()));
			str_indices.push_back(i->str());
		}

		staq.indices(str_indices);
		return staq;
	}
	else {
		return indices.front();
	}
}

template <class F>
struct descend {
	F f;
	const int guard;
	descend() {}
	descend(F f): f(f), guard(0) {}
	descend(F f, const int g): f(f), guard(g) {}
	void operator()(pt_node& node)
	{
		if (guard == 0 || !node_is(node, guard)) {
			f(node);
			for_all(node.children, this);
		}
	}
};

template <class F, class T>
void call_descend(F f, T& t, const int g = 0)
{
	return descend<F>(f, g)(t);
}

template <class F>
struct for_all_xformations {
	F f;
	for_all_xformations(F _f): f(_f) {}
	void operator()(pt_node& node)
	{
		for_all(node.value.xformations, f);
	}
};

template <class F>
for_all_xformations<F> make_for_all_xformations(F f)
{
	return for_all_xformations<F>(f);
}

bool induction_equal(const string& induction, const conditions& c)
{
	return induction == c.induction;
}

struct array_mult_op {
	const condslist& conds;
	bool& found_induction;
	mult_expr& mult;
	string& id;
	bool found_op;

	array_mult_op(const condslist& c, bool& found, mult_expr& m, string& i):
		conds(c), found_induction(found), mult(m), id(i), found_op(false)
		{}
	void operator()(pt_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node_is(node, ids::multiplicative_expression)) {
			mult_expr m;
			array_mult_op o(conds, found_induction, m, id);
			for_all(node.children, &o);

			paren_expr p(new add_expr(m));
			if (!found_op) {
				mult.lhs(p);
			}
			else {
				mult.rhs(p);
			}
		}
		else if (is_kind_of_mul(val)) {
			mult.op(val);
			found_op = true;
		}
		else if (is_ident_or_constant(node)) {
			if (node_is(node, ids::identifier) && exists_in(conds, val, induction_equal)) {
				id = val;
				found_induction = true;
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

struct array_add_op {
	const condslist& conds;
	bool& found_induction;
	add_expr& add;
	bool found_op;

	array_add_op(const condslist& c, bool& found, add_expr& a):
		conds(c), found_induction(found), add(a), found_op(false)
		{}
	void operator()(pt_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node_is(node, ids::multiplicative_expression)) {
			mult_expr mult;
			string id;
			array_mult_op o(conds, found_induction, mult, id);
			for_all(node.children, &o);

			if (!found_op) {
				add.lhs(mult);
			}
			else {
				add.rhs(mult);
			}
		}
		else if (is_ident_or_constant(node)) {
			if (node_is(node, ids::identifier) && exists_in(conds, val, induction_equal)) {
				found_induction = true;
			}

			mult_expr mult;
			mult.lhs(val);

			if (!found_op) {
				add.lhs(mult);
			}
			else {
				add.rhs(mult);
			}
		}
		else if (is_kind_of_add(val)) {
			add.op(val);
			found_op = true;
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct array_op {
	const condslist& conds;
	bool& found_induction;
	add_expr& add;
	mult_expr lmult;

	array_op(const condslist& c, bool& found, add_expr& a):
		conds(c), found_induction(found), add(a)
		{}
	void operator()(pt_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node_is(node, ids::int_constant_dec)) {
			lmult.lhs(val);
			add.lhs(lmult);
		}
		else if (node_is(node, ids::identifier)) {
			// FIXME: do we need this or not?
			if (exists_in(conds, val, induction_equal)) {
				lmult.lhs(val);
				add.lhs(lmult);

				found_induction = true;
			}
		}
		else if (node_is(node, ids::additive_expression)) {
			array_add_op o(conds, found_induction, add);
			for_all(node.children, &o);
		}
		else if (node_is(node, ids::multiplicative_expression)) {
			string id;
			array_mult_op o(conds, found_induction, lmult, id);
			for_all(node.children, &o);

			add.lhs(lmult);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct postfix_op {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	const condslist& conds;
	sharedset& vars;
	bool found_induction;
	bool found_shared;
	bool found_private;
	shared_variable* shared_var;
	private_variable* priv_var;
	string local_id;
	list<add_expr> accesses;
	
	postfix_op(const shared_symtbl& s, const priv_symtbl& p, const condslist& c, sharedset& v):
		shared_symbols(s), priv_symbols(p), conds(c), vars(v), 
		found_induction(false), found_shared(false), found_private(false), shared_var(NULL), priv_var(NULL)
		{}
	void operator()(pt_node& node)
	{
		string val(node.value.begin(), node.value.end());
		if (node_is(node, ids::identifier)) {
			if (!found_shared) {
				shared_symtbl::const_iterator s = shared_symbols.find(val);
				priv_symtbl::const_iterator p = priv_symbols.find(val);
				if (s != shared_symbols.end()) {
					shared_var = s->second;
					vars.insert(shared_var);
					found_shared = true;
				}
				else if (p != priv_symbols.end()) {
					priv_var = p->second;
					found_private = true;
				}
				else {
					local_id = val;
				}
			}
		}
		else if (node_is(node, ids::array_index)) {
			add_expr a;
			array_op o(conds, found_induction, a);
			for_all(node.children, &o);

			accesses.push_back(a);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct struct_access_search {
	pt_node* parent;
	struct_access_search(): parent(NULL) {}

	void operator()(pt_node& node)
	{
		pt_iterator access = find_if_all(node.children, is_struct_access);

		if (access != node.children.end()) {
			parent = &node;
		}
		else {
			for_all(node.children, this);
		}
	}
};

class shared_variable_double_orientation {};

c_type postfix_postop(pt_node& node, const shared_symtbl& shared_symbols, const priv_symtbl& priv_symbols, 
			const condslist& conds, const conditions outer, operations& ops, op_type data, sharedset& vars)
{
	postfix_op o(shared_symbols, priv_symbols, conds, vars);
	for_all(node.children, &o);
	c_type type = UNKNOWN_VAR;

	if (o.found_shared && o.found_induction) {
		add_expr add = construct_access_formula(o.shared_var->dimensions(), o.accesses);
		o.shared_var->math(add);
		o.shared_var->conds(outer);

		// Column or row access?
		if (o.accesses.back().str().find(outer.induction) != string::npos) {
			if (o.shared_var->is_column()) {
				throw shared_variable_double_orientation();
			}

			o.shared_var->row();
		}
		else {
			if (o.shared_var->is_row()) {
				throw shared_variable_double_orientation();
			}

			o.shared_var->column();
		}
		
		ops.inc(data, construct_c_type(o.shared_var->type()));
		node.value.xformations.push_back(new to_buffer_space(o.shared_var, add, outer, index_adapt()(outer)));

		// The to_buffer_space xformer subsumes the code inside the original array access. But, 
		// if it accesses a field in a struct, then we want to preserve that.
		struct_access_search search;
		for_all(node.children, &search);

		pt_node copy;
		if (search.parent) {
			copy = *search.parent;
		}

		node.children.clear();
		node.children.push_back(copy);
		
		type = o.shared_var->scalar_type();
	}
	else if (o.found_private) {
		type = o.priv_var->scalar_type();
	}

	return type;
}

c_type type_promotion(const c_type l, const c_type r)
{
	if (l == DOUBLE || r == DOUBLE) {
		return DOUBLE;
	}
	else if (l == FLOAT || r == FLOAT) {
		return FLOAT;
	}
	else if (l == LONG || r == LONG) {
		return LONG;
	}
	else if (l == INT || r == INT) {
		return INT;
	}
	else if (l == CHAR || r == CHAR) {
		return CHAR;
	}

	return UNKNOWN_VAR;
}

struct local_variable_not_found {
	string name;

	local_variable_not_found(const string& n): name(n) {}
};

c_type ident_or_constant_type(const pt_node& node, const priv_symtbl& privs, const var_symtbl& locals)
{
	c_type type = UNKNOWN_VAR;

	if (is_constant(node)) {
		if (is_int_constant(node)) {
			type = INT;
		}
		else if (is_float_constant(node)) {
			type = DOUBLE;
		}
	}
	else {
		const string name = string(node.value.begin(), node.value.end());
		var_symtbl::const_iterator l;
		priv_symtbl::const_iterator p;

		if ((l = locals.find(name)) != locals.end()) {
			type = construct_c_type(l->second->type());
		}
		else if ((p = privs.find(name)) != privs.end()) {
			type = construct_c_type(p->second->type());
		}
		else {
			throw local_variable_not_found(name);
		}
	}

	return type;
}

struct multiplicative_op {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	const var_symtbl& locals;
	const condslist& conds;
	const conditions outer;
	operations& ops;
	op_type data;
	sharedset& vars;
	c_type type;
	op_type op;
	multiplicative_op(const shared_symtbl& s, const priv_symtbl& p, const var_symtbl& l, const condslist& c, const conditions out, 
			operations& o, op_type d, sharedset& v):
		shared_symbols(s), priv_symbols(p), locals(l), conds(c), outer(out), ops(o), data(d), vars(v),
		type(UNKNOWN_VAR), op(UNKNOWN_OP)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::multiplicative_expression)) {
			multiplicative_op o(shared_symbols, priv_symbols, locals, conds, outer, ops, data, vars);
			for_all(node.children, &o);
			type = o.type;
		}
		else if (node_is(node, ids::multiplicative_expression_helper)) {
			multiplicative_op o(shared_symbols, priv_symbols, locals, conds, outer, ops, data, vars);
			for_all(node.children, &o);

			type = type_promotion(type, o.type);
			ops.inc(o.op, type);
		}
		else if (node_is(node, ids::postfix_expression)) {
			type = postfix_postop(node, shared_symbols, priv_symbols, conds, outer, ops, data, vars);
		}
		else if (is_kind_of_mul(node)) {
			op = construct_op_type(string(node.value.begin(), node.value.end()));
		}
		else if (is_ident_or_constant(node)) {
			type = ident_or_constant_type(node, priv_symbols, locals);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct additive_op {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	const var_symtbl& locals;
	const condslist& conds;
	const conditions outer;
	operations& ops;
	op_type data;
	sharedset& vars;
	c_type type;
	op_type op;
	bool found_mult;
	additive_op(const shared_symtbl& s, const priv_symtbl& p, const var_symtbl& l, const condslist& c, const conditions out, 
			operations& o, op_type d, sharedset& v):
		shared_symbols(s), priv_symbols(p), locals(l), conds(c), outer(out), ops(o), data(d), vars(v), 
		type(UNKNOWN_VAR), op(UNKNOWN_OP), found_mult(false)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::multiplicative_expression)) {
			multiplicative_op o(shared_symbols, priv_symbols, locals, conds, outer, ops, data, vars);
			for_all(node.children, &o);

			type = o.type;
			found_mult = true;
		}
		else if (node_is(node, ids::additive_expression_helper)) {
			additive_op o(shared_symbols, priv_symbols, locals, conds, outer, ops, data, vars);
			for_all(node.children, &o);

			type = type_promotion(type, o.type);
			if (!o.found_mult) {
				ops.inc(o.op, type);
			}
		}
		else if (node_is(node, ids::postfix_expression)) {
			type = postfix_postop(node, shared_symbols, priv_symbols, conds, outer, ops, data, vars);
		}
		else if (is_kind_of_add(node)) {
			op = construct_op_type(string(node.value.begin(), node.value.end()));
		}
		else if (is_ident_or_constant(node)) {
			type = ident_or_constant_type(node, priv_symbols, locals);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct expression_analysis {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	const var_symtbl& locals;
	const condslist& conds;
	const conditions outer;
	operations& ops;
	op_type data;
	sharedset& vars;
	expression_analysis(const shared_symtbl& s, const priv_symtbl& p, const var_symtbl& l, const condslist& c, const conditions out, 
			operations& o, op_type d, sharedset& v):
		shared_symbols(s), priv_symbols(p), locals(l), conds(c), outer(out), ops(o), data(d), vars(v)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::postfix_expression)) {
			// Ignoring type because there's no computation.
			postfix_postop(node, shared_symbols, priv_symbols, conds, outer, ops, data, vars);
		}
		else if (node_is(node, ids::additive_expression)) {
			additive_op o(shared_symbols, priv_symbols, locals, conds, outer, ops, data, vars);
			for_all(node.children, &o);
		}
		else if (node_is(node, ids::multiplicative_expression)) {
			multiplicative_op o(shared_symbols, priv_symbols, locals, conds, outer, ops, data, vars);
			for_all(node.children, &o);
		}
		else {
			for_all(node.children, this);
		}
	};
};

void try_expression_analysis(pt_iterator begin, pt_iterator end, const shared_symtbl& shared_symbols, const priv_symtbl& priv_symbols, const var_symtbl& locals,
		const condslist& conds, const conditions curr, operations& ops, op_type data, sharedset& vars)
{
	try {
		for_each(begin, end, expression_analysis(shared_symbols, priv_symbols, locals, conds, curr, ops, data, vars));
	}
	catch (shared_variable_double_orientation e) {
		throw user_error("Shared variables can only be accessed in row major or column "
				"major format, not both. Make your own alias to get around this "
				"limitation.");
	}
	catch (local_variable_not_found e) {
		cerr << "error: local variable not found: " << e.name << endl;
	}
}

struct assignment_search {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	const var_symtbl& locals;
	const condslist& conds;
	const conditions outer;
	operations& ops;
	sharedset& out;
	sharedset in;
	assignment_search(const shared_symtbl& s, const priv_symtbl& p, const var_symtbl& l, 
			const condslist& c, const conditions& co, operations& op, sharedset& o):
		shared_symbols(s), priv_symbols(p), locals(l), conds(c), outer(co), ops(op), out(o)
		{}
	void operator()(pt_node& node)
	{
		// Needs to work for non-assignment expressions.
		if (node_is(node, ids::assignment_expression)) {
			pt_iterator eqs = find_if_all(node.children, is_equals);

			try_expression_analysis(node.children.begin(), eqs, shared_symbols, priv_symbols, locals, conds, outer, ops, STORE, out);
			try_expression_analysis(eqs, node.children.end(), shared_symbols, priv_symbols, locals, conds, outer, ops, LOAD, in);
		}
		else if (node_is(node, ids::relational_expression)) {
			try_expression_analysis(node.children.begin(), node.children.end(), shared_symbols, priv_symbols, locals, conds, outer, ops, LOAD, in);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct serial_for_op {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	var_symtbl locals;
	sharedset& global_in;
	sharedset& global_out;
	sharedset& global_inout;
	condslist conds;
	conditions curr_cond;
	operations& ops;
	sharedset in;
	sharedset out;
	sharedset inout;
	int expressions_seen; // used for figuring out if a statement is initializer, test or increment

	serial_for_op(const shared_symtbl& s, const priv_symtbl& p, sharedset& gin, sharedset& gout, sharedset& ginout, 
			condslist& c, const conditions cc, operations& o):
		shared_symbols(s), priv_symbols(p), global_in(gin), global_out(gout), global_inout(ginout), 
		conds(c), curr_cond(cc), ops(o), expressions_seen(0)
		{}
	serial_for_op(const shared_symtbl& s, const priv_symtbl& p, const var_symtbl& l, sharedset& gin, sharedset& gout, 
			sharedset& ginout, condslist& c, operations& o):
		shared_symbols(s), priv_symbols(p), locals(l), global_in(gin), global_out(gout), global_inout(ginout), 
		conds(c), ops(o), expressions_seen(0)
		{}
	void merge_inout(sharedset& g_in, sharedset& g_out, sharedset& g_inout)
	{
		// inout = intersection(in + out)
		set_intersection_all(in, out, inserter(inout, inout.begin()));

		// in -= inout
		for_all(inout, erase_from_set<shared_variable*>(in));	

		// out -= inout
		for_all(inout, erase_from_set<shared_variable*>(out));	

		// Global NEVER makes inout distinction. That ONLY happens locally.
		g_in.insert(in.begin(), in.end());
		g_out.insert(out.begin(), out.end());
		g_inout.insert(inout.begin(), inout.end());
	}

	void operator()(pt_node& node);
};

pair<pt_iterator, pt_node*> find_equals(pt_node& node)
{
	for (pt_iterator i = node.children.begin(); i != node.children.end(); ++i) {
		if (is_equals(*i)) {
			return make_pair(i, &node);
		}

		pair<pt_iterator, pt_node*> p = find_equals(*i);
		if (p.first != i->children.end()) {
			return p;
		}
	}

	return make_pair(node.children.end(), &node);
}

struct declaration_op {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	var_symtbl& locals;
	const condslist& conds;
	const conditions outer;
	operations& ops;
	sharedset in;
	string type;

	declaration_op(const shared_symtbl& s, const priv_symtbl& p, var_symtbl& l, const condslist& c, const conditions out, operations& o):
		shared_symbols(s), priv_symbols(p), locals(l), conds(c), outer(out), ops(o)
		{}
	void operator()(pt_node& node)
	{
		if (is_type_specifier(node)) {
			type = string(node.value.begin(), node.value.end());
		}
		else if (node_is(node, ids::init_declarator)) {
			pair<pt_iterator, pt_node*> p = find_equals(node);
			try_expression_analysis(p.first, p.second->children.end(), shared_symbols, priv_symbols, locals, conds, outer, ops, LOAD, in); 

			string name = string(node.children.front().value.begin(), node.children.front().value.end());
			locals[name] = new variable(type, name);
		}
		else if (node_is(node, ids::identifier)) {
			string name = string(node.value.begin(), node.value.end());
			locals[name] = new variable(type, name);
		}
		else {
			for_all(node.children, this);
		}
	}
};

template <class Pred>
pair<pt_node*, pt_node::tree_iterator> find_deep(pt_node& node, Pred p)
{
	pt_node::tree_iterator curr = find_if_all(node.children, p);
	pair<pt_node*, pt_node::tree_iterator> ret;

	if (curr != node.children.end()) {
		ret = make_pair(&node, curr);
	}
	else {
		ret = make_pair(&node, node.children.end());
	}

	for (pt_node::tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
		pair<pt_node*, pt_node::tree_iterator> nested = find_deep(*i, p);
		if (nested.second != i->children.end()) {
			ret = nested;
		}
	}

	return ret;
}

template <class Pred>
pair<pt_node*, pt_node::tree_iterator> find_shallow(pt_node& node, Pred p)
{
	pt_node::tree_iterator curr = find_if_all(node.children, p);

	if (curr != node.children.end()) {
		return make_pair(&node, curr);
	}
	else {
		for (pt_node::tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
			pair<pt_node*, pt_node::tree_iterator> nested = find_shallow(*i, p);
			if (nested.second != i->children.end()) {
				return nested;
			}
		}
	}

	return make_pair(&node, node.children.end());
}

class too_many_expression_statements {};

class build_string {
	string& str;
public:
	build_string(string& s): str(s) {}
	void operator()(pt_node& node)
	{
		str += string(node.value.begin(), node.value.end());
	}
};

struct replace_node {
	const string& to_replace;
	xformer* x;
	pt_node* matched;
	replace_node(const string& t, xformer* x): 
		to_replace(t), x(x), matched(NULL)
	{
		assert(x);	
	}

	void operator()(pt_node& node)
	{
		if (is_math_expression(node) || is_ident_or_constant(node)) {
			string str;
			call_descend(build_string(str), node);
			if (to_replace == str) {
				node.value.xformations.push_back(x);
			}

			node.children.clear();
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct modify_for_loop {
	const shared_variable* v;
	const conditions& conds;
	const string buffer_size;
	int seen;
	modify_for_loop(const shared_variable* _v, const conditions& c, const string& b): 
		v(_v), conds(c), buffer_size(b), seen(0) {}
	void operator()(pt_node& node)
	{
		if (is_expression(node)) {
			++seen;

			switch (seen) {
				case 2:	replace_node(conds.stop, new naked_string(full_adaptor(v).name()))(node);
					break;
				case 3: node.value.xformations.push_back(new loop_increment(conds.induction, buffer_size));
					node.children.clear();
					break;
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

template <class X>
void remove_xforms(pt_node& node)
{
	node.value.xformations.remove_if(is_type<X, xformer>);
}

struct find_induction {
	const conditions& conds;
	const shared_variable* cause;
	find_induction(const conditions& c, const shared_variable* ca):
		conds(c), cause(ca)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::identifier)) {
			const string val = string(node.value.begin(), node.value.end());

			if (val == conds.induction) {
				node.value.xformations.push_back(new augment_induction(index_adapt()(conds), cause));
				node.children.clear();
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct find_compound {
	const conditions& conds;
	const shared_variable* cause;
	find_compound(const conditions& c, const shared_variable* ca):
		conds(c), cause(ca)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::compound)) {
			for_all(node.children, find_induction(conds, cause));
		}
		else {
			for_all(node.children, this);
		}
	}
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
	shared_variable* max;
	max_buffer(): max(NULL) {}
	void operator()(shared_variable* v)
	{
		if (max) {
			int prev = from_string<int>(remove_multop(v->math().non_ihs(v->conds().induction).str()));
			int max_int = from_string<int>(remove_multop(max->math().non_ihs(max->conds().induction).str())); 
			if (max_int < prev) {
				max = v;
			}
		}
		else {
			max = v;
		}
	}
};

// Need to transform postfix accesses for all non-shared variables.
void loop_mitosis(pt_node& for_loop, const shared_symtbl& shared_symbols, const priv_symtbl& priv_symbols, 
		const conditions& conds, const conditions& speconds, const sharedset& seen, const string& buffer_size)
{
	const shared_variable* max = for_all(seen, max_buffer()).max;
	const string& max_factor = max->math().factor(conds.induction);
	xformerlist& lbrace = for_loop.children.back().children.front().value.xformations;
	xformerlist& rbrace = for_loop.children.back().children.back().value.xformations;

	lbrace.push_back(new buffer_loop_start(index_adapt()(conds), buffer_size, rem_adaptor(max).name(), conds.induction, conds.step));
	rbrace.push_back(new buffer_loop_stop());

	append(for_loop.value.xformations, fmap(make_reset_buf_sz(speconds), seen));
	append(for_loop.value.xformations, fmap(make_reset_rem(speconds, max_factor), seen));
	append(for_loop.value.xformations, fmap(make_reset_full(speconds.stop), seen));

	for_all(for_loop.children, find_compound(conds, max));

	pair<pt_node*, pt_node::tree_iterator> left_cmpd = find_shallow(for_loop, is_compound_expression);
	(*left_cmpd.second).value.xformations.push_back(new if_clause(rem_adaptor(max).name()));

	pt_node::tree_iterator nested_for_loop = find_shallow(for_loop, is_for_loop).second;
	if (nested_for_loop != for_loop.children.end()) {
		call_descend(make_for_all_xformations(bind(&xformer::nest_me, _1, conds)), *nested_for_loop);
	}

	pt_node::tree_iterator loop_cmpd = left_cmpd.first->children.insert(left_cmpd.second, *left_cmpd.second);
	modify_for_loop(max, conds, buffer_size)(for_loop);
	remove_xforms<if_clause>(*loop_cmpd); // Hack! hack-hack-hack

	call_descend(make_for_all_xformations(mem_fn(&xformer::remainder_me)), *(++loop_cmpd), ids::for_loop);
}

void loop_mitosis(pt_node& for_loop, const shared_symtbl& shared_symbols, const priv_symtbl& priv_symbols, 
		const conditions& conds, const sharedset& seen, const string& buffer_size)
{
	loop_mitosis(for_loop, shared_symbols, priv_symbols, conds, conds, seen, buffer_size);
}

void print_conditions(const conditions c)
{
	cout << "(" << c.start << " " << c.induction << " " << c.stop << " " << c.step << ") ";
}

struct for_compound_op {
	const shared_symtbl& shared_symbols; 
	const priv_symtbl& priv_symbols;
	var_symtbl locals;
	sharedset& in;
	sharedset& out;
	sharedset& inout;
	sharedset& global_in;
	sharedset& global_out;
	sharedset& global_inout;
	operations& ops;
	condslist& conds;
	const conditions outer;

	for_compound_op(const shared_symtbl& s, const priv_symtbl& p, const var_symtbl& l,  
			sharedset& i, sharedset& o, sharedset& io, 
			sharedset& gi, sharedset& go, sharedset& gio, 
			operations& op, condslist& c, const conditions& out): 
		shared_symbols(s), priv_symbols(p), locals(l), 
		in(i), out(o), inout(io),
		global_in(gi), global_out(go), global_inout(gio),
		ops(op), conds(c), outer(out)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::declaration)) {
			declaration_op o(shared_symbols, priv_symbols, locals, conds, outer, ops);
			for_all(node.children, &o);

			in.insert(o.in.begin(), o.in.end());
		}
		else if (node_is(node, ids::expression_statement, ids::selection_statement)) {
			assignment_search o(shared_symbols, priv_symbols, locals, conds, outer, ops, out);
			for_all(node.children, &o);

			in.insert(o.in.begin(), o.in.end());
		}

		// This is the first nested for loop occurrence. (Figuring this out by tracing the calls is 
		// confusing.)
		else if (is_for_loop(node)) {
			const size_t before_in = global_in.size();
			const size_t before_out = global_out.size();
			const size_t before_inout = global_inout.size();

			condslist::const_iterator curr_scope = previous(conds.end(), conds);
			condslist oldconds = conds;
			serial_for_op o(shared_symbols, priv_symbols, locals, global_in, global_out, global_inout, conds, ops);
			for_all(node.children, &o);
			o.merge_inout(global_in, global_out, global_inout);

			sharedset& local_in = o.in;
			sharedset& local_out = o.out;
			sharedset& local_inout = o.inout;

			/*
			if ((before_in != global_in.size() || before_out != global_out.size() || before_inout != global_inout.size()) ||
					set_union_all(local_in, local_out, local_inout).size() > 0) {
				conds = o.conds;
			}
			*/

			depths local_depths;
			for_all(local_in, make_assign_set<shared_variable*>(2, local_depths));
			for_all(local_out, make_assign_set<shared_variable*>(2, local_depths));
			for_all(local_inout, make_assign_set<shared_variable*>(3, local_depths));

			//const conditions inner = *next(curr_scope, conds);
			const conditions inner = o.conds.back();

			const fn_and<shared_variable> seen_not_in(&shared_variable::seen, &shared_variable::in_not_generated);
			const fn_and<shared_variable> seen_not_out(&shared_variable::seen, &shared_variable::out_not_generated);
			const sharedset& seen_ins = filter(seen_not_in, set_union_all(local_in, local_inout));
			const sharedset& seen_outs = filter(seen_not_out, set_union_all(local_out, local_inout));

			// TODO:
			// 	- Optimization if buffer is same size as (stop - start)?
			// 	- Combine multiple in/out calls? 

			xformerlist& lbrace = node.children.back().children.front().value.xformations;
			append(lbrace, fmap(make_choice<gen_in<row_access>, gen_in<column_access> >(inner, local_depths), seen_ins));
			lbrace.push_back(new define_variable(index_adapt()(inner)));

			xformerlist& rbrace = node.children.back().children.back().value.xformations;
			append(rbrace, fmap(make_choice<gen_out<row_access>, gen_out<column_access> >(inner, local_depths), seen_outs));

			if (seen_ins.size() > 0 || seen_outs.size() > 0) {
				/*
				conditions bridge_in = outer;
				bridge_in.induction = inner.induction;
				*/
				xformerlist& nested = node.value.xformations;
				append(nested, fmap(make_choice<gen_in_first<row_access>, gen_in_first<column_access> >(inner, local_depths), seen_ins));

				const sharedset& seen_all = set_union_all(seen_ins, seen_outs);
				const shared_variable* first = *seen_all.begin();
				const string& buffer_size = buffer_adaptor(first).size();

				loop_mitosis(node, shared_symbols, priv_symbols, inner, seen_all, buffer_size);
			}

			for_all(seen_ins, mem_fn(&shared_variable::in_generated));
			for_all(seen_outs, mem_fn(&shared_variable::out_generated));
		}
		else {
			for_all(node.children, this);
		}
	}
};

// TODO: make this more general by creating add_exprs instead of strings.
struct operator_wedge {
	pt_node*& lhs;
	pt_node*& rhs;
	bool seen_operator;
	operator_wedge(pt_node*& l, pt_node*& r):
		lhs(l), rhs(r), seen_operator(false)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::identifier) || is_constant(node) || is_expression(node)) {
			if (!seen_operator) {
				lhs = &node;
			}
			else {
				rhs = &node;
			}
		}
		else if (is_conditional_operator(node)) {
			seen_operator = true;
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct conditional_search {
	pt_node* lhs;
	pt_node* rhs;
	conditional_search():
		lhs(NULL), rhs(NULL)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::relational_expression, ids::assignment_expression)) {
			operator_wedge w(lhs, rhs);
			for_all(node.children, &w);
		}
		else {
			for_all(node.children, this);
		}
	}
};

void parse_conditions(pt_node& node, const int expressions_seen, condslist& conds, conditions& cond)
{
	if (expressions_seen >= 4) {
		throw user_error("number of expressions seen in parse_conditions.");
	}

	if (expressions_seen < 3) {
		conditional_search conditional;
		for_all(node.children, &conditional);

		if (!conditional.lhs || !conditional.rhs) {
			throw user_error("No relational or assignment expression in nested for loop.");
		}

		if (expressions_seen == 1) {
			cond.induction = string(conditional.lhs->value.begin(), conditional.lhs->value.end());
			call_descend(build_string(cond.start), *conditional.rhs);
		}
		else if (expressions_seen == 2) {
			call_descend(build_string(cond.stop), *conditional.rhs);
		}
	}
	else {
		call_descend(build_string(cond.step), node);

		if (!exists_in(conds, cond)) {
			conds.push_back(cond);
		}
	}
}

void serial_for_op::operator()(pt_node& node)
{
	if (node_is(node, ids::expression, ids::expression_statement, ids::assignment_expression, ids::unary_expression, ids::postfix_expression) && 
			expressions_seen < 3) {
		++expressions_seen;
		parse_conditions(node, expressions_seen, conds, curr_cond);
	}
	else if (node_is(node, ids::compound)) {
		for_compound_op o(shared_symbols, priv_symbols, locals, 
				in, out, inout, 
				global_in, global_out, global_inout, 
				ops, conds, curr_cond);
		for_all(node.children, &o);
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

// I AM A HACK. Every other method for figuring out which region number we're on is worse.
int __region_number = 0;

struct parallel_for_op {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	privset& privs;
	sharedset& global_in;
	sharedset& global_out;
	sharedset& global_inout;
	operations& ops;
	condslist& conds;
	pt_node& parent;
	conditions parcond;
	int expressions_seen;
	parallel_for_op(const shared_symtbl& s, const priv_symtbl& p, privset& ps, sharedset& i, sharedset& o, 
			sharedset& io, operations& op, condslist& c, pt_node& par): 
		shared_symbols(s), priv_symbols(p), privs(ps), global_in(i), global_out(o), global_inout(io), 
		ops(op), conds(c), parent(par), expressions_seen(0)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::expression, ids::expression_statement, ids::assignment_expression, ids::unary_expression, ids::postfix_expression) &&
				expressions_seen < 3) {
			++expressions_seen;
			parse_conditions(node, expressions_seen, conds, parcond);

			if (expressions_seen == 1) {
				for_all(node.children, replace_node(parcond.start, new variable_name(spe_start)));
				privs.insert(new private_variable(spe_start.type(), spe_start.name(), parcond.start, __region_number + 1));
			}
			else if (expressions_seen == 2) {
				for_all(node.children, replace_node(parcond.stop, new variable_name(spe_stop)));
				privs.insert(new private_variable(spe_stop.type(), spe_stop.name(), parcond.stop, __region_number + 1));
				++__region_number;
			}
		}
		else if (node_is(node, ids::compound)) {
			serial_for_op o(shared_symbols, priv_symbols, global_in, global_out, global_inout, conds, parcond, ops);
			o(node);

			o.merge_inout(global_in, global_out, global_inout);
			conds = o.conds;

			sharedset& in = o.in;
			sharedset& out = o.out;
			sharedset& inout = o.inout;

			depths local_depths;
			for_all(in, make_assign_set<shared_variable*>(2, local_depths));
			for_all(out, make_assign_set<shared_variable*>(2, local_depths));
			for_all(inout, make_assign_set<shared_variable*>(3, local_depths));

			// Hi, I'm an inelegant special case.
			xformerlist& lbrace = node.children.front().value.xformations;
			xformerlist& rbrace = node.children.back().value.xformations;
			const fn_and<shared_variable> row_and_flat(&shared_variable::is_row, &shared_variable::is_flat);
			const sharedset& flat_ins = filter(row_and_flat, set_union_all(in, inout));
			const sharedset& flat_outs = filter(row_and_flat, set_union_all(out, inout));
			const make_conditions<gen_in<row_access> > make_gen_in_row(parcond, local_depths);
			const make_conditions<gen_out<row_access> > make_gen_out_row(parcond, local_depths);

			const conditions specond(spe_start.name(), parcond.induction, spe_stop.name(), parcond.step);
			append(parent.value.xformations, fmap(make_conditions<gen_in_first<row_access> >(specond, local_depths), flat_ins));

			append(lbrace, fmap(make_gen_in_row, flat_ins));
			append(rbrace, fmap(make_gen_out_row, flat_outs));
			lbrace.push_back(new define_variable(index_adapt()(specond)));
		
			if (flat_ins.size() > 0 || flat_outs.size() > 0) {
				const sharedset& flat_all = set_union_all(flat_ins, flat_outs);
				const shared_variable* first = *flat_all.begin();
				const string& factor = first->math().factor(parcond.induction);
				string buffer_size = buffer_adaptor(first).size();
			
				if (factor != "") {
					buffer_size = "(" + buffer_size + "/" + factor + ")";
				}

				loop_mitosis(parent, shared_symbols, priv_symbols, parcond, specond, flat_all, buffer_size);
			}

			for_all(flat_ins, mem_fn(&shared_variable::in_generated));
			for_all(flat_outs, mem_fn(&shared_variable::out_generated));
		}
		else {
			for_all(node.children, this);
		}
	}
};

bool has_declaration(const pt_node& node)
{
	if (node_is(node, ids::declaration)) {
		return true;
	}
	else {
		for (pt_node::const_tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
			if (has_declaration(*i)) {
				return true;
			}
		}
		return false;
	}
}

struct wipeout_identifier {
	const string& ident;
	pt_node& root;
	wipeout_identifier(const string& i, pt_node& r):
		ident(i), root(r) {}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::identifier)) {
			if (ident == string(node.value.begin(), node.value.end())) {
				root.value.xformations.push_back(new nop);
				root.children.clear();
			}
		}
	}
};

struct wipeout_declarations {
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::declaration)) {
			node.value.xformations.push_back(new nop);
			node.children.clear();
		}
	}
};

struct init_declarations_to_expressions {
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::declaration_specifiers, ids::pointer) || is_type_specifier(node)) {
			node.value.xformations.push_back(new nop);
			node.children.clear();
		}
		else if (node.value.id() != ids::compound) { // new scopes can keep their declarations
			for_all(node.children, this);
		}
	}
};

bool is_const_declaration(const pt_node& node)
{
	if (node_is(node, ids::declaration_specifiers)) {
		if (node.children.empty()) {
			return false;
		}
		const pt_node& front = node.children.front();
		return "const" == string(front.value.begin(), front.value.end());
	}
	else {
		for (pt_node::const_tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
			if (is_const_declaration(*i)) {
				return true;
			}
		}
		return false;
	}
}

bool is_pure_declaration(const pt_node& node)
{
	if (node_is(node, ids::init_declarator)) {
		return false;
	}
	else {
		for (pt_node::const_tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
			if (!is_pure_declaration(*i)) {
				return false;
			}
		}
		return true;
	}
}

struct wipeout_const_and_pure_declarations {
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::declaration)) {
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

struct compound {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	privset& privs;
	sharedset& in;
	sharedset& out;
	sharedset& inout;
	operations& ops;
	condslist& conds;
	compound(const shared_symtbl& s, const priv_symtbl& p, privset& ps, sharedset& i, sharedset& o, sharedset& io, operations& op, 
			condslist& c): 
		shared_symbols(s), priv_symbols(p), privs(ps), in(i), out(o), inout(io), ops(op), conds(c)
		{}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::for_loop)) {
			parallel_for_op o(shared_symbols, priv_symbols, privs, in, out, inout, ops, conds, node);
			try {
				for_all(node.children, &o);
			} catch (multiple_parallel_induction_variables e) {
				throw user_error(string("Attempt to define multiple parallel induction variables \n\told:") 
						+ e.old + " new: " + e.attempt);
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct count_operations {
	operations& ops;
	count_operations(operations& o):
		ops(o)
		{}
	void operator()(string_node& node)
	{
		const string val = string(node.value.begin(), node.value.end());

		if (is_operation(val)) {
			ops.inc(construct_op_type(val), INT);
		}

		for_all(node.children, this);
	}
};

struct statement_op {
	operations& overhead;
	operations& startup;
	statement_op(operations& o, operations& s):
		overhead(o), startup(s)
		{}
	void operator()(string_node& node)
	{
		const string val = string(node.value.begin(), node.value.end());

		if (is_statement(node)) {
			for_all(node.children, count_operations(startup));
		}
		else if (is_operation(val)) {
			overhead.inc(construct_op_type(val), INT);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct selection_op {
	operations& overhead;
	operations& startup;
	selection_op(operations& o, operations& s):
		overhead(o), startup(s)
		{}
	void operator()(string_node& node)
	{
		const string val = string(node.value.begin(), node.value.end());

		if (node_is(node, ids::selection_statement)) {
			for_all(node.children, statement_op(overhead, startup));
		}
		else if (is_operation(val)) {
			overhead.inc(construct_op_type(val), INT);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct failure_to_parse_xformer {
	const string code;
	const string name;
	failure_to_parse_xformer(const string& c, const string& n): code(c), name(n) {}
};

operations parse_xformation(xformer* x, operations& overhead, operations& startup)
{
	const string code = (*x)("");

	string::const_iterator first = code.begin();
	string::const_iterator last = code.end();

	pt_parse_string parse = ast_parse(first, last, c_free_compound, skip);

	operations cost;
	if (parse.full) {
		for_all(parse.trees, selection_op(overhead, startup));
	}

	return cost;
}

struct calculate_cost {
	operations& overhead;
	operations& startup;
	calculate_cost(operations& o, operations& s):
		overhead(o), startup(s)
		{}
	void operator()(xformer* x)
	{
		parse_xformation(x, overhead, startup);
	}
};

struct accumulate_cost {
	operations& overhead;
	operations& startup;
	accumulate_cost(operations& o, operations& s):
		overhead(o), startup(s)
		{}
	void operator()(pt_node& node)
	{
		for_all(node.value.xformations, calculate_cost(overhead, startup));
	}
};

struct type_comparison: unary_function<const shared_variable*, void> {
	typedef bool (*c_type_comp)(const c_type, const c_type);
	c_type_comp comp;
	string winner;

	type_comparison(c_type_comp c): comp(c) {}
	void operator()(shared_variable* v)
	{
		if (winner != "") {
			if (comp(construct_c_type(buffer_adaptor(v).type()), construct_c_type(winner))) {
				winner = buffer_adaptor(v).type();
			}
		}
		else {
			winner = buffer_adaptor(v).type();
		}
	}
};

/* Reasoning about the call sequence for nested for-loops can be confusing. Excluding 
 * all of the function objects that look through declarations and assignment 
 * statements, the sequence is:
 *
 * cell_region -> compound -> parallel_for_op -> serial_for_op -> for_compound_op |
 *                                               ^---------------------------------
 */
struct cell_region {
	spelist::iterator region;

	cell_region(spelist::iterator r): region(r) {}
	void operator()(pt_node& node)
	{
		if (node_is(node, ids::compound)) {
			// Trust me, this enchances readability.
			sharedset& shared = (*region)->shared();
			privset& privs = (*region)->priv();
			const reduceset& reductions = (*region)->reductions();
			const shared_symtbl& shared_symbols = (*region)->shared_symbols();
			const priv_symtbl& priv_symbols = (*region)->priv_symbols();
			const int user_buffer = (*region)->buffer();

			// Assumption: one parallel induction variable.
			sharedset in;
			sharedset out;
			sharedset inout;
			condslist conds;
			operations iteration;

			compound o(shared_symbols, priv_symbols, privs, in, out, inout, iteration, conds);
			for_all(node.children, &o);

			depths max_depths;
			for_all(in, make_assign_set<shared_variable*>(2, max_depths));
			for_all(out, make_assign_set<shared_variable*>(2, max_depths));
			for_all(inout, make_assign_set<shared_variable*>(3, max_depths));
			for_all(privs, make_assign_set<private_variable*>(1, max_depths));

			const string& par_induction = conds.front().induction;

			const string least = for_all(shared, type_comparison(c_type_less)).winner;
			const string greatest = for_all(shared, type_comparison(c_type_greater)).winner;

			operations overhead;
			operations startup;
			//call_descend(accumulate_cost(overhead, startup), node);	
			operations total = iteration + startup + overhead;
			/*
			cout	<< "iteration " << endl << iteration 
				<< "data " << iteration.data_cycles() << ", " << "comp " << iteration.comp_cycles() << endl
				<< endl
				<< "overhead " << endl << overhead
				<< "data " << overhead.data_cycles() << ", " << "comp " << overhead.comp_cycles () << endl
				<< endl
				<< "startup " << endl << startup
				<< "data " << startup.data_cycles() << ", " << "comp " << startup.comp_cycles() << endl
				<< "total data " << total.data_cycles() << ", " << "total comp " << total.comp_cycles() << endl
				<< endl;
			*/

			/*
			string n = "1";
			for (condslist::iterator i = conds.begin(); i != conds.end(); ++i) {
				n += "*(";

				priv_symtbl::const_iterator p;
				if ((p = priv_symbols.find(i->stop)) != priv_symbols.end()) {
					n += (*p).second->definition();
				}
				else {
					n += conds.back().stop;
				}

				n += "-";

				if ((p = priv_symbols.find(i->start)) != priv_symbols.end()) {
					n += (*p).second->definition();
				}
				else {
					n += conds.back().start;
				}

				n += ")";
			}

			(*region)->estimate("estimate_cycles(" + n + "," + to_string(total.cycles()) + ", sizeof(" + greatest + "),");
			*/

			xformerlist& front = node.children.front().value.xformations;
			const shared_variable* max = for_all(shared, max_buffer()).max;

			front.push_back(new define_variable(prev));
			front.push_back(new compute_bounds(least));

			string clipped_start;
			string clipped_stop;
			int flat = filter(make_fn_and(&shared_variable::is_row, &shared_variable::is_flat), shared).size();
			if (flat) {
				clipped_start = spe_start.name();
				clipped_stop = spe_stop.name();
			}
			else {
				clipped_start = conds.back().start;
				clipped_stop = conds.back().stop;
			}
			front.push_back(new define_clipped_range(clipped_start, clipped_stop, greatest));

			append(front, fmap(make_xformer<private_buffer_size, private_variable>(), privs));
			//front.push_back(new max_buffer_size(max, user_buffer, shared.size(), par_induction, max_depths[max]));
			append(front, fmap(make_shared_buffer_size(max, user_buffer, shared.size(), par_induction, max_depths, greatest), shared));

			append(front, fmap(make_depth_xformer<buffer_allocation, shared_variable>(max_depths), shared));
			append(front, fmap(make_depth_xformer<buffer_allocation, private_variable>(max_depths), privs));
			append(front, fmap(make_depth_xformer<dma_list_allocation, shared_variable>(max_depths), shared));

			append(front, fmap(make_xformer<define_buffer, shared_variable>(), shared));
			append(front, fmap(make_xformer<define_next, shared_variable>(), shared));
			append(front, fmap(make_xformer<define_rem, shared_variable>(), shared));
			append(front, fmap(make_xformer<define_full, shared_variable>(), shared));
			append(front, fmap(make_xformer<define_reduction, reduction_variable>(), reductions));
			append(front, fmap(make_xformer<init_private_buffer, private_variable>(), privs));

			front.push_back(new total_timer_start());

			xformerlist& back = node.children.back().value.xformations;
			append(back, fmap(make_xformer<reduction_assign, reduction_variable>(), reductions));
			append(back, fmap(make_xformer<buffer_deallocation, shared_variable>(), shared));
			append(back, fmap(make_xformer<buffer_deallocation, private_variable>(), privs));
			append(back, fmap(make_depth_xformer<dma_list_deallocation, shared_variable>(max_depths), shared));
			back.push_back(new total_timer_stop());

			++region;
		}
		else {
			for_all(node.children, this);
		}
	}
};

void traverse_pt(pt& trees, spelist& regions)
{
	for_all((*trees.begin()).children, cell_region(regions.begin()));
}

