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

typedef map<xformer*, ast_node*> bind_xformer;
void add_xformer_to_node(bind_xformer::value_type pair)
{
	assert(pair.first && pair.second);
	pair.second->value.xformations.push_back(pair.first);
}

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

// FIXME: For some of these identities, I should use named constants, not string.

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

bool is_int_constant(const ast_node& node)
{
	return node.value.id() == ids::int_constant_dec;
}

bool is_float_constant(const ast_node& node)
{
	return node.value.id() == ids::float_constant_1 ||
		node.value.id() == ids::float_constant_2 ||
		node.value.id() == ids::float_constant_3;
}

bool is_constant(const ast_node& node)
{
	return is_int_constant(node) || is_float_constant(node);
}

bool is_declaration(const ast_node& node)
{
	return node.value.id() == ids::declaration;
}

bool is_bracket(const string& s)
{
	return s == ")" || s == "(" || s == "{" || s == "}" || s == "<" || s == ">" || s == "[" || s == "]";
}

bool is_kind_of_mul(const string& s)
{
	return s == "*" || s == "/" || s == "%";
}

bool is_kind_of_mul(const ast_node& node)
{
	return is_kind_of_mul(string(node.value.begin(), node.value.end()));
}

bool is_kind_of_add(const string& s)
{
	return s == "+" || s == "-";
}

bool is_kind_of_add(const ast_node& node)
{
	return is_kind_of_add(string(node.value.begin(), node.value.end()));
}

bool is_operation(const string& s)
{
	return is_kind_of_add(s) || is_kind_of_mul(s);
}

bool is_ident_or_constant(const ast_node& node)
{
	return node.value.id() == ids::identifier || is_constant(node);
}

bool is_equals(const ast_node& node)
{
	string str(node.value.begin(), node.value.end());
	return str.find("=") != string::npos;
}

bool is_relational(const ast_node& node)
{
	string str(node.value.begin(), node.value.end());
	return str.find(">") != string::npos || str.find("<") != string::npos || str.find("==") != string::npos;
}

bool is_conditional_operator(const ast_node& node)
{
	return is_relational(node) || is_equals(node);
}

bool is_type_specifier(const ast_node& node)
{
	string s(node.value.begin(), node.value.end());
	return	s == "void" || s == "char" || s == "short" || s == "int" || s == "long" ||
		s == "float" || s == "double" || s == "signed" || s == "unsigned";
}

bool is_struct_access(const ast_node& node)
{
	return node.value.id() == ids::dot || node.value.id() == ids::ptr_op;
}

template <class Node>
bool is_statement(const Node& node)
{
	return node.value.id() == ids::compound || 
		node.value.id() == ids::expression_statement || 
		node.value.id() == ids::selection_statement;
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
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node.value.id() == ids::multiplicative_expression) {
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
			if (node.value.id() == ids::identifier) {
				if (exists_in(conds, val, induction_equal)) {
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

struct array_add_op {
	const condslist& conds;
	bool& found_induction;
	add_expr& add;
	bool found_op;

	array_add_op(const condslist& c, bool& found, add_expr& a):
		conds(c), found_induction(found), add(a), found_op(false)
		{}
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node.value.id() == ids::multiplicative_expression) {
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
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node.value.id() == ids::int_constant_dec) {
			lmult.lhs(val);
			add.lhs(lmult);
		}
		else if (node.value.id() == ids::identifier) {
			// FIXME: do we need this or not?
			if (exists_in(conds, val, induction_equal)) {
				lmult.lhs(val);
				add.lhs(lmult);

				found_induction = true;
			}
		}
		else if (node.value.id() == ids::additive_expression) {
			array_add_op o(conds, found_induction, add);
			for_all(node.children, &o);
		}
		else if (node.value.id() == ids::multiplicative_expression) {
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
	list<add_expr> accesses;
	
	postfix_op(const shared_symtbl& s, const priv_symtbl& p, const condslist& c, sharedset& v):
		shared_symbols(s), priv_symbols(p), conds(c), vars(v), 
		found_induction(false), found_shared(false), found_private(false), shared_var(NULL), priv_var(NULL)
		{}
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());
		if (node.value.id() == ids::identifier) {
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
			}
		}
		else if (node.value.id() == ids::array_index) {
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
	ast_node* parent;
	struct_access_search(): parent(NULL) {}

	void operator()(ast_node& node)
	{
		ast_iterator access = find_if_all(node.children, is_struct_access);

		if (access != node.children.end()) {
			parent = &node;
		}
		else {
			for_all(node.children, this);
		}
	}
};

class shared_variable_double_orientation {};

variable_type postfix_postop(ast_node& node, const shared_symtbl& shared_symbols, const priv_symtbl& priv_symbols, 
		operations& ops, const condslist& conds, sharedset& vars)
{
	postfix_op o(shared_symbols, priv_symbols, conds, vars);
	for_all(node.children, &o);
	variable_type type = UNKNOWN_VAR;

	if (o.found_shared && o.found_induction) {
		add_expr add = make_add_expr(o.shared_var->dimensions(), o.accesses);
		//add_expr add = o.accesses.back();
		//cout << add.str() << endl;
		o.shared_var->math(add);

		// Column or row access?
		if (o.accesses.back().str().find(conds.back().induction) != string::npos) {
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
		
		if (o.shared_var->is_flat()) {
			node.value.xformations.push_back(new flat_buffer_space(o.shared_var, add));
		}
		else {
			node.value.xformations.push_back(new multi_buffer_space(o.shared_var, conds.back().induction));
		}

		// The *_buffer_space xformer subsumes the code inside the original array access. But, 
		// if it accesses a field in a struct, then we want to preserve that.
		struct_access_search search;
		for_all(node.children, &search);

		ast_node copy;
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

variable_type type_promotion(const variable_type l, const variable_type r)
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

class local_variable_not_found {};

variable_type ident_or_constant_type(ast_node& node, const var_symtbl& locals)
{
	variable_type type = UNKNOWN_VAR;

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
		var_symtbl::const_iterator l = locals.find(name);

		if (l != locals.end()) {
			type = construct_variable_type(l->second->type());
		}
		else {
			throw local_variable_not_found();
		}
	}

	return type;
}

struct multiplicative_op {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	operations& ops;
	const condslist& conds;
	sharedset& vars;
	const var_symtbl& locals;
	variable_type type;
	op_type op;
	multiplicative_op(const shared_symtbl& s, const priv_symtbl& p, operations& o, const condslist& c, sharedset& v, const var_symtbl& l):
		shared_symbols(s), priv_symbols(p), ops(o), conds(c), vars(v), locals(l), type(UNKNOWN_VAR), op(UNKNOWN_OP)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::multiplicative_expression) {
			multiplicative_op o(shared_symbols, priv_symbols, ops, conds, vars, locals);
			for_all(node.children, &o);
			type = o.type;
		}
		else if(node.value.id() == ids::multiplicative_expression_helper) {
			multiplicative_op o(shared_symbols, priv_symbols, ops, conds, vars, locals);
			for_all(node.children, &o);

			type = type_promotion(type, o.type);
			ops.inc(o.op, type);
		}
		else if (node.value.id() == ids::postfix_expression) {
			type = postfix_postop(node, shared_symbols, priv_symbols, ops, conds, vars);
		}
		else if (is_kind_of_mul(node)) {
			op = construct_op_type(string(node.value.begin(), node.value.end()));
		}
		else if (is_ident_or_constant(node)) {
			type = ident_or_constant_type(node, locals);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct additive_op {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	operations& ops;
	const condslist& conds;
	sharedset& vars;
	const var_symtbl& locals;
	variable_type type;
	op_type op;
	additive_op(const shared_symtbl& s, const priv_symtbl& p, operations& o, const condslist& c, sharedset& v, const var_symtbl& l):
		shared_symbols(s), priv_symbols(p), ops(o), conds(c), vars(v), locals(l), type(UNKNOWN_VAR), op(UNKNOWN_OP)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::multiplicative_expression) {
			multiplicative_op o(shared_symbols, priv_symbols, ops, conds, vars, locals);
			for_all(node.children, &o);

			type = o.type;
		}
		else if (node.value.id() == ids::additive_expression_helper) {
			additive_op o(shared_symbols, priv_symbols, ops, conds, vars, locals);
			for_all(node.children, &o);

			type = type_promotion(type, o.type);
			ops.inc(o.op, type);
		}
		else if (node.value.id() == ids::postfix_expression) {
			type = postfix_postop(node, shared_symbols, priv_symbols, ops, conds, vars);
		}
		else if (is_kind_of_add(node)) {
			op = construct_op_type(string(node.value.begin(), node.value.end()));
		}
		else if (is_ident_or_constant(node)) {
			type = ident_or_constant_type(node, locals);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct assignment_split {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	operations& ops;
	const condslist& conds;
	sharedset& vars;
	const var_symtbl& locals;
	assignment_split(const shared_symtbl& s, const priv_symtbl& p, operations& o, const condslist& c, sharedset& v, const var_symtbl& l):
		shared_symbols(s), priv_symbols(p), ops(o), conds(c), vars(v), locals(l)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::postfix_expression) {
			// Ignoring type because there's no computation.
			postfix_postop(node, shared_symbols, priv_symbols, ops, conds, vars);
		}
		else if (node.value.id() == ids::additive_expression) {
			additive_op o(shared_symbols, priv_symbols, ops, conds, vars, locals);
			for_all(node.children, &o);
		}
		else if (node.value.id() == ids::multiplicative_expression) {
			multiplicative_op o(shared_symbols, priv_symbols, ops, conds, vars, locals);
			for_all(node.children, &o);
		}
		else {
			for_all(node.children, this);
		}
	};
};

struct assignment_search {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	operations& ops;
	const condslist& conds;
	sharedset& out;
	const var_symtbl& locals;
	sharedset in;
	assignment_search(const shared_symtbl& s, const priv_symtbl& p, operations& op, const condslist& c, sharedset& o, const var_symtbl& l):
		shared_symbols(s), priv_symbols(p), ops(op), conds(c), out(o), locals(l)
		{}
	void operator()(ast_node& node)
	{
		// Needs to work for non-assignment expressions.
		if (node.value.id() == ids::assignment_expression) {
			ast_iterator eqs = find_if_all(node.children, is_equals);

			try {
				for_each(node.children.begin(), eqs, assignment_split(shared_symbols, priv_symbols, ops, conds, out, locals));
				for_each(eqs, node.children.end(), assignment_split(shared_symbols, priv_symbols, ops, conds, in, locals));
			}
			catch (shared_variable_double_orientation e) {
				throw user_error("Shared variables can only be accessed in row major or column "
						"major format, not both. Make your own alias to get around this "
						"limitation.");
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

typedef map<conditions_xformer*, ast_node*> bind_condxformer;

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
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	operations& ops;
	condslist& conds;
	bind_xformer& condnodes;
	const int unroll;
	sharedset in;
	sharedset out;
	sharedset inout;
	conditions sercond;
	int expressions_seen; // used for figuring out if a statement is initializer, test or increment

	serial_for_op(const shared_symtbl& s, const priv_symtbl& p, operations& o, condslist& c, bind_xformer& n, const int u):
		shared_symbols(s), priv_symbols(p), ops(o), conds(c), condnodes(n), unroll(u), expressions_seen(0)
		{}
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
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	var_symtbl& locals;
	operations& ops;
	const condslist& conds;
	sharedset in;
	string type;

	declaration_op(const shared_symtbl& s, const priv_symtbl& p, var_symtbl& l, operations& o, const condslist& c):
		shared_symbols(s), priv_symbols(p), locals(l), ops(o), conds(c)
		{}
	void operator()(ast_node& node)
	{
		// TODO: parse/grab local definitions
		if (is_type_specifier(node)) {
			type = string(node.value.begin(), node.value.end());
		}
		else if (node.value.id() == ids::init_declarator) {
			pair<ast_iterator, ast_node*> p = find_equals(node);
			for_each(p.first, p.second->children.end(), assignment_split(shared_symbols, priv_symbols, ops, conds, in, locals)); 
		}
		else if (node.value.id() == ids::identifier) {
			string name = string(node.value.begin(), node.value.end());
			locals[name] = new variable(type, name);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct extract_gen_in_rows {
	xformerlist& lifted;
	extract_gen_in_rows(xformerlist& l): lifted(l) {}

	void operator()(xformer* x)
	{
		//if (is_type<gen_in_row>(x)) {
		if (is_type<gen_in<row_access> >(x)) {
			lifted.push_back(x);
		}
	}
};

struct lift_out_gen_in_rows {
	xformerlist& lifted;
	lift_out_gen_in_rows(xformerlist& l): lifted(l) {}

	void operator()(ast_node& node)
	{
		for_all(node.value.xformations, extract_gen_in_rows(lifted));
		//node.value.xformations.remove_if(is_type<gen_in_row, xformer>);
		node.value.xformations.remove_if(is_type<gen_in<row_access>, xformer>);
	}
};

struct for_compound_op {
	const shared_symtbl& shared_symbols; 
	const priv_symtbl& priv_symbols;
	bind_condxformer& lazy_in;
	sharedset& pre_out;
	sharedset& in;
	sharedset& out;
	sharedset& inout;
	operations& ops;
	condslist& conds;
	bind_xformer& condnodes;
	const int unroll;
	const string par_induction;
	sharedset seen;
	var_symtbl locals;

	for_compound_op(const shared_symtbl& s, const priv_symtbl& p, bind_condxformer& li, sharedset& po, sharedset& i, sharedset& o, sharedset& io, 
			operations& op, condslist& c, bind_xformer& n, const int u): 
		shared_symbols(s), priv_symbols(p), lazy_in(li), pre_out(po), in(i), out(o), inout(io), ops(op), conds(c), condnodes(n), 
		unroll(u), par_induction(c.front().induction)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::declaration) {
			declaration_op o(shared_symbols, priv_symbols, locals, ops, conds);
			for_all(node.children, &o);

			// We need the indirection of o.in because in might already have variables,
			// and we don't want to generate gen_in xformations for them.
			for (sharedset::iterator i = o.in.begin(); i != o.in.end(); ++i) {
				if (seen.find(*i) == seen.end()) {
					if ((*i)->is_row()) {
						//node.value.xformations.push_back(new gen_in_row(*i, conds.back()));
						node.value.xformations.push_back(new gen_in<row_access>(*i, conds.back()));
					}
					else if ((*i)->is_column()) {
						//node.value.xformations.push_back(new gen_in_column(*i, conds.back()));
						node.value.xformations.push_back(new gen_in<column_access>(*i, conds.back()));
					}
					else {
						throw unitialized_access_orientation();
					}

					in.insert(*i);
					seen.insert(*i);
				}
			}
		}
		else if (node.value.id() == ids::expression_statement || node.value.id() == ids::selection_statement) {
			assignment_search o(shared_symbols, priv_symbols, ops, conds, pre_out, locals);
			for_all(node.children, &o);

			// Store the function object somewhere, and call it later once I know 
			// which variables are in, which are out, and which are inout
			for (sharedset::iterator i = o.in.begin(); i != o.in.end(); ++i) {
				if (seen.find(*i) == seen.end()) {
					if ((*i)->is_row()) {
						//lazy_in.insert(bind_condxformer::value_type(new gen_in_row(*i, conds.back()), &node));
						lazy_in.insert(bind_condxformer::value_type(new gen_in<row_access>(*i, conds.back()), &node));
					}
					else if ((*i)->is_column()){
						//lazy_in.insert(bind_condxformer::value_type(new gen_in_column(*i, conds.back()), &node));
						lazy_in.insert(bind_condxformer::value_type(new gen_in<column_access>(*i, conds.back()), &node));
					}
					else {
						throw unitialized_access_orientation();
					}

					seen.insert(*i);
				}
			}
		}

		// This is the first nested for loop occurrence. (Figuring this out by tracing the calls is 
		// confusing.)
		else if (node.value.id() == ids::for_loop) {
			serial_for_op o(shared_symbols, priv_symbols, ops, conds, condnodes, unroll);
			for_all(node.children, &o);
			o.merge_inout(in, out, inout);

			xformerlist& nested = node.value.xformations;
			const conditions& outer = *previous(previous(conds.end(), conds), conds);
			/*
			append(nested, fmap(make_choice<gen_in_first_row, gen_in_first_column>(outer), in));
			append(nested, fmap(make_choice<gen_in_first_row, gen_in_first_column>(outer), inout));
			*/
			append(nested, fmap(make_choice<gen_in_first<row_access>, gen_in_first<column_access> >(outer), in));
			append(nested, fmap(make_choice<gen_in_first<row_access>, gen_in_first<column_access> >(outer), inout));

			// TODO: optimization if buffer is same as last dimension?

			// It's more natural to do this in serial_for_op, at the node for a compound expression. But,
			// that means this would get called once for each for loop encountered, which generates extra 
			// xformers.
			xformerlist& rbrace = node.children.back().children.back().value.xformations;
			append(rbrace, fmap(make_choice<gen_out_row, gen_out_column>(conds.back()), out));
			append(rbrace, fmap(make_choice<gen_out_row, gen_out_column>(conds.back()), inout));

			// This is a hack. I need the induction variable of the outer loop, but the start/stop of the 
			// inner loop.
			conditions bridge = conds.back();
			bridge.induction = outer.induction;
			append(rbrace, fmap(make_choice<gen_out_final_row, gen_out_final_column>(bridge), out));
			append(rbrace, fmap(make_choice<gen_out_final_row, gen_out_final_column>(bridge), inout));
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct bind_condxformer_void_less {
	bool operator()(const void* v, const bind_condxformer::value_type& p)
	{
		return v < p.first->v;
	}

	bool operator()(const bind_condxformer::value_type& p, const void* v)
	{
		return p.first->v < v;
	}

	bool operator()(const bind_condxformer::value_type& a, const bind_condxformer::value_type& b)
	{
		return a.first->v < b.first->v;
	}
};

// TODO: make this more general by creating add_exprs instead of strings.
struct operator_wedge {
	ast_node*& lhs;
	ast_node*& rhs;
	bool seen_operator;
	operator_wedge(ast_node*& l, ast_node*& r):
		lhs(l), rhs(r), seen_operator(false)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::identifier || is_constant(node)) {
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
	ast_node* lhs;
	ast_node* rhs;
	conditional_search():
		lhs(NULL), rhs(NULL)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::relational_expression || node.value.id() == ids::assignment_expression) {
			operator_wedge w(lhs, rhs);
			for_all(node.children, &w);
		}
		else {
			for_all(node.children, this);
		}
	}
};

void serial_for_op::operator()(ast_node& node)
{
	if (node.value.id() == ids::expression || node.value.id() == ids::expression_statement) {
		++expressions_seen;

		// Second expression is the test, which will contain both the induction and 
		// conditional identifiers.
		if (expressions_seen < 3) {
			conditional_search conditional;
			for_all(node.children, &conditional);

			if (!conditional.lhs || !conditional.rhs) {
				throw user_error("No relational or assignment expression in nested for loop.");
			}

			if (expressions_seen == 1) {
				sercond.induction = string(conditional.lhs->value.begin(), conditional.lhs->value.end());
				sercond.start = string(conditional.rhs->value.begin(), conditional.rhs->value.end());
				if (unroll) {
					condnodes.insert(bind_xformer::value_type(new variable_name(unrolled), conditional.rhs));
				}
			}
			else if (expressions_seen == 2) {
				sercond.stop = string(conditional.rhs->value.begin(), conditional.rhs->value.end());
				conds.push_back(sercond);

				if (unroll) {
					condnodes.insert(bind_xformer::value_type(new variable_name(epilogue), conditional.rhs));
				}
			}
			else {
				throw user_error("number of expressions seen in serial_for_op.");
			}
		}
	}
	else if (node.value.id() == ids::compound) {
		bind_condxformer lazy_in;
		sharedset pre_out;
		for_compound_op o(shared_symbols, priv_symbols, lazy_in, pre_out, in, out, inout, ops, conds, condnodes, unroll);
		for_all(node.children, &o);

		// lazy_inout = intersection(lazy_in, pre_out)
		bind_condxformer lazy_inout;
		set_intersection_all(lazy_in, pre_out, 
				inserter(lazy_inout, lazy_inout.begin()),
				bind_condxformer_void_less());

		// out = pre_out - lazy_inout
		set_difference_all(pre_out, lazy_inout, 
				inserter(out, out.begin()),
				bind_condxformer_void_less());

		// diff_in = lazy_in - lazy_inout
		bind_condxformer diff_in;
		set_difference_all(lazy_in, lazy_inout, 
				inserter(diff_in, diff_in.begin()),
				bind_condxformer_void_less());

		// Lazily call gen_in on both in and inout variables.
		for (bind_condxformer::iterator i = diff_in.begin(); i != diff_in.end(); ++i) {
			i->second->value.xformations.push_back(i->first);
			in.insert(i->first->v);
		}

		for (bind_condxformer::iterator i = lazy_inout.begin(); i != lazy_inout.end(); ++i) {
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

template <class Row, class Column>
struct make_row_or_column: public unary_function<const shared_variable*, xformer*> {
	const string& inductions;
	make_row_or_column(const string& i): inductions(i) {}
	xformer* operator()(const shared_variable* v)
	{
		xformer* x = NULL;

		if (v->is_row()) {
			x = new Row(v, inductions);
		}
		else if (v->is_column()) {
			x = new Column(v, inductions);
		}
		else {
			throw unitialized_access_orientation();
		}

		return x;
	}
};

struct parallel_for_op {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	sharedset& in;
	sharedset& out;
	sharedset& inout;
	operations& ops;
	condslist& conds;
	bind_xformer& condnodes;
	int unroll;
	conditions parconds;
	parallel_for_op(const shared_symtbl& s, const priv_symtbl& p, sharedset& i, sharedset& o, sharedset& io, operations& op, condslist& c, bind_xformer& n, int u): 
		shared_symbols(s), priv_symbols(p), in(i), out(o), inout(io), ops(op), conds(c), condnodes(n), unroll(u)
		{}
	void operator()(ast_node& node)
	{
		// if unroll, need to change iteration parameters
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			if (ident != "SPE_start" && ident != "SPE_stop") {
				if (parconds.induction != "" && parconds.induction != ident) {
					throw multiple_parallel_induction_variables(parconds.induction, ident);
				}
				parconds.induction = ident;

				// We assume this now. In the future we should discover this.
				parconds.start = "SPE_start";
				parconds.stop = "SPE_stop";

				conds.push_back(parconds);
			}

			if (unroll) {
				if (ident == "SPE_start") {
					condnodes.insert(bind_xformer::value_type(new variable_name(unrolled), &node));
				}
				else if (ident == "SPE_stop") {
					condnodes.insert(bind_xformer::value_type(new variable_name(epilogue), &node));
				}
			}
		}
		else if (node.value.id() == ids::compound) {
			bind_xformer condnodes_col;
			serial_for_op o(shared_symbols, priv_symbols, ops, conds, condnodes_col, unroll);
			o(node);
			o.merge_inout(in, out, inout);

			const bool column = accumulate_all(set_union_all(in, out, inout), false, make_acc_or(&shared_variable::is_column));
			if (unroll && column) {
				for_all(condnodes_col, add_xformer_to_node);
			}

			xformerlist& rbrace = node.children.back().value.xformations;
			for_all(out, make_append_if<gen_out_row>(rbrace, make_fn_and(&shared_variable::is_row, &shared_variable::is_flat), parconds));
			for_all(inout, make_append_if<gen_out_row>(rbrace, make_fn_and(&shared_variable::is_row, &shared_variable::is_flat), parconds));
			for_all(out, make_append_if<gen_out_final_row>(rbrace, make_fn_and(&shared_variable::is_row, &shared_variable::is_flat), parconds));
			for_all(inout, make_append_if<gen_out_final_row>(rbrace, make_fn_and(&shared_variable::is_row, &shared_variable::is_flat), parconds));

			rbrace.push_back(new total_timer_stop());
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct unroll_single {
	const int unroll;
	unroll_single(const int u): unroll(u) {}
	void operator()(xformer* x)
	{
		x->unroll_me(unroll);
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

void epilogue_single(xformer* x)
{
	x->epilogue_me();
}

struct epilogue_all {
	void operator()(ast_node& node)
	{
		for_all(node.value.xformations, epilogue_single);
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
	const sharedset& in;
	const sharedset& inout;
	const bool is_row;
	const string& stop;
	const string& induction;
	const int unroll;
	const bool dma_unroll;
	unroll_for_op(const sharedset& i, const sharedset& io, const bool r, const string& _stop, const string& ind, const int u, const bool d): 
		in(i), inout(io), is_row(r), stop(_stop), induction(ind), unroll(u), dma_unroll(d)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::expression_statement) {
			descend< remove_xforms<variable_name> >()(node);
			if (is_row) {
				// Yeah, kinda odd. We changed the original for_loop in place when we knew 
				// we were unrolling, so we're going to remove those, then add the new ones.
				for_all(node.children, match_identifier("SPE_stop", new variable_name(unrolled)));
			}
			else {
				for_all(node.children, match_identifier(stop, new variable_name(unrolled)));
			}
		}
		else if (node.value.id() == ids::compound) {

			list<ast_node> to_copy;
			copy_all(node.children, back_inserter(to_copy));

			// First iteration doesn't need sending out stuff.
			for_all(node.children, remove_xforms<gen_out>());
			for_all(node.children, remove_xforms<gen_out_final>());

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
						//descend< remove_xforms<gen_in> >()(copy);
					}
					if (i < unroll - 2) {
						if (!dma_unroll) {
							remove_xforms<gen_out>()(copy);
						}
						remove_xforms<gen_out_final>()(copy);
					}

					node.children.insert(node.children.end() - 1, copy);
				}
			}

			// Columns already have a gen_in_first.
			ast_node& last = node.children.back();
			/*
			append(last.value.xformations, fmap(make_choice<gen_in_first_row, gen_in_first_column>(conditions("", induction, stop)), in));
			append(last.value.xformations, fmap(make_choice<gen_in_first_row, gen_in_first_column>(conditions("", induction, stop)), inout));
			*/
			append(last.value.xformations, fmap(make_choice<gen_in_first<row_access>, gen_in_first<column_access> >(conditions("", induction, stop)), in));
			append(last.value.xformations, fmap(make_choice<gen_in_first<row_access>, gen_in_first<column_access> >(conditions("", induction, stop)), inout));

			descend<epilogue_all>()(last);
			make_descend(unroll_all(unroll))(node);
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct compound {
	const shared_symtbl& shared_symbols;
	const priv_symtbl& priv_symbols;
	sharedset& in;
	sharedset& out;
	sharedset& inout;
	operations& ops;
	condslist& conds;
	bind_xformer& condnodes;
	int unroll;
	compound(const shared_symtbl& s, const priv_symtbl& p, sharedset& i, sharedset& o, sharedset& io, operations& op, condslist& c, bind_xformer& n, int u): 
		shared_symbols(s), priv_symbols(p), in(i), out(o), inout(io), ops(op), conds(c), condnodes(n), unroll(u)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::for_loop) {
			parallel_for_op o(shared_symbols, priv_symbols, in, out, inout, ops, conds, condnodes, unroll);
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

bool is_for_loop(ast_node& node)
{
	return node.value.id() == ids::for_loop;
}

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
			int max_int = from_string<int>(remove_multop(max->math().non_ihs(induction).str())); 
			int prov = from_string<int>(remove_multop(v->math().non_ihs(induction).str()));
			if (max_int < prov) {
				max = v;
			}
		}
		else {
			max = v;
		}
	}
};

template <class Pred>
pair<ast_node*, ast_node::tree_iterator> find_and_duplicate_shallow(ast_node& node, Pred p)
{
	ast_node::tree_iterator curr = find_if_all(node.children, p);

	if (curr != node.children.end()) {
		return make_pair(&node, curr);
	}
	else {
		for (ast_node::tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
			pair<ast_node*, ast_node::tree_iterator> nested = find_and_duplicate_shallow(*i, p);
			if (nested.second != i->children.end()) {
				return nested;
			}
		}
	}

	return make_pair(&node, node.children.end());
}

template <class Pred>
pair<ast_node*, ast_node::tree_iterator> find_and_duplicate_deep(ast_node& node, Pred p)
{
	ast_node::tree_iterator curr = find_if_all(node.children, p);
	pair<ast_node*, ast_node::tree_iterator> ret;

	if (curr != node.children.end()) {
		ret = make_pair(&node, curr);
	}
	else {
		ret = make_pair(&node, node.children.end());
	}

	for (ast_node::tree_iterator i = node.children.begin(); i != node.children.end(); ++i) {
		pair<ast_node*, ast_node::tree_iterator> nested = find_and_duplicate_deep(*i, p);
		if (nested.second != i->children.end()) {
			ret = nested;
		}
	}

	return ret;
}

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

		if (node.value.id() == ids::selection_statement) {
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

	ast_parse_string parse = ast_parse(first, last, c_free_compound, skip);

	if (!parse.full) {
		throw failure_to_parse_xformer(code, x->class_name());
	}

	operations cost;
	for_all(parse.trees, selection_op(overhead, startup));
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
	void operator()(ast_node& node)
	{
		for_all(node.value.xformations, calculate_cost(overhead, startup));
	}
};

bool is_row_and_flat(const shared_variable* v)
{
	return v->is_row() && v->is_flat();
}

struct cell_region {
	spelist::iterator region;

	cell_region(spelist::iterator r): region(r) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::compound) {

			// Trust me, this enchances readability.
			sharedset& shared = (*region)->shared();
			sharedset& out = (*region)->out();
			sharedset& in = (*region)->in();
			sharedset& inout = (*region)->inout();
			const privset& priv = (*region)->priv();
			const reduceset& reductions = (*region)->reductions();
			const shared_symtbl& shared_symbols = (*region)->shared_symbols();
			const priv_symtbl& priv_symbols = (*region)->priv_symbols();
			const int unroll = (*region)->unroll();
			const int buffer = (*region)->buffer();
			const bool dma_unroll = (*region)->dma_unroll();

			// Assumption: one parallel induction variable.
			condslist conds;
			bind_xformer condnodes_row;
			operations iteration;
			compound o(shared_symbols, priv_symbols, in, out, inout, iteration, conds, condnodes_row, unroll);
			for_all(node.children, &o);

			for_all(priv, assign_depth(1));
			for_all(in, assign_depth(2));
			for_all(out, assign_depth(2));
			for_all(inout, assign_depth(3));

			const string& par_induction = conds.front().induction;
			(*region)->induction(par_induction);

			// Deliberately do this AFTER compund and depth assignments, but before unrolling
			operations overhead;
			operations startup;
			try {
				make_descend(accumulate_cost(overhead, startup))(node);	
			}
			catch (failure_to_parse_xformer e)
			{
				cerr	<< "Failed to parse " << e.name << ":" << endl
					<< "---" << endl
					<< e.code << endl
					<< "---" << endl;
			}


			/*
			cout	<< "iteration: " << endl
				<< iteration 
				<< "cycles: " << iteration.cycles() << endl 
				<< endl
				<< "overhead: " << endl
				<< overhead
				<< "cycles: " << overhead.cycles() << endl
				<< endl
				<< "startup: " << endl
				<< startup 
				<< "cycles: " << startup.cycles() << endl
				<< endl
				<< "buffer size : " << estimate_buffer_size(iteration.cycles() + overhead.cycles(), startup.cycles()) 
				<< endl;
			*/

			const bool column = accumulate_all(shared, false, make_acc_or(&shared_variable::is_column));
			const bool row = accumulate_all(shared, false, make_acc_or(&shared_variable::is_row));
			if (unroll) {
				const string& serial_induction = conds.back().induction;
				const string& serial_start = conds.back().start;
				const string& serial_stop = conds.back().stop;
				string unroll_induction;

				pair<ast_node*, ast_node::tree_iterator> pos;
				if (column && row) {
					throw user_error("unroll requested when shared variables have both row and column access.");
				}
				else if (row) {
					pos = find_and_duplicate_shallow(node, is_for_loop);
					for_all(condnodes_row, add_xformer_to_node);
					unroll_induction = par_induction;
				}
				else if (column) {
					pos = find_and_duplicate_deep(node, is_for_loop);
					pos.first->children.front().value.xformations.push_back(new define_unroll_boundaries(serial_start, serial_stop, unroll));
					unroll_induction = serial_induction;
				}
				else {
					throw user_error("no row or column access found for shared variables during loop unrolling.");
				}

				assert(pos.second != node.children.end());

				ast_node::tree_iterator dup = pos.first->children.insert(pos.second, *(pos.second));
				unroll_for_op(in, inout, row, serial_stop, unroll_induction, unroll, dma_unroll)(*dup);
				descend<epilogue_all>()(*(next(dup, pos.first->children)));
			}

			xformerlist& front = node.children.front().value.xformations;
			front.push_back(new define_prev());

			if (unroll) {
				const const_variable unroll_factor("int", "unroll_factor", to_string(unroll));
				front.push_back(new define_const(unroll_factor));
			}

			append(front, fmap(make_xformer<private_buffer_size, private_variable>(), priv));
			append(front, fmap(make_shared_buffer_size(buffer, unroll), shared));

			append(front, fmap(make_xformer<buffer_allocation, shared_variable>(), shared));
			append(front, fmap(make_xformer<buffer_allocation, private_variable>(), priv));
			append(front, fmap(make_xformer<dma_list_allocation, shared_variable>(), shared));

			front.push_back(new compute_bounds((for_all(shared, max_buffer(par_induction)).max)));

			if (unroll && row) {
				const string& par_start = conds.front().start;
				const string& par_stop = conds.front().stop;

				front.push_back(new define_unroll_boundaries(par_start, par_stop, unroll));
			}

			append(front, fmap(make_xformer<define_buffer, shared_variable>(), shared));
			append(front, fmap(make_xformer<define_next, shared_variable>(), shared));

			/*
			for_all(in, make_append_if<gen_in_first_row>(front, make_fn_and(&shared_variable::is_row, &shared_variable::is_flat), conds.front()));
			for_all(inout, make_append_if<gen_in_first_row>(front, make_fn_and(&shared_variable::is_row, &shared_variable::is_flat), conds.front()));
			*/
			for_all(in, make_append_if<gen_in_first<row_access> >(front, make_fn_and(&shared_variable::is_row, &shared_variable::is_flat), conds.front()));
			for_all(inout, make_append_if<gen_in_first<row_access> >(front, make_fn_and(&shared_variable::is_row, &shared_variable::is_flat), conds.front()));

			append(front, fmap(make_xformer<init_private_buffer, private_variable>(), priv));
			append(front, fmap(make_xformer<reduction_declare, reduction_variable>(), reductions));

			front.push_back(new total_timer_start());

			xformerlist& back = node.children.back().value.xformations;
			append(back, fmap(make_xformer<reduction_assign, reduction_variable>(), reductions));
			append(back, fmap(make_xformer<buffer_deallocation, shared_variable>(), shared));
			append(back, fmap(make_xformer<buffer_deallocation, private_variable>(), priv));
			append(back, fmap(make_xformer<dma_list_deallocation, shared_variable>(), shared));

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

