#include <string>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <sstream>
#include <iterator>
#include <map>
using namespace std;

#include <boost/function.hpp>
using namespace boost;

#include "parse_tree.h"
#include "ids.h"
#include "variable.h"
#include "spe_region.h"
#include "utility.h"

const int NO_UNROLL = 0;

xformerlist_data::xformerlist_data(const xformerlist_data& o): char_data(o)
{
	is_root(o.is_root());
	id(o.id());
	value(o.value());

	for (xformerlist::const_iterator i = o.xformations.begin(); i != o.xformations.end(); ++i) {
		xformations.push_back((*i)->clone());
	}
}

xformerlist_data& xformerlist_data::operator=(const xformerlist_data& rhs)
{
	char_data::operator=(rhs);

	for (xformerlist::const_iterator i = rhs.xformations.begin(); i != rhs.xformations.end(); ++i) {
		xformations.push_back((*i)->clone());
	}

	return *this;
}

bool is_constant(ast_node& node)
{
	return node.value.id() == ids::int_constant_dec;
}

bool is_bracket(string s)
{
	return s == ")" || s == "(" || s == "{" || s == "}" || s == "<" || s == ">" || s == "[" || s == "]";
}

bool is_multop(string s)
{
	return s == "*" || s == "/" || s == "%";
}

bool is_equals(ast_node& node)
{
	string str(node.value.begin(), node.value.end());
	return str.find("=") != string::npos;
}

template <class F>
struct descend {
	F f;
	void operator()(ast_node& node)
	{
		f(node);
		for_all(node.children, this);
	}
};

class to_buffer_space: public xformer {
	string math;
	string rem;
public:
	to_buffer_space(const string& m, const string& r): math(m), rem(r) {}
	string operator()(const string& old)
	{
		return old + "[((" + math + ")" + rem + ")%" + buff_size.name() + "]";
	}

	xformer* clone() const { return new to_buffer_space(math, rem); }
};

struct mult_op {
	const symset& cond;
	bool& found_cond;
	mult_expr& math;
	bool found_op;

	mult_op(const symset& c, bool& f, mult_expr& m):
		cond(c), found_cond(f), math(m), found_op(false)
		{}
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node.value.id() == ids::identifier) {
			if (cond.find(val) != cond.end()) {
				math.ivar(val);
				found_cond = true;
			}
		}

		if (is_multop(val)) {
			math.op(val);
			found_op = true;
		}
		else if (!found_op) {
			math.build_lhs(val);
		}
		else {
			math.build_rhs(val);
		}

		for_all(node.children, this);
	}
};

struct array_op {
	const symset& cond;
	bool& found_cond;
	mult_expr& math;
	string rem;

	array_op(const symset& c, bool& f, mult_expr& m):
		cond(c), found_cond(f), math(m)
		{}
	void operator()(ast_node& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node.value.id() == ids::identifier) {
			if (cond.find(val) != cond.end()) {
				math.ivar(val);
				found_cond = true;
			}
		}
		else if (node.value.id() == ids::multiplicative_expression) {
			mult_op o(cond, found_cond, math);
			for_all(node.children, &o);
		}
		else {
			if (found_cond && !is_bracket(val)){
				rem += val;
			}

			for_all(node.children, this);
		}
	}
};

struct postfix_op {
	const symtbl& shared;
	const symset& cond;
	varset& lst;
	bool found_cond;
	bool found_shared;
	region_variable* var;
	
	postfix_op(const symtbl& s, const symset& c, varset& l):
		shared(s), cond(c), lst(l), found_cond(false), found_shared(false)
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
			mult_expr math;
			array_op o(cond, found_cond, math);
			for_all(node.children, &o);

			if (found_shared && found_cond) {
				var->math(math);
				node.value.xformations.push_back(new to_buffer_space(math.as_written(), o.rem)); 
				
				// If we don't do this, redundant printing. The to_buffer_space xformer
				// is a reduction of all the children.
				node.children.clear(); 
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct handness {
	const symtbl& shared;
	const symset& cond;
	varset& lst;
	handness(const symtbl& s, const symset& c, varset& l):
		shared(s), cond(c), lst(l)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::postfix_expression) {
			postfix_op o(shared, cond, lst);
			for_all(node.children, &o);
		}
		else {
			for_all(node.children, this);
		}
	};
};

struct expression_statement_op {
	const symtbl& shared;
	const symset& cond;
	varset& out;
	varset in;
	expression_statement_op(const symtbl& s, const symset& c, varset& o):
		shared(s), cond(c), out(o)
		{}
	void operator()(ast_node& node)
	{
		// Needs to work for non-assignment expressions.
		if (node.value.id() == ids::assignment_expression) {
			ast_iterator e;
			for (e = node.children.begin(); e != node.children.end(); ++e) {
				if (is_equals(*e)) {
					break;
				}
			}

			for_each(node.children.begin(), e, handness(shared, cond, out));
			for_each(e, node.children.end(), handness(shared, cond, in));
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct gen_in: public xformer {
	region_variable* v;
	size_t depth;
	int unroll;
	gen_in(region_variable* _v, int u): 
		v(_v), depth(2), unroll(u)
		{}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v, depth);
		orig_adaptor orig(v);

		string if_statement;

		if (!unroll) {
			if_statement = "if (!((" + v->math().as_written() + ")%" + buff_size.name() + "))"; 
		}

		return old + if_statement + "{\n" 
				"int old=" + next.name() + ";\n" +
				next.name() + "= (" + next.name() + "+1)%" + buff.depth() + ";\n"
				"mfc_get(" + 
					buff.name() + "[" + next.name() + "]," 
					"(unsigned long)(" + v->name() + "+((" + v->math().as_written() + ")+" + buff_size.name() + "))," 
					"sizeof(" + buff.type() + ") *" + buff_size.name() + "," +
					next.name() + ", 0, 0);\n" +
				orig.name() + "=" + buff.name() + "[old];\n"
				"MMGP_SPE_dma_wait(old);\n"
			"}\n";
	}

	xformer* clone() const { return new gen_in(v, unroll); }
	void unroll_me(int u) { unroll = u; }
	void make_triple() { depth = 3; }
};

typedef multimap<ast_node*, gen_in*> bind_gen_in;

struct for_compound_op {
	const symtbl& shared; 
	const symset& cond;
	varset& out;
	bind_gen_in& in;
	int unroll;
	varset seen;
	for_compound_op(const symtbl& s, symset& c, varset& o, bind_gen_in& l, int u): 
		shared(s), cond(c), out(o), in(l), unroll(u)
		{}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::expression_statement || node.value.id() == ids::selection_statement) {
			expression_statement_op o(shared, cond, out);
			for_all(node.children, &o);

			// Store the function object somewhere, and call it later once I know 
			// which variables are in, which are out, and which are inout
			for (varset::iterator i = o.in.begin(); i != o.in.end(); ++i) {
				if (seen.find(*i) == seen.end()) {
					in.insert(bind_gen_in::value_type(&node, new gen_in(*i, unroll)));
					seen.insert(*i);
				}
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct gen_out: public xformer {
	const region_variable* v;
	const int depth;
	int unroll;

	gen_out(const region_variable* v, int d, int u): v(v), depth(d), unroll(u) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v, depth);
		orig_adaptor orig(v);
		string var_switch;
		string if_statement;

		if (depth < 3) {
			next_adaptor next(v);

			var_switch =	next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; \n" +
					orig.name() + "=" + buff.name() + "[" + next.name() + "];";
		}

		if (!unroll) {
			if_statement = "if (!(" + v->math().next_iteration() + "%" + buff_size.name() + "))";
		}

		return if_statement + "{\n"
					"MMGP_SPE_dma_wait(out_tag); \n"
					"mfc_put(" + orig.name() + "," + "(unsigned long)(" + v->name() + 
							"+" + v->math().next_iteration() + "-" + buff_size.name() + "),"
						"sizeof(" + buff.type() + ")*" + buff_size.name() + ","
						"out_tag, 0, 0); \n" +
					var_switch +
			"} \n" + old;
	}

	xformer* clone() const { return new gen_out(v, depth, unroll); }
	void unroll_me(int u) { unroll = u; }
};

struct gen_final_out: public xformer {
	const region_variable* v;
	int unroll;

	gen_final_out(const region_variable* v, int u): v(v), unroll(u) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v, 2);
		orig_adaptor orig(v);
		string ret;

		if (!unroll) {
			ret = "if ((((SPE_stop - SPE_start)" + v->math().factor() + ")%buff_size)) {" +
					"MMGP_SPE_dma_wait(out_tag); \n mfc_put(" + 
					orig.name() + "," + 
					"(unsigned long)(" + v->name() +
					"+(SPE_stop" + v->math().factor() + ")-(((SPE_stop-SPE_start)" + v->math().factor() + ")%" + 
					buff_size.name() + ")), sizeof(" + buff.type() + ")*(((SPE_stop-SPE_start)" + v->math().factor() +
					")%" + buff_size.name() + "),out_tag, 0, 0); \n" +
				"MMGP_SPE_dma_wait(out_tag); \n }";
		}

		return old + ret;
	}

	xformer* clone() const { return new gen_final_out(v, unroll); }
	void unroll_me(int u) { unroll = u; }
};

class make_reduction_declarations: public xformer {
	const region_variable* v;
public:
	make_reduction_declarations(const region_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return old + orig_adaptor(v).declare() + "; \n";
	}

	xformer* clone() const { return new make_reduction_declarations(v); }
};

struct create_make_reduction_declarations: unary_function<const region_variable*, xformer*> {
	xformer* operator()(const region_variable* v)
	{
		return new make_reduction_declarations(v);
	}
};

struct void_less {
	bool operator()(const void* v, const bind_gen_in::value_type& p)
	{
		if (v < p.first) return true;
		else return false;
	}

	bool operator()(const bind_gen_in::value_type& p, const void* v)
	{
		if (p.first < v) return true;
		else return false;
	}

	bool operator()(const bind_gen_in::value_type& a, const bind_gen_in::value_type& b)
	{
		if (a.first < b.first) return true; 
		else return false;
	}
};

struct create_gen_out: public unary_function<const region_variable*, xformer*> {
	const int depth;
	int unroll;
	create_gen_out(const int d, const int u): depth(d), unroll(u) {}
	xformer* operator()(const region_variable* v)
	{
		return new gen_out(v, depth, unroll);
	}
};

struct create_gen_final_out: public unary_function<const region_variable*, xformer*> {
	const int unroll;
	create_gen_final_out(const int u): unroll(u) {}
	xformer* operator()(const region_variable* v)
	{
		return new gen_final_out(v, unroll);
	}
};

struct for_op {
	const symtbl& shared;
	symset& cond;
	varset& in;
	varset& out;
	varset& inout;
	int unroll;
	for_op(const symtbl& s, symset& c, varset& i, varset& o, varset& io, int u): 
		shared(s), cond(c), in(i), out(o), inout(io), unroll(u) {}
	void operator()(ast_node& node)
	{
		// if unroll, need to change iteration parameters
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			cond.insert(ident);
		}
		else if (node.value.id() == ids::compound) {
			bind_gen_in lazy_in;
			varset pre_out;

			for_compound_op o(shared, cond, pre_out, lazy_in, NO_UNROLL);
			for_all(node.children, &o);

			bind_gen_in lazy_inout;
			set_intersection(lazy_in.begin(), lazy_in.end(), 
					pre_out.begin(), pre_out.end(), 
					inserter(lazy_inout, lazy_inout.begin()),
					void_less());

			set_difference(pre_out.begin(), pre_out.end(), 
					lazy_inout.begin(), lazy_inout.end(), 
					inserter(out, out.begin()),
					void_less());

			bind_gen_in diff_in;
			set_difference(lazy_in.begin(), lazy_in.end(), 
					lazy_inout.begin(), lazy_inout.end(), 
					inserter(diff_in, diff_in.begin()),
					void_less());

			// Lazily call gen_in on both in and inout variables.
			for (bind_gen_in::iterator i = diff_in.begin(); i != diff_in.end(); ++i) {
				i->first->value.xformations.push_back(i->second);
				in.insert(i->second->v);
			}

			for (bind_gen_in::iterator i = lazy_inout.begin(); i != lazy_inout.end(); ++i) {
				i->second->make_triple(); // inout variables need triple buffers

				i->first->value.xformations.push_back(i->second);
				inout.insert(i->second->v);
			}

			xformerlist& xformations = node.children.back().value.xformations;
			append(xformations, fmap(create_gen_out(3, NO_UNROLL), inout));
			append(xformations, fmap(create_gen_out(3, NO_UNROLL), out));
			append(xformations, fmap(create_gen_final_out(NO_UNROLL), inout));
			append(xformations, fmap(create_gen_final_out(NO_UNROLL), out));
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct unroll_xformation {
	const int unroll;
	unroll_xformation(const int u): unroll(u) {}
	void operator()(xformer* xformer)
	{
		xformer->unroll_me(unroll);
	}
};

struct transform_all {
	const int unroll;
	transform_all(const int u): unroll(u) {}
	void operator()(ast_node& node)
	{
		for_all(node.value.xformations, unroll_xformation(unroll));
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

struct copy_expressions {
	list<ast_node>& nodes;
	copy_expressions(list<ast_node>& n): nodes(n) {}
	void operator()(ast_node& node)
	{
		if (!has_declaration(node)) {
			nodes.push_back(node);

			// This is the first unrolled section, so we don't need 
			// any of the sending stuff.
			remove_xforms<gen_out>()(node);
			remove_xforms<gen_final_out>()(node);
		}
	}
};

struct unroll_for_op {
	const symtbl& shared;
	symset& cond;
	const int unroll;
	unroll_for_op(const symtbl& s, symset& c, const int u): 
		shared(s), cond(c), unroll(u) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::compound) {
			for_all(node.children, transform_all(unroll));

			list<ast_node> to_copy;
			for_all(node.children, copy_expressions(to_copy));

			// We copy it unroll-1 times because the first iteration
			// already exists.
			for (int i = 0; i < unroll - 1; ++i) {
				for (list<ast_node>::iterator j = to_copy.begin(); j != to_copy.end(); ++j) {
					ast_node copy = *j;

					// All "inner" iterations don't need any in/out xformations, 
					// but the final iteration needs out xformations.
					descend< remove_xforms<gen_in> >()(copy);
					if (i < unroll - 2) {
						remove_xforms<gen_out>()(copy);
						remove_xforms<gen_final_out>()(copy);
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

struct compound {
	const symtbl& shared;
	symset& cond;
	varset& in;
	varset& out;
	varset& inout;
	int unroll;
	compound(const symtbl& s, symset& c, varset& i, varset& o, varset& io, int u): 
		shared(s), cond(c), in(i), out(o), inout(io), unroll(u) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::for_loop) {
			for_op o(shared, cond, in, out, inout, unroll);
			for_all(node.children, &o);
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

struct make_reduction_assignments: public xformer {
	const region_variable* v;
	make_reduction_assignments(const region_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return old + "*" + v->name() + "=" + orig_adaptor(v).name() + "; \n";
	}

	xformer* clone() const { return new make_reduction_assignments(v); }
};

struct create_make_reduction_assignments: unary_function<const region_variable*, xformer*> {
	xformer* operator()(const region_variable* v)
	{
		return new make_reduction_assignments(v);
	}
};

class init_buffers: public xformer {
	const region_variable* v;
	const size_t depth;

public:
	init_buffers(const region_variable* _v, size_t d): v(_v), depth(d) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v, depth);
		next_adaptor next(v);
		orig_adaptor orig(v);

		return	old + 
			orig.declare() + "; \n" +
			next.declare() + " = 0; \n" +
			orig.name() + " = " + buff.name() + "[" + next.name() + "]; \n";
	}

	xformer* clone() const { return new init_buffers(v, depth); }
};

struct create_init_buffers: public unary_function<const region_variable*, xformer*> {
	const size_t depth;
	create_init_buffers(const size_t depth): depth(depth) {}
	xformer* operator()(const region_variable* v)
	{
		return new init_buffers(v, depth);
	}
};
	
class in_init_buffers: public xformer {
	const region_variable* v;
	const size_t depth;

public:
	in_init_buffers(const region_variable* _v, size_t d): v(_v), depth(d) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v, depth);
		next_adaptor next(v);

		return	init_buffers(v, depth)(old) +
			"mfc_get(" +
				buff.name() + "[" + next.name() + "], " 
				"(unsigned long)(" + v->name() + " + (SPE_start" + v->math().factor() + ")),"
				"sizeof(" + buff.type() + ")*" + buff_size.name() + ", " +
				next.name() + ", 0, 0); \n";
	}

	xformer* clone() const { return new in_init_buffers(v, depth); }
};

struct create_in_init_buffers: public unary_function<const region_variable*, xformer*> {
	const size_t depth;
	create_in_init_buffers(const size_t depth): depth(depth) {}
	xformer* operator()(const region_variable* v)
	{
		return new in_init_buffers(v, depth);
	}
};

class init_private_buffers: public xformer {
	const region_variable* v;

public:
	init_private_buffers(const region_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string ret;
		if (v->is_pointer()) {
			buffer_adaptor buff(v, 1);
			orig_adaptor orig(v);

			ret =	"mfc_get(" +
					buff.name() + "," +
					"(unsigned long)" + orig.name() + "," +
					"sizeof(" + buff.type() + ")*" + buff_size.name() + ","
					"3, 0, 0); \n" + 
				orig.name() + "=" + buff.name() + ";"
				"MMGP_SPE_dma_wait(3); \n";
		}

		return old + ret;
	}

	xformer* clone() const { return new init_private_buffers(v); }
};

struct create_init_private_buffers: public unary_function<const region_variable*, xformer*> {
	xformer* operator()(const region_variable* v)
	{
		return new init_private_buffers(v);
	}
};

/*
struct deep_copy {
	xformerlist_data operator()(const xformerlist_data& node)
	{
		dupe.value.value(node.value.value());
		for (xformerlist::iterator it = dupe.value.xformations.begin(); it != dupe.value.xformations.end(); ++it) {
			*it = *(it)->clone();
		}
		for_all(node.children, deep_copy(back_inserter(dupe.children)));
	}
};
*/

struct cell_region {
	spelist::iterator region;

	cell_region(spelist::iterator r): region(r) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::compound) {
			// We only support one for loop per cell_region & compound 
			// statement, so we might as well define cond here and just 
			// deal with supporting multiple loops in one region when it's 
			// needed.
			symset cond;
			compound o(*((*region)->symbols()), cond,
					(*region)->in(), (*region)->out(), (*region)->inout(), 
					(*region)->unroll());

			if ((*region)->unroll()) {
				ast_node::tree_iterator unroll_pos = for_all_duplicate(node.children, &o, is_for_loop);

				if (unroll_pos != node.children.end()) {
					unroll_for_op(*((*region)->symbols()), cond, (*region)->unroll())(*unroll_pos);
				}
			}
			else {
				for_all(node.children, &o);
			}

			xformerlist& front_xforms = node.children.front().value.xformations;
			append(front_xforms, fmap(create_init_buffers(2), (*region)->out()));
			append(front_xforms, fmap(create_in_init_buffers(2), (*region)->in()));
			append(front_xforms, fmap(create_in_init_buffers(3), (*region)->inout()));
			append(front_xforms, fmap(create_init_private_buffers(), (*region)->priv()));
			append(front_xforms, fmap(create_make_reduction_declarations(), (*region)->reductions()));

			xformerlist& back_xforms = node.children.back().value.xformations;
			append(back_xforms, fmap(create_make_reduction_assignments(), (*region)->reductions()));

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

