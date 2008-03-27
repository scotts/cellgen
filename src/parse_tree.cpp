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
	string math;
	string rem;
	const region_variable* v;
public:
	to_buffer_space(const string& m, const string& r, const region_variable* v): math(m), rem(r), v(v) {}
	string operator()(const string& old)
	{
		return old + "[((" + math + ")" + rem + ")%" + buffer_adaptor(v).size() + "]";
	}

	xformer* clone() const { return new to_buffer_space(math, rem, v); }
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
				node.value.xformations.push_back(new to_buffer_space(math.as_written(), o.rem, var)); 
				
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
	int unroll;
	gen_in(region_variable* _v, int u): 
		v(_v), unroll(u) {}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		string if_statement;

		if (!unroll) {
			if_statement = "if (!((" + v->math().as_written() + ")%" + buff.size() + "))"; 
		}

		return old + if_statement + "{\n" 
				"int old=" + next.name() + ";\n" +
				next.name() + "= (" + next.name() + "+1)%" + buff.depth() + ";\n"
				"mfc_get(" + 
					buff.name() + "[" + next.name() + "]," 
					"(unsigned long)(" + v->name() + "+((" + v->math().as_written() + ")+" + buff.size() + "))," 
					"sizeof(" + buff.type() + ") *" + buff.size() + "," +
					next.name() + ", 0, 0);\n" +
				orig.name() + "=" + buff.name() + "[old];\n"
				"MMGP_SPE_dma_wait(old);\n"
			"}\n";
	}

	xformer* clone() const { return new gen_in(v, unroll); }
	void unroll_me(int u) { unroll = u; }
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
	int unroll;

	gen_out(const region_variable* v, int u): v(v), unroll(u) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		string var_switch;
		string if_statement;

		if (v->depth() < 3) {
			next_adaptor next(v);

			var_switch =	next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; \n" +
					orig.name() + "=" + buff.name() + "[" + next.name() + "];";
		}

		if (!unroll) {
			if_statement = "if (!(" + v->math().next_iteration() + "%" + buff.size() + "))";
		}

		return if_statement + "{\n"
					"MMGP_SPE_dma_wait(out_tag); \n"
					"mfc_put(" + orig.name() + "," + "(unsigned long)(" + v->name() + 
							"+" + v->math().next_iteration() + "-" + buff.size() + "),"
						"sizeof(" + buff.type() + ")*" + buff.size() + ","
						"out_tag, 0, 0); \n" +
					var_switch +
			"} \n" + old;
	}

	xformer* clone() const { return new gen_out(v, unroll); }
	void unroll_me(int u) { unroll = u; }
};

struct gen_final_out: public xformer {
	const region_variable* v;
	int unroll;

	gen_final_out(const region_variable* v, int u): v(v), unroll(u) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		string ret;

		if (!unroll) {
			ret = "if ((((SPE_stop - SPE_start)" + v->math().factor() + ")%" + buff.size() + ")) {" +
					"MMGP_SPE_dma_wait(out_tag); \n mfc_put(" + 
					orig.name() + "," + 
					"(unsigned long)(" + v->name() +
					"+(SPE_stop" + v->math().factor() + ")-(((SPE_stop-SPE_start)" + v->math().factor() + ")%" + 
					buff.size() + ")), sizeof(" + buff.type() + ")*(((SPE_stop-SPE_start)" + v->math().factor() +
					")%" + buff.size() + "),out_tag, 0, 0); \n" +
				"MMGP_SPE_dma_wait(out_tag); \n }";
		}

		return old + ret;
	}

	xformer* clone() const { return new gen_final_out(v, unroll); }
	void unroll_me(int u) { unroll = u; }
};

class reduction_declare: public xformer {
	const region_variable* v;
public:
	reduction_declare(const region_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return old + orig_adaptor(v).declare() + "; \n";
	}

	xformer* clone() const { return new reduction_declare(v); }
};

struct create_reduction_declare: unary_function<const region_variable*, xformer*> {
	xformer* operator()(const region_variable* v)
	{
		return new reduction_declare(v);
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
	xformer* operator()(const region_variable* v)
	{
		return new gen_out(v, NO_UNROLL);
	}
};

struct create_gen_final_out: public unary_function<const region_variable*, xformer*> {
	xformer* operator()(const region_variable* v)
	{
		return new gen_final_out(v, NO_UNROLL);
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

	xformer* clone() const { return new variable_name(v); }
};

class assign_depth {
	int d;
public:
	assign_depth(int d): d(d) {}
	void operator()(region_variable* v)
	{
		if (v->is_pointer()) {
			v->depth(d);
		}
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
			if (ident != "SPE_start" && ident != "SPE_stop") {
				cond.insert(ident);
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
			bind_gen_in lazy_in;
			varset pre_out;

			for_compound_op o(shared, cond, pre_out, lazy_in, NO_UNROLL);
			for_all(node.children, &o);

			bind_gen_in lazy_inout;
			set_intersection_all(lazy_in, pre_out, 
					inserter(lazy_inout, lazy_inout.begin()), void_less());
			set_difference_all(pre_out, lazy_inout, 
					inserter(out, out.begin()), void_less());

			bind_gen_in diff_in;
			set_difference_all(lazy_in, lazy_inout, 
					inserter(diff_in, diff_in.begin()), void_less());

			// Lazily call gen_in on both in and inout variables.
			for (bind_gen_in::iterator i = diff_in.begin(); i != diff_in.end(); ++i) {
				i->first->value.xformations.push_back(i->second);
				in.insert(i->second->v);
			}

			for (bind_gen_in::iterator i = lazy_inout.begin(); i != lazy_inout.end(); ++i) {
				i->first->value.xformations.push_back(i->second);
				inout.insert(i->second->v);
			}

			// Finally, we know what kind of buffers the variables need.
			for_all(in, assign_depth(2));
			for_all(out, assign_depth(2));
			for_all(inout, assign_depth(3));

			xformerlist& xformations = node.children.back().value.xformations;
			append(xformations, fmap(create_gen_out(), inout));
			append(xformations, fmap(create_gen_out(), out));
			append(xformations, fmap(create_gen_final_out(), inout));
			append(xformations, fmap(create_gen_final_out(), out));
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

struct nop: public xformer {
	string operator()(const string&)
	{
		return "";
	}
	xformer* clone() const { return new nop; }
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
		else {
			for_all(node.children, this);
		}
	}
};

string var_increment(const string& old, const string var)
{
	return old + "++" + var + ";";
}

struct cond_increment: public xformer {
	symset cond;
	cond_increment(symset c): cond(c) {}
	string operator()(const string& old)
	{
		return accumulate_all(cond, old, var_increment);
	}

	xformer* clone() const { return new cond_increment(cond); }
};

struct unroll_for_op {
	const symtbl& shared;
	symset& cond;
	const int unroll;
	unroll_for_op(const symtbl& s, symset& c, const int u): 
		shared(s), cond(c), unroll(u) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::expression_statement) {
			// Yeah, kinda odd. We changed the original for_loop in place when we knew 
			// we were unrolling, so we're going to remove those, then add the new ones.
			descend< remove_xforms<variable_name> >()(node);
			for_all(node.children, match_identifier("SPE_stop", new variable_name(unrolled)));
		}
		else if (node.value.id() == ids::postfix_expression) {
			for (symset::iterator i = cond.begin(); i != cond.end(); ++i) {
				for_all(node.children, make_descend(wipeout_identifier(*i, node)));
			}
		}
		else if (node.value.id() == ids::compound) {
			for_all(node.children, transform_all(unroll));

			list<ast_node> to_copy;
			for_all(node.children, copy_expressions(to_copy));

			if (!to_copy.empty()) {
				to_copy.front().value.xformations.push_back(new cond_increment(cond));
			}

			// We copy it unroll-1 times because the first iteration
			// already exists.
			for (int i = 0; i < unroll - 1; ++i) {
				for (list<ast_node>::iterator j = to_copy.begin(); j != to_copy.end(); ++j) {
					ast_node copy = *j;

					// All "inner" iterations don't need any in/out xformations, 
					// but the final iteration needs out xformations. The final node 
					// of the final iteration needs an increment so the condition 
					// variable is correct for the next iteration.
					descend< remove_xforms<gen_in> >()(copy);
					if (i < unroll - 2) {
						remove_xforms<gen_out>()(copy);
						remove_xforms<gen_final_out>()(copy);
					}
					else if (next(j) == to_copy.end()) {
						copy.value.xformations.push_front(new cond_increment(cond));
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
	const region_variable* v;
public:
	compute_bounds(const region_variable* v): v(v) {}
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

	xformer* clone() const { return new compute_bounds(v); }
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

struct reduction_assign: public xformer {
	const region_variable* v;
	reduction_assign(const region_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return "*" + v->name() + "=" + orig_adaptor(v).name() + "; \n" + old;
	}

	xformer* clone() const { return new reduction_assign(v); }
};

struct create_reduction_assign: unary_function<const region_variable*, xformer*> {
	xformer* operator()(const region_variable* v)
	{
		return new reduction_assign(v);
	}
};

class init_buffers: public xformer {
	const region_variable* v;

public:
	init_buffers(const region_variable* _v): v(_v) {}
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

	xformer* clone() const { return new init_buffers(v); }
};

struct create_init_buffers: public unary_function<const region_variable*, xformer*> {
	xformer* operator()(const region_variable* v)
	{
		return new init_buffers(v);
	}
};
	
class in_init_buffers: public xformer {
	const region_variable* v;

public:
	in_init_buffers(const region_variable* _v): v(_v) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return	init_buffers(v)(old) +
			"mfc_get(" +
				buff.name() + "[" + next.name() + "], " 
				"(unsigned long)(" + v->name() + " + (SPE_start" + v->math().factor() + ")),"
				"sizeof(" + buff.type() + ")*" + buff.size() + ", " +
				next.name() + ", 0, 0); \n";
	}

	xformer* clone() const { return new in_init_buffers(v); }
};

struct create_in_init_buffers: public unary_function<const region_variable*, xformer*> {
	xformer* operator()(const region_variable* v)
	{
		return new in_init_buffers(v);
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

	xformer* clone() const { return new init_private_buffers(v); }
};

struct create_init_private_buffers: public unary_function<const region_variable*, xformer*> {
	xformer* operator()(const region_variable* v)
	{
		return new init_private_buffers(v);
	}
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

	xformer* clone() const { return new unroll_boundaries(unroll); }
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
	region_variable* max;
	max_buffer(): max(NULL) {}
	void operator()(region_variable* v)
	{
		if (max) {
			if (	from_string<int>(remove_multop(max->math().factor())) < 
				from_string<int>(remove_multop(v->math().factor()))
			) {
				max = v;
			}
		}
		else {
			max = v;
		}
	}
};

struct cell_region {
	spelist::iterator region;

	cell_region(spelist::iterator r): region(r) {}
	void operator()(ast_node& node)
	{
		if (node.value.id() == ids::compound) {
			// Only depth we know before any tree traversal.
			for_all((*region)->priv(), assign_depth(1));

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

			// We're assuming here that all buffers have the same size, so 
			// using any of (in ^ out ^ inout) should be valid.
			varset combined;
			set_union_all((*region)->in(), (*region)->out(), (*region)->inout(), 
					inserter(combined, combined.begin()));
			front_xforms.push_back(new compute_bounds((for_all(combined, max_buffer()).max)));

			append(front_xforms, fmap(create_init_buffers(), (*region)->out()));
			append(front_xforms, fmap(create_in_init_buffers(), (*region)->in()));
			append(front_xforms, fmap(create_in_init_buffers(), (*region)->inout()));
			append(front_xforms, fmap(create_init_private_buffers(), (*region)->priv()));
			append(front_xforms, fmap(create_reduction_declare(), (*region)->reductions()));

			// Order matters; unroll_boundaries generates code that depends on 
			// code generated from compute_bounds.
			if ((*region)->unroll()) {
				front_xforms.push_back(new unroll_boundaries((*region)->unroll()));
			}

			xformerlist& back_xforms = node.children.back().value.xformations;
			append(back_xforms, fmap(create_reduction_assign(), (*region)->reductions()));

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

