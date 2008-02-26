#include <string>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <iterator>
#include <map>
using namespace std;

#include <boost/function.hpp>
using namespace boost;

#include "parse_tree.h"

const int NO_UNROLL = 0;

bool is_constant(tree_node_t& node)
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

bool is_equals(tree_node_t& node)
{
	string str(node.value.begin(), node.value.end());
	return str.find("=") != string::npos;
}


class to_buffer_space: node_xformer {
	string math;
	string rem;
public:
	to_buffer_space(const string& m, const string& r): math(m), rem(r) {}
	string operator()(tree_node_t& node)
	{
		return "[((" + math + ")" + rem + ")%" + buff_size.name() + "]";
	}
};

struct mult_op {
	const symset& cond;
	bool& found_cond;
	mult_expr& math;
	bool found_op;

	mult_op(const symset& c, bool& f, mult_expr& m):
		cond(c), found_cond(f), math(m), found_op(false)
		{}
	void operator()(tree_node_t& node)
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
	void operator()(tree_node_t& node)
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
	varlist& lst;
	bool found_cond;
	bool found_shared;
	region_variable* var;
	
	postfix_op(const symtbl& s, const symset& c, varlist& l):
		shared(s), cond(c), lst(l), found_cond(false), found_shared(false)
		{}
	void operator()(tree_node_t& node)
	{
		string val(node.value.begin(), node.value.end());
		if (node.value.id() == ids::identifier) {
			if (!found_shared) {
				symtbl::const_iterator it = shared.find(val);
				if (it != shared.end()) {
					var = it->second;
					lst.push_back(var);
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
				// XFORM CHANGE
				node.value.value("[((" + math.as_written() + ")" + o.rem + ")%" + buff_size.name() + "]");
				//node.value.xformations.push_back(new to_buffer_space(math.as_written(), o.rem)); 
				
				// If we don't do this, redundant printing. The above is a string 
				// reduction of all the children.
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
	varlist& lst;
	handness(const symtbl& s, const symset& c, varlist& l):
		shared(s), cond(c), lst(l)
		{}
	void operator()(tree_node_t& node)
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
	varlist& out;
	varlist in;
	expression_statement_op(const symtbl& s, const symset& c, varlist& o):
		shared(s), cond(c), out(o)
		{}
	void operator()(tree_node_t& node)
	{
		// Needs to work for non-assignment expressions.
		if (node.value.id() == ids::assignment_expression) {
			tree_iterator_t e;
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

// XFORM CHANGE
struct gen_in: public unary_function<void, region_variable*> {
	tree_node_t& node;
	size_t depth;
	int unroll;
	gen_in(tree_node_t& n, int u): 
		node(n), depth(buffer_adaptor::dbl), unroll(u)
		{}
	void operator()(const region_variable* v)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v, depth);
		orig_adaptor orig(v);

		string use(node.value.begin(), node.value.end());
		string if_statement;

		cout << "unroll: " << unroll << endl;
		if (!unroll) {
			if_statement = "if (!((" + v->math().as_written() + ")%" + buff_size.name() + "))"; 
		}

		use += node.value.value();
		node.value.value(if_statement + "{\n" 
				"int old=" + next.name() + ";\n" +
				next.name() + "= (" + next.name() + "+1)%" + buff.depth() + ";\n"
				"mfc_get(" + 
					buff.name() + "[" + next.name() + "]," 
					"(unsigned long)(" + v->name() + "+((" + v->math().as_written() + ")+" + buff_size.name() + "))," 
					"sizeof(" + buff.type() + ") *" + buff_size.name() + "," +
					next.name() + ", 0, 0);\n" +
				orig.name() + "=" + buff.name() + "[old];\n"
				"MMGP_SPE_dma_wait(old);\n }\n"
			+ use);
	}

	void make_triple() { depth = buffer_adaptor::triple; }
};

/*
struct gen_in: public node_xformer {
	const region_variable* v;
	size_t depth;
	int unroll;
	gen_in(const region_variable* _v, int u): 
		v(_v), depth(buffer_adaptor::dbl), unroll(u)
		{}
	string operator()(tree_node_t& node)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v, depth);
		orig_adaptor orig(v);

		string if_statement;

		if (!unroll) {
			if_statement = "if (!((" + v->math().as_written() + ")%" + buff_size.name() + "))"; 
		}

		return if_statement + "{\n" 
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

	void make_triple() { depth = buffer_adaptor::triple; }
};
*/

typedef map<region_variable*, gen_in> lazy_gen_in;

struct for_compound_op {
	const symtbl& shared; 
	const symset& cond;
	varlist& out;
	lazy_gen_in& in;
	int unroll;
	for_compound_op(const symtbl& s, symset& c, varlist& o, lazy_gen_in& l, int u): 
		shared(s), cond(c), out(o), in(l), unroll(u)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::expression_statement || node.value.id() == ids::selection_statement) {
			expression_statement_op o(shared, cond, out);
			for_all(node.children, &o);

			// Store the function object somewhere, and call it later once I know 
			// which variables are in, which are out, and which are inout
			for (varlist::iterator it = o.in.begin(); it != o.in.end(); ++it) {
				if (in.find(*it) == in.end()) {
					in.insert(lazy_gen_in::value_type(*it, gen_in(node, unroll)));
				}
			}
		}
		else {
			for_all(node.children, this);
		}
	}
};

// XFORM CHANGE

struct gen_out: public unary_function<void, region_variable*>  {
	tree_node_t& node;
	const int depth;
	int unroll;

	gen_out(tree_node_t& n, int d, int u): 
		node(n), depth(d), unroll(u) {}
	void operator()(const region_variable* v)
	{
		buffer_adaptor buff(v, depth);
		orig_adaptor orig(v);
		string var_switch;
		string if_statement;
		string old;

		if (node.value.value() == "") {
			old.assign(node.value.begin(), node.value.end());
		}
		else {
			old = node.value.value();
		}

		if (depth < buffer_adaptor::triple) {
			next_adaptor next(v);

			var_switch =	next.name() + "=(" + next.name() + "+1)%" + buff.depth() + ";\n" +
					orig.name() + "=" + buff.name() + "[" + next.name() + "];";
		}

		if (!unroll) {
			if_statement = "if (!(" + v->math().next_iteration() + "%" + buff_size.name() + "))";
		}

		node.value.value(if_statement + "{\n"
					"MMGP_SPE_dma_wait(out_tag); \n"
					"mfc_put(" + orig.name() + "," + "(unsigned long)(" + v->name() + 
							"+" + v->math().next_iteration() + "-" + buff_size.name() + "),"
						"sizeof(" + buff.type() + ")*" + buff_size.name() + ","
						"out_tag, 0, 0);\n" +
					var_switch +
				"}\n" + 
				old);
	}
};

/*
struct gen_out: public node_xformer {
	const region_variable* v;
	const int depth;
	int unroll;

	gen_out(region_variable* _v, int d, int u): 
		v(_v), depth(d), unroll(u) {}
	string operator()(tree_node_t& node)
	{
		buffer_adaptor buff(v, depth);
		orig_adaptor orig(v);
		string var_switch;
		string if_statement;

		if (depth < buffer_adaptor::triple) {
			next_adaptor next(v);

			var_switch =	next.name() + "=(" + next.name() + "+1)%" + buff.depth() + ";\n" +
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
						"out_tag, 0, 0);\n" +
					var_switch +
			"}\n";
	}
};
*/

// XFORM CHANGE
struct gen_final_out {
	tree_node_t& node;
	gen_final_out(tree_node_t& n):
		node(n) {}
	void operator()(const region_variable* v)
	{
		buffer_adaptor buff(v, buffer_adaptor::dbl);
		orig_adaptor orig(v);
		string old;

		if (node.value.value() == "") {
			old.assign(node.value.begin(), node.value.end());
		}
		else {
			old = node.value.value();
		}

		node.value.value(old + "if ((((SPE_stop - SPE_start)" + v->math().factor() + ")%buff_size)) {" +
					"MMGP_SPE_dma_wait(out_tag); \n mfc_put(" + 
					orig.name() + "," + 
					"(unsigned long)(" + v->name() +
					"+(SPE_stop" + v->math().factor() + ")-(((SPE_stop-SPE_start)" + v->math().factor() + ")%" + 
					buff_size.name() + ")), sizeof(" + buff.type() + ")*(((SPE_stop-SPE_start)" + v->math().factor() +
					")%" + buff_size.name() + "),out_tag, 0, 0); \n" +
				"MMGP_SPE_dma_wait(out_tag); \n }");
	}

	void operator()(const lazy_gen_in::value_type& p)
	{
		operator()(p.first);
	}
};

/*
struct gen_final_out {
	const region_variable* v;
	gen_final_out(const region_variable* _v): v(_v) {}
	string operator()(tree_node_t& node)
	{
		buffer_adaptor buff(v, buffer_adaptor::dbl);
		orig_adaptor orig(v);

		return "if ((((SPE_stop - SPE_start)" + v->math().factor() + ")%buff_size)) {" +
					"MMGP_SPE_dma_wait(out_tag); \n mfc_put(" + 
					orig.name() + "," + 
					"(unsigned long)(" + v->name() +
					"+(SPE_stop" + v->math().factor() + ")-(((SPE_stop-SPE_start)" + v->math().factor() + ")%" + 
					buff_size.name() + ")), sizeof(" + buff.type() + ")*(((SPE_stop-SPE_start)" + v->math().factor() +
					")%" + buff_size.name() + "),out_tag, 0, 0); \n" +
				"MMGP_SPE_dma_wait(out_tag); \n }";
	}
};
*/

class make_reduction_declarations {
	stringstream& ss;

public:
	make_reduction_declarations(stringstream& s): ss(s) {}
	void operator()(const region_variable* v)
	{
		ss << orig_adaptor(v).declare() << ";" << endl;
	}
};

struct variable_less {
	bool operator()(const region_variable* v, const lazy_gen_in::value_type& p)
	{
		if (v < p.first) return true;
		else return false;
	}

	bool operator()(const lazy_gen_in::value_type& p, const region_variable* v)
	{
		if (p.first < v) return true;
		else return false;
	}

	bool operator()(const lazy_gen_in::value_type& a, const lazy_gen_in::value_type& b)
	{
		if (a.first < b.first) return true; 
		else return false;
	}
};

struct for_op {
	const symtbl& shared;
	symset& cond;
	varlist& in;
	varlist& out;
	varlist& inout;
	int unroll;
	for_op(const symtbl& s, symset& c, varlist& i, varlist& o, varlist& io, int u): 
		shared(s), cond(c), in(i), out(o), inout(io), unroll(u) {}
	void operator()(tree_node_t& node)
	{
		// if unroll, need to change iteration parameters
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			cond.insert(ident);
		}
		else if (node.value.id() == ids::compound) {
			lazy_gen_in lazy_in;
			varlist pre_out;

			for_compound_op o(shared, cond, pre_out, lazy_in, NO_UNROLL);
			for_all(node.children, &o);

			pre_out.sort();
			pre_out.unique();

			lazy_gen_in lazy_inout;
			set_intersection(lazy_in.begin(), lazy_in.end(), 
					pre_out.begin(), pre_out.end(), 
					inserter(lazy_inout, lazy_inout.begin()),
					variable_less());

			set_difference(pre_out.begin(), pre_out.end(), 
					lazy_inout.begin(), lazy_inout.end(), 
					inserter(out, out.begin()),
					variable_less());

			lazy_gen_in diff_in;
			set_difference(lazy_in.begin(), lazy_in.end(), 
					lazy_inout.begin(), lazy_inout.end(), 
					inserter(diff_in, diff_in.begin()),
					variable_less());

			// Lazily call gen_in on both in and inout variables.
			for (lazy_gen_in::iterator it = diff_in.begin(); it != diff_in.end(); ++it) {
				it->second(it->first);
				in.push_back(it->first);
			}

			for (lazy_gen_in::iterator it = lazy_inout.begin(); it != lazy_inout.end(); ++it) {
				it->second.make_triple(); // inout variables need triple buffers
				it->second(it->first);
				inout.push_back(it->first);
			}

			tree_node_t& back = node.children.back();
			// XFORM CHANGE
			//append(back.xformations, fmap(create_gen_out(buffer_adaptor::triple, NO_UNROLL), inout));
			for_all(inout, gen_out(back, buffer_adaptor::triple, NO_UNROLL));
			for_all(out, gen_out(back, buffer_adaptor::dbl, NO_UNROLL));
			for_all(inout, gen_final_out(back));
			for_all(out, gen_final_out(back));
		}
		else {
			for_all(node.children, this);
		}
	}
};

struct unroll_for_op {
	const symtbl& shared;
	symset& cond;
	varlist& in;
	varlist& out;
	varlist& inout;
	int unroll;
	unroll_for_op(const symtbl& s, symset& c, varlist& i, varlist& o, varlist& io, int u): 
		shared(s), cond(c), in(i), out(o), inout(io), unroll(u) {}
	void operator()(tree_node_t& node)
	{
		cout << "You ain't done yet." << endl;
	}
};

template <class N>
struct non_destructive_deep_copy {
	N& dupe;
	non_destructive_deep_copy(N& d): dupe(d) {}

	void operator()(N node)
	{
		dupe.value.value(node.value.value());
		for_all(node.children, non_destructive_deep_copy(back_inserter(dupe.children)));
	}
};

struct compound {
	const symtbl& shared;
	symset& cond;
	varlist& in;
	varlist& out;
	varlist& inout;
	int unroll;
	compound(const symtbl& s, symset& c, varlist& i, varlist& o, varlist& io, int u): 
		shared(s), cond(c), in(i), out(o), inout(io), unroll(u) {}
	void operator()(tree_node_t& node)
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

bool is_for_loop(tree_node_t& node)
{
	return node.value.id() == ids::for_loop;
}

class make_reduction_assignments {
	stringstream& a;

public:
	make_reduction_assignments(stringstream& _a): a(_a) {}
	void operator()(const region_variable* v)
	{
		a << "*" << v->name() << "=" << orig_adaptor(v).name() << ";" << endl;
	}
};

class init_buffers {
	stringstream& ss;
	const size_t depth;

public:
	init_buffers(stringstream& s, size_t d): ss(s), depth(d) {}
	void operator()(const region_variable* v)
	{
		buffer_adaptor buff(v, depth);
		next_adaptor next(v);
		orig_adaptor orig(v);

		ss 	<< orig.declare() << ";" << endl
			<< next.declare() << " = 0;" << endl
			<< orig.name() << " = " << buff.name() << "[" << next.name() << "];" << endl;
	}
};

class in_init_buffers {
	stringstream& ss;
	const size_t depth;

public:
	in_init_buffers(stringstream& s, size_t d): ss(s), depth(d) {}
	void operator()(const region_variable* v)
	{
		buffer_adaptor buff(v, depth);
		next_adaptor next(v);

		init_buffers(ss, depth)(v);

		ss	<< "mfc_get(" 
			<< buff.name() << "[" << next.name() << "], " 
			<< "(unsigned long)(" << v->name() << " + (SPE_start" + v->math().factor() + ")),"
			<< "sizeof(" << buff.type() << ")*" << buff_size.name() << ", "
			<< next.name() << ", 0, 0); \n";
	}
};

class init_private_buffers {
	stringstream& ss;

public:
	init_private_buffers(stringstream& s): ss(s) {}
	void operator()(const region_variable* v)
	{
		if (v->is_pointer()) {
			buffer_adaptor buff(v, buffer_adaptor::single);
			orig_adaptor orig(v);

			ss	<< "mfc_get("
					<< buff.name() << ","
					<< "(unsigned long)" << orig.name() << ","
					<< "sizeof(" << buff.type() << ")*" << buff_size.name() << ","
					<< "3, 0, 0);" << endl
				<< orig.name() << "=" << buff.name() << ";" << endl
				<< "MMGP_SPE_dma_wait(3);" << endl;
		}
	}
};

struct cell_region {
	spelist::iterator region;

	cell_region(spelist::iterator r): region(r) {}
	void operator()(tree_node_t& node)
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
				tree_node_t::children_t::iterator unroll_pos = for_all_duplicate(is_for_loop, &o, node.children);

				if (unroll_pos != node.children.end()) {
					unroll_for_op(*((*region)->symbols()), cond,
							(*region)->in(), (*region)->out(), (*region)->inout(), 
							(*region)->unroll())
						(*unroll_pos);
				}
			}
			else {
				for_all(node.children, &o);
			}

			stringstream decs;
			for_all((*region)->out(), init_buffers(decs, buffer_adaptor::dbl));
			for_all((*region)->in(), in_init_buffers(decs, buffer_adaptor::dbl));
			for_all((*region)->inout(), in_init_buffers(decs, buffer_adaptor::triple));

			for_all((*region)->priv(), init_private_buffers(decs));
			for_all((*region)->reductions(), make_reduction_declarations(decs));
			node.children.front().value.value("{" + decs.str());

			stringstream a;
			for_all((*region)->reductions(), make_reduction_assignments(a));
			node.children.back().value.value(a.str() + "}");

			++region;
		}
		else {
			for_all(node.children, this);
		}
	}
};

void traverse_ast(tree_t& trees, spelist& regions)
{
	for_all((*trees.begin()).children, cell_region(regions.begin()));
}

