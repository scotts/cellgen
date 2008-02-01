#include <string>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <map>
using namespace std;

#include <boost/bind.hpp>
#include <boost/function.hpp>
using namespace boost;

#include "parse_tree.h"

class mult_expr {
	string _lhs;
	string _op;
	string _rhs;
	string _ivar;

public:
	mult_expr() {}
	mult_expr(const mult_expr& o):
		_lhs(o._lhs), _op(o._op), _rhs(o._rhs), _ivar(o._ivar)
		{}

	void lhs(const string& s) { _lhs = s; }
	void op(const string& s) { _op = s; }
	void rhs(const string& s) { _rhs = s; }
	void ivar(const string& s) { _ivar = s; }

	void build_lhs(const string& s) { _lhs += s; }
	void build_rhs(const string& s) { _rhs += s; }

	string as_written()
	{
		return "(" + _lhs + _op + _rhs + ")";
	}

	string next_iteration()
	{
		string plus_one = "(" + _ivar + "+1)";

		if (_lhs == _ivar) {
			return "(" + plus_one + _op + _rhs + ")";
		}
		else {
			return "(" +_rhs + _op + plus_one + ")";
		}
	}

	string factor()
	{
		if (_lhs == _ivar) {
			return _op + _rhs;
		}
		else {
			return _op + _lhs;
		}
	}

	string report()
	{
		return "'" + _lhs + "' '" + _op + "' '" + _rhs + "' ivar: '" + _ivar + "'";
	}
};

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

		fmap(this, node.children);
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

		if (node.value.id() == ids::multiplicative_expression) {
			mult_op o(cond, found_cond, math);
			fmap(&o, node.children);
		}
		else {
			if (found_cond && !is_bracket(val)){
				rem += val;
			}

			fmap(this, node.children);
		}
	}
};

struct postfix_op {
	const symtbl& shared;
	const symset& cond;
	varlist& lst;
	mult_expr& math;
	bool found_cond;
	bool found_shared;
	const variable* shared_var;
	
	postfix_op(const symtbl& s, const symset& c, varlist& l, mult_expr& m):
		shared(s), cond(c), lst(l), math(m), found_cond(false), found_shared(false)
		{}
	void operator()(tree_node_t& node)
	{
		string val(node.value.begin(), node.value.end());
		if (node.value.id() == ids::identifier) {
			if (!found_shared) {
				symtbl::const_iterator it = shared.find(val);
				if (it != shared.end()) {
					shared_var = it->second;
					found_shared = true;
					lst.push_back(shared_var);
				}
			}
		}
		else if (node.value.id() == ids::array_index) {
			mult_expr m;
			array_op o(cond, found_cond, m);
			fmap(&o, node.children);

			if (found_shared && found_cond) {
				node.value.value("[((" + m.as_written() + ")" + o.rem + ")%" + buff_size.name() + "]");
				
				// If we don't do this, redundant printing. ivar and multrhs is string 
				// reduction of all the children.
				node.children.clear(); 
				math = m;
			}
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct handness {
	const symtbl& shared;
	const symset& cond;
	mult_expr& math;
	varlist& lst;
	handness(const symtbl& s, const symset& c, mult_expr& m, varlist& l):
		shared(s), cond(c), math(m), lst(l)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::postfix_expression) {
			postfix_op o(shared, cond, lst, math);
			fmap(&o, node.children);
		}
		else {
			fmap(this, node.children);
		}
	};
};


struct expression_statement_op {
	const symtbl& shared;
	const symset& cond;
	varlist& out;
	mult_expr& math;
	varlist in;
	expression_statement_op(const symtbl& s, const symset& c, varlist& o, mult_expr& m):
		shared(s), cond(c), out(o), math(m)
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

			for_each(node.children.begin(), e, handness(shared, cond, math, out));
			for_each(e, node.children.end(), handness(shared, cond, math, in));
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct gen_in: public unary_function<void, const variable*> {
	tree_node_t& node;
	const symtbl& shared;
	const string iexp;
	size_t depth;
	gen_in(tree_node_t& n, const symtbl& s, const string& i): 
		node(n), shared(s), iexp(i), depth(2)
		{}
	gen_in(const gen_in& o): node(o.node), shared(o.shared), iexp(o.iexp), depth(o.depth) {}
	void operator()(const variable* shared_var)
	{
		next_variable next(shared_var);
		buffer_variable buff(shared_var, depth);
		orig_variable orig(shared_var);

		string use(node.value.begin(), node.value.end());
		use += node.value.value();
		node.value.value(
			"if (!((" + iexp + ")%" + buff_size.name() + ")) {\n" 
				"int old=" + next.name() + ";\n" +
				next.name() + "= (" + next.name() + "+1)%" + buff.depth() + ";\n"
				"mfc_get(" + 
					buff.name() + "[" + next.name() + "]," 
					"(unsigned long)(" + shared_var->name() + "+((" + iexp + ")+" + buff_size.name() + "))," 
					"sizeof(" + buff.type() + ") *" + buff_size.name() + "," +
					next.name() + ", 0, 0);\n" +
				orig.name() + "=" + buff.name() + "[old];\n"
				"MMGP_SPE_dma_wait(old);\n }\n"
			+ use);
	}

	void make_triple() { depth = 3; }
};

typedef list<function1<void, const variable*> > funlist;
typedef map<const variable*, gen_in> lazy_gen_in;

struct for_compound_op {
	const symtbl& shared; 
	const symset& cond;
	varlist& out;
	lazy_gen_in& in;
	mult_expr& math;
	for_compound_op(const symtbl& s, symset& c, varlist& o, lazy_gen_in& l, mult_expr& m): 
		shared(s), cond(c), out(o), in(l), math(m)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::expression_statement || node.value.id() == ids::selection_statement) {
			expression_statement_op o(shared, cond, out, math);
			fmap(&o, node.children);

			cout << math.as_written() << ": ";
			// Store the function object somewhere, and call it later once I know 
			// which variables are in, which are out, and which are inout
			for (varlist::iterator it = o.in.begin(); it != o.in.end(); ++it) {
				if (in.find(*it) == in.end()) {
					cout << (*it)->name() << ", ";
					in.insert(lazy_gen_in::value_type(*it,
									gen_in(node, shared, math.as_written())));
				}
			}
			cout << endl;
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct gen_out {
	tree_node_t& node;
	const string& iexp;
	const size_t depth;
	gen_out(tree_node_t& n, const string& i, size_t d): 
		node(n), iexp(i), depth(d) {}
	void operator()(const variable* out_var)
	{
		buffer_variable buff(out_var, depth);
		orig_variable orig(out_var);
		string var_switch;
		string old;

		if (node.value.value() == "") {
			old.assign(node.value.begin(), node.value.end());
		}
		else {
			old = node.value.value();
		}

		if (depth < 3) {
			next_variable next(out_var);

			var_switch =	next.name() + "=(" + next.name() + "+1)%" + buff.depth() + ";\n" +
					orig.name() + "=" + buff.name() + "[" + next.name() + "];";
		}

		node.value.value("if (!(" + iexp + "%" + buff_size.name() + ")) {\n "
					"MMGP_SPE_dma_wait(out_tag); \n"
					"mfc_put(" + orig.name() + "," + "(unsigned long)(" + out_var->name() + 
							"+" + iexp + "-" + buff_size.name() + "),"
						"sizeof(" + buff.type() + ")*" + buff_size.name() + ","
						"out_tag, 0, 0);\n" +
					var_switch +
				"}\n" + 
				old);
	}
};

struct gen_final_out {
	tree_node_t& node;
	const string& multrhs;
	gen_final_out(tree_node_t& n, const string& m):
		node(n), multrhs(m) {}
	void operator()(const variable* out_var)
	{
		buffer_variable buff(out_var, 2);
		orig_variable orig(out_var);
		string old;

		if (node.value.value() == "") {
			old.assign(node.value.begin(), node.value.end());
		}
		else {
			old = node.value.value();
		}

		node.value.value(old + "if ((((SPE_stop - SPE_start)" + multrhs + ")%buff_size)) {" +
					"MMGP_SPE_dma_wait(out_tag); \n mfc_put(" + 
					orig.name() + "," + 
					"(unsigned long)(" + out_var->name() +
					"+(SPE_stop" + multrhs + ")-(((SPE_stop-SPE_start)" + multrhs + ")%" + 
					buff_size.name() + ")), sizeof(" + buff.type() + ")*(((SPE_stop-SPE_start)" + multrhs +
					")%" + buff_size.name() + "),out_tag, 0, 0); \n" +
				"MMGP_SPE_dma_wait(out_tag); \n }");
	}

	void operator()(const lazy_gen_in::value_type& p)
	{
		operator()(p.first);
	}
};

class make_reduction_declarations {
	stringstream& ss;

public:
	make_reduction_declarations(stringstream& s): ss(s) {}
	void operator()(const variable* v)
	{
		ss << orig_variable(v).declare() << ";" << endl;
	}
};

class add_declarations {
	const string& declarations;

public:
	add_declarations(const string& d): declarations(d) {}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::compound) {
			tree_node_t& lparen = *node.children.begin();
			lparen.value.value("{" + declarations);
		}
	}
};

void printlist(const variable* v)
{
	cout << v << " ";
}

void printlazy(lazy_gen_in::value_type& l)
{
	cout << l.first << " ";
}

struct c_vars_less {
	bool operator()(const variable* v, const lazy_gen_in::value_type& p)
	{
		if (v < p.first) return true;
		else return false;
	}

	bool operator()(const lazy_gen_in::value_type& p, const variable* v)
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
	mult_expr& math;
	for_op(const symtbl& s, symset& c, varlist& i, varlist& o, varlist& io, mult_expr& m): 
		shared(s), cond(c), in(i), out(o), inout(io), math(m) {}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			cond.insert(ident);
		}
		else if (node.value.id() == ids::compound) {
			lazy_gen_in lazy_in;
			varlist pre_out;

			for_compound_op o(shared, cond, pre_out, lazy_in, math);
			fmap(&o, node.children);

			pre_out.sort();
			pre_out.unique();

			lazy_gen_in lazy_inout;
			set_intersection(lazy_in.begin(), lazy_in.end(), 
					pre_out.begin(), pre_out.end(), 
					inserter(lazy_inout, lazy_inout.begin()),
					c_vars_less());

			set_difference(pre_out.begin(), pre_out.end(), 
					lazy_inout.begin(), lazy_inout.end(), 
					inserter(out, out.begin()),
					c_vars_less());

			lazy_gen_in diff_in;
			set_difference(lazy_in.begin(), lazy_in.end(), 
					lazy_inout.begin(), lazy_inout.end(), 
					inserter(diff_in, diff_in.begin()),
					c_vars_less());

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

			fmap(gen_out(node.children.back(), math.next_iteration(), 3), inout);
			fmap(gen_out(node.children.back(), math.next_iteration(), 2), out);
			fmap(gen_final_out(node.children.back(), math.factor()), inout);
			fmap(gen_final_out(node.children.back(), math.factor()), out);
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct compound {
	const symtbl& shared;
	mult_expr math;
	varlist& in;
	varlist& out;
	varlist& inout;
	compound(const symtbl& s, varlist& i, varlist& o, varlist& io): 
		shared(s), in(i), out(o), inout(io) {}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::for_loop) {
			symset cond;
			for_op o(shared, cond, in, out, inout, math);
			fmap(&o, node.children);
		}
		else {
			fmap(this, node.children);
		}
	}
};

class make_reduction_assignments {
	stringstream& a;

public:
	make_reduction_assignments(stringstream& _a): a(_a) {}
	void operator()(const variable* v)
	{
		a << "*" << v->name() << "=" << orig_variable(v).name() << ";" << endl;
	}
};

class base_init_buffers {
	stringstream& ss;
	const size_t depth;
public:
	base_init_buffers(stringstream& s, size_t d): ss(s), depth(d) {}
	void operator()(const variable* v)
	{
		buffer_variable buff(v, depth);
		next_variable next(v);
		orig_variable orig(v);

		ss 	<< orig.declare() << ";" << endl
			<< next.declare() << " = 0;" << endl
			<< orig.name() << " = " << buff.name() << "[" << next.name() << "];" << endl;
	}
};

class in_init_buffers {
	stringstream& ss;
	const string& factor;
	const size_t depth;

public:
	in_init_buffers(stringstream& s, const string& f, size_t d): ss(s), factor(f), depth(d) {}
	void operator()(const variable* v)
	{
		buffer_variable buff(v, depth);
		next_variable next(v);
		orig_variable orig(v);

		base_init_buffers(ss, depth)(v);

		ss	<< "mfc_get(" 
			<< buff.name() << "[" << next.name() << "], " 
			<< "(unsigned long)(" << v->name() << " + (SPE_start" + factor + ")),"
			<< "sizeof(" << buff.type() << ")*" << buff_size.name() << ", "
			<< next.name() << ", 0, 0); \n";
	}
};

class init_private_buffers {
	stringstream& ss;

public:
	init_private_buffers(stringstream& s): ss(s) {}
	void operator()(const variable* v)
	{
		if (v->is_pointer()) {
			buffer_variable buff(v, 1);

			ss	<< "mfc_get("
					<< buff.name() << ","
					<< "(unsigned long)" << v->name() << ","
					<< "sizeof(" << buff.type() << ")*" << buff_size.name() << ","
					<< "3, 0, 0);" << endl
				<< v->name() << "=" << buff.name() << ";" << endl
				<< "MMGP_SPE_dma_wait(3);" << endl;
		}
	}
};

struct cell_region {
	const symtbl& shared;
	spelist::iterator rit;
	cell_region(const symtbl& s, spelist::iterator r): shared(s), rit(r) {}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::compound) {
			compound o(shared, (*rit)->in(), (*rit)->out(), (*rit)->inout());
			fmap(&o, node.children);

			stringstream decs;
			fmap(base_init_buffers(decs, 2), (*rit)->out());
			fmap(in_init_buffers(decs, o.math.factor(), 2), (*rit)->in());
			fmap(in_init_buffers(decs, o.math.factor(), 3), (*rit)->inout());

			fmap(init_private_buffers(decs), (*rit)->priv());
			fmap(make_reduction_declarations(decs), (*rit)->reductions());
			node.children.front().value.value("{" + decs.str());

			stringstream a;
			fmap(make_reduction_assignments(a), (*rit)->reductions());
			node.children.back().value.value(a.str() + "}");

			++rit;
		}
		else {
			fmap(this, node.children);
		}
	}
};

void root_eval(tree_t& trees, const symtbl& shared, spelist& regions)
{
	fmap(cell_region(shared, regions.begin()), (*trees.begin()).children);
}

