#include <string>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <map>
using namespace std;

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

struct mult_op {
	const symtbl_t& cond;
	bool found_cond;
	bool found_op;
	mult_expr math;

	mult_op(const symtbl_t&c):
		cond(c), found_cond(false), found_op(false)
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
	const symtbl_t& cond;
	bool found_cond;
	mult_expr math;
	string rem;

	array_op(const symtbl_t& c):
		cond(c), found_cond(false) 
		{}
	void operator()(tree_node_t& node)
	{
		string val(node.value.begin(), node.value.end());

		if (node.value.id() == ids::multiplicative_expression) {
			mult_op o(cond);
			fmap(&o, node.children);
			math = o.math;
			found_cond = o.found_cond;
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
	const symtbl_t& shared;
	const symtbl_t& cond;
	bool found_shared;
	bool found_cond;
	string shared_ident;
	mult_expr math;
	
	postfix_op(const symtbl_t& s, const symtbl_t& c):
		shared(s), cond(c), 
		found_shared(false), found_cond(false)
		{}
	void operator()(tree_node_t& node)
	{
		string val(node.value.begin(), node.value.end());
		if (node.value.id() == ids::identifier) {
			if (!found_shared) {
				if (shared.find(val) != shared.end()) {
					shared_ident = val;
					found_shared = true;
				}
			}
		}
		else if (node.value.id() == ids::array_index) {
			array_op o(cond);
			fmap(&o, node.children);
			math = o.math;
			found_cond = o.found_cond;

			if (found_shared && found_cond) {
				node.value.value("[((" + math.as_written() + ")" + o.rem + ")%" + buff_size.name() + "]");
				
				// If we don't do this, redundant printing. ivar and multrhs is string 
				// reduction of all the children.
				node.children.clear(); 
			}
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct lhs_op {
	const symtbl_t& shared;
	symtbl_t& out;
	lhs_op(const symtbl_t& s, symtbl_t& o):
		shared(s), out(o)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			if (shared.find(ident) != shared.end()) {
				out[ident] = (shared.find(ident))->second;
			}
		}
		else {
			fmap(this, node.children);
		}
	};
};

bool find_equals(tree_node_t& node)
{
	string str(node.value.begin(), node.value.end());
	return str.find("=") != string::npos;
}

struct exprstmnt_op {
	const symtbl_t& shared;
	const symtbl_t& cond;
	symtbl_t& out;
	list<string> shared_idents;
	mult_expr math;
	exprstmnt_op(const symtbl_t& s, const symtbl_t& c, symtbl_t& o):
		shared(s), cond(c), out(o)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::assignment_expression) {
			tree_iterator_t e;
			for (e = node.children.begin(); e != node.children.end(); ++e) {
				if (find_equals(*e)) {
					break;
				}
			}

			for_each(node.children.begin(), e, lhs_op(shared, out));
		}
		else if (node.value.id() == ids::postfix_expression) {
			postfix_op o(shared, cond);
			fmap(&o, node.children);

			if (o.found_shared && o.found_cond) {
				shared_idents.push_back(o.shared_ident);
				math = o.math;
			}
		}

		fmap(this, node.children);
	}
};

struct gen_wait {
	tree_node_t& node;
	map<const c_variable*, bool>& first;
	const symtbl_t& shared;
	const string iexp;
	gen_wait(tree_node_t& n, map<const c_variable*, bool>& f,
			const symtbl_t& s, const string& i): 
		node(n), first(f), shared(s), iexp(i) 
		{}
	void operator()(const string& shared_ident)
	{
		const c_variable* cv = (shared.find(shared_ident))->second;
		next_variable next(cv);
		double_buffer buff(cv);
		orig_variable orig(cv);

		if (!first[cv]) {
			string use(node.value.begin(), node.value.end());
			use += node.value.value();
			node.value.value(
					"if (!((" + iexp + ")%" + buff_size.name() + ")) {\n" +
						next.name() + "= !" + next.name() + 
						";\n mfc_get("
							+ buff.name() + "[" + next.name() + "]," 
							+ "(unsigned long)(" + cv->name() + "+((" + iexp + ")+" + buff_size.name() + "))," 
							+ "sizeof(" + buff.type() + ") *" + buff_size.name() + ","
							+ next.name() + ", 0, 0);\n"
						+ orig.name() + "=" + buff.name() + "[!" + next.name() + "];\n"
						+ "MMGP_SPE_dma_wait(!" + next.name() + ");\n }\n"
					+ use);
			first[cv] = true;
		}
	}
};

struct for_compound_op {
	const symtbl_t& shared; 
	const symtbl_t& cond;
	symtbl_t& out;
	map<const c_variable*, bool> first;
	mult_expr math;
	for_compound_op(const symtbl_t& s, symtbl_t& c, symtbl_t& o): 
		shared(s), cond(c), out(o)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::expression_statement || node.value.id() == ids::selection_statement) {
			exprstmnt_op o(shared, cond, out);
			fmap(&o, node.children);
			math = o.math;

			fmap(gen_wait(node, first, shared, math.as_written()), o.shared_idents);
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct gen_out {
	tree_node_t& node;
	const string& iexp;
	gen_out(tree_node_t& n, const string& i): 
		node(n), iexp(i) {}
	void operator()(pair<string, const c_variable*> p)
	{
		double_buffer buff(p.second);
		orig_variable orig(p.second);
		string old;

		if (node.value.value() == "") {
			old.assign(node.value.begin(), node.value.end());
		}
		else {
			old = node.value.value();
		}

		node.value.value("if (!(" + iexp + "%" + buff_size.name() + 
					")) {\n MMGP_SPE_dma_wait(out_tag); \n mfc_put(" +
					orig.name() + "," + "(unsigned long)(" + (p.second)->name() + 
					"+" + iexp + "-" + buff_size.name() + "), sizeof(" + 
					buff.type() + ")*" + buff_size.name() + 
					", out_tag, 0, 0);\n }\n" + 
				old);
	}
};

struct gen_final_out {
	tree_node_t& node;
	const string& multrhs;
	gen_final_out(tree_node_t& n, const string& m):
		node(n), multrhs(m) {}
	void operator()(pair<string, const c_variable*> p)
	{
		double_buffer buff(p.second);
		orig_variable orig(p.second);
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
					"(unsigned long)(" + (p.second)->name() +
					"+(SPE_stop" + multrhs + ")-(((SPE_stop-SPE_start)" + multrhs + ")%" + 
					buff_size.name() + ")), sizeof(" + buff.type() + ")*(((SPE_stop-SPE_start)" + multrhs +
					")%" + buff_size.name() + "),out_tag, 0, 0); \n" +
				"MMGP_SPE_dma_wait(out_tag); \n }");
	}
};

class init_double_buffers {
	stringstream& ss;
	const string& factor;

public:
	init_double_buffers(stringstream& s, const string& f): ss(s), factor(f) {}
	void operator()(const c_variable* cv)
	{
		double_buffer buff(cv);
		next_variable next(cv);
		orig_variable orig(cv);

		ss 	<< orig.declare() << ";" << endl
			<< next.declare() << " = 0;" << endl
			<< "mfc_get(" 
				<< buff.name() << "[" << next.name() << "], " 
				<< "(unsigned long)(" << cv->name() << " + (SPE_start" + factor + ")),"
				<< "sizeof(" << buff.type() << ")*" << buff_size.name() << ", "
				<< next.name() << ", 0, 0); \n" 
			<< orig.name() << " = " << buff.name() << "[" << next.name() << "];" << endl;
	}
};

class init_private_buffers {
	stringstream& ss;

public:
	init_private_buffers(stringstream& s): ss(s) {}
	void operator()(const c_variable* cv)
	{
		if (cv->is_pointer()) {
			private_buffer buff(cv);

			ss	<< "mfc_get("
					<< buff.name() << ","
					<< "(unsigned long)" << cv->name() << ","
					<< "sizeof(" << buff.type() << ")*" << buff_size.name() << ","
					<< "3, 0, 0);" << endl
				<< cv->name() << "=" << buff.name() << ";" << endl
				<< "MMGP_SPE_dma_wait(3);" << endl;
		}
	}
};

class make_reduction_declarations {
	stringstream& ss;

public:
	make_reduction_declarations(stringstream& s): ss(s) {}
	void operator()(const c_variable* cv)
	{
		ss << orig_variable(cv).declare() << ";" << endl;
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

struct for_op {
	const symtbl_t& shared;
	symtbl_t& cond;
	mult_expr math;
	for_op(const symtbl_t& s, symtbl_t& c): shared(s), cond(c) {}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			cond[ident] = NULL;
		}
		else if (node.value.id() == ids::compound) {
			symtbl_t out;
			for_compound_op o(shared, cond, out);
			fmap(&o, node.children);
			math = o.math;
			
			fmap(gen_out(node.children.back(), math.next_iteration()), out);
			fmap(gen_final_out(node.children.back(), math.factor()), out);
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct compound {
	const symtbl_t& shared;
	mult_expr math;
	compound(const symtbl_t& s): shared(s) {}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::for_loop) {
			symtbl_t cond;
			for_op o(shared, cond);
			fmap(&o, node.children);
			math = o.math;
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
	void operator()(const c_variable* cv)
	{
		a << "*" << cv->name() << "=" << orig_variable(cv).name() << ";" << endl;
	}
};

struct cell_region {
	const symtbl_t& shared;
	spelist_t::iterator rit;
	cell_region(const symtbl_t& s, spelist_t::iterator r): shared(s), rit(r) {}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::compound) {
			compound o(shared);
			fmap(&o, node.children);

			stringstream decs;
			fmap(init_double_buffers(decs, o.math.factor()), (*rit)->shared());
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

void root_eval(tree_t& trees, const symtbl_t& shared, spelist_t& regions)
{
	fmap(cell_region(shared, regions.begin()), (*trees.begin()).children);
}

