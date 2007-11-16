#include <string>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <map>
using namespace std;

#include "parse_tree.h"

template <class Rule>
bool descend_rule(tree_node_t& node, Rule r)
{
	bool found = false;
	for (const tree_iterator_t it = node.children.begin(); it != node.children.end(); ++it) {
		if ((*it).value.id() == Rule::id || Rule::id == ids::any_rule) {
			r(*it);
			found = true;
		}
	}
	return found;
}

struct call_rule {
	const symtbl_t& shared;
	call_rule(const symtbl_t& s): shared(s) {}
	void operator()(tree_node_t& node);
};

template <class F>
void for_each_rule(tree_node_t& node, F f)
{
	for (const tree_iterator_t i = node.children.begin(); i != node.children.end(); ++i) {
		f(*i);
	}
}

struct postfix_op {
	const symtbl_t& shared;
	const symtbl_t& cond;
	bool found_shared;
	string shared_ident;
	string itervar;
	postfix_op(const symtbl_t& s, const symtbl_t& c):
		shared(s), cond(c), 
		found_shared(false)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			if (found_shared) {
				if (cond.find(ident) != cond.end()) {
					itervar = ident;
					node.value.value("(" + ident + " % " + buff_size.alias() + ")");
				}
			}
			else {
				if (shared.find(ident) != shared.end()) {
					shared_ident = ident;
					found_shared = true;
				}
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
	string itervar;
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
			postfix_op p(shared, cond);
			fmap(&p, node.children);

			if (p.found_shared) {
				shared_idents.push_back(p.shared_ident);
				itervar = p.itervar;
			}
		}

		fmap(this, node.children);
	}
};

struct gen_wait {
	tree_node_t& node;
	map<const c_variable*, bool>& first;
	const symtbl_t& shared;
	const string& itervar;
	gen_wait(tree_node_t& n, map<const c_variable*, bool>& f,
			const symtbl_t& s, const string& i): 
		node(n), first(f), shared(s), itervar(i) 
		{}
	void operator()(const string& shared_ident)
	{
		const c_variable* cv = (shared.find(shared_ident))->second;
		index_variable index(cv);
		buff_variable buff(cv);
		orig_variable orig(cv);

		if (!first[cv]) {
			string use(node.value.begin(), node.value.end());
			use += node.value.value();
			node.value.value(
					"if (!(" + itervar + "%" + buff_size.alias() + ")) {\n" +
						index.name() + "= !" + index.name() + 
						";\n mfc_get("
							+ buff.name() + "[" + index.name() + "]," 
							+ cv->name() + "+(" + itervar + "+" + buff_size.alias() + ")," 
							+ "sizeof(" + buff.type() + ") *" + buff_size.alias() + ","
							+ index.name() + ", 0, 0);\n"
						+ orig.name() + "=" + buff.name() + "[!" + index.name() + "];\n"
						+ "MMGP_SPE_dma_wait(!" + index.name() + ");\n }\n"
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
	string itervar;
	for_compound_op(const symtbl_t& s, symtbl_t& c, symtbl_t& o): 
		shared(s), cond(c), out(o)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::expression_statement) {
			exprstmnt_op e(shared, cond, out);
			fmap(&e, node.children);
			itervar = e.itervar;
			fmap(gen_wait(node, first, shared, itervar), e.shared_idents);
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct gen_out {
	tree_node_t& node;
	const string& itervar;
	gen_out(tree_node_t& n, const string& i): node(n), itervar(i) {}
	void operator()(pair<string, const c_variable*> p)
	{
		buff_variable buff(p.second);
		orig_variable orig(p.second);

		node.value.value("if (!((" + itervar + "+1) % " + buff_size.alias() + 
					")) {\n MMGP_SPE_dma_wait(out_tag); \n mfc_put(" +
					orig.name() + "," + (p.second)->name() + 
					"+(" + itervar + "-" + buff_size.alias() + "+1), sizeof(" + 
					buff.type() + ")*" + buff_size.alias() + 
					", out_tag, 0, 0);\n }\n }");
	}
};

struct for_op {
	const symtbl_t& shared;
	symtbl_t& cond;
	for_op(const symtbl_t& s, symtbl_t& c): 
		shared(s), cond(c)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			cond[ident] = NULL;
		}
		else if (node.value.id() == ids::compound) {
			symtbl_t out;
			for_compound_op f(shared, cond, out);
			fmap(&f, node.children);
			fmap(gen_out(node.children.back(), f.itervar), out);
		}
		else {
			fmap(this, node.children);
		}
	}
};

void for_loop(tree_node_t& node, const symtbl_t& shared)
{
	symtbl_t iter_cond;
	fmap(for_op(shared, iter_cond), node.children);
}

void compound(tree_node_t& node, const symtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void cell_region(tree_node_t& node, const symtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void root_eval(tree_t& trees, const symtbl_t& shared)
{
	fmap(call_rule(shared), (*trees.begin()).children);
}

void call_rule::operator()(tree_node_t& node)
{
	// WTF doesn't a parser_d convert to int?
	stringstream ss;
	ss << node.value.id();
	int id;
	ss >> id;

	switch (id) {
		case ids::for_loop: 
			for_loop(node, shared); 
			break;
		case ids::compound: 
			compound(node, shared); 
			break;
		case ids::cell_region: 
			cell_region(node, shared); 
			break;
	}
}

