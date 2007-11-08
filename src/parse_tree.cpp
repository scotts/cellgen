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
	const sharedtbl_t& shared;
	call_rule(const sharedtbl_t& s): shared(s) {}
	void operator()(tree_node_t& node);
};

template <class F>
void for_each_rule(tree_node_t& node, F f)
{
	for (const tree_iterator_t i = node.children.begin(); i != node.children.end(); ++i) {
		f(*i);
	}
}

void identifier(tree_node_t& node, const sharedtbl_t& shared)
{
	string ident(node.value.begin(), node.value.end());
	if (node.value.id() == ids::identifier) {
		//cout << ident;
	}
	else {
		cerr << "error: ast processing, '" << ident << "' is not an identifier. " << endl;
	}
}

void declaration_list(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void declaration(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void array_index(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void argument_expression_list(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void postfix_expression_helper(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void postfix_expression(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void statement_list(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void statement(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void relational_expression(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void assignment_expression(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void expression_helper(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void expression(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void expression_statement(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

struct postfix_op {
	const sharedtbl_t& shared;
	const symtbl_t& cond;
	bool found_shared;
	string shared_ident;
	string cond_ident;
	postfix_op(const sharedtbl_t& s, const symtbl_t& c):
		shared(s), cond(c), 
		found_shared(false)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			if (found_shared) {
				if (find(cond, ident.c_str())) {
					cond_ident = ident;
					node.value.value(ident + " % " + buff_size.actual());
				}
			}
			else {
				if (find(shared, ident.c_str())) {
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
	const sharedtbl_t& shared;
	sharedtbl_t& out;
	lhs_op(const sharedtbl_t& s, sharedtbl_t& o):
		shared(s), out(o)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			if (find(shared, ident.c_str())) {
				out.add(node.value.begin(), node.value.end());
			}
		}
		else {
			fmap(this, node.children);
		}
	};
};

bool find_equals(tree_node_t& node)
{
	/*
	 * TODO: Uh, IT.
	if (node.value.id() == ASSIGNMENT) {
		return true;
	}
	else {
		return false;
	}
	*/
	return false;
}

struct exprstmnt_op {
	const sharedtbl_t& shared;
	const symtbl_t& cond;
	list<string> shared_idents;
	string cond_ident;
	sharedtbl_t& out;
	exprstmnt_op(const sharedtbl_t& s, const symtbl_t& c, sharedtbl_t& o):
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
				cond_ident = p.cond_ident; // not necessary to record every time, not worth coding around
			}
		}

		fmap(this, node.children);
	}
};

struct gen_wait {
	tree_node_t& node;
	map<const shared_variable*, bool>& first;
	const sharedtbl_t& shared;
	const string& cond_ident;
	gen_wait(tree_node_t& n, map<const shared_variable*, bool>& f,
			const sharedtbl_t& s, const string& c): 
		node(n), first(f), shared(s), cond_ident(c) 
		{}
	void operator()(const string& shared_ident)
	{
		const shared_variable* sv = *(find(shared, shared_ident.c_str()));
		if (!first[sv]) {
			string use(node.value.begin(), node.value.end());
			use += node.value.value();
			node.value.value(
					"if (!(" + cond_ident + "%" + buff_size.actual() + ")) {\n" +
						sv->index_name() + "= !" + sv->index_name() + 
						";\n mfc_get("
							+ sv->buff_name() + "[" + sv->index_name() + "]," 
							+ sv->name() + "+(" + cond_ident + "+" + buff_size.actual() + ")," 
							+ "sizeof(" + sv->buff_type() + ") *" + buff_size.actual() + ","
							+ sv->index_name() + ", 0, 0);\n"
						+ sv->orig_name() + "=" + sv->buff_name() + "[!" + sv->index_name() + "];\n"
						+ "MMGP_SPE_dma_wait(!" + sv->index_name() + ");\n }\n"
					+ use);
			first[sv] = true;
		}
	}
};

struct for_compound_op {
	const sharedtbl_t& shared; 
	const symtbl_t& cond;
	map<const shared_variable*, bool> first;
	sharedtbl_t out;
	for_compound_op(const sharedtbl_t& s, symtbl_t& c): 
		shared(s), cond(c)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::expression_statement) {
			exprstmnt_op e(shared, cond, out);
			fmap(&e, node.children);
			fmap(gen_wait(node, first, shared, e.cond_ident), e.shared_idents);
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct for_op {
	const sharedtbl_t& shared;
	symtbl_t& cond;
	for_op(const sharedtbl_t& s, symtbl_t& c): 
		shared(s), cond(c)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			cond.add(node.value.begin(), node.value.end());
		}
		else if (node.value.id() == ids::compound) {
			fmap(for_compound_op(shared, cond), node.children);
		}
		else {
			fmap(this, node.children);
		}
	}
};

void for_loop(tree_node_t& node, const sharedtbl_t& shared)
{
	symtbl_t iter_cond;
	fmap(for_op(shared, iter_cond), node.children);
}

void compound(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void cell_region(tree_node_t& node, const sharedtbl_t& shared)
{
	fmap(call_rule(shared), node.children);
}

void root_eval(tree_t& trees, const sharedtbl_t& shared)
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
		case ids::identifier: 
			identifier(node, shared); 
			break;
		case ids::declaration: 
			declaration(node, shared); 
			break;
		case ids::array_index: 
			array_index(node, shared); 
			break;
		case ids::for_loop: 
			for_loop(node, shared); 
			break;
		case ids::compound: 
			compound(node, shared); 
			break;
		case ids::cell_region: 
			cell_region(node, shared); 
			break;
		case ids::postfix_expression: 
			postfix_expression(node, shared); 
			break;
		case ids::expression_statement: 
			expression_statement(node, shared); 
			break;
		case ids::statement: 
			statement(node, shared); 
			break;
		case ids::statement_list: 
			statement_list(node, shared); 
			break;
		case ids::declaration_list: 
			declaration_list(node, shared); 
			break;
		case ids::expression: 
			expression(node, shared); 
			break;
		case ids::assignment_expression: 
			assignment_expression(node, shared); 
			break;
		case ids::expression_helper: 
			expression_helper(node, shared); 
			break;
		case ids::relational_expression: 
			relational_expression(node, shared); 
			break;
		case ids::argument_expression_list: 
			argument_expression_list(node, shared); 
			break;
		case ids::postfix_expression_helper: 
			postfix_expression_helper(node, shared); 
			break;
	}
}

