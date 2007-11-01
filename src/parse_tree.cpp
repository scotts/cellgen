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
	const string indent;
	const sharedtbl_t& shared;
	call_rule(const string& i, const sharedtbl_t& s): indent(i), shared(s) {}
	void operator()(tree_node_t& node);
};

template <class F>
void for_each_rule(tree_node_t& node, const string indent, F f)
{
	for (const tree_iterator_t i = node.children.begin(); i != node.children.end(); ++i) {
		f(*i, indent);
	}
}

void identifier(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "identifier: ";
	string ident(node.value.begin(), node.value.end());
	if (node.value.id() == ids::identifier) {
		//cout << ident;
	}
	else {
		cerr << "error: ast processing, '" << ident << "' is not an identifier. " << endl;
	}
	//cout << endl;
}

void declaration_list(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "declaration_list" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void declaration(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "declaration" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void array_index(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "array_index" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void argument_expression_list(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "argument_expression_list" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void postfix_expression_helper(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "postfix_expression_helper" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void postfix_expression(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "postfix_expression" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void statement_list(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "statement_list" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void statement(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "statement(" << node.value.id() << ")" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void relational_expression(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "relational_expression" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void assignment_expression(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "assignment_expression" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void expression_helper(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "expression_helper" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void expression(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "expression ?" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void expression_statement(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "expression_statement" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

struct postfix_op {
	const string indent;
	const sharedtbl_t& shared;
	symtbl_t& cond;
	bool found_shared;
	string shared_ident;
	string cond_ident;
	postfix_op(const string i, const sharedtbl_t& s, symtbl_t& c):
		indent(i), shared(s), cond(c), 
		found_shared(false)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			string ident(node.value.begin(), node.value.end());
			if (found_shared) {
				if (find(cond, ident.c_str())) {
					cond_ident = ident;
					node.value.value(ident + " % " + buff_size);
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

struct exprstmnt_op {
	const string indent;
	const sharedtbl_t& shared;
	symtbl_t& cond;
	string shared_ident;
	string cond_ident;
	exprstmnt_op(const string i, const sharedtbl_t& s, symtbl_t& c):
		indent(i), shared(s), cond(c)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::postfix_expression) {
			postfix_op p(indent + "  ", shared, cond);
			fmap(&p, node.children);
			shared_ident = p.shared_ident;
			cond_ident = p.cond_ident;
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct for_compound_op {
	const string indent;
	const sharedtbl_t& shared; 
	symtbl_t& cond;
	map<const shared_variable*, bool> first;
	for_compound_op(const string i, const sharedtbl_t& s, symtbl_t& c): 
		indent(i), shared(s), cond(c)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::expression_statement) {
			exprstmnt_op e(indent + "  ", shared, cond);
			fmap(&e, node.children);
			if (e.shared_ident != "") {
				const shared_variable* sv = *(find(shared, e.shared_ident.c_str()));
				if (!first[sv]) {
					string use(node.value.begin(), node.value.end());
					node.value.value(
							sv->index_name() + "= !" + sv->index_name() + 
							";\n mfc_get("
								+ sv->buff_name() + "[" + sv->index_name() + "]," 
								+ sv->name() + "+" + e.cond_ident + "+" + buff_size + "," 
								+ "sizeof(" + sv->buff_type() + ") *" + buff_size + ","
								+ sv->index_name() + ", 0, 0);\n"
							+ sv->orig_name() + "=" + sv->buff_name() + "[!" + sv->index_name() + "];\n"
							+ use);
					first[sv] = true;
				}
			}
		}
		else {
			fmap(this, node.children);
		}
	}
};

struct for_op {
	const string indent;
	const sharedtbl_t& shared;
	symtbl_t& cond;
	for_op(const string i, const sharedtbl_t& s, symtbl_t& c): 
		indent(i), shared(s), cond(c)
		{}
	void operator()(tree_node_t& node)
	{
		if (node.value.id() == ids::identifier) {
			cond.add(node.value.begin(), node.value.end());
		}
		else if (node.value.id() == ids::compound) {
			fmap(for_compound_op(indent + "  ", shared, cond), node.children);
		}
		else {
			fmap(this, node.children);
		}
	}
};

void for_loop(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "for_loop" << endl;
	symtbl_t iter_cond;

	fmap(for_op(indent + "  ", shared, iter_cond), node.children);
}

void compound(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "compound" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void cell_region(tree_node_t& node, const string indent, const sharedtbl_t& shared)
{
	//cout << indent << "cell_region" << endl;
	fmap(call_rule(indent + "  ", shared), node.children);
}

void root_eval(tree_t& trees, const sharedtbl_t& shared)
{
	fmap(call_rule("", shared), (*trees.begin()).children);
}

void call_rule::operator()(tree_node_t& node)
{
	// WTF doesn't a parser_d convert to int?
	stringstream ss;
	ss << node.value.id();
	int id;
	ss >> id;

	switch (id) {
		case ids::identifier: identifier(node, indent, shared); break;
		case ids::declaration: declaration(node, indent, shared); break;
		case ids::array_index: array_index(node, indent, shared); break;
		case ids::for_loop: for_loop(node, indent, shared); break;
		case ids::compound: compound(node, indent, shared); break;
		case ids::cell_region: cell_region(node, indent, shared); break;
		case ids::postfix_expression: postfix_expression(node, indent, shared); break;
		case ids::expression_statement: expression_statement(node, indent, shared); break;
		case ids::statement: statement(node, indent, shared); break;
		case ids::statement_list: statement_list(node, indent, shared); break;
		case ids::declaration_list: declaration_list(node, indent, shared); break;
		case ids::expression: expression(node, indent, shared); break;
		case ids::assignment_expression: assignment_expression(node, indent, shared); break;
		case ids::expression_helper: expression_helper(node, indent, shared); break;
		case ids::relational_expression: relational_expression(node, indent, shared); break;
		case ids::argument_expression_list: argument_expression_list(node, indent, shared); break;
		case ids::postfix_expression_helper: postfix_expression_helper(node, indent, shared); break;
	}
}

