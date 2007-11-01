#ifndef PARSE_TREE_H
#define PARSE_TREE_H

#include <limits>
#include <string>
using namespace std;

#include <boost/spirit/iterator/file_iterator.hpp>
#include <boost/spirit/tree/ast.hpp>
using namespace boost::spirit;

#include "ids.h"
#include "variable.h"

typedef file_iterator<char>				fileiter_t;
typedef node_val_data_factory<string>			string_factory_t;
typedef tree_parse_info<fileiter_t, string_factory_t>	tree_parse_info_t;
typedef tree_match<fileiter_t, string_factory_t>	parse_tree_match_t;
typedef parse_tree_match_t::tree_iterator		tree_iterator_t;
typedef parse_tree_match_t::container_t			tree_t;
typedef parse_tree_match_t::node_t			tree_node_t;

inline std::ostream& operator<<(std::ostream& out, const parser_id& rid)
{
	out << rid.to_long();
	return out;
}

void root_eval(tree_t& trees, const sharedtbl_t& shared);

template <class I, class F>
F* for_each(I first, I last, F* f)
{
	for ( ; first != last; ++first) {
		(*f)(*first);
	}
	return f;
}

template <class C, class F>
F fmap(F f, C& c)
{
	return for_each(c.begin(), c.end(), f);
}

template <class C, class F>
F fmap(F f, C* c)
{
	return for_each(c->begin(), c->end(), f);
}

/*
template <int i>
struct id_base {
	enum { id = i };
};

template <class Rule>
struct find_rule: id_base<any_rule> { 
	Rule r;
	//find_rule(): r() {}
	find_rule(const string& i): r(i) {}
	find_rule(const Rule& _r): r(_r) {}
	void operator()(const tree_node_t& node)
	{
		// This is not a base case; the rule we're looking 
		// for can be within itself. Recursion stops when we 
		// hit the leaves of the tree.
		cout << r.indent << "find: " << Rule::id << endl;
		if (node.value.id() == Rule::id) {
			r(node);
		}

		for_each(node.children.begin(), node.children.end(), *this);
	}
};
*/

#endif	// PARSE_TREE_H

