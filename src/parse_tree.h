#ifndef PARSE_TREE_H
#define PARSE_TREE_H

#include <list>
#include <string>
using namespace std;

#include <boost/spirit/iterator/file_iterator.hpp>
#include <boost/spirit/tree/ast.hpp>
using namespace boost::spirit;

typedef node_val_data<string::iterator, string> string_node;

struct node_xformer;
typedef list<node_xformer*> xformerlist;

struct xformerlist_node: public string_node {
	xformerlist_node(): string_node() {}
	xformerlist_node(const string::iterator& first, const string::iterator& last): string_node(first, last) {}

	template <class I>
	xformerlist_node(const I& first, const I& last): string_node(first, last) {}

	xformerlist xformations;
};

struct xformer_factory {
	template <class Iter>
	struct factory {
		typedef xformerlist_node node_t;

		static node_t create_node(const Iter& first, const Iter& last, bool is_leaf_node)
		{
			if (is_leaf_node) {
				return node_t(first, last);
			}
			else {
				return node_t();
			}
		}

		static node_t empty_node()
		{
			return node_t();
		}

		template <typename Container>
		static node_t group_nodes(Container const& nodes)
		{
			typename node_t::container_t c;
			for (typename Container::const_iterator i = nodes.begin();
				i != nodes.end(); ++i) {
				assert(i->children.size() == 0);
				c.insert(c.end(), i->value.begin(), i->value.end());
			}
			return node_t(c.begin(), c.end());
		}

	};
};

typedef file_iterator<char>				fileiter;
typedef tree_parse_info<fileiter, xformer_factory>	ast_parse_info;
typedef tree_match<fileiter, xformer_factory>		ast_match;
typedef ast_match::tree_iterator			ast_iterator;
typedef ast_match::container_t				ast;
typedef ast_match::node_t				ast_node;

struct node_xformer: public unary_function<void, ast_node&> {
	virtual ~node_xformer() {}
	virtual string operator()(ast_node&) = 0;
	virtual void unroll_me(int u) {}
};

inline std::ostream& operator<<(std::ostream& out, const parser_id& rid)
{
	out << rid.to_long();
	return out;
}

#include "spe_region.h"

void traverse_ast(ast& trees, spelist& regions);

#endif	// PARSE_TREE_H

