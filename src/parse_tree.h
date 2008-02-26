#ifndef PARSE_TREE_H
#define PARSE_TREE_H

#include <algorithm>
#include <numeric>
#include <string>
#include <list>
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

typedef file_iterator<char>			fileiter;
//typedef node_val_data_factory<transformations>	node_factory;
typedef tree_parse_info<fileiter, xformer_factory>	tree_parse_info_t;
typedef tree_match<fileiter, xformer_factory>	tree_match_t;
typedef tree_match_t::tree_iterator		tree_iterator_t;
typedef tree_match_t::container_t		tree_t;
typedef tree_match_t::node_t			tree_node_t;

struct node_xformer: public unary_function<void, tree_node_t&> {
	virtual ~node_xformer() {}
	virtual string operator()(tree_node_t&) = 0;
	virtual void unroll() {}
};

#include "ids.h"
#include "variable.h"
#include "spe_region.h"

inline std::ostream& operator<<(std::ostream& out, const parser_id& rid)
{
	out << rid.to_long();
	return out;
}

void traverse_ast(tree_t& trees, spelist& regions);

#endif	// PARSE_TREE_H

