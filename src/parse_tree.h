#ifndef PARSE_TREE_H
#define PARSE_TREE_H

#include <list>
#include <string>
using namespace std;

#include <boost/shared_ptr.hpp>
#include <boost/spirit/iterator/file_iterator.hpp>
#include <boost/spirit/tree/ast.hpp>
using namespace boost;
using namespace boost::spirit;

#include "utility.h"
#include "xformers.h"

typedef node_val_data<const char*> char_data;

typedef list<xformer*> xformerlist;

struct xformerlist_data: public char_data {
	xformerlist xformations;

	xformerlist_data(): 
		char_data() {}
	xformerlist_data(const string::iterator& first, const string::iterator& last): 
		char_data(first, last) {}
	xformerlist_data(const xformerlist_data& n);

	template <class I>
	xformerlist_data(const I& first, const I& last): 
		char_data(first, last) {}

	~xformerlist_data()
	{
		for_all(xformations, delete_ptr<xformer>);
	}

	xformerlist_data& operator=(const xformerlist_data& rhs);
};

struct xformer_factory {
	template <class Iter>
	struct factory {
		typedef xformerlist_data node_t;

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
			for (typename Container::const_iterator i = nodes.begin(); i != nodes.end(); ++i) {
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

#include "spe_region.h"

inline std::ostream& operator<<(std::ostream& out, const parser_id& rid)
{
	out << rid.to_long();
	return out;
}

void traverse_ast(ast& trees, spelist& regions);

#endif	// PARSE_TREE_H

