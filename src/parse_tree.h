#ifndef PARSE_TREE_H
#define PARSE_TREE_H

#include <limits>
#include <string>
using namespace std;

#include <boost/spirit/iterator/file_iterator.hpp>
#include <boost/spirit/tree/ast.hpp>
using namespace boost::spirit;

typedef file_iterator<char>				fileiter_t;
typedef node_val_data_factory<string>			string_factory_t;
typedef tree_parse_info<fileiter_t, string_factory_t>	tree_parse_info_t;
typedef tree_match<fileiter_t, string_factory_t>	parse_tree_match_t;
typedef parse_tree_match_t::tree_iterator		tree_iterator_t;
typedef parse_tree_match_t::container_t			tree_t;
typedef parse_tree_match_t::node_t			tree_node_t;

#include "ids.h"
#include "variable.h"
#include "spe_region.h"

inline std::ostream& operator<<(std::ostream& out, const parser_id& rid)
{
	out << rid.to_long();
	return out;
}

void root_eval(tree_t& trees, const symtbl_t& shared, spelist_t& regions);

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

#endif	// PARSE_TREE_H

