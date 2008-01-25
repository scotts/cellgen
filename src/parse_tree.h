#ifndef PARSE_TREE_H
#define PARSE_TREE_H

#include <string>
using namespace std;

#include <boost/spirit/iterator/file_iterator.hpp>
#include <boost/spirit/tree/ast.hpp>
using namespace boost::spirit;

typedef file_iterator<char>				fileiter;
typedef node_val_data_factory<string>			string_factory;
typedef tree_parse_info<fileiter, string_factory>	tree_parse_info_t;
typedef tree_match<fileiter, string_factory>		tree_match_t;
typedef tree_match_t::tree_iterator			tree_iterator_t;
typedef tree_match_t::container_t			tree_t;
typedef tree_match_t::node_t				tree_node_t;

#include "ids.h"
#include "variable.h"
#include "spe_region.h"

inline std::ostream& operator<<(std::ostream& out, const parser_id& rid)
{
	out << rid.to_long();
	return out;
}

void root_eval(tree_t& trees, const symtbl& shared, spelist& regions);

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

