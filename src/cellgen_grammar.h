#ifndef CELLGEN_GRAMMAR_H
#define CELLGEN_GRAMMAR_H

#include "c_grammar.h"
#include "parse_tree.h"
#include "variable.h"
#include "spe_region.h"
#include "skip.h"

#include <list>
#include <iostream>
#include <sstream>
using namespace std;

#include <boost/spirit/core.hpp>
#include <boost/spirit/symbols.hpp>
#include <boost/spirit/utility/confix.hpp>
#include <boost/spirit/iterator/file_iterator.hpp>
using namespace boost::spirit;

typedef list<stringstream*> sslist;

extern bool print_ast;
void parse_src(const string& src_name, sslist& ppe_blocks, spelist& spe_regions);

#endif // CELLGEN_GRAMMAR_H

