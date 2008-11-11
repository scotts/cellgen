#ifndef CELLGEN_GRAMMAR_H
#define CELLGEN_GRAMMAR_H

typedef list<stringstream*> sslist;

#include "spe_region.h"

extern bool print_ast;
void parse_src(const string& src_name, sslist& ppe_blocks, spelist& spe_regions, bool print_ast);

#endif // CELLGEN_GRAMMAR_H

