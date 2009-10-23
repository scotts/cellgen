#ifndef CELLGEN_GRAMMAR_H
#define CELLGEN_GRAMMAR_H

typedef list<stringstream*> sslist;

#include "spe_region.h"

void parse_src(const string& src_name, sslist& ppe_blocks, spelist& spe_regions, bool print_pt);

#endif // CELLGEN_GRAMMAR_H

