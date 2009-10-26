#ifndef CELLGEN_H
#define CELLGEN_H

#include <malloc_align.h>
#include <free_align.h>
#include <numa.h>
#include <numaif.h>

void* cellgen_malloc(size_t sz);
void cellgen_free(void* adr);
void cellgen_numify(void* adr, size_t sz);

#endif // CELLGEN_H
