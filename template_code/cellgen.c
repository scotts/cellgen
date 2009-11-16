#include <assert.h>
#include <stdio.h>

#include "cellgen.h"
#include "MMGP.h"

void* cellgen_malloc(size_t sz)
{
	return _malloc_align(sz, page_shift);
}

void cellgen_free(void* adr)
{
	_free_align(adr);
}

void cellgen_numify_local(void* adr, size_t sz)
{
	if (has_numa) {
		unsigned int i;
		for (i = 0; i < spe_threads; ++i) {
			const int spe = phys_map[i];
			unsigned long node = (spe / 8) + 1;
			size_t node_sz = sz / spe_threads;
			if (i == spe_threads - 1) {
				node_sz += sz % spe_threads;
			}

			void* page_aligned = (void*)((unsigned long)(adr + i * node_sz) & ~((1 << page_shift) - 1));
			int res = mbind(page_aligned, node_sz, MPOL_BIND, &node, sizeof(unsigned long) * 8, MPOL_MF_STRICT | MPOL_MF_MOVE);
			if (res < 0) {
				perror("mbind");
			}
		}
	}
}

void cellgen_numify_interleave(void* adr, size_t sz)
{
	unsigned long both = 3;
	int res = mbind(adr, sz, MPOL_INTERLEAVE, &both, sizeof(unsigned long) * 8, MPOL_MF_STRICT | MPOL_MF_MOVE);
	if (res < 0) {
		perror("mbind");
	}
}

