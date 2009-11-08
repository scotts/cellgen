#include <assert.h>
#include <stdio.h>

#include "cellgen.h"
#include "MMGP.h"

void* cellgen_malloc(size_t sz)
{
	return _malloc_align(sz, 16);
}

void cellgen_free(void* adr)
{
	_free_align(adr);
}

void cellgen_numify(void* adr, size_t sz)
{
	if (has_numa) {
		const int n_regions = numa_max_node();
		const size_t node_sz = sz / n_regions;
		const size_t node_rm = sz % n_regions;

		unsigned long node = 1UL | 2UL;
		int res = mbind(adr, sz,
				MPOL_INTERLEAVE, &node, sizeof(unsigned long) * 8, MPOL_MF_STRICT | MPOL_MF_MOVE);

		if (res < 0) {
			perror("mbind");
		}

		/*
		int i;
		for (i = 0; i < n_regions + 1; ++i) {
			unsigned long node = 1; //(1UL << i); // 0th node is indicated with first bit set
			int res = mbind(adr + i * node_sz, 
					(i == n_regions - 1 ? node_sz + node_rm : node_sz),
					MPOL_BIND, &node, sizeof(unsigned long) * 8, MPOL_MF_STRICT | MPOL_MF_MOVE);

			if (res < 0) {
				perror("mbind");
			}
		}
		*/
	}
}

