#include <assert.h>
#include <stdio.h>

#include "cellgen.h"

void* cellgen_malloc(size_t sz)
{
	return _malloc_align(sz, 7);
}

void cellgen_free(void* adr)
{
	_free_align(adr);
}

void cellgen_numify(void* adr, size_t sz)
{
	const int n_regions = numa_available();
	assert(n_regions > 0);
	const size_t node_sz = sz / n_regions;
	const size_t node_rm = sz % n_regions;

	int i;
	for (i = 0; i < n_regions; ++i) {
		nodemask_t node;
		nodemask_zero(&node);
		nodemask_set(&node, i);

		int res = mbind((unsigned long)adr + i * node_sz, 
				(i == n_regions - 1 ? node_sz + node_rm : node_sz),
				MPOL_BIND, &node, n_regions, MPOL_MF_MOVE | MPOL_MF_STRICT);

		if (res != 0) {
			perror("mbind");
		}
	}
}

