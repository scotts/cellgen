#include "stddef.h"
#include <spu_intrinsics.h>
#include <spu_mfcio.h>
#include "MMGP_spu.h"
#include "../pass_struct.h"
#include "cellgen_timer.h"
#include "cellstrider_dma.h"

const int out_tag = 2;

inline int min(const int a, const int b)
{
	return (a < b) ? a : b;
}

void compute_bounds (unsigned int *start, unsigned int *stop, size_t element_sz);
