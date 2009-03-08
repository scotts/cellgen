#include "stddef.h"
#include <spu_intrinsics.h>
#include <spu_mfcio.h>
#include <malloc_align.h>
#include <free_align.h>
#include "MMGP_spu.h"
#include "../pass_struct.h"
#include "cellgen_timer.h"
#include "cellstrider_dma.h"

const int out_tag = 2;

void compute_bounds (int *start, int *stop, size_t element_sz);
