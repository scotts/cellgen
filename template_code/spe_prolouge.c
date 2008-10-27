#include "stddef.h"
#include <spu_intrinsics.h>
#include <spu_mfcio.h>
#include <malloc_align.h>
#include <free_align.h>
#include "MMGP_spu.h"
#include "../pass_struct.h"
#include "cellgen_timer.h"

const int out_tag = 2;

void compute_bounds (int *start, int *stop, int buff_sz);
