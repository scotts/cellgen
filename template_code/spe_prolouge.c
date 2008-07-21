#include "stddef.h"
#include "../pass_struct.h"
#include <spu_intrinsics.h>
#include <spu_mfcio.h>
#include "MMGP_spu.h"
#include "cellgen_timer.h"

const int out_tag = 2;

void compute_bounds (int *start, int *stop, int buff_sz);
