#include "../pass_struct.h"
#include <spu_intrinsics.h>
#include <spu_mfcio.h>
#include "MMGP_spu.h"

#define TERMINATE 0

int __dblbf = 0;

int n_buff[2][16] __attribute__ ((aligned(128)));

void loop1(int SPE_start, int SPE_stop, int l1, int *n_addr)
{
	int *n;
	mfc_get(n_buff[__dblbf], n_addr + SPE_start, sizeof(int) * 16, __dblbf, 0, 0);
	n = n_buff[__dblbf];

	int i;
	for (i = SPE_start; i < SPE_stop; ++i) {
		if (!(i % 16)) {
			__dblbf = !__dblbf;
			mfc_get(n_buff[__dblbf], n_addr + i + 16, sizeof(int) * 16, __dblbf, 0, 0);
			MMGP_SPE_dma_wait(!__dblbf);
			n = n_buff[!__dblbf];
		}

		printf("%d: n[%d]: %d\n", SPE_id, i, n[i % 16]);
	}
}

void loop2(int SPE_start, int SPE_stop, int l2)
{
	int i;
	for (i = SPE_start; i < SPE_stop; ++i) {
		printf("loop %d on SPE %d: %d\n", l2, SPE_id, i);
	}
}

void compute_bounds(int *global_start, int *global_stop)
{
	int start = *global_start;
	int stop = *global_stop;
	int slice = (stop - start) / SPE_threads;
	int rem = (stop - start) % SPE_threads;

	*global_start = start + (SPE_id * slice);

	if (SPE_id == SPE_threads - 1) {
		*global_stop = start + ((SPE_id + 1) * slice) + rem;
	}
	else {
		*global_stop = start + ((SPE_id + 1) * slice);
	}
}

int main()
{
	int received;

	MMGP_exchange();

	while (1) {

		/* MMGP call used for receiving the PPE starting signal */
		received = MMGP_SPE_wait();

		switch (received) {
		case 1:
			compute_bounds(&pass.SPE_start, &pass.SPE_stop);
			loop1(pass.SPE_start, pass.SPE_stop, pass.l1,
			      pass.n_addr);
			MMGP_SPE_stop();
			break;
		case 2:
			compute_bounds(&pass.SPE_start, &pass.SPE_stop);
			loop2(pass.SPE_start, pass.SPE_stop, pass.l2);
			MMGP_SPE_stop();
			break;


		case TERMINATE:
			goto done;
		}
	}

      done:
	return 0;
}
