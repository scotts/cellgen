void compute_bounds(int* global_start, int* global_stop)
{
	int start = *global_start;
	int stop = *global_stop;
	int slice = (stop - start) / SPE_threads;
	int rem = (stop - start) % SPE_threads;

	if (SPE_id == SPE_threads - 1) {
		*global_start = start + (SPE_id * slice);
		*global_stop = start + ((SPE_id + 1) * slice) + rem;
	}
	else {
		*global_start = start + (SPE_id * slice);
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

			case TERMINATE: goto done;
		}
	}

done:
	return 0;
}

