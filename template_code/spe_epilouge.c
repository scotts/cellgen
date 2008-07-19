void compute_bounds(int *start, int *stop, int buff_sz)
{
	int total_chunks = (*stop - *start) / buff_sz;
	int thread_chunks = total_chunks / SPE_threads;
	int rem = (*stop - *start) % (buff_sz * SPE_threads);

	if (SPE_id == SPE_threads - 1) {
		*start = *start + (SPE_id * thread_chunks * buff_sz);
		*stop = *start + (thread_chunks * buff_sz) + rem;
	}
	else {
		*start = *start + (SPE_id * thread_chunks * buff_sz);
		*stop = *start + (thread_chunks * buff_sz);
	}
}

int main()
{
	int received;

	MMGP_exchange();

        cellgen_timer_begin();

	while (1) {

        	/* MMGP call used for receiving the PPE starting signal */
        	received = MMGP_SPE_wait();
                cellgen_timer_reset();

		switch (received) {
			CASES
                        case GET_TIMES:
                            cellgen_report();
                            break;
			case TERMINATE:
                            goto done;
		}
	}

done:
	return 0;
}

