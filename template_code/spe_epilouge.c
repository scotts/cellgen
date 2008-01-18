void compute_bounds(int *start, int *stop)
{
	int total_chunks = (*stop - *start) / buff_size;
	int thread_chunks = total_chunks / SPE_threads;
	int rem = (*stop - *start) % (buff_size * SPE_threads);

	if (SPE_id == SPE_threads - 1) {
		*start = *start + (SPE_id * thread_chunks * buff_size);
		*stop = *start + (thread_chunks * buff_size) + rem;
	}
	else {
		*start = *start + (SPE_id * thread_chunks * buff_size);
		*stop = *start + (thread_chunks * buff_size);
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
			CASES
			case TERMINATE: goto done;
		}
	}

done:
	return 0;
}

