
inline void bounds_assign(unsigned int* start, unsigned int* stop, const int cutoff_id, const int bytes16, const int base_chunks, const int leftover)
{
      *start = *start + (spe_id * base_chunks * bytes16);

      if (spe_id > cutoff_id) {
          *start += bytes16 * (spe_id - cutoff_id);
      }

      if (spe_id >= cutoff_id) {
          *stop = *start + ((base_chunks + 1) * bytes16 + leftover);
      }
      else {
          *stop = *start + (base_chunks * bytes16 + leftover);
      }
}

void compute_bounds(unsigned int *start, unsigned int *stop, size_t element_sz)
{
	int bytes;
	if (element_sz > 16) {
		bytes = element_sz + element_sz % 16;
	}
	else {
		bytes = 16 / element_sz;
	}

	const int total_chunks = (*stop - *start) / bytes;
	const int base_chunks = total_chunks / spe_threads;
	const int thread_bytes_rem = ((*stop - *start) % (bytes * spe_threads));
	const int leftover = thread_bytes_rem % bytes;

	const int cutoff_id = spe_threads - (thread_bytes_rem / bytes);

	if (spe_id == spe_threads - 1) {
		bounds_assign(start, stop, cutoff_id, bytes, base_chunks, leftover);
	}
	else {
		bounds_assign(start, stop, cutoff_id, bytes, base_chunks, 0);
	}
}

int main()
{
	int received;

	ppe_exchange();

        cellgen_timer_begin();

	while (1) {

        	/* MMGP call used for receiving the PPE starting signal */
        	received = wait_for_ppe();
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

