
inline void bounds_assign(int* start, int* stop, const int cutoff_id, const int bytes16, const int base_chunks, const int leftover)
{
      *start = *start + (SPE_id * base_chunks * bytes16);

      if (SPE_id > cutoff_id) {
          *start += bytes16 * (SPE_id - cutoff_id);
      }

      if (SPE_id >= cutoff_id) {
          *stop = *start + ((base_chunks + 1) * bytes16 + leftover);
      }
      else {
          *stop = *start + (base_chunks * bytes16 + leftover);
      }
}

void compute_bounds (int *start, int *stop, size_t element_sz)
{
	int bytes;
	if (element_sz > 16) {
		bytes = element_sz + element_sz % 16;
	}
	else {
		bytes = 16 / element_sz;
	}

	const int total_chunks = (*stop - *start) / bytes;
	const int base_chunks = total_chunks / SPE_threads;
	const int thread_bytes_rem = ((*stop - *start) % (bytes * SPE_threads));
	const int leftover = thread_bytes_rem % bytes;

	const int cutoff_id = SPE_threads - (thread_bytes_rem / bytes);

	if (SPE_id == SPE_threads - 1) {
		bounds_assign(start, stop, cutoff_id, bytes, base_chunks, leftover);
	}
	else {
		bounds_assign(start, stop, cutoff_id, bytes, base_chunks, 0);
	}
}

/*
 * Scott's new version. It distributes the remaineder iterations as close to evenly 
 * as possible.
 *
 * UPDATE: Sigh. I forgot when I wrote this that I actually do depend on iterations 
 * being distributed in buffer size chunks. I don't feel like fixing this now, so I'm 
 * going to revert to the above version.
 */
/*
void compute_bounds(int* start, int *stop, int buff_size)
{
	int base = (*stop - *start) / SPE_threads;
	int rem = (*stop - *start) % SPE_threads;

	*start = *start + (SPE_id * base);
	*stop = *start + base;

	if (SPE_threads - SPE_id <= rem) {
		*start += rem - (SPE_threads - SPE_id);
		*stop += rem - (SPE_threads - SPE_id - 1);
	}
}
*/

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

