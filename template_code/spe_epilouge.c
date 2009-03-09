
inline void bounds_assign(int* start, int* stop, const int rem_id, const int bytes16, const int thread_chunks, const int rem_rem)
{
      if (SPE_id > rem_id) {
          *start = *start + (SPE_id * thread_chunks * bytes16 + bytes16 * (SPE_id - rem_id));
      }
      else {
          *start = *start + (SPE_id * thread_chunks * bytes16);
      }

      if (SPE_id >= rem_id) {
          *stop = *start + ((thread_chunks + 1) * bytes16 + rem_rem);
      }
      else {
          *stop = *start + (thread_chunks * bytes16 + rem_rem);
      }
}

void compute_bounds (int *start, int *stop, size_t element_sz)
{
	const int bytes16 = 16 / element_sz;
	const int total_chunks = (*stop - *start) / bytes16;
	const int thread_chunks = total_chunks / SPE_threads;
	const int thread_bytes_rem = ((*stop - *start) % (bytes16 * SPE_threads));
	const int thread_rem = thread_bytes_rem / bytes16;
	const int rem_rem = thread_bytes_rem % bytes16;

	const int rem_id = SPE_threads - thread_rem;

	if (SPE_id == SPE_threads - 1) {
		bounds_assign(start, stop, rem_id, bytes16, thread_chunks, rem_rem);
	}
	else {
		bounds_assign(start, stop, rem_id, bytes16, thread_chunks, 0);
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

