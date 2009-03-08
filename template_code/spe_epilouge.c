
/*
 * Scott's original version.
 */
/*
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
*/

/*
 * Jae-seung's revision.
 */
void compute_bounds (int *start, int *stop, size_t element_sz)
{
  int bytes16 = 16 / element_sz;
  int total_chunks = (*stop - *start) / bytes16;
  int thread_chunks = total_chunks / SPE_threads;
  int rem     = ((*stop - *start) % (bytes16 * SPE_threads)) / bytes16;
  int rem_rem = ((*stop - *start) % (bytes16 * SPE_threads)) % bytes16;

  int id = SPE_threads - rem;

  if (SPE_id == SPE_threads - 1) {
      if (SPE_id > id) {
          *start = *start + (SPE_id * thread_chunks * bytes16 + bytes16 * (SPE_id - id));
      }
      else {
          *start = *start + (SPE_id * thread_chunks * bytes16);
      }

      if (SPE_id >= id) {
          *stop = *start + ((thread_chunks + 1) * bytes16 + rem_rem);
      }
      else {
          *stop = *start + (thread_chunks * bytes16 + rem_rem);
      }
  }
  else {
      if (SPE_id > id) {
          *start = *start + (SPE_id * thread_chunks * bytes16 + bytes16 * (SPE_id - id));
      }
      else {
          *start = *start + (SPE_id * thread_chunks * bytes16);
      }

      if (SPE_id >= id) {
          *stop = *start + ((thread_chunks + 1) * bytes16);
      }
      else {
          *stop = *start + (thread_chunks * bytes16);
      }
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

