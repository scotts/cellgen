/* This file was added to the original version to support the Cell execution of PBPI.
 * Contact:
 *	Filip Blagojevic
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: filip@cs.vt.edu
*/

#ifndef MMGP_SPU_H
#define MMGP_SPU_H

#define TERMINATE 0
#define GET_TIMES (NUM_KERNELS+1)
#define NUM_FNs NUM_KERNELS

#include "../pass_struct.h"

#ifdef PROFILING

#if defined(__cplusplus)
extern "C" {
#endif

#include "cellgen_timer.h"

#if defined(__cplusplus)
}
#endif

#define TB 79800000UL
unsigned long long total_time_start, loop_time_start;
unsigned long long dma_time_start, idle_time_start;
int idle_has_begun;

#endif // PROFILING

/* Structure used for PPE <-> SPE communication */
struct signal_t {
	int start, stop;
	unsigned long long total_time, loop_time;
	double result;
	int result_int;

	#ifdef PROFILING
	unsigned long long T_fn[NUM_FNs];
	unsigned long long T_DMA[NUM_FNs];
	unsigned long long T_total[NUM_FNs];
	unsigned long long idle_time;
	unsigned long long all_fn;
	unsigned long long all_dma;
	unsigned long long all_dma_prep;
	unsigned long long all_total;
	#endif
};

volatile struct signal_t sig __attribute__((aligned(128)));

int spe_id; // SPE thread id
int spe_threads; // Each SPE trhead knows the total number of SPE threads in use
volatile struct pass_t pass __attribute__((aligned(128))); // User defined structure used for PPE <-> SPE comm.

/* Function used for waiting for the PPE signal */
inline int wait_for_ppe()
{
        while (sig.start==0);

        return sig.start;
}

/* Function used for main memory <-> LS synchronization */

inline void cellgen_report(void)
{
	#ifdef PROFILING
	int i = 0;

	for (i = 0; i < NUM_FNs; i++) {
		sig.all_fn += sig.T_fn[i];
		sig.all_dma += sig.T_DMA[i];
		sig.all_total += sig.T_total[i];
	}

	sig.start = 0;
	spu_dsync();
	sig.stop = 1;
	#endif
}

inline void cellgen_dma_start()
{
	#ifdef PROFILING
	dma_time_start = GET_TIME();
	#endif
}

inline void cellgen_dma_stop(int loop __attribute__((unused)))
{
	#ifdef PROFILING
	sig.T_DMA[loop-1] += GET_TIME() - dma_time_start;
	#endif
}

inline void cellgen_total_start()
{
	#ifdef PROFILING
	total_time_start = GET_TIME();
	#endif
}

inline void cellgen_total_stop(int loop __attribute__((unused)))
{
	#ifdef PROFILING
	sig.T_total[loop-1] += GET_TIME() - total_time_start;
	#endif
}

inline void cellgen_dma_prep_start()
{
	#ifdef PROFILING
	dma_time_start = GET_TIME();
	#endif
}

inline void cellgen_dma_prep_stop()
{
	#ifdef PROFILING
	sig.all_dma_prep += GET_TIME() - dma_time_start;
	#endif
}

inline void cellgen_idle_start()
{
	#ifdef PROFILING
	idle_time_start = (unsigned long long) GET_TIME();
	idle_has_begun = 1;
	#endif
}

inline void cellgen_idle_stop()
{
	#ifdef PROFILING
	unsigned long long idle_time_end = 0;
	if (idle_has_begun) {
		idle_time_end = (unsigned long long) GET_TIME();
		sig.idle_time += idle_time_end - idle_time_start;
	}
	idle_has_begun = 0;
	#endif
}

inline void dma_wait(int TAG_ID, int fn_id)
{ 
	cellgen_dma_start();
	mfc_write_tag_mask(1 << ((unsigned int)TAG_ID));
	mfc_read_tag_status_all(); 
	cellgen_dma_stop(fn_id); 
}

inline void cellgen_timer_reset()
{
	#ifdef PROFILING
	cellgen_idle_stop();
	cellgen_timer_init();
	total_time_start = GET_TIME();
	#endif
}

inline void cellgen_timer_begin()
{
	#ifdef PROFILING
	unsigned int i;
	for (i = 0; i < NUM_FNs; i++) {
		sig.T_fn[i] = 0;
		sig.T_DMA[i] = 0;
		sig.T_total[i] = 0;
	}
	sig.idle_time = (unsigned long long) 0;
	sig.all_fn = (unsigned long long) 0;
	sig.all_dma = (unsigned long long) 0;
	sig.all_total = (unsigned long long) 0;
	idle_has_begun = 0;
	#endif
}

/* Function used for establishing the memory
 * regions in LS, used for PPE <-> SPE 
 * communication */
inline void ppe_exchange()
{
	spe_threads = spu_read_in_mbox();
	spe_id = spu_read_in_mbox();
	spu_write_out_mbox((unsigned int)&pass);
	spu_write_out_mbox((unsigned int)&sig);
}

/* Function used for signaling the PPE */
inline void spe_stop(int fn_id __attribute__((unused)))
{
	sig.start = 0;
	#ifdef PROFILING
	cellgen_dma_start();
	#endif
	spu_dsync();
	#ifdef PROFILING
	cellgen_dma_stop(fn_id);
	#endif
	sig.stop=1;
	#ifdef PROFILING
	sig.T_fn[fn_id-1] += GET_TIME() - total_time_start;
	cellgen_idle_start();
	#endif
}

#endif

