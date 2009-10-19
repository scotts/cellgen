/* This file was added to the original version to support the Cell execution of PBPI.
 * Contact:
 *	Filip Blagojevic
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: filip@cs.vt.edu
*/

#ifndef MMGP_H
#define MMGP_H

#include <asm/unistd.h>
#include <unistd.h>
#include <malloc_align.h>
#include <sched.h>

int sched_yield();

#define MAX_NUM_SPEs 20
#define TERMINATE 0
#define GET_TIMES (NUM_KERNELS+1)
#define NUM_FNs	NUM_KERNELS
#define TB 79800000UL

#if defined(__cplusplus)
extern "C" {
#endif

void spe_init(unsigned int num_threads);
void cellgen_start();
void cellgen_finish();

void spe_offloads(void);
void spe_start(unsigned int i, int value);
void wait_for_spes(int fn_id);
void spe_create_threads();

#if defined(__cplusplus)
}
#endif

#define spe_reduction(c, op, fn_id) \
({ \
	unsigned int i; \
	sched_yield(); \
	for (i = 0; i < __SPE_threads; i++) { \
		while (((struct signal_t *)sig[i])->stop==0) { \
			sched_yield(); \
		} \
		*c op##= ((struct signal_t *)sig[i])->result;  \
	} \
})

static inline unsigned long mftb()
{
	unsigned long rval; 
	asm volatile("mftb %0" : "=r" (rval)); 
	return rval;
}

static inline unsigned long mftbl()
{
	unsigned long rval;
	asm volatile("mftbl %0" : "=r" (rval));
	return rval;
}

static inline unsigned long mftbu()
{
	unsigned long rval;
	asm volatile("mftbu %0" : "=r" (rval)); 
	return rval;
}

#define _sync    __asm__ __volatile("sync")

/* Timing instruction for Power arch. */
static inline unsigned long long get_tb()
{
	unsigned int tbhi, tblo, tbhi2;
     
	do {
		tbhi = mftbu();
		tblo = mftbl();
		tbhi2 = mftbu();
	} while (tbhi != tbhi2);
     
	return ((unsigned long long)tbhi << 32) | tblo;
}

/* Structure used for PPE<->SPE signaling */
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

volatile unsigned int pass[MAX_NUM_SPEs];
volatile unsigned int sig[MAX_NUM_SPEs];

unsigned long long MPI_calls;
unsigned long long COMM_rec[10], MPI_total, MPI_count;
unsigned int __SPE_threads;
unsigned int NUM_SPE;

#ifdef PROFILING
unsigned long long time_loop[NUM_FNs];
unsigned long long loop_time;
unsigned int cnt_loop[NUM_FNs];
unsigned int loop_started;
unsigned long long time_ppu_start;
unsigned long long time_ppu_between_loops;
unsigned long long time_cellgen_start;
#endif

static inline void profile_start_fn()
{
	#ifdef PROFILING
	loop_time = get_tb();
	if (loop_started) {
		time_ppu_between_loops += loop_time - time_ppu_start;
	}
	else {
		loop_started = 1;
	}
	#endif
}

static inline void profile_end_fn(unsigned int fn_id)
{
	#ifdef PROFILING
	cnt_loop[fn_id-1]++;
	time_loop[fn_id-1] += get_tb() - loop_time;
	time_ppu_start = get_tb();
	#endif
}

#endif // MMGP_H
