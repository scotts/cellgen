/* This file was added to the original version to support the Cell execution of PBPI.
 * Contact:
 *	Filip Blagojevic
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: filip@cs.vt.edu
*/
#include <asm/unistd.h>
#include <unistd.h>
#include <malloc_align.h>
#include <sched.h>

int sched_yield(void);

#define MAX_NUM_SPEs    20
#define TERMINATE	0
#define GET_TIMES	(NUM_KERNELS+1)
#define NUM_FNs		NUM_KERNELS
#define TB      	79800000UL

void MMGP_init(unsigned int num_threads);
void cellgen_start(void);
void cellgen_finish(void);

void MMGP_offload(void);
void MMGP_start_SPE(unsigned int i, int value);
void MMGP_wait_SPE(int fn_id);
void MMGP_create_threads(void);

#define MMGP_reduction(c, op, fn_id) \
({ \
	int i; \
	sched_yield(); \
	for(i=0; i<__SPE_threads; i++) { \
		while (((struct signal *)signal[i])->stop==0) { \
			sched_yield(); \
		} \
		*c op##= ((struct signal *)signal[i])->result;  \
	} \
})

#define mftb()  ({unsigned long rval;   \
                    asm volatile("mftb %0" : "=r" (rval)); rval;})

#define mftbl()         ({unsigned long rval;   \
                         asm volatile("mftbl %0" : "=r" (rval)); rval;})

#define mftbu()         ({unsigned long rval;   \
                         asm volatile("mftbu %0" : "=r" (rval)); rval;})

#define _sync    __asm__ __volatile("sync")

/* Timing instruction for Power arch. */
static inline unsigned long long get_tb() {

     unsigned int tbhi, tblo, tbhi2;
     
     do {
         tbhi = mftbu();
         tblo = mftbl();
         tbhi2 = mftbu();
     } while (tbhi != tbhi2);
     
     return ((unsigned long long)tbhi << 32) | tblo;
}

/* Structure used for PPE<->SPE signaling */
struct signal {

    int start, stop;
    unsigned long long total_time, loop_time;
    double result;
    int result_int;
    
    #ifdef PROFILING
    unsigned long long T_fn[NUM_FNs];
    unsigned long long T_DMA[NUM_FNs];
    unsigned long long T_comp[NUM_FNs];
    unsigned long long idle_time;
    unsigned long long all_fn;
    unsigned long long all_dma;
    unsigned long long all_dma_prep;
    unsigned long long all_comp;
    #endif
};

volatile unsigned int pass[MAX_NUM_SPEs];
volatile unsigned int signal[MAX_NUM_SPEs];

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

#define profile_start_fn(fn_id) {                             \
      loop_time = get_tb ();                                  \
      if (loop_started)                                       \
        time_ppu_between_loops += loop_time - time_ppu_start; \
      else                                                    \
        loop_started = 1;                                     \
}

#define profile_end_fn(fn_id) {                               \
      cnt_loop[fn_id-1]++;                                    \
      time_loop[fn_id-1] += get_tb () - loop_time;            \
      time_ppu_start = get_tb ();                             \
}

#else

#define profile_start_fn(fn_id)
#define profile_end_fn(fn_id) 

#endif
