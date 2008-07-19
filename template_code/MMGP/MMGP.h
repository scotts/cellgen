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

#define MAX_NUM_SPEs    20
#define TERMINATE	0
#define GET_TIMES	(NUM_KERNELS+1)
#define NUM_FNs		NUM_KERNELS
#define TB      	79800000UL

void MMGP_init(unsigned int num_threads);
void MMGP_finish(double total_time);

void (*MMGP_offload)(void);
void (*MMGP_start_SPE)(unsigned int i, int value);
void (*MMGP_wait_SPE)(unsigned int num);
//void (*MMGP_reduction)(double *cont, int num);
void (*MMGP_prediction)(void);
void (*MMGP_create_threads)(void);
void (*MMGP_prediction)(void);

#define MMGP_reduction(c, op) \
({ \
	int i; \
	sched_yield(); \
	for(i=0; i<__SPE_threads; i++) { \
		while (((struct signal *)signal[i])->stop==0) { \
			sched_yield(); \
		} \
		*c op##= ((struct signal *)signal[i])->result; \
	} \
})


//extern spe_program_handle_t PROGRAM_NAME_spe;

#define mftb()  ({unsigned long rval;   \
                    asm volatile("mftb %0" : "=r" (rval)); rval;})

#define mftbl()         ({unsigned long rval;   \
                         asm volatile("mftbl %0" : "=r" (rval)); rval;})

#define mftbu()         ({unsigned long rval;   \
                         asm volatile("mftbu %0" : "=r" (rval)); rval;})

#define _sync    __asm__ __volatile("sync")

/* Timing instruction for Power arch. */
static inline unsigned long long get_tb(){

     unsigned int tbhi, tblo, tbhi2;
     
     do {
         tbhi = mftbu();
         tblo = mftbl();
         tbhi2 = mftbu();
     } while (tbhi != tbhi2);
     
     return ((unsigned long long)tbhi << 32) | tblo;
}



/* Structure used for PPE<->SPE signaling */
volatile struct signal{

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
    unsigned long long all_comp;
    #endif
};

volatile unsigned int pass[MAX_NUM_SPEs];
volatile unsigned int signal[MAX_NUM_SPEs];

unsigned long long MPI_calls;
//unsigned long long ppe_T[MAX_NUM_SPEs], ppe_N[MAX_NUM_SPEs],  total_time, start_time;
//unsigned long long spe_L[MAX_NUM_SPEs], spe_T[MAX_NUM_SPEs];
unsigned long long COMM_rec[10], MPI_total, MPI_count;
unsigned int __SPE_threads;
//double prediction[20][20];
unsigned int NUM_SPE;

#ifdef PROFILING
unsigned long long time_loop[NUM_FNs];
unsigned long long loop_time;
unsigned int cnt_loop[NUM_FNs];
unsigned int loop_started;
unsigned long long time_ppu_start;
unsigned long long time_ppu_between_loops;

#define profile_start_fn(fn_id) {                             \
      cnt_loop[fn_id-1]++;                                      \
      loop_time = get_tb ();                                  \
      if (loop_started)                                       \
        time_ppu_between_loops += loop_time - time_ppu_start; \
      else                                                    \
        loop_started = 1;                                     \
}

#define profile_end_fn(fn_id) {                               \
      time_loop[fn_id-1] += get_tb () - loop_time;              \
      time_ppu_start = get_tb ();                             \
}

#else

#define profile_start_fn(fn_id) 
#define profile_end_fn(fn_id) 

#endif
