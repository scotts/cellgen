/* This file was added to the original version to support the Cell execution of PBPI.
 * Contact:
 *	Filip Blagojevic
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: filip@cs.vt.edu
*/

#define TERMINATE	0
#define GET_TIMES	(NUM_KERNELS+1)
#define NUM_FNs		NUM_KERNELS


#ifdef PROFILING
#include "cellgen_timer.h"
#define TB      79800000UL
unsigned long long total_time_start, loop_time_start;
unsigned long long dma_time_start, comp_time_start, idle_time_start;
int idle_has_begun;
#endif

/* Structure used for PPE <-> SPE communication */
struct signal {

    int start, stop;
    unsigned long long total_time,loop_time;
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

volatile struct signal signal __attribute__((aligned(128)));

int SPE_id; // SPE thread id
int SPE_threads; // Each SPE trhead knows the total number of SPE threads in use
volatile struct pass_t pass __attribute__((aligned(128))); // User defined structure used for PPE <-> SPE comm.


/* Function used for waiting for the PPE signal */
inline int MMGP_SPE_wait(void){

        while (signal.start==0);

        return signal.start;
}

/* Function used for main memory <-> LS synchronization */

inline void cellgen_report(void)
{
   #ifdef PROFILING
   int i = 0;

   for(i=0; i<NUM_FNs; i++) {
      signal.all_fn += signal.T_fn[i];
      signal.all_dma += signal.T_DMA[i];
      signal.all_comp += signal.T_comp[i];
   }

   signal.start=0;
   spu_dsync();
   signal.stop=1;
   #endif
}

#ifdef PROFILING
inline void cellgen_DMA_start(void)
{
    dma_time_start = GET_TIME();
}

inline void cellgen_DMA_stop(int loop)
{
    signal.T_DMA[loop-1] += GET_TIME() - dma_time_start;
}

#define cellgen_comp_start() {    \
    comp_time_start = GET_TIME(); \
}

#define cellgen_comp_stop(loop) {                           \
    signal.T_comp[loop-1] += GET_TIME() - comp_time_start;  \
}

inline void cellgen_idle_start(void)
{
    idle_time_start = (unsigned long long) GET_TIME();
    idle_has_begun = 1;
}

inline void cellgen_idle_stop(void)
{
    unsigned long long idle_time_end = 0;
    if (idle_has_begun) {
       idle_time_end = (unsigned long long) GET_TIME();
       signal.idle_time += idle_time_end - idle_time_start;
    }
    idle_has_begun = 0;
}

#define cellgen_timer_reset() {                    \
    cellgen_idle_stop();                           \
    cellgen_timer_init();                          \
    total_time_start = GET_TIME();                 \
}

#define cellgen_timer_begin() {                    \
    unsigned int i;                                \
    for(i=0; i<NUM_FNs; i++) {                     \
       signal.T_fn[i] = 0;                         \
       signal.T_DMA[i] = 0;                        \
       signal.T_comp[i] = 0;                       \
    }                                              \
    signal.idle_time = (unsigned long long) 0;     \
    signal.all_fn = (unsigned long long) 0;        \
    signal.all_dma = (unsigned long long) 0;       \
    signal.all_comp = (unsigned long long) 0;      \
    idle_has_begun = 0;                            \
}

#define MMGP_SPE_dma_wait(TAG_ID, fn_id)           \
{                                                  \
    cellgen_DMA_start();                           \
    mfc_write_tag_mask(1<<((unsigned int)TAG_ID)); \
    mfc_read_tag_status_all();                     \
    cellgen_DMA_stop(fn_id);                       \
}

/*
inline void cellgen_comp_start(void)
{
    comp_time_start = GET_TIME();
}

inline void cellgen_comp_stop(int loop)
{
    signal.T_comp[loop-1] += GET_TIME() - comp_time_start;
}

inline void cellgen_timer_reset(void)
{
    cellgen_idle_stop();
    cellgen_timer_init();
    total_time_start = GET_TIME();
}

inline void cellgen_timer_begin(void)
{
    unsigned int i;
    for(i=0; i<NUM_FNs; i++) {
       signal.T_fn[i] = 0;
       signal.T_DMA[i] = 0;
       signal.T_comp[i] = 0;
    }
    signal.idle_time = (unsigned long long) 0;
    signal.all_fn = (unsigned long long) 0;
    signal.all_dma = (unsigned long long) 0;
    signal.all_comp = (unsigned long long) 0;
    idle_has_begun = 0;
    //cellgen_timer_init();
}

*/

#else   // no profiling


#define cellgen_comp_start()

#define cellgen_comp_stop(fn_id)

#define cellgen_timer_reset()

#define cellgen_timer_begin()

#define MMGP_SPE_dma_wait(TAG_ID, fn_id)           \
{                                                  \
    mfc_write_tag_mask(1<<((unsigned int)TAG_ID)); \
    mfc_read_tag_status_all();                     \
}

#endif  // end of no profiling


/* Function used for establishing the memory
 * regions in LS, used for PPE <-> SPE 
 * communication */
inline void MMGP_exchange(){

    SPE_threads = spu_read_in_mbox();
    SPE_id = spu_read_in_mbox();
    spu_write_out_mbox((unsigned int)&pass);
    spu_write_out_mbox((unsigned int)&signal);

}

/* Function used for signaling the PPE */
inline void MMGP_SPE_stop(int fn_id){

    signal.start=0;
    #ifdef PROFILING
    cellgen_DMA_start();
    #endif
    spu_dsync();
    #ifdef PROFILING
    cellgen_DMA_stop(fn_id);
    #endif
    signal.stop=1;
    #ifdef PROFILING
    signal.T_fn[fn_id-1] += GET_TIME() - total_time_start;
    cellgen_idle_start();
    #endif
	
}
