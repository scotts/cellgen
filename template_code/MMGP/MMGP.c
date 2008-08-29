/* This file was added to the original version to support the Cell execution of PBPI.
 * Contact:
 *	Filip Blagojevic
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: filip@cs.vt.edu
*/

#include <libspe2.h>
#include "MMGP.h"
#include <stdio.h>
#include <sched.h>
#include <sys/sysinfo.h>
#include <string.h>

/* Sending mail to an SPE. Parameters:
 * id - id of the targeting SPE,
 * data - 32 bit data that is sent. */
#define send_mail(speid, data)                                        \
{                                                                     \
    void *data_addr = (void*) &data;                                  \
    spe_in_mbox_write(speid, data_addr, 1, SPE_MBOX_ANY_NONBLOCKING); \
}

extern spe_program_handle_t PROGRAM_NAME_spe;

typedef struct pthread_data {
    spe_context_ptr_t speid;
    pthread_t pthread;
    unsigned int entry;
    spe_stop_info_t stopinfo;
    pthread_attr_t attr;
} pthread_data_t;

volatile unsigned int ls_addr[MAX_NUM_SPEs];
pthread_data_t ptdata[MAX_NUM_SPEs];
spe_mfc_command_area_t *mfc_ps_area[MAX_NUM_SPEs];
spe_spu_control_area_t *mbox_ps_area[MAX_NUM_SPEs];
spe_sig_notify_1_area_t *sig_notify_ps_area[MAX_NUM_SPEs];
spe_mssync_area_t *mssync_ps_area[MAX_NUM_SPEs];

void *pthread_function(void *arg) {
    pthread_data_t *datap = (pthread_data_t *)arg;
    int rc;
    unsigned int entry = SPE_DEFAULT_ENTRY;
    if ((rc = spe_context_run(datap->speid, &entry, 0, NULL, NULL, NULL)) < 0) {
        fprintf (stderr, "Error: spe_context_runi() (rc=%d, errno=%d, strerror=%s)\n", rc, errno, strerror(errno));
        exit (1);
    }
    pthread_exit(NULL);
}

/* Creates SPE trheads, parameters:
 * 1. SPE_trheads, number of SPE threads to be created */
void MMGP_create_threads(void)
{
        unsigned int i, rval;
        int rc;

        /* If the number SPE trheads is larger than 
         * the number of SPEs */
        if (__SPE_threads>NUM_SPE){
            printf("Error: not enough SPEs %d %d\n",__SPE_threads,NUM_SPE);
            exit(0);
        }

        /* Forking SPE threads */
        for(i=0; i<__SPE_threads; i++){
            if ((ptdata[i].speid = spe_context_create (SPE_MAP_PS | SPE_CFG_SIGNOTIFY1_OR, NULL)) == NULL) {
                fprintf (stderr, "Error: spe_context_create (errno=%d strerror=%s)\n", errno, strerror(errno));
                exit (1);
            }

            if ((rc = spe_program_load (ptdata[i].speid, &PROGRAM_NAME_spe)) != 0) {
                fprintf (stderr, "Error: spe_program_load() (errno=%d strerror=%s)\n", errno, strerror(errno));
                exit (1);
            }

            if ((rc=pthread_create(&(ptdata[i].pthread), NULL, &pthread_function, &ptdata[i])) != 0)
            {
                fprintf (stderr, "Error: pthread_create() (errno=%d strerror=%s)\n", errno, strerror(errno));
                exit (1);
            }

            /* Get the direct program store addresses */
            mfc_ps_area[i] = spe_ps_area_get(ptdata[i].speid, SPE_MFC_COMMAND_AREA);
            if (mfc_ps_area[i] == NULL) {
                fprintf (stderr, "Error: spe_ps_area_get(SPE_MFC_COMMAND_AREA) (errno=%d strerror=%s)\n", errno, strerror(errno));
                exit (1);
            }

            mbox_ps_area[i] = spe_ps_area_get(ptdata[i].speid, SPE_CONTROL_AREA);
            if (mbox_ps_area[i] == NULL) {
                fprintf (stderr, "Error: spe_ps_area_get(SPE_CONTROL_AREA) (errno=%d strerror=%s)\n", errno, strerror(errno));
                exit (1);
            }

            sig_notify_ps_area[i] = spe_ps_area_get(ptdata[i].speid, SPE_SIG_NOTIFY_1_AREA);
            if (sig_notify_ps_area[i] == NULL) {
                fprintf (stderr, "Error: spe_ps_area_get(SPE_SIG_NOTIFY_1_AREA)  (errno=%d strerror=%s)\n", errno, strerror(errno));
                exit (1);
            }

            mssync_ps_area[i] = spe_ps_area_get(ptdata[i].speid, SPE_MSSYNC_AREA);
            if (mssync_ps_area[i] == NULL) {
                fprintf (stderr, "Error: spe_ps_area_get(SPE_MSSYNC_AREA) (errno=%d strerror=%s)\n", errno, strerror(errno));
                exit (1);
            }

            send_mail(ptdata[i].speid, __SPE_threads);
            send_mail(ptdata[i].speid,i);
        }
        
       
        /* Getting the LS addresses of all SPE threads */
        for(i=0; i<__SPE_threads; i++) {
            ls_addr[i] = (unsigned long) spe_ls_area_get(ptdata[i].speid);
        }

        for(i=0; i<__SPE_threads; i++){
            /* Getting the addresses of the communication parameters of the SPE threads */
            while(spe_out_mbox_status(ptdata[i].speid)==0);
            spe_out_mbox_read(ptdata[i].speid, &rval, 1);
            pass[i] = ls_addr[i] + rval;

            while(spe_out_mbox_status(ptdata[i].speid)==0);
            spe_out_mbox_read(ptdata[i].speid, &rval, 1);
            signal[i] = ls_addr[i] + rval;
        }
}

/* Signal an SPE to start performing work, parameters:
 * 1. num - the SPE number (the work can be distributed 
 *          across multiple SPEs) ,
 * 2. value - values sent to an SPE in order to specify
 *            which function should be executed (multiple
 *            SPE functions can reside in the same SPE
 *            module) */
inline void MMGP_start_SPE(unsigned int num, int value){

    /* Send starting signal to an SPE,
     * before that set signal.stop to 0 */
    ((struct signal *)signal[num])->stop=0;
    _sync;
    ((struct signal *)signal[num])->start=value; 
               
}

inline void MMGP_offload(void)
{
    profile_start_fn();
}


/* The same as _wait_SPET(), just without the 
 * timing instructions */
inline void MMGP_wait_SPE(int fn_id)
{
    unsigned int i=0;
    
    sched_yield();
    for(i=0; i<__SPE_threads; i++){
       
        while (((struct signal *)signal[i])->stop==0){
            sched_yield();
        }
    }
    profile_end_fn(fn_id);
}


inline void cellgen_start(void)
{
    #ifdef PROFILING
    time_cellgen_start = get_tb();
    #endif
}

inline void cellgen_finish(void)
{
    unsigned int i;

    #ifdef PROFILING
    unsigned long long time_cellgen_end = get_tb();
    unsigned int loop;
    unsigned int loop_cnt_all = 0;
    unsigned long long loop_time_all = 0UL;

    unsigned long long T_L[MAX_NUM_SPEs][NUM_FNs];
    unsigned long long T_DMA[MAX_NUM_SPEs][NUM_FNs];
    unsigned long long T_comp[MAX_NUM_SPEs][NUM_FNs];
    unsigned long long T_L_spe[__SPE_threads];
    unsigned long long T_DMA_spe[__SPE_threads];
    unsigned long long T_DMA_prep_spe[__SPE_threads];
    unsigned long long T_comp_spe[__SPE_threads];
    unsigned long long T_idle_spe[__SPE_threads];
    unsigned long long T_L_all, T_DMA_all, T_comp_all;

    printf("\n\nTotal Time: %f (sec)\n\n", (double)(time_cellgen_end-time_cellgen_start) / TB);
    printf("\n========== PPE stats ==========\n\n");

    for (i=0; i<NUM_FNs; i++) {
        printf("fn%u call count: %u\n", i+1, cnt_loop[i]);
        loop_cnt_all += cnt_loop[i];
    }

    printf("fn count total = %u\n\n", loop_cnt_all);

    for (i=0; i<NUM_FNs; i++) {
        printf("L%u: %.6f (sec)\n", i+1, ((double)time_loop[i])/TB);
        loop_time_all += time_loop[i];
    }

    printf("time spent on PPU workload between offloaded loops: %.6f (sec)\n",((double)time_ppu_between_loops)/TB);
    printf("time spend on prolog and epilog: %.6f (sec)\n", ((double)(time_cellgen_end-time_cellgen_start)-(loop_time_all+time_ppu_between_loops))/TB );


    printf("\n========== SPE stats ==========\n");

    for (i=0; i<__SPE_threads; i++)
        MMGP_start_SPE(i, GET_TIMES);

    MMGP_wait_SPE (GET_TIMES);

    for(i = 0; i < __SPE_threads; i++) { 
        T_L_spe[i] =  ((struct signal *)signal[i])->all_fn;
        T_comp_spe[i] =  ((struct signal *)signal[i])->all_comp;
        T_DMA_spe[i] =  ((struct signal *)signal[i])->all_dma;
        T_DMA_prep_spe[i] =  ((struct signal *)signal[i])->all_dma_prep;
        T_idle_spe[i] =  ((struct signal *)signal[i])->idle_time;
    }

    for (loop = 0; loop < NUM_FNs; loop++) {
        T_L_all = T_DMA_all = T_comp_all = 0;

        printf("\nL%d :          Loop      DMA     Comp\n", loop+1);
        for (i = 0; i < __SPE_threads; i++) {
            T_L_all += (T_L[i][loop] = ((struct signal *)signal[i])->T_fn[loop]);
            T_DMA_all += (T_DMA[i][loop] = ((struct signal *)signal[i])->T_DMA[loop]);
            T_comp_all += (T_comp[i][loop] = ((struct signal *)signal[i])->T_comp[loop]);
            printf("     SPE%u %8.6f %8.6f %8.6f\n",i+1, (double)T_L[i][loop] /TB, (double)T_DMA[i][loop] /TB, (double)T_comp[i][loop] /TB);
        }
        printf("\n");
        printf("avg L%u loop time: %8.6f (sec)\n", loop+1, (double)(T_L_all)/ TB / __SPE_threads);
        printf("avg L%u DMA wait time: %8.6f (sec)\n", loop+1, (double)(T_DMA_all)/ TB / __SPE_threads);
        printf("avg L%u computation time: %8.6f (sec)\n", loop+1, (double)(T_comp_all)/ TB / __SPE_threads);
    }
    printf("\n\n           --- Summary ---            \n");
    #ifndef DMA_PREP_REPORT
    printf("SPE         fn fn_kernel   DMA_all      idle\n");
    for (i = 0; i < __SPE_threads; i++) {
        printf("SPE%u  %7.6f  %7.6f  %7.6f  %7.6f\n", i+1, (double)T_L_spe[i] /TB, (double)T_comp_spe[i] /TB, (double)T_DMA_spe[i] /TB, (double)T_idle_spe[i] / TB);
    }
    #else
    printf("SPE         fn fn_kernel   DMA_all      DMA_prep  idle\n");
    for (i = 0; i < __SPE_threads; i++) {
        printf("SPE%u  %7.6f  %7.6f  %7.6f  %7.6f  %7.6f\n", i+1, (double)T_L_spe[i] /TB, (double)T_comp_spe[i] /TB, (double)T_DMA_spe[i] /TB, (double)T_DMA_prep_spe[i] /TB, (double)T_idle_spe[i] / TB);
    }
    #endif
    printf("\n\n");
    printf("========== Time legend ==========\n");
    printf("Loop: total time spent on loops\n");
    printf("DMA:  non-overlapped DMA time\n");
    printf("Comp: time spent on the computing results\n");
    printf("idle: idle SPE time due to signaling mechanism and ppu workload while switching between different loops due to signaling and ppu workload\n");
    printf("\n");
    #endif

    for (i=0; i<__SPE_threads; i++)
        MMGP_start_SPE(i, TERMINATE);

}

void MMGP_init(unsigned int num_threads)
{
    /*Determine the total number of SPEs*/
    NUM_SPE = spe_cpu_info_get(SPE_COUNT_PHYSICAL_SPES, -1);
    __SPE_threads = num_threads;


    #ifdef PROFILING
    unsigned int i;
    for (i=0; i<NUM_FNs; i++) {
        time_loop[NUM_FNs] = 0UL;
        cnt_loop[NUM_FNs] = 0UL;
    }
    loop_time = 0UL;
    loop_started = 0;
    time_ppu_start = 0UL;
    time_ppu_between_loops = 0UL;
    #endif
}

__attribute__((constructor)) void initialize_MMGP()
{
	MMGP_init(NUM_THREADS_HOOK);
	MMGP_create_threads();
}

