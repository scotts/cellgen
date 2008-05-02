/* This file was added to the original version to support the Cell execution of PBPI.
 * Contact:
 *	Filip Blagojevic
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: filip@cs.vt.edu
*/

#include <libmisc.h>
#include <libspe.h>
#include <libsync.h>
#include "MMGP.h"
#include <stdio.h>
#include <sched.h>
#include <sys/sysinfo.h>

/* Sending mail to an SPE. Parameters:
 * id - id of the targeting SPE,
 * data - 32 bit data that is sent. */
inline void send_mail(speid_t id, unsigned int data){
           spe_write_in_mbox(id,data);

}

/* Creates SPE trheads, parameters:
 * 1. SPE_trheads, number of SPE threads to be created */
void _create_threads(void)
{
        int i;

        /* If the number SPE trheads is larger than 
         * the number of SPEs */
        if (__SPE_threads>NUM_SPE){
            printf("Error: not enough SPEs %d %d\n",__SPE_threads,NUM_SPE);
            exit(0);
        }
        
        /* Forking SPE threads */
        for(i=0; i<__SPE_threads; i++){
 
            spe_id[i] = spe_create_thread(spe_gid, &PROGRAM_NAME_spe, NULL, NULL, -1, 0);

            if (spe_id[i]==NULL){
                printf("INTERNAL ERROR\n");
                exit(1);
            }

            send_mail(spe_id[i],(unsigned int) __SPE_threads);
            send_mail(spe_id[i],i);
        }
        
       
        /* Getting the LS addresses of all SPE threads */
        for(i=0; i<__SPE_threads; i++){
            ls_addr[i] = (unsigned int) spe_get_ls(spe_id[i]);
        }
      
        for(i=0; i<__SPE_threads; i++){

            /* Getting the addresses of the communication parameters of the SPE threads */
            while(spe_stat_out_mbox(spe_id[i])<=0);
            pass[i] = spe_read_out_mbox(spe_id[i]);
            pass[i] += ls_addr[i];
            
            while(spe_stat_out_mbox(spe_id[i])<=0);
            signal[i] = spe_read_out_mbox(spe_id[i]);
            signal[i] += ls_addr[i];
        }
    
}

/* Signal an SPE to start performing work, parameters:
 * 1. num - the SPE number (the work can be distributed 
 *          across multiple SPEs) ,
 * 2. value - values sent to an SPE in order to specify
 *            which function should be executed (multiple
 *            SPE functions can reside in the same SPE
 *            module) */
inline void _start_SPE(int num, int value){

    /* Send starting signal to an SPE,
     * before that set signal.stop to 0 */
    ((struct signal *)signal[num])->stop=0;
    _sync;
    ((struct signal *)signal[num])->start=value; 
               
}

/* The function called before each off-loading region.
 * This function is used to measure the off-loading time */
inline void _offload(){

    /* Start measuring the SPE time 
     * from the PPE */
    start_time = get_tb();

}

/* The function called at the end of the off-loaded region.
 * Parameters:
 * 1. num - the SPE number (the work can be distributed 
 *          among multiple SPEs) */
inline void _wait_SPET(int num){

    int i=0;
    
    sched_yield();

    /* Wait for all SPE trheads to return */
    for(i=0; i<__SPE_threads; i++){
       
        /* If an SPE thread is not done, release the PPE */
        while (((struct signal *)signal[i])->stop==0){
            sched_yield();
        }

        /* Get the SPE timings */
        if (i==0){
            spe_T[num] += ((struct signal *)signal[i])->total_time;
            spe_L[num] += ((struct signal *)signal[i])->loop_time;
        }
    }

    ppe_T[num] += (get_tb()-start_time);
    ppe_N[num]++;

}

/* The same as _wait_SPET(), just without the 
 * timing instructions */
inline void _wait_SPE(int num){

    int i=0;
    
    sched_yield();
    for(i=0; i<__SPE_threads; i++){
       
        while (((struct signal *)signal[i])->stop==0){
            sched_yield();
        }
    }

}

inline void _empty() {}

void MMGP_init(int num_threads)
{
    /*Determine the total number of SPEs*/
    //int nprocs = get_nprocs();
    NUM_SPE = spe_count_physical_spes();
    __SPE_threads = num_threads;

    /* SPEs will be enumerated from 1 to NUM_SPE+1 */
    NUM_SPE++;

    MMGP_offload = &_empty;
    MMGP_create_threads = &_create_threads;
    MMGP_wait_SPE = &_wait_SPE;
    MMGP_start_SPE = &_start_SPE;
    MMGP_create_threads = &_create_threads;
}

__attribute__((constructor)) void initialize_MMGP()
{
	MMGP_init(NUM_THREADS_HOOK);
	MMGP_create_threads();
}

