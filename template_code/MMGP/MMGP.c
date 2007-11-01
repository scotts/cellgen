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

/* The next four functions are equivalent to _wait_SPET()
 * and _wait_SPE(), except that they are used in the
 * case when a global reduction needs to be performed.
 * Two versions are available, for double and integer
 * reduction. Parameters:
 * 1. cont - pointer to the variable that should contain 
 *           the sum of the returning values from all SPEs.
 * 2. num - the SPE number (the work can be distributed 
 *          among multiple SPEs). */ 
inline void _reductionDT(double *cont, int num){

    int i;

    sched_yield();
    for(i=0; i<__SPE_threads; i++){
        while (((struct signal *)signal[i])->stop==0){
            sched_yield();
        }

        *cont += ((struct signal *)signal[i])->result;
        
        if (i==0){
            COMM_rec[i] = get_tb();
            spe_T[num] += ((struct signal *)signal[i])->total_time;
            spe_L[num] += ((struct signal *)signal[i])->loop_time;
        }
    }
    ppe_T[num] += (get_tb()-start_time);
    ppe_N[num]++;
    
}

inline void _reductionD(double *cont, int num){

    int i;

    sched_yield();
    for(i=0; i<__SPE_threads; i++){
        while (((struct signal *)signal[i])->stop==0){
            sched_yield();
        }

        *cont += ((struct signal *)signal[i])->result;
        
    }
}


inline void _reductionIT(int *cont, int num){

    int i;

    sched_yield();
    for(i=0; i<__SPE_threads; i++){
        while (((struct signal *)signal[i])->stop==0){
            sched_yield();
        }

        *cont += ((struct signal *)signal[i])->result_int;
        
        if (i==0){
            COMM_rec[i] = get_tb();
            spe_T[num] += ((struct signal *)signal[i])->total_time;
            spe_L[num] += ((struct signal *)signal[i])->loop_time;
        }
    }
    ppe_T[num] += (get_tb()-start_time);
    ppe_N[num]++;
    
}

inline void _reductionI(int *cont, int num){

    int i;

    sched_yield();
    for(i=0; i<__SPE_threads; i++){
        while (((struct signal *)signal[i])->stop==0){
            sched_yield();
        }

        *cont += ((struct signal *)signal[i])->result_int;
        
    }
}


void _prediction(){
        
    /* Getting the total execution time */
    total_time = get_tb() - total_time;
 
    double init_Thpu, init_Tapu, init_g, init_o;
    double Thpu, Tapu, Tmpi=0, Tgap, Tsca;
    double time, ctsw, cont_swch;

    /* Estimated number of processes per a single
     * execution context */
    double proc_per_core[]={0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0,4.0};
    /* Relation between the context sitching 
     * overhead and the number of processes */
    double ctsw_over[]={0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0};
    /* Overhead caused by the MPI messages
     * (measured with mpptest */
    double mpi_over[]={0,0,16.0,70.0,118.0,167.0,173.0,215.0,265.0,385.0,632.0,600.0,573.0,590.0,674.0,740.0,842.0};
    int i,j;
    
    unsigned long long ppe_Tot, spe_Tot, spe_Sca, ppe_Num;
    ppe_Tot=0; spe_Tot=0 ; spe_Sca=0; ppe_Num=0;
        
    for(i=1; i<4; i++){
        /* Total time spent on an SPE,
         * measured from the PPE (used to
         * estimate the gap) */
        ppe_Tot += ppe_T[i];
        /* Total time spent on an SPE */
        spe_Tot += spe_T[i];
        /* Total time if the SPE loop
         * (this part scales with the
         * number of SPEs) */
        spe_Sca += spe_L[i];
        /* Total number of SPE invocations */
        ppe_Num += ppe_N[i];
    }
    
    /* Translate clock ticks into seconds */
    init_Thpu = (double)(total_time - ppe_Tot)/TB;
    init_Tapu = (double)spe_Sca/TB;
    init_g = (double)(ppe_Tot - spe_Tot)/TB;
    init_o = (double)(spe_Tot - spe_Sca)/TB;

    //printf("%f %f %f %f %llu %llu\n", init_Thpu,init_Tapu,init_g,init_o, MPI_calls, ppe_Num);
    
    
    /* Since the inital sampling is with 8 SPE threads,
     * adjust the SPE timing and the gap (the number
     * of SPEs in the sampling phase can be changed) */
    init_Tapu*=8;
    init_g/=8;

    /* Determine the total number of context switches */
    ctsw = ppe_Num;
    cont_swch=0;

    double min = 0;
    int minI=0, minJ=0;
   
    /* Calculate the timings (i represents 
     * the number of processes, j represents 
     * the number of SPEs per a process) */
    for(i=1;i<NUM_SPE;i++){
        for(j=1; i*j<NUM_SPE; j++){

            /* If the number of processes is larger
             * than 1, we need to add some contention, MPI,
             * and context switch overhead */
            if (i>1){
                /* PPE Time */
                Thpu = 1.28*(init_Thpu)*proc_per_core[i]; 
                /* MPI Time */
                Tmpi = mpi_over[i]*MPI_calls/1000000;
                /* Context switch time, estimated time for 
                 * a single context switch is 1.5us */
                cont_swch = 1*(ctsw/100000)*ctsw_over[i];
            }
            else Thpu = init_Thpu;
            
            /* Calculate the total SPE time */
            Tgap = j*init_g;
            Tsca = init_Tapu/(i*j);
            Tapu = Tsca + init_o + cont_swch;

            /* Check if context switch overhead is larger 
             * than the time spent on an SPE */           
            if (cont_swch > Tapu) Tapu = 1*(ctsw/100000)*ctsw_over[i];
            else Tapu = Tsca;

            /* Calculate the final (predicted) time */
            prediction[i][j] = Thpu + Tmpi + Tapu + Tgap + init_o + cont_swch; 

	    //printf("%d x %d, Predicted Time %f, THPU %f, MPI %f, TLIK %f, TAPU %f, CTSWC %f, TC %f, o %f\n",i,j,prediction[i][j],Thpu,Tmpi,init_Tapu/(i*j),Tapu+Tgap+init_o+cont_swch,cont_swch,Tgap,init_o);

            if (i==1 && j==1){
                min = prediction[i][j];
                minI=1;
                minJ=1;
            }
            else if (prediction[i][j]<min){
                min = prediction[i][j];
                minI=i;
                minJ=j;
            }
	}
    }

    FILE *f = fopen("PBPI_Cell.configuration","w");
    /* Number of processes */
    fprintf(f,"%d\n",minI);
    /* Number of SPEs */
    fprintf(f,"%d\n",minJ);
    /* Sampling phase */
    fprintf(f,"%d\n",0);
    fclose(f);
    
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

    TB = 14318000;

    MMGP_offload = &_empty;
    MMGP_prediction = &_empty;
    MMGP_reduction = &_reductionD; 
    MMGP_create_threads = &_create_threads;
    MMGP_wait_SPE = &_wait_SPE;
    MMGP_start_SPE = &_start_SPE;
    MMGP_create_threads = &_create_threads;
}



