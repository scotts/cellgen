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
#include <asm/unistd.h>
#include <unistd.h>
#include <malloc_align.h>

void MMGP_init(int num_threads);

void (*MMGP_offload)(void);
void (*MMGP_start_SPE)(int i, int value);
void (*MMGP_wait_SPE)(int num);
//void (*MMGP_reduction)(double *cont, int num);
void (*MMGP_prediction)(void);
void (*MMGP_create_threads)(void);
void (*MMGP_prediction)(void);

inline void send_mail(speid_t id, unsigned int data);

extern spe_program_handle_t PROGRAM_NAME_spe;

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


volatile unsigned int ls_addr[20];
speid_t spe_id[20];
spe_gid_t spe_gid;

/* Structure used for PPE<->SPE signaling */
volatile struct signal{

    int start, stop;
    unsigned long long total_time, loop_time;
    double result;
    int result_int;
    
};

volatile unsigned int pass[20];
volatile unsigned int signal[20];

unsigned long long MPI_calls;
unsigned long long ppe_T[20], ppe_N[20],  total_time, start_time;
unsigned long long spe_L[20], spe_T[20];
unsigned long long COMM_rec[10], MPI_total, MPI_count;
int __SPE_threads,N;
double TB;
double prediction[20][20];
int NUM_SPE;
