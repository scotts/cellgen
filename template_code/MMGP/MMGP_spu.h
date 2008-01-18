/* This file was added to the original version to support the Cell execution of PBPI.
 * Contact:
 *	Filip Blagojevic
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: filip@cs.vt.edu
*/

/* Function used for main memory <-> LS synchronization */
inline void MMGP_SPE_dma_wait(unsigned int tag_id) 
{ 
	mfc_write_tag_mask(1<<(tag_id));
	mfc_read_tag_status_all();
}

/* Structure used for PPE <-> SPE communication */
struct signal {

    int start, stop;
    unsigned long long total_time,loop_time;
    double result;
};

volatile struct signal signal __attribute__((aligned(128)));

int SPE_id; // SPE thread id
int SPE_threads; // Each SPE trhead knows the total number of SPE threads in use
volatile struct pass_t pass __attribute__((aligned(128))); // User defined structure used for PPE <-> SPE comm.

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
inline void MMGP_SPE_stop(){

    signal.start=0;
    spu_dsync();
    signal.stop=1;
	
}

/* Function used for waiting for the PPE signal */
inline int MMGP_SPE_wait(){

        while (signal.start==0);

        return signal.start;
}

