/* This file was added to the original version to support the Cell execution of PBPI.
 * Contact:
 *	Filip Blagojevic
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060 
 *	Email: filip@cs.vt.edu
*/

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <sched.h>
#include <unistd.h>
#include <sys/sysinfo.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <libspe2.h>

#include "MMGP.h"

pid_t getpid(void);

typedef struct pthread_data {
	spe_context_ptr_t speid;
	pthread_t pthread;
	unsigned int entry;
	spe_stop_info_t stopinfo;
	pthread_attr_t attr;
} pthread_data_t;

extern spe_program_handle_t PROGRAM_NAME_spe;

volatile unsigned int ls_addr[MAX_NUM_SPEs];
pthread_data_t ptdata[MAX_NUM_SPEs];
spe_mfc_command_area_t* mfc_ps_area[MAX_NUM_SPEs];
spe_spu_control_area_t* mbox_ps_area[MAX_NUM_SPEs];
spe_sig_notify_1_area_t* sig_notify_ps_area[MAX_NUM_SPEs];
spe_mssync_area_t* mssync_ps_area[MAX_NUM_SPEs];

unsigned int phys_map[MAX_NUM_SPEs];

double model_estimate[NUM_FNs];
unsigned long long offload_count[NUM_FNs];

inline int max(const int a, const int b)
{
	return (a > b) ? a : b;
}

/* Sending mail to an SPE. Parameters:
 * id - id of the targeting SPE,
 * data - 32 bit data that is sent. */
void send_mail(spe_context_ptr_t speid, unsigned int data)
{ 
	void *data_addr = (void*) &data;
	spe_in_mbox_write(speid, data_addr, 1, SPE_MBOX_ANY_NONBLOCKING);
}

int linear_model(const int n_bytes)
{
	assert(n_bytes > 0);

	int cycles = 0;

	if (n_bytes <= 2048) {
		cycles = 349.70 + 0.13 * n_bytes;
	}
	else if (n_bytes <= 4096) {
		cycles = 472.76 + 0.16 * n_bytes;
	}
	else {
		cycles = 306.45 + 0.21 * n_bytes;
	}

	return cycles;
}

int transfer_cycles(const int n_bytes)
{
	return 128 + linear_model(n_bytes);
}

int computation_cycles(const int n, const int iteration_cycles)
{
	return (n * iteration_cycles) / spe_threads;
}

int estimate_cycles(const int n, const int iteration_cycles, const size_t elem_sz, int loop)
{
	const int cycles = max(transfer_cycles(n* elem_sz), computation_cycles(n, iteration_cycles));

	model_estimate[loop-1] = (model_estimate[loop-1] * offload_count[loop-1] + cycles) / (offload_count[loop-1] + 1);
	++offload_count[loop-1];
	//printf("%d\n", max(transfer_cycles(n * elem_sz), computation_cycles(n, iteration_cycles)));
	return cycles;
}

void *pthread_function(void *arg)
{
	pthread_data_t *datap = (pthread_data_t *)arg;
	int rc;
	unsigned int entry = SPE_DEFAULT_ENTRY;
	if ((rc = spe_context_run(datap->speid, &entry, 0, NULL, NULL, NULL)) < 0) {
		fprintf (stderr, "Error: spe_context_runi() (rc=%d, errno=%d, strerror=%s)\n", rc, errno, strerror(errno));
		exit (1);
	}
	pthread_exit(NULL);
}

void spe_create_threads()
{
	unsigned int i, rval;
	int rc;

	/* If the number SPE trheads is larger than 
	 * the number of SPEs */
	if (spe_threads > num_physical_spes) {
		printf("Error: not enough SPEs %d %d\n", spe_threads, num_physical_spes);
		exit(0);
	}

	/* Forking SPE threads */
	for (i = 0; i < spe_threads; i++) {
		if ((ptdata[i].speid = spe_context_create (SPE_MAP_PS | SPE_CFG_SIGNOTIFY1_OR, NULL)) == NULL) {
			fprintf(stderr, "Error: spe_context_create (errno=%d strerror=%s)\n", errno, strerror(errno));
			exit(1);
		}

		if ((rc = spe_program_load (ptdata[i].speid, &PROGRAM_NAME_spe)) != 0) {
			fprintf (stderr, "Error: spe_program_load() (errno=%d strerror=%s)\n", errno, strerror(errno));
			exit(1);
		}

		if ((rc = pthread_create(&(ptdata[i].pthread), NULL, &pthread_function, &ptdata[i])) != 0) {
			fprintf (stderr, "Error: pthread_create() (errno=%d strerror=%s)\n", errno, strerror(errno));
			exit(1);
		}

		/* Get the direct program store addresses */
		mfc_ps_area[i] = spe_ps_area_get(ptdata[i].speid, SPE_MFC_COMMAND_AREA);
		if (mfc_ps_area[i] == NULL) {
			fprintf (stderr, "Error: spe_ps_area_get(SPE_MFC_COMMAND_AREA) (errno=%d strerror=%s)\n", errno, strerror(errno));
			exit(1);
		}

		mbox_ps_area[i] = spe_ps_area_get(ptdata[i].speid, SPE_CONTROL_AREA);
		if (mbox_ps_area[i] == NULL) {
			fprintf (stderr, "Error: spe_ps_area_get(SPE_CONTROL_AREA) (errno=%d strerror=%s)\n", errno, strerror(errno));
			exit(1);
		}

		sig_notify_ps_area[i] = spe_ps_area_get(ptdata[i].speid, SPE_SIG_NOTIFY_1_AREA);
		if (sig_notify_ps_area[i] == NULL) {
			fprintf (stderr, "Error: spe_ps_area_get(SPE_SIG_NOTIFY_1_AREA)  (errno=%d strerror=%s)\n", errno, strerror(errno));
			exit(1);
		}

		mssync_ps_area[i] = spe_ps_area_get(ptdata[i].speid, SPE_MSSYNC_AREA);
		if (mssync_ps_area[i] == NULL) {
			fprintf (stderr, "Error: spe_ps_area_get(SPE_MSSYNC_AREA) (errno=%d strerror=%s)\n", errno, strerror(errno));
			exit(1);
		}

		send_mail(ptdata[i].speid, spe_threads);
		send_mail(ptdata[i].speid, i);

		/* Getting the LS addresses of all SPE threads */
		ls_addr[i] = (unsigned long) spe_ls_area_get(ptdata[i].speid);

		/* Getting the addresses of the communication parameters of the SPE threads */
		while (spe_out_mbox_status(ptdata[i].speid) == 0);
		spe_out_mbox_read(ptdata[i].speid, &rval, 1);
		pass[i] = ls_addr[i] + rval;

		while (spe_out_mbox_status(ptdata[i].speid) == 0);
		spe_out_mbox_read(ptdata[i].speid, &rval, 1);
		sig[i] = ls_addr[i] + rval;

		/* Construct logical-physical SPE mapping. */
		char filename[256];
		sprintf(filename, "/spu/spethread-%d-%lu/phys-id", getpid(), (unsigned long)ptdata[i].speid);
		int fd = open(filename, O_RDONLY, 0);
		if (fd < 0) {
			perror("open /spu/spethread\n");
		}
		else {
			int res = read(fd, filename, 255);
			if (res < 0) {
				perror("read /spu/spethread\n");
			}
			else if (res > 0 && res < 256) {
				filename[res - 1] = '\0';
				unsigned int phys = strtol(filename, NULL, 0);
				phys_map[i] = phys;
			}
			else {
				fprintf(stderr, "EOF in read /spu/spethread\n");
			}
			close(fd);
		}
        }
}

/* Signal an SPE to start performing work, parameters:
 * 1. num - the SPE number (the work can be distributed 
 *          across multiple SPEs) ,
 * 2. value - values sent to an SPE in order to specify
 *            which function should be executed (multiple
 *            SPE functions can reside in the same SPE
 *            module) */
inline void spe_start(unsigned int num, int value)
{
	/* Send starting signal to an SPE,
	* before that set signal.stop to 0 */
	((struct signal_t *)sig[num])->stop = 0;
	_sync;
	((struct signal_t *)sig[num])->start = value; 
}

inline void spe_offloads(void)
{
	profile_start_fn();
}


/* The same as _wait_SPET(), just without the 
 * timing instructions */
inline void wait_for_spes(int fn_id)
{
	unsigned int i = 0;
    
	sched_yield();
	for (i = 0; i < spe_threads; i++) {
		while (((struct signal_t *)sig[i])->stop == 0) {
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
	unsigned long long T_total[MAX_NUM_SPEs][NUM_FNs];
	unsigned long long T_L_spe[spe_threads];
	unsigned long long T_DMA_spe[spe_threads];
	unsigned long long T_DMA_prep_spe[spe_threads];
	unsigned long long T_total_spe[spe_threads];
	unsigned long long T_idle_spe[spe_threads];
	unsigned long long T_L_all, T_DMA_all, T_total_all;

	/*
	printf("\n\nTotal Time: %f (sec)\n\n", (double)(time_cellgen_end-time_cellgen_start) / TB);
	printf("\n========== PPE stats ==========\n\n");
	*/

	for (i=0; i<NUM_FNs; i++) {
		//printf("fn%u call count: %u\n", i+1, cnt_loop[i]);
		loop_cnt_all += cnt_loop[i];
	}

	//printf("fn count total = %u\n\n", loop_cnt_all);

	for (i=0; i<NUM_FNs; i++) {
		//printf("L%u: %.6f (sec)\n", i+1, ((double)time_loop[i])/TB);
		loop_time_all += time_loop[i];
	}

	/*
	printf("time spent on PPU workload between offloaded loops: %.6f (sec)\n",((double)time_ppu_between_loops)/TB);
	printf("time spend on prolog and epilog: %.6f (sec)\n", ((double)(time_cellgen_end-time_cellgen_start)-(loop_time_all+time_ppu_between_loops))/TB );

	printf("\n========== SPE stats ==========\n");
	*/

	for (i = 0; i < spe_threads; i++) {
		spe_start(i, GET_TIMES);
	}

	wait_for_spes(GET_TIMES);

	for (i = 0; i < spe_threads; i++) { 
		T_L_spe[i] =  ((struct signal_t *)sig[i])->all_fn;
		T_total_spe[i] =  ((struct signal_t *)sig[i])->all_total;
		T_DMA_spe[i] =  ((struct signal_t *)sig[i])->all_dma;
		T_DMA_prep_spe[i] =  ((struct signal_t *)sig[i])->all_dma_prep;
		T_idle_spe[i] =  ((struct signal_t *)sig[i])->idle_time;
	}

	for (loop = 0; loop < NUM_FNs; loop++) {
		T_L_all = T_DMA_all = T_total_all = 0;

		//printf("loop%u model %f\n", loop + 1, model_estimate[loop]);
		//printf("\nL%d :          Loop      DMA     Comp\n", loop+1);
		for (i = 0; i < spe_threads; i++) {
			T_L_all += (T_L[i][loop] = ((struct signal_t *)sig[i])->T_fn[loop]);
			T_DMA_all += (T_DMA[i][loop] = ((struct signal_t *)sig[i])->T_DMA[loop]);
			T_total_all += (T_total[i][loop] = ((struct signal_t *)sig[i])->T_total[loop]);
			//printf("%d %f\n", i, (((double)T_L[i][loop] / TB) * 3.2e9) / offload_count[loop]);
			//printf("     SPE%u %8.6f %8.6f %8.6f\n",i+1, (double)T_L[i][loop] /TB, (double)T_DMA[i][loop] /TB, (double)T_total[i][loop] /TB);
		}
		//printf("\n");
		printf("%e ", (double)(T_total_all)/ TB / spe_threads / cnt_loop[loop]);
		printf("%e\n", (double)(T_DMA_all)/ TB / spe_threads / cnt_loop[loop]);
		//printf("avg L%u computation time: %8.6f (sec)\n", loop+1, (double)(T_total_all)/ TB / spe_threads);
	}

	//printf("\n\n           --- Summary ---            \n");
	#ifndef DMA_PREP_REPORT
	//printf("SPE         fn fn_kernel   DMA_all      idle\n");
	for (i = 0; i < spe_threads; i++) {
		//printf("SPE%u  %7.6f  %7.6f  %7.6f  %7.6f\n", i+1, (double)T_L_spe[i] /TB, (double)T_total_spe[i] /TB, (double)T_DMA_spe[i] /TB, (double)T_idle_spe[i] / TB);
	}
	#else
	//printf("SPE         fn fn_kernel   DMA_all      DMA_prep  idle\n");
	for (i = 0; i < spe_threads; i++) {
	//printf("SPE%u  %7.6f  %7.6f  %7.6f  %7.6f  %7.6f\n", i+1, (double)T_L_spe[i] /TB, (double)T_total_spe[i] /TB, (double)T_DMA_spe[i] /TB, (double)T_DMA_prep_spe[i] /TB, (double)T_idle_spe[i] / TB);
	}
	#endif // DMA_PREP_REPORT
	/*
	printf("\n\n");
	printf("========== Time legend ==========\n");
	printf("Loop: total time spent on loops\n");
	printf("DMA:  non-overlapped DMA time\n");
	printf("Comp: time spent on the computing results\n");
	printf("idle: idle SPE time due to signaling mechanism and ppu workload while switching between different loops due to signaling and ppu workload\n");
	printf("\n");
	*/
	#endif // PROFILING

	for (i = 0; i < spe_threads; i++) {
		spe_start(i, TERMINATE);
	}
}

void spe_init(unsigned int num_threads)
{
	/*Determine the total number of SPEs*/
	num_physical_spes = spe_cpu_info_get(SPE_COUNT_PHYSICAL_SPES, -1);
	if (!num_threads) {
		spe_threads = num_physical_spes;
	}
	else {
		spe_threads = num_threads;
	}

	#ifdef PROFILING
	unsigned int i;
	for (i = 0; i < NUM_FNs; i++) {
		time_loop[i] = 0UL;
		cnt_loop[i] = 0UL;
		model_estimate[i] = 1.0;
		offload_count[i] = 0UL;
	}
	loop_time = 0UL;
	loop_started = 0;
	time_ppu_start = 0UL;
	time_ppu_between_loops = 0UL;
	#endif
}

__attribute__((constructor)) void __initialize()
{
	spe_init(NUM_THREADS_HOOK);
	spe_create_threads();
}

__attribute__((destructor)) void __finalize()
{
	cellgen_finish();	
}

