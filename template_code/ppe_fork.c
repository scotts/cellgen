MMGP_offload();
int __iLOOP_ID;
for (__iLOOP_ID = 0; __iLOOP_ID < __SPE_threads; ++__iLOOP_ID) {
	PASS_ASSIGNMENT	
	MMGP_start_SPE(__iLOOP_ID, LOOP_ID);
}

