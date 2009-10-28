spe_offloads();
unsigned int __iLOOP_ID;
for (__iLOOP_ID = 0; __iLOOP_ID < spe_threads; ++__iLOOP_ID) {
	PASS_ASSIGNMENT	
	spe_start(__iLOOP_ID, LOOP_ID);
}

