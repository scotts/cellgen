#ifndef my_SPU_SLIH_REG_H_
#define my_SPU_SLIH_REG_H_	1

#include <spu_intrinsics.h>
#include <spu_mfcio.h>

#define DECR_COUNT	0x7fffffff

typedef 	unsigned int (*spu_slih_func)(unsigned int);

extern		spu_slih_func spu_slih_handlers[];
extern 		void spu_slih_reg(unsigned int, spu_slih_func);
extern		void spu_flih(void);


unsigned int my_decr_handler(unsigned int status);
unsigned int decr_handler(unsigned int status);

unsigned long long GET_TIME(void);

void cellgen_timer_init();

#endif /* _SPU_SLIH_REG_H_ */
