#include <spu_intrinsics.h>

#define DECR_COUNT	0x7fffffff

unsigned long long int  add_tim;
int cnt_tim;

unsigned int decr_handler(unsigned int status)
{
  status &= ~MFC_DECREMENTER_EVENT;

  cnt_tim++;
  /* Reset counter.
   */
  add_tim+=DECR_COUNT;
  spu_writech(SPU_WrDec, DECR_COUNT);
  return (status);
}

void spu_time_init()
{
  add_tim = 0;
  spu_writech(SPU_WrDec, DECR_COUNT);
}

unsigned long long spu_time_now()
{
	return add_tim + (DECR_COUNT - spu_readch(SPU_RdDec));
}
