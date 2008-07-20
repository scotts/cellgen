#include "cellgen_timer.h"

#define SPU_EVENT_ID(_mask)	(spu_extract(spu_cntlz(spu_promote(_mask, 0)), 0))

/* spu_default_slih
 * ----------------
 * This function is called whenever an event occurs for which 
 * no second level event handler was registered. The default 
 * event handler does nothing and zeros the most significant 
 * event bit indicating that the event was processed (when 
 * in reality, it was discarded..
 */
static unsigned int spu_default_slih(unsigned int events)
{
  unsigned int mse;	

  mse = 0x80000000 >> SPU_EVENT_ID(events);
  events &= ~mse;

  return (events);
}


/* spu_slih_handlers[]
 * ------------------
 * Here we initialize 33 default event handlers.  The first entry in this array
 * corresponds to the event handler for the event associated with bit 0 of
 * Channel 0 (External Event Status).  The 32nd entry in this array corresponds
 * to bit 31 of Channel 0 (DMA Tag Status Update Event).  The 33rd entry in
 * this array is a special case entry to handle "phantom events" which occur
 * when the channel count for Channel 0 is 1, causing an asynchronous SPU
 * interrupt, but the value returned for a read of Channel 0 is 0.  The index 
 * calculated into this array by spu_flih() for this case is 32, hence the 
 * 33rd entry.
 */
spu_slih_func  spu_slih_handlers[33] __attribute__ ((aligned (16))) = {
  spu_default_slih,  spu_default_slih, spu_default_slih,  spu_default_slih,
  spu_default_slih,  spu_default_slih, spu_default_slih,  spu_default_slih,
  spu_default_slih,  spu_default_slih, spu_default_slih,  spu_default_slih,
  spu_default_slih,  spu_default_slih, spu_default_slih,  spu_default_slih,
  spu_default_slih,  spu_default_slih, spu_default_slih,  spu_default_slih,
  spu_default_slih,  spu_default_slih, spu_default_slih,  spu_default_slih,
  spu_default_slih,  spu_default_slih, spu_default_slih,  spu_default_slih,
  spu_default_slih,  spu_default_slih, spu_default_slih,  spu_default_slih,
  spu_default_slih,
};

#ifdef WORKAROUND_TOBEY_DEFECT_2994
/* Initialize this temporary global variable to indicate that all members of
 * the spu_slih_handlers[] array have NOT YET been initialized.
 * As of 031215, the TOBEY compiler does not appear to be doing it properly
 * with the global array initialization above.
 */
int spu_slih_handlers_init_done = 0;
#endif /* WORKAROUND_TOBEY_DEFECT_2994 */

/* spu_slih_reg
 * ------------
 * Registers a SPU second level interrupt handler for the events specified by
 * mask. The event mask consists of a set of bits corresponding to the 
 * event status bits (see channel 0 description). A mask containing multiple
 * 1 bits will set the second level event handler for each of the events.
 */
void spu_slih_reg(unsigned int mask, spu_slih_func func)
{
  unsigned int id;

#ifdef WORKAROUND_TOBEY_DEFECT_2994
/* Individually initialize all members of the spu_slih_handlers[] array
 * since the TOBEY compiler does not appear to be doing it properly
 * with the global array initialization above.
 */
  if (!spu_slih_handlers_init_done)
  {
    int i;
    for (i=0; i < 33; i++)
    {
	spu_slih_handlers[i] = spu_default_slih;
    }
    spu_slih_handlers_init_done = 1;
  }
#endif /* WORKAROUND_TOBEY_DEFECT_2994 */

  while (mask) {
    id = SPU_EVENT_ID(mask);
    spu_slih_handlers[id] = func;
    mask &= ~(0x80000000 >> id);
  }
}

unsigned long long add_tim;

unsigned long long GET_TIME(void)
{
  return (add_tim + ((unsigned long long)DECR_COUNT - spu_readch(SPU_RdDec)));
}

unsigned int my_decr_handler(unsigned int status)
{
  status &= ~MFC_DECREMENTER_EVENT;

  /* Reset counter.
   */
  add_tim+=DECR_COUNT;
  spu_writech(SPU_WrDec, DECR_COUNT);
  return (status);
}

unsigned int decr_handler(unsigned int status)
{
  status &= ~MFC_DECREMENTER_EVENT;

  /* Reset counter.
   */
  add_tim+=DECR_COUNT;
  spu_writech(SPU_WrDec, DECR_COUNT);
  return (status);
}

void cellgen_timer_init()
{
  add_tim = 0;
  spu_writech(SPU_WrDec, DECR_COUNT);
  //spu_slih_reg(MFC_DECREMENTER_EVENT, my_decr_handler);
}

