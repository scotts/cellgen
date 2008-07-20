/* --------------------------------------------------------------  */
/* (C)Copyright 2001,2007,                                         */
/* International Business Machines Corporation,                    */
/* Sony Computer Entertainment, Incorporated,                      */
/* Toshiba Corporation,                                            */
/*                                                                 */
/* All Rights Reserved.                                            */
/*                                                                 */
/* Redistribution and use in source and binary forms, with or      */
/* without modification, are permitted provided that the           */
/* following conditions are met:                                   */
/*                                                                 */
/* - Redistributions of source code must retain the above copyright*/
/*   notice, this list of conditions and the following disclaimer. */
/*                                                                 */
/* - Redistributions in binary form must reproduce the above       */
/*   copyright notice, this list of conditions and the following   */
/*   disclaimer in the documentation and/or other materials        */
/*   provided with the distribution.                               */
/*                                                                 */
/* - Neither the name of IBM Corporation nor the names of its      */
/*   contributors may be used to endorse or promote products       */
/*   derived from this software without specific prior written     */
/*   permission.                                                   */
/*                                                                 */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND          */
/* CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,     */
/* INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF        */
/* MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE        */
/* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR            */
/* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,    */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT    */
/* NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;    */
/* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)        */
/* HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN       */
/* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR    */
/* OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,  */
/* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.              */
/* --------------------------------------------------------------  */
/* PROLOG END TAG zYx                                              */
//#
//# spu_flih.s (SPU First Level Interrupt Handler)
//# ----------------------------------------------
//# This file contains an example SPU first level interrupt handler. This
//# handler is meant to be used in conjunction with the second level
//# interrupt handlers registered using spu_register_slih function.
//#
//# This FLIH is designed to support Second Level Interrupt Handlers
//# that are written in C and following the SPU's ABI calling interface.
//# As such, it saves all the volatile registers before envoking the
//# SLIH. Because the SPU has so many volatile registers, this function
//# is not recommended for frequent, realtime interrupt service processing.
//# For high performance interrupt handling, it is recommended that:	
//# 1) the interrupt handler be written entirely in assembly and only the
//#    registers referenced are save and restored,
//# 2) utilize interprocedural analysis to reduce the number of registers
//#    save and restored, 
//# 3) use compiler features/pragma to specify SLIH functions with
//#    non-standard calling conventions (for example, declare all register
//#    as non-volatile) and remove the save and restores from the FLIH.
//#
//# The first level interrupt handler is expected to be linked and located
//# starting at SPU local space address 0. 
//#
//# This function can be assembled to use either the standard runtime
//# stack or a private interrupt stack for temporary state variables.
//# Setting INTERRUPT_STACK_SIZE to a non-zero value will result in 
//# code the maintains a separate stack for the interrupt handler.
//# The size of the separate interrupt stack is defined to be
//# its value in bytes.
	
.set		INTERRUPT_STACK_SIZE, 0
	
	.extern		spu_slih_handlers

	
//# The following two convenience macros assist in the coding of the
//# saving and restoring the volatile register starting from register
//# 2 up to register 79.
//# 	
//# saveregs     first, last	Saves registers from first to the last.
//# restoreregs  first, last	Restores registers from last down to first.
//#
//# Note:	first must be less than or equal to last.
//#		
.macro	saveregs	first, last
	stqd		$\first, -(STACK_SKIP+\first)*16($SP)
.if	\last-\first
	saveregs	"(\first+1)",\last
.endif	
.endm		
	

.macro	restoreregs	first, last
	lqd		$\last, (82-\last)*16($SP)
.if	\last-\first
	restoreregs	\first,"(\last-1)"
.endif	
.endm		
	
	.section	.interrupt,"ax"
	.align		3
	
#ifdef SPU_BISLED_H
//# This assembly file is also used to generate an event handler for SPU
//# bisled events when the SPU_BISLED_H define is set at compile time.
	.global		spu_bisled_h
spu_bisled_h:
	//# Save the current link register on the callers stack frame like 
	//# most other non-leaf functions.
	stqd		$0,  16($SP)
#else // normal use for spu_flih
	.global		spu_flih
spu_flih:
#endif // SPU_BISLED_H
	
.if INTERRUPT_STACK_SIZE
	//# Establish a private stack used only by the interrupt handler.
	//# This interrupt handler will not support nested interrupts.
	
	.set		STACK_SKIP, 0
	stqr		$SP, main_stack_ptr
	ila		$SP, interrupt_stack-16
.else
	//# Adjust the stack pointer to skip the maximum register save area
	//# (STACK_SKIP quadword registers) in case an interrupt occurred while
	//# executing a leaf function that used the stack area without actually 
	//# allocating its own stack frame.
	
	.set		STACK_SKIP, 125
.endif
	
#ifndef SPU_BISLED_H
	//# Save the current link register on a new stack frame for the
	//# normal spu_flih() version of this file.
	stqd		$0,  -(STACK_SKIP+80)*16($SP)
#endif // not SPU_BISLED_H
	stqd		$SP, -(STACK_SKIP+82)*16($SP)	//# save back chain pointer
	
	saveregs	2, 39
	
	il		$2,  -(STACK_SKIP+82)*16	//# stack frame size
	rdch		$3, $SPU_RdEventStat		//# read event status
	
	rdch		$6, $SPU_RdEventMask		//# read event mask
	hbrp						//# open a slot for instruction prefetch		

	saveregs	40,59
	
	clz		$4, $3				//# get first slih index
	stqd		$6,  -(STACK_SKIP+1)*16($SP)	//# save event mask on stack

	saveregs	60, 67

	andc		$7, $6, $3			//# clear event bits
	saveregs	68, 69
		
	wrch		$SPU_WrEventMask, $7		//# disable event(s)
	wrch		$SPU_WrEventAck, $3		//# acknowledge events(s)
	
	saveregs	70, 79

	a		$SP, $SP, $2			//# instantiate flih stack frame 
next_event:	
	//# Fetch and dispatch the event handler for the first non-zero event. The
	//# dispatch handler is indexed into the spu_slih_handlers array using the
	//# count of zero off the event status as an index.
	
	ila		$5, spu_slih_handlers		//# slih array offset
	
	shli		$4, $4, 2			//# slih entry offset
	lqx		$5, $4, $5			//# load slih address
	rotqby		$5, $5, $4			//# rotate to word 0
	bisl		$0, $5				//# branch to slih
	
	clz		$4, $3				//# get next slih index
	brnz		$3, next_event
	
	
	lqd		$2, 81*16($SP)			//# read event mask from stack
	
	restoreregs	40, 79

	wrch		$SPU_WrEventMask, $2		//# restore event mask
	hbrp						//# open a slot for instruction pre-fetch

	restoreregs	2, 39

#ifndef SPU_BISLED_H
	//# Restore the link register from the new stack frame for the
	//# normal spu_flih() version of this file.
	lqd		$0,  2*16($SP)
#endif // not SPU_BISLED_H

.if	INTERRUPT_STACK_SIZE	
	lqr		$SP, main_stack_ptr
.else	
	lqd		$SP, 0*16($SP)	//# restore stack pointer from back chain ptr
.endif	
	
#ifdef SPU_BISLED_H
	//# Restore link register from callers stack
	lqd		$0,   1*16($SP)
	//# Return from SPU bisled event handler using link register
	bi		$0
#else // normal use for spu_flih
	//# Return from interrupt and re-enable interrupts
	irete
#endif // SPU_BISLED_H
	
.if	INTERRUPT_STACK_SIZE
	//# Private interrupt stack
	.skip		INTERRUPT_STACK_SIZE
        .align          4
  interrupt_stack:			//# top of interrupt stack
	
	//# Place to save main runtime stack pointer
  main_stack_ptr:
	.skip		16
.endif
