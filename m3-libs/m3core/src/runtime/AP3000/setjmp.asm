* Copyright (C) 1992, Ray Lischner
* All rights reserved.
*
* Last modified on Tue Feb 11 15:03:42 PST 1992 by muller  
*      modified on Mon Feb 10 17:57:47 PST 1992 by lischner
*
* apollo_set_regs.asm - define apollo_set_regs to make _longjmp() work
*
* void apollo_set_regs(int* a6, int* a7)
* Apollo's longjmp() checks to see if the jump is backwards in the stack.
* If not, it assumes that something is wrong and ungracefully terminates
* the program.  Since we don't want this to happen, we need to fake
* out Domain/OS.  This is done by setting the stack pointer (a7) and
* frame pointer (a6) to the destination frame, thus circumventing
* longjmp's checks.  Note that _longjmp() does the same check as longjmp().
*
* To accomplish this takes some clever tricks.  First, we need to know
* how the stack is layed out:
*
* (lower addresses)
*       +----------------------------+
*    A7 |  local storage ...         |
*       +----------------------------+
*    A6 | link to previous frame     |
*       +----------------------------+
*       | return address             |
*       +----------------------------+
*       | arguments pushed by caller |
*       +----------------------------+
* (higher addresses)
* Note that we are ignoring floating point control blocks.
*
* The caller pushes the desired values for A7 and A6.  On entry,
* A6 points to the caller's frame, and A7 points to the return address.
* We can retrieve the caller's arguments by dereferencing a7: the second
* argument is in 8(a7), and the first is in 4(a7).  We can just copy
* them into the registers we want, but first we need to save the return
* address before we lose the pointer to it; since JMP A0 is not allowed,
* we save a pointer to the return address in A0, and JMP (A0).  After getting
* the new register values, we know that the caller will try to pop the
* arguments off the stack by adding 8 to A7.  We circumvent this by
* subtracting 8 now.

	module	apollo_set_regs
	abs

.text	dfsect	readonly,instruction
	sect	.text

	entry	apollo_set_regs

apollo_set_regs	equ *
	movem.l	(a7),a0/a6-a7
	subq.l	#8,a7
	jmp	(a0)
