/* libgcc1 routines for 68000 w/o floating-point hardware,
   calling the Sun3 floating point support routines.  */
/* Copyright (C) 1992 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifdef  L_mulsi3
	.text
	.proc
	.globl	SYM (__mulsi3)
SYM (__mulsi3):
	movew	sp@(4), d0	/* x0 -> d0 */
	muluw	sp@(10), d0	/* x0*y1 */
	movew	sp@(6), d1	/* x1 -> d1 */
	muluw	sp@(8), d1	/* x1*y0 */
	addw	d1, d0
	swap	d0
	clrw	d0
	movew	sp@(6), d1	/* x1 -> d1 */
	muluw	sp@(10), d1	/* x1*y1 */
	addl	d1, d0

	rts
#endif /* L_mulsi3 */

#ifdef  L_udivsi3
	.text
	.proc
	.globl	SYM (__udivsi3)
SYM (__udivsi3):
	movel	d2, sp@-
	movel	sp@(12), d1	/* d1 = divisor */
	movel	sp@(8), d0	/* d0 = dividend */

	cmpl	#0x10000, d1	/* divisor >= 2 ^ 16 ?   */
	jcc	L3		/* then try next algorithm */
	movel	d0, d2
	clrw	d2
	swap	d2
	divu	d1, d2          /* high quotient in lower word */
	movew	d2, d0		/* save high quotient */
	swap	d0
	movew	sp@(10), d2	/* get low dividend + high rest */
	divu	d1, d2		/* low quotient */
	movew	d2, d0
	jra	L6

L3:	movel	d1, d2		/* use d2 as divisor backup */
L4:	lsrl	#1, d1		/* shift divisor */
	lsrl	#1, d0		/* shift dividend */
	cmpl	#0x10000, d1	/* still divisor >= 2 ^ 16 ?  */
	jcc	L4
	divu	d1, d0		/* now we have 16 bit divisor */
	andl	#0xffff, d0	/* mask out divisor, ignore remainder */

/* Muliply the 16 bit tentative quotient with the 32 bit divisor.  Because of
   the operand ranges, this might give a 33 bit product.  If this product is
   greater than the dividend, the tentative quotient was too large. */
	movel	d2, d1
	mulu	d0, d1		/* low part, 32 bits */
	swap	d2
	mulu	d0, d2		/* high part, at most 17 bits */
	swap	d2		/* align high part with low part */
	btst	#0, d2		/* high part 17 bits? */
	jne	L5		/* if 17 bits, quotient was too large */
	addl	d2, d1		/* add parts */
	jcs	L5		/* if sum is 33 bits, quotient was too large */
	cmpl	sp@(8), d1	/* compare the sum with the dividend */
	jls	L6		/* if sum > dividend, quotient was too large */
L5:	subql	#1, d0		/* adjust quotient */

L6:	movel	sp@+, d2
	rts
#endif /* L_udivsi3 */

#ifdef  L_divsi3
	.text
	.proc
	.globl	SYM (__divsi3)
SYM (__divsi3):
	movel	d2, sp@-

	moveb	#1, d2		/* sign of result stored in d2 (=1 or =-1) */
	movel	sp@(12), d1	/* d1 = divisor */
	jpl	L1
	negl	d1
	negb	d2		/* change sign because divisor <0  */
L1:	movel	sp@(8), d0	/* d0 = dividend */
	jpl	L2
	negl	d0
	negb	d2

L2:	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__udivsi3)	/* divide abs(dividend) by abs(divisor) */
	addql	#8, sp

	tstb	d2
	jpl	L3
	negl	d0

L3:	movel	sp@+, d2
	rts
#endif /* L_divsi3 */

#ifdef  L_umodsi3
	.text
	.proc
	.globl	SYM (__umodsi3)
SYM (__umodsi3):
	movel	sp@(8), d1	/* d1 = divisor */
	movel	sp@(4), d0	/* d0 = dividend */
	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__udivsi3)
	addql	#8, sp
	movel	sp@(8), d1	/* d1 = divisor */
	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__mulsi3)	/* d0 = (a/b)*b */
	addql	#8, sp
	movel	sp@(4), d1	/* d1 = dividend */
	subl	d0, d1		/* d1 = a - (a/b)*b */
	movel	d1, d0
	rts
#endif /* L_umodsi3 */

#ifdef  L_modsi3
	.text
	.proc
	.globl	SYM (__modsi3)
SYM (__modsi3):
	movel	sp@(8), d1	/* d1 = divisor */
	movel	sp@(4), d0	/* d0 = dividend */
	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__divsi3)
	addql	#8, sp
	movel	sp@(8), d1	/* d1 = divisor */
	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__mulsi3)	/* d0 = (a/b)*b */
	addql	#8, sp
	movel	sp@(4), d1	/* d1 = dividend */
	subl	d0, d1		/* d1 = a - (a/b)*b */
	movel	d1, d0
	rts
#endif /* L_modsi3 */


#ifdef  L_divdf3
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 07
	LF17	=	8
	LS17	=	0
	LFF17	=	8
	LSS17	=	0
	LV17	=	8
	.text
	.globl	___divdf3
___divdf3:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	lea	a6@(16),a0
	jbsr	Fdivd
	movl	d0,a6@(-8)
	movl	d1,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_divdf3 */

#ifdef  L_muldf3
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 07
	LF17	=	8
	LS17	=	0
	LFF17	=	8
	LSS17	=	0
	LV17	=	8
	.text
	.globl	___muldf3
___muldf3:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	lea	a6@(16),a0
	jbsr	Fmuld
	movl	d0,a6@(-8)
	movl	d1,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_muldf3 */

#ifdef  L_negdf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 07
	LF17	=	8
	LS17	=	0
	LFF17	=	8
	LSS17	=	0
	LV17	=	8
	.text
	.globl	___negdf2
___negdf2:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),a6@(-4)
	bchg	#31,d0
	movl	d0,a6@(-8)
	movl	a6@(-4),d1
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_negdf2 */

#ifdef  L_adddf3
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 07
	LF17	=	8
	LS17	=	0
	LFF17	=	8
	LSS17	=	0
	LV17	=	8
	.text
	.globl	___adddf3
___adddf3:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	lea	a6@(16),a0
	jbsr	Faddd
	movl	d0,a6@(-8)
	movl	d1,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_adddf3 */

#ifdef  L_subdf3
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 07
	LF17	=	8
	LS17	=	0
	LFF17	=	8
	LSS17	=	0
	LV17	=	8
	.text
	.globl	___subdf3
___subdf3:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	lea	a6@(16),a0
	jbsr	Fsubd
	movl	d0,a6@(-8)
	movl	d1,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_subdf3 */

#ifdef  L_fixdfsi
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	4
	LS17	=	128
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___fixdfsi
___fixdfsi:
|#PROLOGUE# 0
	link	a6,#-4
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	jbsr	Fintd
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_fixdfsi */

#ifdef  L_fixsfsi
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	4
	LS17	=	128
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___fixsfsi
___fixsfsi:
|#PROLOGUE# 0
	link	a6,#-4
|#PROLOGUE# 1
	movl	a6@(8),d0
	jbsr	Fints
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_fixsfsi */

#ifdef  L_floatsidf
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 07
	LF17	=	8
	LS17	=	0
	LFF17	=	8
	LSS17	=	0
	LV17	=	8
	.text
	.globl	___floatsidf
___floatsidf:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	jbsr	Ffltd
	movl	d0,a6@(-8)
	movl	d1,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_floatsidf */

#ifdef  L_floatsisf
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	128
	LFF17	=	4
	LSS17	=	0
	LV17	=	4
	.text
	.globl	___floatsisf
___floatsisf:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	jbsr	Fflts
	movl	d0,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_floatsisf */

#ifdef  L_truncdfsf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	128
	LFF17	=	4
	LSS17	=	0
	LV17	=	4
	.text
	.globl	___truncdfsf2
___truncdfsf2:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	jbsr	Fdtos
	movl	d0,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_truncdfsf2 */

#ifdef  L_extendsfdf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 07
	LF17	=	8
	LS17	=	0
	LFF17	=	8
	LSS17	=	0
	LV17	=	8
	.text
	.globl	___extendsfdf2
___extendsfdf2:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	jbsr	Fstod
	movl	d0,a6@(-8)
	movl	d1,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_extendsfdf2 */

#ifdef  L_addsf3
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	128
	LFF17	=	4
	LSS17	=	0
	LV17	=	4
	.text
	.globl	___addsf3
___addsf3:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	jbsr	Fadds
	movl	d0,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_addsf3 */

#ifdef  L_negsf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	128
	LFF17	=	4
	LSS17	=	0
	LV17	=	4
	.text
	.globl	___negsf2
___negsf2:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	bchg	#31,d0
	movl	d0,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_negsf2 */

#ifdef  L_subsf3
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	128
	LFF17	=	4
	LSS17	=	0
	LV17	=	4
	.text
	.globl	___subsf3
___subsf3:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	jbsr	Fsubs
	movl	d0,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_subsf3 */

#ifdef  L_mulsf3
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	128
	LFF17	=	4
	LSS17	=	0
	LV17	=	4
	.text
	.globl	___mulsf3
___mulsf3:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	jbsr	Fmuls
	movl	d0,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_mulsf3 */

#ifdef  L_divsf3
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	128
	LFF17	=	4
	LSS17	=	0
	LV17	=	4
	.text
	.globl	___divsf3
___divsf3:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	jbsr	Fdivs
	movl	d0,a6@(-4)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_divsf3 */

#ifdef  L_eqdf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	4
	LS17	=	128
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___eqdf2
___eqdf2:
|#PROLOGUE# 0
	link	a6,#-4
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	lea	a6@(16),a0
	jbsr	Fcmpd
	jfeq	L77003
	moveq	#1,d0
	jra	L77005
L77003:
	moveq	#0,d0
L77005:
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_eqdf2 */

#ifdef  L_nedf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___nedf2
___nedf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d7,sp@
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	moveq	#0,d7
	lea	a6@(16),a0
	jbsr	Fcmpd
	sfneq	d7
	negb	d7
	movl	d7,d0
|#PROLOGUE# 2
	movl	a6@(-8),d7
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_nedf2 */

#ifdef  L_gtdf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___gtdf2
___gtdf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d7,sp@
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	moveq	#0,d7
	lea	a6@(16),a0
	jbsr	Fcmpd
	sfgt	d7
	negb	d7
	movl	d7,d0
|#PROLOGUE# 2
	movl	a6@(-8),d7
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_gtdf2 */

#ifdef  L_gedf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___gedf2
___gedf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d7,sp@
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	moveq	#0,d7
	lea	a6@(16),a0
	jbsr	Fcmpd
	sfge	d7
	negb	d7
	subql	#1,d7
	movl	d7,d0
|#PROLOGUE# 2
	movl	a6@(-8),d7
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_gedf2 */

#ifdef  L_ltdf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___ltdf2
___ltdf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d7,sp@
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	moveq	#0,d7
	lea	a6@(16),a0
	jbsr	Fcmpd
	sflt	d7
	negb	d7
	negl	d7
	movl	d7,d0
|#PROLOGUE# 2
	movl	a6@(-8),d7
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_ltdf2 */

#ifdef  L_ledf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___ledf2
___ledf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d2,sp@
|#PROLOGUE# 1
	movl	a6@(8),d0
	movl	a6@(12),d1
	moveq	#0,d2
	lea	a6@(16),a0
	jbsr	Fcmpd
	sfle	d2
	negb	d2
	moveq	#1,d0
	subl	d2,d0
|#PROLOGUE# 2
	movl	a6@(-8),d2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_ledf2 */

#ifdef  L_eqsf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	4
	LS17	=	128
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___eqsf2
___eqsf2:
|#PROLOGUE# 0
	link	a6,#-4
|#PROLOGUE# 1
	movl	a6@(12),d1
	movl	a6@(8),d0
	jbsr	Fcmps
	jfeq	L77003
	moveq	#1,d0
	jra	L77005
L77003:
	moveq	#0,d0
L77005:
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_eqsf2 */

#ifdef  L_nesf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___nesf2
___nesf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d7,sp@
|#PROLOGUE# 1
	movl	a6@(12),d1
	movl	a6@(8),d0
	moveq	#0,d7
	jbsr	Fcmps
	sfneq	d7
	negb	d7
	movl	d7,d0
|#PROLOGUE# 2
	movl	a6@(-8),d7
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_nesf2 */

#ifdef  L_gtsf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___gtsf2
___gtsf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d7,sp@
|#PROLOGUE# 1
	movl	a6@(12),d1
	movl	a6@(8),d0
	moveq	#0,d7
	jbsr	Fcmps
	sfgt	d7
	negb	d7
	movl	d7,d0
|#PROLOGUE# 2
	movl	a6@(-8),d7
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_gtsf2 */

#ifdef  L_gesf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___gesf2
___gesf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d7,sp@
|#PROLOGUE# 1
	movl	a6@(12),d1
	movl	a6@(8),d0
	moveq	#0,d7
	jbsr	Fcmps
	sfge	d7
	negb	d7
	subql	#1,d7
	movl	d7,d0
|#PROLOGUE# 2
	movl	a6@(-8),d7
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_gesf2 */

#ifdef  L_ltsf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___ltsf2
___ltsf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d7,sp@
|#PROLOGUE# 1
	movl	a6@(12),d1
	movl	a6@(8),d0
	moveq	#0,d7
	jbsr	Fcmps
	sflt	d7
	negb	d7
	negl	d7
	movl	d7,d0
|#PROLOGUE# 2
	movl	a6@(-8),d7
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_ltsf2 */

#ifdef  L_lesf2
LL0:
	.data
	.data
	.text
	.proc
|#PROC# 04
	LF17	=	8
	LS17	=	132
	LFF17	=	0
	LSS17	=	0
	LV17	=	0
	.text
	.globl	___lesf2
___lesf2:
|#PROLOGUE# 0
	link	a6,#-8
	movl	d2,sp@
|#PROLOGUE# 1
	movl	a6@(12),d1
	movl	a6@(8),d0
	moveq	#0,d2
	jbsr	Fcmps
	sfle	d2
	negb	d2
	moveq	#1,d0
	subl	d2,d0
|#PROLOGUE# 2
	movl	a6@(-8),d2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_lesf2 */

