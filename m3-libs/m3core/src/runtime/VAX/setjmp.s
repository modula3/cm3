# Copyright (C) 1991, Digital Equipment Corporation
# All rights reserved.
# See the file COPYRIGHT for a full description.
#
# Last modified on Sat Jan 25 15:59:12 PST 1992 by muller

# Our jmp_buf is layed out as follows:
#
#	Name	       		Offset

.set    jmp_buf_ap,	 	  0
.set    jmp_buf_fp,		  4

.set    jmp_buf_frame_mask,    	  8
.set 	jmp_buf_frame_psw,	 12
.set    jmp_buf_frame_ap,        16
.set    jmp_buf_frame_fp,        20
.set    jmp_buf_frame_pc,        24

.set    jmp_buf_r1,              28
.set	jmp_buf_r2,	         32
.set	jmp_buf_r3,              36
.set 	jmp_buf_r4,              40
.set 	jmp_buf_r5,              44
.set 	jmp_buf_r6,              48
.set    jmp_buf_r7,              52
.set 	jmp_buf_r8,              56
.set	jmp_buf_r9,     	 60
.set 	jmp_buf_r10,             64
.set 	jmp_buf_r11,    	 68


# We assume that transfer is done by a call to WildSetjmp to save the
# context followed by a call to WildLongjmp to transfer to a new context.
# We also assume that the frame of WildLongjmp is in the same place 
# as the frame of WildSetjmp.

# When we switch to another context, we reuse the frame of the WildLongjmp 
# that got us out of that context, fill it with the values we collected
# at the time of the WildSetjmp of that context and return. We leave the 
# frame of the current WildLongjmp without change.

	.text
	.align 2
	.globl _WildSetjmp
_WildSetjmp:

# 4(ap): * jmp_buf
#	Fill jmp_buf with the necessary values to restore that stack frame

        .text
	.globl _WildLongjmp
	.word 0
	
	# get the jmp_buf
	movl	4(ap),	r0

	# save the current frame
	movl      (fp),	jmp_buf_frame_mask (r0)
	movl	 4(fp),	jmp_buf_frame_psw (r0)
	movl	 8(fp),	jmp_buf_frame_ap (r0)
	movl	12(fp),	jmp_buf_frame_fp (r0)
	movl	16(fp),	jmp_buf_frame_pc (r0)

	# save the registers
	movl	r1, 	jmp_buf_r1 (r0)
	movl	r2, 	jmp_buf_r2 (r0)
	movl	r3, 	jmp_buf_r3 (r0)
	movl	r4, 	jmp_buf_r4 (r0)
	movl	r5, 	jmp_buf_r5 (r0)
	movl	r6, 	jmp_buf_r6 (r0)
	movl	r7, 	jmp_buf_r7 (r0)
	movl	r8, 	jmp_buf_r8 (r0)
	movl	r9, 	jmp_buf_r9 (r0)
	movl	r10, 	jmp_buf_r10 (r0)
	movl	r11, 	jmp_buf_r11 (r0)

	# remember where the frame is supposed to be
	movl	fp,	jmp_buf_fp (r0)
        movl    ap,	jmp_buf_ap (r0)


	# done
	clrl	r0
	ret

	.text
	.align 2
	.globl _WildLongjmp
# the stack frame of this longjmp call will serve for the return 
_WildLongjmp:
# 4(ap): * jmpbuf
# 8(ap): value
	.word 0

	# get the jmp_buf
	movl	4(ap),	r0

	# restore the general registers
	movl	jmp_buf_r1 (r0),	r1
	movl	jmp_buf_r2 (r0),	r2
	movl	jmp_buf_r3 (r0),	r3
	movl	jmp_buf_r4 (r0),	r4
	movl	jmp_buf_r5 (r0),	r5
	movl	jmp_buf_r6 (r0),	r6
	movl	jmp_buf_r7 (r0),	r7
	movl	jmp_buf_r8 (r0),	r8
	movl	jmp_buf_r9 (r0),	r9
	movl	jmp_buf_r10 (r0),	r10
	movl	jmp_buf_r11 (r0),	r11

	# restore the frame of the setjmp of the context to which we go
	movl	jmp_buf_fp (r0),                 fp
	movl    jmp_buf_frame_mask (r0),	(fp)
	movl	jmp_buf_frame_psw (r0),	       4(fp)
	movl	jmp_buf_frame_ap (r0),	       8(fp)
	movl	jmp_buf_frame_fp (r0),        12(fp)
	movl	jmp_buf_frame_pc (r0),	      16(fp)

	# save the return value in sp
	movl    8(ap),			sp

        # restore the arguments 
	movl	jmp_buf_ap (r0),	  ap
	movl	$1,		 	 (ap)
	movl    r0,			4(ap)

	movl	sp,			r0
	movl	fp,			sp
	ret



