/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Thu May  4 09:20:01 PDT 1995 by kalsow     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* <*EXTERNAL "RTStack__CurFrame" *> */
/* PROCEDURE CurrentFrame (): Frame; */
/* Return the frame of its caller.  Returns with pc = NIL on failure. */

/* Returns the pc and sp of the caller */
	.text	
	.align	2
	.globl	RTStack__CurFrame
	.ent	RTStack__CurFrame
RTStack__CurFrame:
	.frame	$sp, 0, $31
	sw	$31, 0($4)
	sw	$sp, 4($4)
	move	$2, $4
	j	$31
	.end	RTStack__CurFrame
