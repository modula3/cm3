/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Mon Nov 21 10:24:49 PST 1994 by kalsow     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the coroutine transfer: RTThread.Transfer */

#include <setjmp.h>

RTThread__Transfer (from, to)
jmp_buf *from, *to;
{

/************ BEAR TRAP ****************/
/********
{
extern etext;
static jmp_buf last_thread_jmp;
memcpy (last_thread_jmp, to, sizeof (jmp_buf));
if (from != to) {
if (((*to)[JB_S8] <= (int)(&etext)) || ((*to)[JB_PC] == 0) || ((*to)[JB_SP] == 0)) abort ();
}
}
*********/
/***************************************/

  (*from)[JB_ONSIGSTK] = 0; /* Ultrix setjmp doesn't set this field! */
  if (_setjmp(*from) == 0) _longjmp (*to, 1);
}


/* global thread ID used by 'etp' */
int ThreadF__myId = 1;

/* low-level runtime lock */
int RT0u__inCritical = 0;

/* global, per-thread linked list of exception handlers */
void* RTThread__handlerStack = 0;

