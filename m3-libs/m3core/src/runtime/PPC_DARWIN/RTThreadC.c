/* Copyright according to COPYRIGHT-CMASS. */

/* This file implements the coroutine transfer: RTThread.Transfer */

#include <setjmp.h>


RTThread__Transfer (from, to)
jmp_buf *from, *to;
{
  if (setjmp(*from) == 0) longjmp (*to, 1);
}


/* global thread ID used by 'etp' */
int ThreadF__myId = 1;

/* low-level runtime lock */
/* int ThreadF__inCritical = 0; MOVED TO RTHeapDepC.c */

/* global, per-thread linked list of exception handlers */
void* ThreadF__handlerStack = 0;

