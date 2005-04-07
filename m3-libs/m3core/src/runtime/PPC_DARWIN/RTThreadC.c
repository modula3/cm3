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

/* global, per-thread linked list of exception handlers */
void* RTThread__handlerStack = 0;

