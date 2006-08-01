/* Copyright according to COPYRIGHT-CMASS. */
/* FIXME: copied from FreeBSD3 target. Probably needs to be changed. */

/* This file implements the coroutine transfer: RTThread.Transfer */

#include <setjmp.h>


RTThread__Transfer (from, to)
jmp_buf *from, *to;
{
  if (_fpsetjmp(*from) == 0) _fplongjmp (*to, 1);
}


/* global thread ID used by 'etp' */
int ThreadF__myId = 1;

/* low-level runtime lock */
int ThreadF__inCritical = 0;

/* global, per-thread linked list of exception handlers */
void* ThreadF__handlerStack = 0;

