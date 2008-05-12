/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/* This file implements the coroutine transfer: RTThread.Transfer */

#include <setjmp.h>

RTThread__Transfer (from, to)
jmp_buf *from, *to;
{
  if (_setjmp(*from) == 0) _longjmp (*to, 1);
}

/* global, per-thread linked list of exception handlers */
void* ThreadF__handlerStack = 0;

