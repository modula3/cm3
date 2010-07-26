/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#ifndef __OpenBSD__

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

/* avoid empty file */

void ThreadOpenBSD__Dummy(void)
{
}

#else

#include "m3core.h"
#include <sys/signal.h>
#include <pthread_np.h>

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

#ifdef __cplusplus
extern "C" {
#endif

int
ThreadPThread__SuspendThread (m3_pthread_t mt)
/* same as FreeBSD */
{
  int a = pthread_suspend_np(PTHREAD_FROM_M3(mt));
  int success = (a == 0);
  assert(success);
  return success;
}

int
ThreadPThread__RestartThread (m3_pthread_t mt)
/* same as FreeBSD */
{
  int a = pthread_resume_np(PTHREAD_FROM_M3(mt));
  int success = (a == 0);
  assert(success);
  return success;
}

void
ThreadPThread__ProcessStopped (m3_pthread_t mt, void *bottom, void *context,
                              void (*p)(void *start, void *limit))
{
  stack_t stack = { 0 };
  char* stackaddr = { 0 };
  size_t stacksize = { 0 };
  int i = { 0 };

  /*
     NOTE: Like FreeBSD, this code would scan the
     entire stack, not just the "currently used" part.
   */

  i = pthread_stackseg_np(PTHREAD_FROM_M3(mt), &stack);
  assert(i == 0);
  stackaddr = ((char*)stack.ss_sp) - stack.ss_size;
  stacksize = stack.ss_size;
  
  /* same as FreeBSD from here */

#if 0
  assert(stack_grows_down); /* See ThreadPThreadC.c */
#endif
  assert(context == 0);
  assert((char *)bottom >= stackaddr);
  assert((char *)bottom <= (stackaddr + stacksize));
  p(stackaddr, bottom);
  /* assume registers are stored in the stack */
  /* but call p to simulate processing registers: see RTHeapStats.m3 */
  p(0, 0);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
