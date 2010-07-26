/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#ifndef __FreeBSD__

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

/* avoid empty file */

void ThreadFreeBSD__Dummy(void)
{
}

#else

#include "m3core.h"

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

#ifdef __cplusplus
extern "C" {
#endif

int
ThreadPThread__SuspendThread (m3_pthread_t mt)
{
  int a = pthread_suspend_np(PTHREAD_FROM_M3(mt));
  int success = (a == 0);
  assert(success);
  return success;
}

int
ThreadPThread__RestartThread (m3_pthread_t mt)
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
  pthread_attr_t attr;
  char *stackaddr;
  WORD_T stacksize;

  /*
     NOTE: This will scan the entire allocated stack,
     not just the currently in use part.
   */

  /* process the stacks */
  if (pthread_attr_init(&attr) != 0) abort();
  if (pthread_attr_get_np(PTHREAD_FROM_M3(mt), &attr) != 0) abort();
  if (pthread_attr_getstack(&attr, (void **)&stackaddr, &stacksize) != 0) abort();
  if (pthread_attr_destroy(&attr) != 0) abort();
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
