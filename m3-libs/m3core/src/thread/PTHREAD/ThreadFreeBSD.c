/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#ifdef __FreeBSD__
#include "m3core.h"
#endif

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __FreeBSD__

void ThreadFreeBSD__Dummy(void) { } /* avoid empty file */

#else /* FreeBSD */

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

static void
ThreadFreeBSD__Fail(int error, const char* message)
{
  fprintf (stderr, "ThreadFreeBSD fatal error: %d %s\n", error, message);
  abort ();
}

static void
ThreadFreeBSD__Check(int error, const char* message)
{
  if (error)
    ThreadFreeBSD__Fail(error, message);
}

void
ThreadPThread__ProcessStopped (m3_pthread_t mt, char *bottom, char *context,
                              void (*p)(void *start, void *limit))
{
  pthread_attr_t attr = { 0 };
  char *stackaddr = { 0 };
  size_t stacksize = { 0 };

  /*
     NOTE: This will scan the entire allocated stack,
     not just the currently in use part.
   */

  /* process the stacks */
  ThreadFreeBSD__Check(pthread_attr_init(&attr), "pthread_attr_init");
  ThreadFreeBSD__Check(pthread_attr_get_np(PTHREAD_FROM_M3(mt), &attr), "pthread_attr_get_np");
  ThreadFreeBSD__Check(pthread_attr_getstack(&attr, (void **)&stackaddr, &stacksize), "pthread_attr_getstack");
  ThreadFreeBSD__Check(pthread_attr_destroy(&attr), "pthread_attr_destroy");
#if 0
  assert(stack_grows_down); /* See ThreadPThreadC.c */
#endif
  assert(context == 0);
  assert(bottom >= stackaddr);
  assert(bottom <= (stackaddr + stacksize));
  p(stackaddr, bottom);
  /* assume registers are stored in the stack */
  /* but call p to simulate processing registers: see RTHeapStats.m3 */
  p(0, 0);
}

#endif /* FreeBSD */

#ifdef __cplusplus
} /* extern "C" */
#endif
