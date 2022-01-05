// OpenBSD 5.2 dropped thread suspend/resume,
// and ThreadPThread__ProcessStopped does not seem to work here either.
void __cdecl ThreadOpenBSD__Dummy(void) { } /* avoid empty file */

#if 0
/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#ifdef __OpenBSD__
#include <sys/signal.h>
#include <pthread_np.h>
#endif

#if M3_HAS_VISIBILITY
#pragma GCC visibility push(hidden)
#endif

M3_EXTERNC_BEGIN

#ifdef __OpenBSD__

int
__cdecl
ThreadPThread__SuspendThread (m3_pthread_t mt)
/* same as FreeBSD */
{
  int a = pthread_suspend_np(PTHREAD_FROM_M3(mt));
  int success = (a == 0);
  assert(success);
  return success;
}

int
__cdecl
ThreadPThread__RestartThread (m3_pthread_t mt)
/* same as FreeBSD */
{
  int a = pthread_resume_np(PTHREAD_FROM_M3(mt));
  int success = (a == 0);
  assert(success);
  return success;
}

void
__cdecl
ThreadPThread__ProcessStopped (m3_pthread_t mt, char *bottom, char *context,
                              void (*p)(char *start, char *limit))
{
  stack_t stack = { 0 };
  char* stackaddr = { 0 };
  size_t stacksize = { 0 };
  int i = { 0 };

  if (!bottom) return;

  /*
     NOTE: Like FreeBSD, this code would scan the
     entire stack, not just the "currently used" part.
   */

  i = pthread_stackseg_np(PTHREAD_FROM_M3(mt), &stack);
  assert(i == 0);
  stackaddr = ((char*)stack.ss_sp) - stack.ss_size;
  stacksize = stack.ss_size;

  assert(context == 0);
  if (stackaddr < bottom)
  {
    assert(stackaddr >= (bottom - stacksize));
    p(stackaddr, bottom);
  }
  else if (stackaddr > bottom)
  {
    assert(stackaddr <= (bottom + stacksize));
    p(bottom, stackaddr);
  }
  /* assume registers are stored in the stack */
  /* but call p to simulate processing registers: see RTHeapStats.m3 */
  p(0, 0);
}

#endif /* OpenBSD */

M3_EXTERNC_END

#if M3_HAS_VISIBILITY
#pragma GCC visibility pop
#endif

#endif
