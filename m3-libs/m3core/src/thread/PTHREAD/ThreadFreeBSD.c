/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __FreeBSD__
#include <pthread_np.h>
#endif

#if M3_HAS_VISIBILITY
#pragma GCC visibility push(hidden)
#endif

M3_EXTERNC_BEGIN

void __cdecl ThreadFreeBSD__Dummy(void) { } /* avoid empty file */

#if 0 // Seems to not work?
#ifdef __FreeBSD__

static void __cdecl
ThreadFreeBSD__Fatal(int error, const char* message)
{
  if (error)
  {
    fprintf (stderr, "ThreadFreeBSD fatal error: %d %s\n", error, message);
    abort ();
  }
}

int
__cdecl
ThreadPThread__SuspendThread (m3_pthread_t mt)
{
  ThreadFreeBSD__Fatal(pthread_suspend_np(PTHREAD_FROM_M3(mt)), "pthread_suspend_np");
  return 1;
}

int
__cdecl
ThreadPThread__RestartThread (m3_pthread_t mt)
{
  ThreadFreeBSD__Fatal(pthread_resume_np(PTHREAD_FROM_M3(mt)), "pthread_resume_np");
  return 1;
}

void
__cdecl
ThreadPThread__ProcessStopped (m3_pthread_t mt, char *top, char *context,
                              void (*p)(void *start, void *limit))
{
  pthread_attr_t attr = { 0 };
  char *stackaddr = { 0 };
  size_t stacksize = { 0 };

  if (!top) return;

  /*
     NOTE: This will scan the entire allocated stack,
     not just the currently in use part.
   */

  /* process the stacks */
  ThreadFreeBSD__Fatal(pthread_attr_init(&attr), "pthread_attr_init");
  ThreadFreeBSD__Fatal(pthread_attr_get_np(PTHREAD_FROM_M3(mt), &attr), "pthread_attr_get_np");
  ThreadFreeBSD__Fatal(pthread_attr_getstack(&attr, (void **)&stackaddr, &stacksize), "pthread_attr_getstack");
  ThreadFreeBSD__Fatal(pthread_attr_destroy(&attr), "pthread_attr_destroy");
  assert(context == 0);
  assert(top >= stackaddr);
  assert(top <= (stackaddr + stacksize));
  p(stackaddr, top);
  /* assume registers are stored in the stack */
  /* but call p to simulate processing registers: see RTHeapStats.m3 */
  p(0, 0);
}

#endif /* FreeBSD */
#endif // 0

M3_EXTERNC_END

#if M3_HAS_VISIBILITY
#pragma GCC visibility pop
#endif
