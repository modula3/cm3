/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#ifndef __APPLE__

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

/* avoid empty file */

void ThreadApple__Dummy(void)
{
}

#else

/* Older and newer headers default __DARWIN_UNIX03 to 1,
 * but only newer headers rename the symbols we use
 * under it, so we have to change to to 0.
 */
#ifndef __DARWIN_UNIX03
#define __DARWIN_UNIX03 0
#endif

#include "m3core.h"
#include <sys/ucontext.h>

#include <mach/mach.h>
#include <mach/thread_act.h>
#if defined(__ppc__) || defined(__ppc64__)
#include <architecture/ppc/cframe.h>
#endif

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

#ifdef __cplusplus
extern "C" {
#endif

int
ThreadPThread__SuspendThread (m3_pthread_t mt)
{
  pthread_t t = PTHREAD_FROM_M3(mt);
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  if (thread_suspend(mach_thread) != KERN_SUCCESS) return 0;
  if (thread_abort_safely(mach_thread) != KERN_SUCCESS) {
    if (thread_resume(mach_thread) != KERN_SUCCESS) abort();
    return 0;
  }
  return 1;
}

int
ThreadPThread__RestartThread (m3_pthread_t mt)
{
  pthread_t t = PTHREAD_FROM_M3(mt);
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  return thread_resume(mach_thread) == KERN_SUCCESS;
}

void
ThreadPThread__ProcessStopped (m3_pthread_t mt, void *bottom, void *context,
                               void (*p)(void *start, void *limit))
{
  void *sp;
  pthread_t t = PTHREAD_FROM_M3(mt);
  mach_port_t mach_thread = pthread_mach_thread_np(t);
#if defined(__ppc__)
  ppc_thread_state_t state;
  mach_msg_type_number_t thread_state_count = PPC_THREAD_STATE_COUNT;
  if (thread_get_state(mach_thread, PPC_THREAD_STATE,
                       (thread_state_t)&state, &thread_state_count)
      != KERN_SUCCESS) abort();
  if (thread_state_count != PPC_THREAD_STATE_COUNT) abort();
#if __DARWIN_UNIX03
  sp = (void *)(state.__r1 - C_RED_ZONE);
#else
  sp = (void *)(state.r1 - C_RED_ZONE);
#endif
#elif defined(__ppc64__)
  ppc_thread_state64_t state;
  mach_msg_type_number_t thread_state_count = PPC_THREAD_STATE64_COUNT;
  if (thread_get_state(mach_thread, PPC_THREAD_STATE64,
                       (thread_state_t)&state, &thread_state_count)
      != KERN_SUCCESS) abort();
  if (thread_state_count != PPC_THREAD_STATE64_COUNT) abort();
#if __DARWIN_UNIX03
  sp = (void *)(state.__r1 - C_RED_ZONE);
#else
  sp = (void *)(state.r1 - C_RED_ZONE);
#endif
#elif defined(__i386__)
  i386_thread_state_t state;
  mach_msg_type_number_t thread_state_count = i386_THREAD_STATE_COUNT;
  if (thread_get_state(mach_thread, i386_THREAD_STATE,
                       (thread_state_t)&state, &thread_state_count)
      != KERN_SUCCESS) abort();
  if (thread_state_count != i386_THREAD_STATE_COUNT) abort();
#if __DARWIN_UNIX03
  sp = (void *)(state.__esp);
#else
  sp = (void *)(state.esp);
#endif
#elif defined(__x86_64__)
  x86_thread_state64_t state;
  mach_msg_type_number_t thread_state_count = x86_THREAD_STATE64_COUNT;
  if (thread_get_state(mach_thread, x86_THREAD_STATE64,
                       (thread_state_t)&state, &thread_state_count)
      != KERN_SUCCESS) abort();
  if (thread_state_count != x86_THREAD_STATE64_COUNT) abort();
#if __DARWIN_UNIX03
  sp = (void *)(state.__rsp - 128);
#else
  sp = (void *)(state.rsp - 128);
#endif
#elif defined(__arm__)
  mach_msg_type_number_t thread_state_count = ARM_THREAD_STATE_COUNT;
  if (thread_get_state(mach_thread, ARM_THREAD_STATE,
                       state, &thread_state_count)
      != KERN_SUCCESS) abort();
  if (thread_state_count != ARM_THREAD_STATE_COUNT) abort();
  sp = (void *)(state.r13);
#endif
  /* process the stack */
#if 0
  assert(stack_grows_down); /* See ThreadPThreadC.c */
#endif
  assert(context == 0);
  p(sp, bottom);
  /* process the registers */
  p(&state, (char *)&state + sizeof(state));
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
