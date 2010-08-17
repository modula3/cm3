/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

#ifndef __APPLE__

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

#ifdef __cplusplus
extern "C" {
#endif

int
ThreadPThread__SuspendThread (m3_pthread_t mt)
{
  mach_port_t mach_thread = pthread_mach_thread_np(PTHREAD_FROM_M3(mt));
  if (thread_suspend(mach_thread) != KERN_SUCCESS)
    return 0;
  if (thread_abort_safely(mach_thread) != KERN_SUCCESS)
  {
    kern_return_t status = thread_resume(mach_thread);
    if (status != KERN_SUCCESS)
    {
      fprintf(stderr, "thread_resume returned %d instead of %d\n",
              (int)status, (int)KERN_SUCCESS);
      abort();
    }
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

#if __DARWIN_UNIX03
#define M3__(x) __##x
#else
#define M3__(x) x
#endif

#ifdef __ppc__
typedef ppc_thread_state_t m3_thread_state_t;
#define M3_THREAD_STATE       PPC_THREAD_STATE
#define M3_THREAD_STATE_COUNT PPC_THREAD_STATE_COUNT
#define M3_STACK_REGISTER M3__(r1)
#define M3_STACK_ADJUST C_RED_ZONE
#endif /* ppc */

#ifdef __ppc64__
typedef ppc_thread_state64_t m3_thread_state_t;
#define M3_THREAD_STATE       PPC_THREAD_STATE64
#define M3_THREAD_STATE_COUNT PPC_THREAD_STATE64_COUNT
#define M3_STACK_REGISTER M3__(r1)
#define M3_STACK_ADJUST C_RED_ZONE
#endif /* ppc64 */

#ifdef __i386__
typedef i386_thread_state_t m3_thread_state_t;
#define M3_THREAD_STATE       i386_THREAD_STATE
#define M3_THREAD_STATE_COUNT i386_THREAD_STATE_COUNT
#define M3_STACK_REGISTER M3__(esp)
#define M3_STACK_ADJUST 0
#endif /* i386 */

#ifdef __x86_64__
typedef x86_thread_state64_t m3_thread_state_t;
#define M3_THREAD_STATE       x86_THREAD_STATE64
#define M3_THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#define M3_STACK_REGISTER M3__(rsp)
#define M3_STACK_ADJUST 128
#endif /* amd64 */

#ifdef __arm__
#if __DARWIN_UNIX03
typedef struct __darwin_arm_thread_state m3_thread_state_t;
#else
typedef struct arm_thread_state m3_thread_state_t;
#endif
#define M3_THREAD_STATE       ARM_THREAD_STATE
#define M3_THREAD_STATE_COUNT ARM_THREAD_STATE_COUNT
/*define M3_STACK_REGISTER r13*/
#define M3_STACK_REGISTER sp
#define M3_STACK_ADJUST 0
#endif /* arm */

void
ThreadPThread__ProcessStopped (m3_pthread_t mt, void *bottom, void *context,
                               void (*p)(void *start, void *limit))
{
  void *sp;
  m3_thread_state_t state;
  kern_return_t status;
  mach_port_t mach_thread = pthread_mach_thread_np(PTHREAD_FROM_M3(mt));
  mach_msg_type_number_t thread_state_count = M3_THREAD_STATE_COUNT;
  status = thread_get_state(mach_thread, M3_THREAD_STATE,
                            (thread_state_t)&state, &thread_state_count);
  if (status != KERN_SUCCESS)
  {
    fprintf(stderr, "thread_get_state returned %d instead of %d\n",
            (int)status, (int)KERN_SUCCESS);
    abort();
  }
  if (thread_state_count != M3_THREAD_STATE_COUNT)
  {
    fprintf(stderr,
            "thread_get_state returned thread_state_count %d instead of %d\n",
            (int)thread_state_count, (int)M3_THREAD_STATE_COUNT);
    abort();
  }
  sp = (void *)(state.M3_STACK_REGISTER - M3_STACK_ADJUST);
  /* process the stack */
#if 0
  assert(stack_grows_down); /* See ThreadPThreadC.c */
#endif
  assert(context == 0);
  p(sp, bottom);
  /* process the registers */
  p(&state, &state + 1);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
