/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#include "m3core.h"
#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/thread_act.h>
#if defined(__ppc__) || defined(__ppc64__)
#include <architecture/ppc/cframe.h>
#endif
#endif /* Apple */

#if M3_HAS_VISIBILITY
#pragma GCC visibility push(hidden)
#endif

M3_EXTERNC_BEGIN

#ifndef __APPLE__

void __cdecl ThreadApple__Dummy(void) { } /* avoid empty file */

#else /* Apple */

int
__cdecl
ThreadPThread__SuspendThread (m3_pthread_t mt)
{
  kern_return_t status = { 0 };
  mach_port_t mach_thread = PTHREAD_FROM_M3(mt);
  status = thread_suspend(mach_thread);
  if (status != KERN_SUCCESS)
  {
    fprintf(stderr, "thread_suspend returned %d instead of %d\n",
            (int)status, (int)KERN_SUCCESS);
    return 0;
  }
  status = thread_abort_safely(mach_thread);
  if (status != KERN_SUCCESS)
  {
    fprintf(stderr, "thread_abort_safely returned %d instead of %d\n",
            (int)status, (int)KERN_SUCCESS);
    status = thread_resume(mach_thread);
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
__cdecl
ThreadPThread__RestartThread (m3_pthread_t mt)
{
  mach_port_t mach_thread = PTHREAD_FROM_M3(mt);
  return thread_resume(mach_thread) == KERN_SUCCESS;
}

#ifdef __ppc__
typedef ppc_thread_state_t m3_thread_state_t;
#define M3_THREAD_STATE       PPC_THREAD_STATE
#define M3_THREAD_STATE_COUNT PPC_THREAD_STATE_COUNT
#define M3_STACK_REGISTER     r1
#define M3_STACK_ADJUST       C_RED_ZONE
#endif /* ppc */

#ifdef __ppc64__
typedef ppc_thread_state64_t m3_thread_state_t;
#define M3_THREAD_STATE       PPC_THREAD_STATE64
#define M3_THREAD_STATE_COUNT PPC_THREAD_STATE64_COUNT
#define M3_STACK_REGISTER     r1
#define M3_STACK_ADJUST       C_RED_ZONE
#endif /* ppc64 */

#ifdef __i386__
typedef i386_thread_state_t m3_thread_state_t;
#define M3_THREAD_STATE       i386_THREAD_STATE
#define M3_THREAD_STATE_COUNT i386_THREAD_STATE_COUNT
#define M3_STACK_REGISTER     esp
#define M3_STACK_ADJUST       0
#endif /* i386 */

#ifdef __x86_64__
typedef x86_thread_state64_t  m3_thread_state_t;
#define M3_THREAD_STATE       x86_THREAD_STATE64
#define M3_THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#define M3_STACK_REGISTER     rsp
#define M3_STACK_ADJUST       128
#endif /* amd64 */

#ifdef __arm__
typedef struct arm_thread_state m3_thread_state_t;
#define M3_THREAD_STATE         ARM_THREAD_STATE
#define M3_THREAD_STATE_COUNT   ARM_THREAD_STATE_COUNT
/*define M3_STACK_REGISTER r13*/
#define M3_STACK_REGISTER       sp
#define M3_STACK_ADJUST         0
#endif /* arm */

void
__cdecl
ThreadPThread__ProcessStopped (m3_pthread_t mt, void *bottom, void *context,
                               void (*p)(void *start, void *limit))
{
  void *sp = { 0 };
  m3_thread_state_t state = { 0 };
  kern_return_t status = { 0 };
  mach_msg_type_number_t thread_state_count = M3_THREAD_STATE_COUNT;

  if (!bottom) return;
  status = thread_get_state(PTHREAD_FROM_M3(mt),
                            M3_THREAD_STATE, (thread_state_t)&state,
                            &thread_state_count);
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

#endif /* Apple */

M3_EXTERNC_END
