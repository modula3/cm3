/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

/*
  This file demonstrates deficiency in powerpc emulation on x86.
  It is reduced from ThreadApple.c.
  You can't get the state of a suspended thread.
*/

#define __DARWIN_UNIX03 0
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <mach/mach.h>
#include <mach/thread_act.h>

void* thread1(void*a)
{
    volatile void* b = alloca(0x10000);
    b = b;
    printf("1 stack at approx %p\n", &a);
    while (1)
      sleep(100);
    return 0;
}

int
SuspendThread (pthread_t t)
{
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  if (thread_suspend(mach_thread) != KERN_SUCCESS)
    return 0;
  if (thread_abort_safely(mach_thread) != KERN_SUCCESS)
  {
    kern_return_t status = thread_resume(mach_thread);
    assert(status == KERN_SUCCESS);
    return 0;
  }
  return 1;
}

#ifdef __ppc__
typedef ppc_thread_state_t m3_thread_state_t;
#define M3_THREAD_STATE       PPC_THREAD_STATE
#define M3_THREAD_STATE_COUNT PPC_THREAD_STATE_COUNT
#define M3_STACK_REGISTER r1
#endif /* ppc */

#ifdef __ppc64__
typedef ppc_thread_state64_t m3_thread_state_t;
#define M3_THREAD_STATE       PPC_THREAD_STATE64
#define M3_THREAD_STATE_COUNT PPC_THREAD_STATE64_COUNT
#define M3_STACK_REGISTER r1
#endif /* ppc64 */

#ifdef __i386__
typedef i386_thread_state_t m3_thread_state_t;
#define M3_THREAD_STATE       i386_THREAD_STATE
#define M3_THREAD_STATE_COUNT i386_THREAD_STATE_COUNT
#define M3_STACK_REGISTER esp
#endif /* i386 */

#ifdef __x86_64__
typedef x86_thread_state64_t m3_thread_state_t;
#define M3_THREAD_STATE       x86_THREAD_STATE64
#define M3_THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#define M3_STACK_REGISTER rsp
#endif /* amd64 */

void
ProcessStopped (pthread_t t)
{
  m3_thread_state_t state = { 0 };
  kern_return_t status = { 0 };
  mach_port_t mach_thread = pthread_mach_thread_np(t);
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
  printf("2 stack at approx %p\n", (void*)state.M3_STACK_REGISTER);
}

int main()
{  
    pthread_t t = { 0 };
    assert(0 == pthread_create(&t, NULL, thread1, 0));
    sleep(1);
    assert(1 == SuspendThread(t));
    ProcessStopped(t);
    return 0;
}
