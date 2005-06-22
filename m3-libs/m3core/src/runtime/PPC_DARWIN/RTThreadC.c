/* Copyright according to COPYRIGHT-CMASS. */

/* This file implements the coroutine transfer: RTThread.Transfer */

#include <stdlib.h>
#include <setjmp.h>


RTThread__Transfer (from, to)
jmp_buf *from, *to;
{
  if (setjmp(*from) == 0) longjmp (*to, 1);
}


/* global thread ID used by 'etp' */
int ThreadF__myId = 1;

/* low-level runtime lock */
/* int ThreadF__inCritical = 0; MOVED TO RTHeapDepC.c */

/* global, per-thread linked list of exception handlers */
void* ThreadF__handlerStack = 0;

#include <pthread.h>
#include <architecture/ppc/cframe.h>
#include <mach/mach.h>
#include <mach/thread_act.h>

void
RTMachine__SuspendThread (pthread_t t, ppc_thread_state_t *state, void **sp)
{
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  mach_msg_type_number_t thread_state_count = MACHINE_THREAD_STATE_COUNT;
  mach_error_t r;
  r = thread_suspend(mach_thread);
  if (r != KERN_SUCCESS) abort();
  r = thread_get_state(mach_thread, MACHINE_THREAD_STATE,
		       (thread_state_t)state, &thread_state_count);
  if (r != KERN_SUCCESS) abort();
  *sp = (void *)(state->r1 - C_RED_ZONE);
}

void
RTMachine__RestartThread (pthread_t t)
{
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  mach_error_t r = thread_resume(mach_thread);
  if (r != KERN_SUCCESS) abort();
}
