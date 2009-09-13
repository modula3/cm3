#include <stdlib.h>
#include <pthread.h>
#include <mach/mach.h>
#include <mach/thread_act.h>
#include "m3unix.h"

int
RTMachine__SuspendThread (m3_pthread_t mt)
{
  pthread_t t = PTHREAD_FROM_M3(mt);
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  if (thread_suspend(mach_thread) != KERN_SUCCESS) abort();
  return thread_abort_safely(mach_thread) == KERN_SUCCESS;
}

void *
RTMachine__GetState (m3_pthread_t mt, arm_thread_state_t *state)
{
  pthread_t t = PTHREAD_FROM_M3(mt);
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  mach_msg_type_number_t thread_state_count = ARM_THREAD_STATE_COUNT;
  if (thread_get_state(mach_thread, ARM_THREAD_STATE,
		       (thread_state_t)state, &thread_state_count)
      != KERN_SUCCESS) abort();
  if (thread_state_count != ARM_THREAD_STATE_COUNT) abort();
  return (void *)(state->r13);
}

void
RTMachine__RestartThread (m3_pthread_t mt)
{
  pthread_t t = PTHREAD_FROM_M3(mt);
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  if (thread_resume(mach_thread) != KERN_SUCCESS) abort();
}
