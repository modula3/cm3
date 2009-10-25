#include <stdlib.h>
#include <pthread.h>
#include <mach/mach.h>
#include <mach/thread_act.h>
#include "m3unix.h"


void *
RTMachine__GetState (m3_pthread_t mt, x86_thread_state64_t *state)
{
  pthread_t t = PTHREAD_FROM_M3(mt);
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  mach_msg_type_number_t thread_state_count = x86_THREAD_STATE64_COUNT;
  if (thread_get_state(mach_thread, x86_THREAD_STATE64,
		       (thread_state_t)state, &thread_state_count)
      != KERN_SUCCESS) abort();
  if (thread_state_count != x86_THREAD_STATE64_COUNT) abort();
#if __DARWIN_UNIX03
  return (void *)(state->__rsp - 128);
#else
  return (void *)(state->rsp - 128);
#endif
}

void
RTMachine__RestartThread (m3_pthread_t mt)
{
  pthread_t t = PTHREAD_FROM_M3(mt);
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  if (thread_resume(mach_thread) != KERN_SUCCESS) abort();
}
