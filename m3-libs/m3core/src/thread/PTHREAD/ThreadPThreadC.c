/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#include "m3unix.h"
#include <stdlib.h>
#include <pthread.h>
#include <setjmp.h>
#ifdef __hpux
#include <stdio.h>
#endif
#ifdef __OpenBSD__
#include <pthread_np.h>
#endif
#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/thread_act.h>
#endif
#if defined(__APPLE__) || defined(__OpenBSD__) || defined(__FreeBSD__)
/* These operating systems use pthread_suspend_np, Mach thread_suspend, etc. */
#define M3_DIRECT_SUSPEND
#else
/* Else we send a signal to the thread to suspend/resume it. */
#endif
#ifndef M3_DIRECT_SUSPEND
#include <semaphore.h>
#endif

#ifdef M3_DIRECT_SUSPEND
#include <stdio.h>
#define M3_DIRECT_SUSPEND_ASSERT_FALSE do { \
    assert(0 && "MacOS X, FreeBSD, OpenBSD should not get here."); \
    fprintf(stderr, "MacOS X, FreeBSD, OpenBSD should not get here.\n"); \
    abort(); \
} while(0)
#endif

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const" */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define SignalHandler           ThreadPThread__SignalHandler
#define SetupHandlers           ThreadPThread__SetupHandlers
#define sizeof_pthread_mutex_t  ThreadPThread__sizeof_pthread_mutex_t
#define sizeof_pthread_cond_t   ThreadPThread__sizeof_pthread_cond_t
#define SIG_SUSPEND             ThreadPThread__SIG_SUSPEND
 
/* expected values for compat, if compat matters:
    Solaris: 17 (at least 32bit SPARC?)
    Cygwin: 19 -- er, but maybe that's wrong
    Linux: 64
    FreeBSD: 31
    OpenBSD: 31
    HPUX: 44
  Look at the history of Usignal and RTMachine to find more values.
  There was RTMachine.SIG_SUSPEND and SIG was aliased to it.
  Both SIG and SIG_SUSPEND were only defined for systems using pthreads.
  SIG was shorthand.
*/
#ifdef M3_DIRECT_SUSPEND
EXTERN_CONST int SIG_SUSPEND = 0;
#elif defined(__sun) || defined(__CYGWIN__)
EXTERN_CONST int SIG_SUSPEND = SIGUSR2;
#elif defined(__linux)
EXTERN_CONST int SIG_SUSPEND = NSIG - 1;
#elif defined(__hpux)
EXTERN_CONST int SIG_SUSPEND = _SIGRTMAX;
#elif defined(SIGRTMAX)
/* This might be a function call, in which case try _SIGRTMAX or
initializing it somewhere. */
EXTERN_CONST int SIG_SUSPEND = SIGRTMAX;
#elif defined(SIGUSR2)
EXTERN_CONST int SIG_SUSPEND = SIGUSR2;
#else
#error Unable to determine SIG_SUSPEND.
#endif

#ifndef M3_DIRECT_SUSPEND

typedef struct sigaction sigaction_t;

static sigset_t mask;

/* Signal based suspend/resume */
static sem_t ackSem;

void SignalHandler(int);

void SetupHandlers(void)
{
    sigaction_t act;
    sigaction_t oact;
    int r;

    ZeroMemory(&act, sizeof(act));
    ZeroMemory(&oact, sizeof(oact));

    r = sem_init(&ackSem, 0, 0); assert(r == 0);

    r = sigfillset(&mask); assert(r == 0);
    r = sigdelset(&mask, SIG_SUSPEND); assert(r == 0);
    r = sigdelset(&mask, SIGINT); assert(r == 0);
    r = sigdelset(&mask, SIGQUIT); assert(r == 0);
    r = sigdelset(&mask, SIGABRT); assert(r == 0);
    r = sigdelset(&mask, SIGTERM); assert(r == 0);

    act.sa_flags = SA_RESTART;
    act.sa_handler = SignalHandler;
    r = sigfillset(&act.sa_mask); assert(r == 0);
    r = sigaction(SIG_SUSPEND, &act, &oact); assert(r == 0);
}

int ThreadPThread__sem_wait(void)           { return sem_wait(&ackSem); }
int ThreadPThread__sem_post(void)           { return sem_post(&ackSem); }
int ThreadPThread__sem_getvalue(int* value) { return sem_getvalue(&ackSem, value); }
int ThreadPThread__sigsuspend(void)         { return sigsuspend(&mask); }

int ThreadPThread__SuspendThread (m3_pthread_t mt)
{
  abort();
}

int ThreadPThread__RestartThread (m3_pthread_t mt)
{
  abort();
}

void ThreadPThread__ProcessStopped (m3_pthread_t mt, void *start, void *end,
				    void (*p)(void *start, void *end))
{
  p(start, end);
}

#else /* M3_DIRECT_SUSPEND */

void SetupHandlers(void)                { M3_DIRECT_SUSPEND_ASSERT_FALSE }
void ThreadPThread__sem_wait(void)      { M3_DIRECT_SUSPEND_ASSERT_FALSE }
void ThreadPThread__sem_post(void)      { M3_DIRECT_SUSPEND_ASSERT_FALSE }
void ThreadPThread__sem_getvalue(void)  { M3_DIRECT_SUSPEND_ASSERT_FALSE }
void ThreadPThread__sigsuspend(void)    { M3_DIRECT_SUSPEND_ASSERT_FALSE }

#if defined(__OpenBSD__) || defined(__FreeBSD__)

int ThreadPThread__SuspendThread (m3_pthread_t mt)
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

#endif /* OpenBSD | FreeBSD */

#ifdef __OpenBSD__

void
ThreadPThread__ProcessStopped (m3_pthread_t mt, void *start, void *end,
			       void (*p)(void *start, void *end))
{
  stack_t sinfo;
  if (pthread_stackseg_np(PTHREAD_FROM_M3(mt), &sinfo) != 0) abort();
  assert(start == 0);
  assert(end >= sinfo.ss_sp);
  assert((char *)end <= (char *)sinfo.ss_sp + sinfo.ss_size);
  p(sinfo.ss_sp, end);
}

#endif /* OpenBSD */

#ifdef __FreeBSD__

void
ThreadPThread__ProcessStopped (m3_pthread_t mt, void *start, void *end,
			      void (*p)(void *start, void *end))
{
  pthread_attr_t attr;
  void *stackaddr;
  size_t stacksize;
  /* assume registers of stopped threads are in the stack so don't process */
  if (pthread_attr_init(&attr) != 0) abort();
  if (pthread_attr_get_np(PTHREAD_FROM_M3(mt), &attr) != 0) abort();
  if (pthread_attr_getstack(&attr, &stackaddr, &stacksize) != 0) abort();
  assert(start == 0);
  assert(end >= stackaddr);
  assert((char *)end <= (char *)stackaddr + stacksize);
  p(stackaddr, end);
}

#endif /* FreeBSD */

#ifdef __APPLE__

int ThreadPThread__SuspendThread (m3_pthread_t mt)
{
  pthread_t t = PTHREAD_FROM_M3(mt);
  mach_port_t mach_thread = pthread_mach_thread_np(t);
  if (thread_suspend(mach_thread) != KERN_SUCCESS) return 0;
  if (thread_abort_safely(mach_thread) != KERN_SUCCESS) {
    if (thread_resume(mach_thread != KERN_SUCCESS)) abort();
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
ThreadPThread__ProcessStopped (m3_pthread_t mt, void *start, void *end,
			       void (*p)(void *start, void *end))
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
  p(&state, (char *)&state + sizeof(state));
  assert(start == 0);
  p(sp, end);
}

#endif /* Apple */
#endif /* M3_DIRECT_SUSPEND */

void *ThreadPThread__SaveRegsInStack(void)
/* On register window machines, we need a way to force registers into 	*/
/* the stack.	Return sp.						*/
# ifdef __sparc
{
    asm("	.seg 	\"text\"");
#   if defined(SVR4) || defined(NETBSD) || defined(FREEBSD)
      asm("	.globl	ThreadPThread__SaveRegsInStack");
      asm("ThreadPThread__SaveRegsInStack:");
      asm("	.type ThreadPThread__SaveRegsInStack,#function");
#   else
      asm("	.globl	_ThreadPThread__SaveRegsInStack");
      asm("_ThreadPThread__SaveRegsInStack:");
#   endif
#   if defined(__arch64__) || defined(__sparcv9)
      asm("	save	%sp,-128,%sp");
      asm("	flushw");
      asm("	ret");
      asm("	restore %sp,2047+128,%o0");
#   else
      asm("	ta	0x3   ! ST_FLUSH_WINDOWS");
      asm("	retl");
      asm("	mov	%sp,%o0");
#   endif
#   ifdef SVR4
      asm("	.ThreadPThread__SaveRegsInStack_end:");
      asm("	.size ThreadPThread__SaveRegsInStack,.ThreadPThread__SaveRegsInStack_end-ThreadPThread__SaveRegsInStack");
#   endif
# else
  return 0;
# endif
}

void
ThreadPThread__ProcessLive(void *start, void * end,
			   void (*p)(void *start, void *stop))
{
  jmp_buf buf;
  setjmp(buf);
  p(&buf, (char *)&buf + sizeof(buf));
#ifdef __sparc
  start = ThreadPThread__SaveRegsInStack();
#endif
  assert(start < end);
  p(start, end);
}

#define M3_MAX(x, y) (((x) > (y)) ? (x) : (y))
typedef void* (*start_routine_t)(void*);

int
ThreadPThread__thread_create(
    pthread_t* pthread,
    size_t stackSize,
    start_routine_t start_routine,
    void* arg)
{
    int r;
    size_t bytes;
    pthread_attr_t attr;

    r = pthread_attr_init(&attr);
#ifdef __hpux
    if (r == ENOSYS)
    {
        fprintf(
            stderr,
            "You got the nonfunctional pthread stubs on HP-UX. You need to"
            " adjust your build commands, such as to link to -lpthread or"
            " use -pthread, and not link explicitly to -lc.\n");
    }
#endif
    assert(r == 0);
    
    r = pthread_attr_getstacksize(&attr, &bytes); assert(r == 0);

    bytes = M3_MAX(bytes, stackSize);
    pthread_attr_setstacksize(&attr, bytes);

    r = pthread_create(pthread, &attr, start_routine, arg);

    pthread_attr_destroy(&attr);

    return r;
}

#define MUTEX(name) \
static pthread_mutex_t name##Mu = PTHREAD_MUTEX_INITIALIZER; \
pthread_mutex_t *ThreadPThread__##name##Mu = &name##Mu; \

#define CONDITION_VARIABLE(name) \
static pthread_cond_t name##Cond = PTHREAD_COND_INITIALIZER; \
pthread_cond_t *ThreadPThread__##name##Cond = &name##Cond; \

#define THREAD_LOCAL(name) \
static pthread_key_t name; \
int ThreadPThread__pthread_key_create_##name(void) \
{ \
    return pthread_key_create(&name, NULL); \
} \
int ThreadPThread__pthread_setspecific_##name(void* value) \
{ \
    return pthread_setspecific(name, value); \
} \
void* ThreadPThread__pthread_getspecific_##name(void) \
{ \
    return pthread_getspecific(name); \
} \

/* activeMu slotMu initMu perfMu heapMu heapCond */

MUTEX(active) /* global lock for list of active threads */
MUTEX(slots)  /* global lock for thread slots table */
MUTEX(init)   /* global lock for initializers */
MUTEX(perf)
MUTEX(heap)
CONDITION_VARIABLE(heap)
THREAD_LOCAL(activations)

typedef int (*generic_init_t)(void*, const void*);

void* ThreadPThread_pthread_generic_new(size_t size, generic_init_t init)
{
    int r;
    void* p = calloc(1, size);
    if (p == NULL)
        goto Error;
    r = init(p, NULL);
    if (r == EAGAIN)
        r = init(p, NULL);
    if (r == ENOMEM)
        goto Error;
    assert(r == 0);
    if (r != 0)
        goto Error;
    return p;
Error:
    if (p) free(p);
    return NULL;
}

#define THREADPTHREAD__PTHREAD_GENERIC_NEW(type) \
    typedef pthread_##type##_t T; \
    typedef pthread_##type##attr_t attr_t; \
    typedef int (*init_t)(T*, const attr_t*); \
    /* make sure the type matches */ \
    init_t init = pthread_##type##_init; \
    return ThreadPThread_pthread_generic_new(sizeof(T), (generic_init_t)init);

void* ThreadPThread__pthread_mutex_new(void)
{
    THREADPTHREAD__PTHREAD_GENERIC_NEW(mutex);
}

void* ThreadPThread__pthread_cond_new(void)
{
    THREADPTHREAD__PTHREAD_GENERIC_NEW(cond);
}

void ThreadPThread__pthread_mutex_delete(pthread_mutex_t* p)
{
    int e;
    if (p == NULL) return;
#if defined(__hpux) || defined(__osf)
    /* workaround Tru64 5.1 and HP-UX bug
    pthread_mutex_destroy() intermittently returns
    EBUSY even when there are no threads accessing the mutex. */
    while ((e = pthread_mutex_destroy(p)) == EBUSY) { }
#else
    e = pthread_mutex_destroy(p);
#endif
    assert(e == 0);
    free(p);
}

void ThreadPThread__pthread_cond_delete(pthread_cond_t* p)
{
    int r;
    if (p == NULL) return;
    r = pthread_cond_destroy(p);
    assert(r == 0);
    free(p);
}

int ThreadPThread__Nanosleep(timespec_T* req, timespec_T* rem)
{
#ifdef __INTERIX
    /* This is only an approximation. */
    if (rem != NULL)
        memset(rem, 0, sizeof(*rem));
    if (req->tv_sec > 0)
        sleep(req->tv_sec);
    else
        usleep(req->tv_nsec / 1000);
    return 0;
#else
    return nanosleep(req, rem);
#endif
}

int
ThreadPThread__pthread_cond_wait(
    pthread_cond_t* cond,
    pthread_mutex_t* mutex)
{
    return pthread_cond_wait(cond, mutex);
}

int
ThreadPThread__pthread_cond_timedwait(
	pthread_cond_t* cond,
	pthread_mutex_t* mutex,
	const timespec_T* abs)
{
    return pthread_cond_timedwait(cond, mutex, abs);
}

int ThreadPThread__pthread_cond_signal(pthread_cond_t* cond)
{
    return pthread_cond_signal(cond);
}

int ThreadPThread__pthread_cond_broadcast(pthread_cond_t* cond)
{
    return pthread_cond_broadcast(cond);
}

int ThreadPThread__pthread_detach(m3_pthread_t thread)
{
    return pthread_detach(PTHREAD_FROM_M3(thread));
}

m3_pthread_t ThreadPThread__pthread_self(void)
{
    pthread_t a = pthread_self();
    return PTHREAD_TO_M3(a);
}

int ThreadPThread__pthread_equal(m3_pthread_t t1, m3_pthread_t t2)
{
    return pthread_equal(PTHREAD_FROM_M3(t1), PTHREAD_FROM_M3(t2));
}

int ThreadPThread__pthread_kill(m3_pthread_t thread, int sig)
{
    return pthread_kill(PTHREAD_FROM_M3(thread), sig);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
