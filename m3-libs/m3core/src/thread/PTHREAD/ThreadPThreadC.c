/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#include "m3core.h"

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__)
/* See ThreadApple.c, ThreadFreeBSD.c, ThreadOpenBSD.c. */
#define M3_DIRECT_SUSPEND
#endif

#if defined(__APPLE__)
#include <mach/mach_port.h>
#include <mach/mach_init.h>
#endif

#define M3MODULE ThreadPThread

#if defined(__sparc) || defined(__ia64__)
#define M3_REGISTER_WINDOWS
#endif

#ifdef M3_DIRECT_SUSPEND
#define M3_DIRECT_SUSPEND_ASSERT_FALSE do {                     \
    assert(0 && "MacOS X, FreeBSD, OpenBSD should not get here."); \
    fprintf(stderr, "MacOS X, FreeBSD, OpenBSD should not get here.\n"); \
    abort();                                                    \
  } while(0);
#endif

M3_EXTERNC_BEGIN

#define InitC                   ThreadPThread__InitC
#define SignalHandler           ThreadPThread__SignalHandler
#define sizeof_pthread_mutex_t  ThreadPThread__sizeof_pthread_mutex_t
#define sizeof_pthread_cond_t   ThreadPThread__sizeof_pthread_cond_t
#define SIG_SUSPEND             ThreadPThread__SIG_SUSPEND

void __cdecl SignalHandler(int signo, siginfo_t *info, void *context);

#if M3_HAS_VISIBILITY
#pragma GCC visibility push(hidden)
#endif

/* expected values for compat, if compat matters:
    Solaris: 17 (at least 32bit SPARC?)
    Cygwin: 19 -- er, but maybe that's wrong
    Linux: 64
    FreeBSD: 31 (not used)
    OpenBSD: 31 (not used)
    HPUX: 44
  Look at the history of Usignal and RTMachine to find more values.  There was
  RTMachine.SIG_SUSPEND and SIG was aliased to it.  Both SIG and SIG_SUSPEND
  were only defined for systems using pthreads. SIG was shorthand. */
#ifdef M3_DIRECT_SUSPEND
EXTERN_CONST int SIG_SUSPEND = 0;
#elif defined(__sun) || defined(__CYGWIN__)
EXTERN_CONST int SIG_SUSPEND = SIGUSR2;
#elif defined(__linux)
EXTERN_CONST int SIG_SUSPEND = NSIG - 1;
#elif defined(__hpux)
EXTERN_CONST int SIG_SUSPEND = _SIGRTMAX;
#elif defined(SIGRTMAX) && !defined(__osf__)
/* This might be a function call, in which case try _SIGRTMAX or initializing
   it somewhere. SIGRTMAX is sysconf(132) on OSF. We may be
   able to use direct suspend/resume on OSF. */
EXTERN_CONST int SIG_SUSPEND = SIGRTMAX;
#elif defined(SIGUSR2)
EXTERN_CONST int SIG_SUSPEND = SIGUSR2;
#else
#error Unable to determine SIG_SUSPEND.
#endif

static int stack_grows_down;

#ifndef M3_DIRECT_SUSPEND

static sigset_t mask;

/* Signal based suspend/resume */
static sem_t ackSem;

static void __cdecl SignalHandlerC(int signo, siginfo_t *info, void *context)
/* wrapper to workaround on ALPHA_LINUX:
   /usr/bin/ld: ThreadPThreadC.o: gp-relative relocation against dynamic symbol ThreadPThread__SignalHandler
   http://gcc.gnu.org/bugzilla/show_bug.cgi?id=46861 */
{
  SignalHandler(signo, info, context);
}

int __cdecl ThreadPThread__sem_wait(void)           { return sem_wait(&ackSem); }
int __cdecl ThreadPThread__sem_post(void)           { return sem_post(&ackSem); }
int __cdecl ThreadPThread__sem_getvalue(int *value) { return sem_getvalue(&ackSem, value); }

void
__cdecl
ThreadPThread__sigsuspend(void)
{
  struct {
    sigjmp_buf jb;
  } s;

  ZERO_MEMORY(s);

  if (sigsetjmp(s.jb, 0) == 0) /* save registers to stack */
#ifdef M3_REGISTER_WINDOWS
    siglongjmp(s.jb, 1); /* flush register windows */
  else
#endif
    sigsuspend(&mask);
}

void
__cdecl
ThreadPThread__SuspendThread (m3_pthread_t mt)
{
  abort();
}

void
__cdecl
ThreadPThread__RestartThread (m3_pthread_t mt)
{
  abort();
}

void
__cdecl
ThreadPThread__ProcessStopped (m3_pthread_t mt, char *bottom, char *context,
                               void (*p)(void *start, void *limit))
{
  /* process stack */
  if (!bottom) return;
  if (stack_grows_down)
  {
    assert(context <= bottom);  /* is it not OK to have an empty stack? */
    p(context, bottom);
  }
  else
  {
    assert(bottom <= context);
    p(bottom, context);
  }
  /* process register context */
  /*p(context, context + sizeof(ucontext_t));*/ /* cant be right */
}

#else /* M3_DIRECT_SUSPEND */

void __cdecl ThreadPThread__sem_wait(void)      { M3_DIRECT_SUSPEND_ASSERT_FALSE }
void __cdecl ThreadPThread__sem_post(void)      { M3_DIRECT_SUSPEND_ASSERT_FALSE }
void __cdecl ThreadPThread__sem_getvalue(void)  { M3_DIRECT_SUSPEND_ASSERT_FALSE }
void __cdecl ThreadPThread__sigsuspend(void)    { M3_DIRECT_SUSPEND_ASSERT_FALSE }

#endif /* M3_DIRECT_SUSPEND */

void
__cdecl
ThreadPThread__ProcessLive(char *bottom, void (*p)(void *start, void *limit))
{
/*
cc: Warning: ThreadPThreadC.c, line 170: In this statement, & before array "jb" is ignored. (addrarray)
    p(&jb, ((char *)&jb) + sizeof(jb));
------^
cc: Warning: ThreadPThreadC.c, line 170: In this statement, & before array "jb" is ignored. (addrarray)
    p(&jb, ((char *)&jb) + sizeof(jb));
--------------------^

jb may or may not be an array, & is necessary, wrap it in struct.
*/
  struct {
    sigjmp_buf jb;
  } s;

  ZERO_MEMORY(s);

  if (sigsetjmp(s.jb, 0) == 0) /* save registers to stack */
#ifdef M3_REGISTER_WINDOWS
    siglongjmp(s.jb, 1); /* flush register windows */
  else
#endif
  {
    /* capture top after longjmp because longjmp can clobber non-volatile locals */
    char *top = (char*)&top;
    assert(bottom);
    if (stack_grows_down)
    {
      assert(top < bottom);
      p(top, bottom);
    }
    else
    {
      assert(bottom < top);
      p(bottom, top);
    }
    p(&s, sizeof(s) + (char *)&s);
  }
}

#define M3_MAX(x, y) (((x) > (y)) ? (x) : (y))
typedef void *(*start_routine_t)(void *);

#define M3_RETRY(expr)                                  \
  r = (expr);                                           \
  if (r == EAGAIN || r == ENOMEM || r == ENOSPC)        \
  {                                                     \
    /* try again right away */                          \
    r = (expr);                                         \
    if (r == EAGAIN || r == ENOMEM || r == ENOSPC)      \
    {                                                   \
      /* try again after short delay */                 \
      sleep(1);                                         \
      r = (expr);                                       \
    }                                                   \
  }

int
__cdecl
ThreadPThread__thread_create(WORD_T stackSize,
                             start_routine_t start_routine,
                             void *arg)
{
  int r = { 0 };
  WORD_T bytes = { 0 };
  pthread_attr_t attr;
  pthread_t pthread;

  ZERO_MEMORY(pthread);
  ZERO_MEMORY(attr);
  
  M3_RETRY(pthread_attr_init(&attr));
#ifdef __hpux
  if (r == ENOSYS)
    {
      fprintf(stderr,
              "You got the nonfunctional pthread stubs on HP-UX. You need to"
              " adjust your build commands, such as to link to -lpthread or"
              " use -pthread, and not link explicitly to -lc.\n");
    }
#endif
  assert(r == 0);

  r = pthread_attr_getstacksize(&attr, &bytes); assert(r == 0);

  bytes = M3_MAX(bytes, stackSize);
  pthread_attr_setstacksize(&attr, bytes);

  M3_RETRY(pthread_create(&pthread, &attr, start_routine, arg));
#ifdef __sun
  if (r == ENOENT)
  {
    fprintf(stderr,
            "You got the nonfunctional pthread stubs on Solaris earlier than 5.10. "
            "You need to adjust your build commands, such as to link to -lpthread "
            " ahead of -lc.\n");
  }
#endif  
  if (r != 0)
  {
    fprintf(stderr,
            "pthread_create(stack_size:0x%X):0x%X errno:0x%X\n",
            (unsigned)stackSize,
            (unsigned)r,
            (unsigned)errno);
  }

  pthread_attr_destroy(&attr);

  return r;
}


#define MUTEX(name) \
static pthread_mutex_t name##Mu = PTHREAD_MUTEX_INITIALIZER; \
extern pthread_mutex_t * const ThreadPThread__##name##Mu; \
pthread_mutex_t * const ThreadPThread__##name##Mu = &name##Mu; \

#define CONDITION_VARIABLE(name) \
static pthread_cond_t name##Cond = PTHREAD_COND_INITIALIZER; \
extern pthread_cond_t * const ThreadPThread__##name##Cond; \
pthread_cond_t * const ThreadPThread__##name##Cond = &name##Cond; \

/* activeMu slotMu initMu perfMu heapMu heapCond */

MUTEX(active)                   /* global lock for list of active threads */
MUTEX(slots)                    /* global lock for thread slots table */
MUTEX(init)                     /* global lock for initializers */
MUTEX(perf)                     /* global lock for thread state tracing */
MUTEX(heap)                     /* global lock for heap atomicity */
CONDITION_VARIABLE(heap)        /* CV for heap waiters */

/*
NetBSD 5.0.2 compiles __thread, but segfault at runtime.
OpenBSD 4.7 compiles __thread, but segfault at runtime.
Apple doesn't compile
FreeBSD not tested
AIX probably works, not tested
Solaris: failed to link on Solaris 2.9: http://hudson.modula3.com:8080/job/cm3-current-build-SOLsun-opencsw-current9s/166/console
HP-UX? AIX?
Linux/arm: /usr/bin/ld: /usr/local/cm3/pkg/m3core/ARMEL_LINUX/libm3core.a(ThreadPThreadC.o)(.stab+0x2e28): R_ARM_ABS32 used with TLS symbol activations
*/
#if 0 /* defined(__linux) && !defined(__arm__) */

#define M3_COMPILER_THREAD_LOCAL

static __thread void* activations;

void
__cdecl
ThreadPThread__SetActivation(void *value)
{
  activations = value;
}

void*
__cdecl
ThreadPThread__GetActivation(void)
{
  return activations;
}

#else

static pthread_key_t activations;

void
__cdecl
ThreadPThread__SetActivation(void *value)
{
  int r = { 0 };
  M3_RETRY(pthread_setspecific(activations, value));
  assert(r == 0);
}

void *
__cdecl
ThreadPThread__GetActivation(void)
{
  return pthread_getspecific(activations);
}

#endif

typedef int (*generic_init_t)(void *, const void *);

void *
__cdecl
ThreadPThread_pthread_generic_new(WORD_T size, generic_init_t init)
{
  int r = ENOMEM;
  void *p = calloc(1, size);
  if (p == NULL)
    goto Error;
  M3_RETRY(init(p, NULL));
  if (r == ENOMEM)
    goto Error;
  assert(r == 0);
  if (r != 0)
    goto Error;
  return p;
Error:
  if (r)
  {
    fprintf(stderr, "ERROR: pthread_generic_new:%d\n", r);
    abort();
  }
  if (p) free(p);
  return NULL;
}

#define THREADPTHREAD__PTHREAD_GENERIC_NEW(type) {                      \
    typedef pthread_##type##_t T;                                       \
    typedef pthread_##type##attr_t attr_t;                              \
    typedef int (*init_t)(T *, const attr_t *);                         \
    /* make sure the type matches */                                    \
    init_t init = pthread_##type##_init;                                \
    return ThreadPThread_pthread_generic_new(sizeof(T),                 \
                                             (generic_init_t)init);     \
  }

void *
__cdecl
ThreadPThread__pthread_mutex_new(void)
{
  THREADPTHREAD__PTHREAD_GENERIC_NEW(mutex);
}

void *
__cdecl
ThreadPThread__pthread_cond_new(void)
{
  THREADPTHREAD__PTHREAD_GENERIC_NEW(cond);
}

void
__cdecl
ThreadPThread__pthread_mutex_delete(pthread_mutex_t* p)
{
  int e = { 0 };
  if (p == NULL) return;
#if defined(__hpux) || defined(__osf)
  /* workaround Tru64 5.1 and HP-UX bug: pthread_mutex_destroy()
     intermittently returns EBUSY even when there are no threads accessing the
     mutex. */
  do { e = pthread_mutex_destroy(p); } while (e == EBUSY);
#else
  e = pthread_mutex_destroy(p);
#endif
  if (e)
  {
    if (e == EBUSY)
      fprintf(stderr, "pthread_mutex_destroy:EBUSY\n");
    else
      fprintf(stderr, "pthread_mutex_destroy:%d\n", e);
    abort();
  }
  free(p);
}

void
__cdecl
ThreadPThread__pthread_cond_delete(pthread_cond_t *p)
{
  int r = { 0 };
  if (p == NULL) return;
  r = pthread_cond_destroy(p);
  assert(r == 0);
  free(p);
}

#define BILLION (1000 * 1000 * 1000)

void
__cdecl
ThreadPThread__Nanosleep(INTEGER nanoseconds)
{
#ifdef __INTERIX
  assert(nanoseconds >= 0);
  assert(nanoseconds < BILLION);
  /* This is only an approximation. We don't try to complete the sleep
   * if interrupted, because we don't cheaply know how much time has elapsed.
   */
  usleep(nanoseconds / 1000);
#else
  struct timespec wait;
  struct timespec remaining;

  assert(nanoseconds >= 0);
  assert(nanoseconds < BILLION);
  ZERO_MEMORY(wait);
  ZERO_MEMORY(remaining);
  wait.tv_sec = 0;
  wait.tv_nsec = nanoseconds;
  while (nanosleep(&wait, &remaining) == -1 && errno == EINTR)
      wait = remaining;
#endif
}

M3WRAP2(int, pthread_cond_wait, pthread_cond_t*, pthread_mutex_t*)
M3WRAP1(int, pthread_cond_signal, pthread_cond_t*)
M3WRAP1(int, pthread_cond_broadcast, pthread_cond_t*)

int
__cdecl
ThreadPThread__pthread_cond_timedwait(pthread_cond_t* cond,
                                      pthread_mutex_t* mutex,
                                      LONGREAL m3timeout)
{
  struct timespec timeout;
  double n = { 0 };
  
  ZERO_MEMORY(timeout);
  timeout.tv_nsec = modf(m3timeout, &n) * BILLION;
  timeout.tv_sec = n;
  return pthread_cond_timedwait(cond, mutex, &timeout);
}

int
__cdecl
ThreadPThread__pthread_detach_self(m3_pthread_t t)
{
#if defined(__APPLE__)
  mach_port_t a = PTHREAD_FROM_M3(t);
  mach_port_deallocate(mach_task_self(), a);
#endif
  return pthread_detach(pthread_self());
}

m3_pthread_t
__cdecl
ThreadPThread__pthread_self(void)
{
#if defined(__APPLE__)
  mach_port_t a = mach_thread_self();
  return PTHREAD_TO_M3(a);
#else
  pthread_t a = pthread_self();
  return PTHREAD_TO_M3(a);
#endif
}

int
__cdecl
ThreadPThread__pthread_kill(m3_pthread_t thread, int sig)
{
#if defined(__APPLE__)
  abort();
#else
  return pthread_kill(PTHREAD_FROM_M3(thread), sig);
#endif
}

int
__cdecl
ThreadPThread__pthread_mutex_lock(pthread_mutex_t* mutex)
{
  int a = pthread_mutex_lock(mutex);
  if (a)
  {
    if (a == EINVAL)
      fprintf(stderr, "ERROR: pthread_mutex_lock:EINVAL\n");
    else
      fprintf(stderr, "ERROR: pthread_mutex_lock:%d\n", a);
    abort();
  }
  return a;
}

int
__cdecl
ThreadPThread__pthread_mutex_unlock(pthread_mutex_t* mutex)
{
  int a = pthread_mutex_unlock(mutex);
  if (a)
  {
    fprintf(stderr, "ERROR: pthread_mutex_unlock:%d\n", a);
    abort();
  }
  return a;
}

void
__cdecl
InitC(int *bottom)
{
  int r = { 0 };

#ifndef M3_DIRECT_SUSPEND
  struct sigaction act;
  ZERO_MEMORY(act);
#endif

  stack_grows_down = (bottom > &r);
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__INTERIX)
  assert(stack_grows_down); /* See ThreadApple.c, ThreadFreeBSD.c */
#endif
#ifndef M3_COMPILER_THREAD_LOCAL
  M3_RETRY(pthread_key_create(&activations, NULL)); assert(r == 0);
#endif

#ifndef M3_DIRECT_SUSPEND
  ZERO_MEMORY(act);

  M3_RETRY(sem_init(&ackSem, 0, 0)); assert(r == 0);

  r = sigfillset(&mask); assert(r == 0);
  r = sigdelset(&mask, SIG_SUSPEND); assert(r == 0);
  r = sigdelset(&mask, SIGINT); assert(r == 0);
  r = sigdelset(&mask, SIGQUIT); assert(r == 0);
  r = sigdelset(&mask, SIGABRT); assert(r == 0);
  r = sigdelset(&mask, SIGTERM); assert(r == 0);

  act.sa_flags = SA_RESTART | SA_SIGINFO;
  act.sa_sigaction = SignalHandlerC;
  r = sigfillset(&act.sa_mask); assert(r == 0);
  r = sigaction(SIG_SUSPEND, &act, NULL); assert(r == 0);
#endif
}

M3_EXTERNC_END
