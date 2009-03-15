/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#include <assert.h>

#ifdef __APPLE__
/* MacOSX diverges in a good way and therefore many functions
in this file are just stubs for it, that other code dynamically choses
not to call (statically, but the compiler can't or won't tell). */
#define APPLE_ASSERT_FALSE assert(!"MacOS X should not get here.");
#else
#include <signal.h>
#include <semaphore.h>
#include <string.h>
#ifdef __hpux
#include <stdio.h>
#endif /* hpux */
#if defined(__hpux) || defined(__osf)
#include <errno.h>
#endif /* hpux || osf */
#endif
#include <pthread.h>

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


#define SIG_SUSPEND ThreadPThread__SIG_SUSPEND
 
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
#if defined(__APPLE__)
const int SIG_SUSPEND = 0;
#elif defined(__sun) || defined(__CYGWIN__) || defined(__FreeBSD__)
const int SIG_SUSPEND = SIGUSR2;
#elif defined(__linux)
const int SIG_SUSPEND = NSIG - 1;
#elif defined(__hpux)
const int SIG_SUSPEND = _SIGRTMAX;
#elif defined(SIGRTMAX)
/* This might be a function call, in which case try _SIGRTMAX or
initializing it somewhere. */
const int SIG_SUSPEND = SIGRTMAX;
#elif defined(SIGUSR2)
const int SIG_SUSPEND = SIGUSR2;
#else
#error Unable to determine SIG_SUSPEND.
#endif

#ifndef __APPLE__

#define ZeroMemory(a, b) (memset((a), 0, (b)))

typedef struct sigaction sigaction_t;

#define SignalHandler ThreadPThread__SignalHandler

static sigset_t mask;

/* Signal based suspend/resume */
static sem_t ackSem;

void SignalHandler(int, siginfo_t*, void* /* ucontext_t* */);

#endif /* Apple */

void ThreadPThread__SetupHandlers(void)
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    sigaction_t act;
    sigaction_t oact;
    int r;

    ZeroMemory(&act, sizeof(act));
    ZeroMemory(&oact, sizeof(oact));

    r = sem_init(&ackSem, 0, 0);
    assert(r == 0);

    r = sigfillset(&mask);
    assert(r == 0);
    r = sigdelset(&mask, SIG_SUSPEND);
    assert(r == 0);
    r = sigdelset(&mask, SIGINT);
    assert(r == 0);
    r = sigdelset(&mask, SIGQUIT);
    assert(r == 0);
    r = sigdelset(&mask, SIGABRT);
    assert(r == 0);
    r = sigdelset(&mask, SIGTERM);
    assert(r == 0);

    act.sa_flags = SA_RESTART | SA_SIGINFO;
    act.sa_sigaction = SignalHandler;
    r = sigfillset(&act.sa_mask);
    assert(r == 0);
    r = sigaction(SIG_SUSPEND, &act, &oact);
    assert(r == 0);
#endif
}

int ThreadPThread__sem_wait(void)
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    return sem_wait(&ackSem);
#endif
}

int ThreadPThread__sem_post(void)
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    return sem_post(&ackSem);
#endif
}

int ThreadPThread__sem_getvalue(int* value)
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    return sem_getvalue(&ackSem, value);
#endif
}

int ThreadPThread__sigsuspend(void)
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    return sigsuspend(&mask);
#endif
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
    
    r = pthread_attr_getstacksize(&attr, &bytes);
    assert(r == 0);

    bytes = M3_MAX(bytes, stackSize);
    pthread_attr_setstacksize(&attr, bytes);

    r = pthread_create(pthread, &attr, start_routine, arg);

    pthread_attr_destroy(&attr);

    return r;
}

#define MUTEX(name) \
static pthread_mutex_t name##Mu = PTHREAD_MUTEX_INITIALIZER; \
int \
ThreadPThread__pthread_mutex_lock_##name( \
    void) \
{ \
    return pthread_mutex_lock(&name##Mu); \
} \
 \
int \
ThreadPThread__pthread_mutex_unlock_##name( \
    void) \
{ \
    return pthread_mutex_unlock(&name##Mu); \
} \


#define CONDITION_VARIABLE(name) \
static pthread_cond_t name##Cond = PTHREAD_COND_INITIALIZER; \
int \
ThreadPThread__pthread_cond_broadcast_##name( \
    void) \
{ \
    return pthread_cond_broadcast(&name##Cond); \
} \
 \
int \
ThreadPThread__pthread_cond_wait_##name( \
    void) \
{ \
    return pthread_cond_wait(&name##Cond, &name##Mu); \
} \


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
MUTEX(slot)   /* global lock for thread slot table */
MUTEX(init)   /* global lock for initializers */
MUTEX(perf)
MUTEX(heap)
CONDITION_VARIABLE(heap)
THREAD_LOCAL(activations)
THREAD_LOCAL(handlers)

EXTERN_CONST
int ThreadPThread__sizeof_pthread_mutex_t = sizeof(pthread_mutex_t);

EXTERN_CONST
int ThreadPThread__sizeof_pthread_cond_t = sizeof(pthread_cond_t);

int
ThreadPThread__pthread_mutex_destroy(
    pthread_mutex_t* mutex)
{
#if defined(__hpux) || defined(__osf)
    /* workaround Tru64 5.1 and HP-UX bug
    pthread_mutex_destroy() intermittently returns
    EBUSY even when there are no threads accessing the mutex. */
    int e;
    do
    {
        e = pthread_mutex_destroy(mutex);
    }
    while (e == EBUSY);
    return e;
#else
    return pthread_mutex_destroy(mutex);
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
