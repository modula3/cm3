/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#include <assert.h>
#include <stdlib.h>
#include <errno.h>
#include <pthread.h>

#ifdef __APPLE__
/* MacOSX diverges in a good way and therefore many functions
in this file are just stubs for it, that other code dynamically choses
not to call (statically, but the compiler can't or won't tell). */
#define APPLE_ASSERT_FALSE assert(0 && "MacOS X should not get here.");
#else
#include <signal.h>
#include <semaphore.h>
#include <string.h>
#ifdef __hpux
#include <stdio.h>
#endif /* hpux */
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
#if defined(__APPLE__)
EXTERN_CONST int SIG_SUSPEND = 0;
#elif defined(__sun) || defined(__CYGWIN__) || defined(__FreeBSD__)
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

#ifndef __APPLE__

#define ZeroMemory(a, b) (memset((a), 0, (b)))

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

#else /* Apple */

void SetupHandlers(void)                { APPLE_ASSERT_FALSE }
void ThreadPThread__sem_wait(void)      { APPLE_ASSERT_FALSE }
void ThreadPThread__sem_post(void)      { APPLE_ASSERT_FALSE }
void ThreadPThread__sem_getvalue(void)  { APPLE_ASSERT_FALSE }
void ThreadPThread__sigsuspend(void)    { APPLE_ASSERT_FALSE }

#endif /* Apple */

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
int ThreadPThread__pthread_mutex_lock_##name(void) \
{ \
    return pthread_mutex_lock(&name##Mu); \
} \
 \
int ThreadPThread__pthread_mutex_unlock_##name(void) \
{ \
    return pthread_mutex_unlock(&name##Mu); \
} \


#define CONDITION_VARIABLE(name) \
static pthread_cond_t name##Cond = PTHREAD_COND_INITIALIZER; \
int ThreadPThread__pthread_cond_broadcast_##name(void) \
{ \
    return pthread_cond_broadcast(&name##Cond); \
} \
 \
int ThreadPThread__pthread_cond_wait_##name(void) \
{ \
    return pthread_cond_wait(&name##Cond, &name##Mu); \
} \


#define THREAD_LOCAL_FAST(name) \
static __thread void* name; \
int ThreadPThread__pthread_key_create_##name(void) \
{ \
    /* nothing */ \
} \
int ThreadPThread__pthread_setspecific_##name(void* value) \
{ \
    name = value; \
} \
void* ThreadPThread__pthread_getspecific_##name(void) \
{ \
    return name; \
} \

#define THREAD_LOCAL_SLOW(name) \
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

#if 0 /* M3CONFIG_THREAD_LOCAL_STORAGE */
#define THREAD_LOCAL(name) THREAD_LOCAL_FAST(name)
#else
#define THREAD_LOCAL(name) THREAD_LOCAL_SLOW(name)
#endif

/* activeMu slotMu initMu perfMu heapMu heapCond */

MUTEX(active) /* global lock for list of active threads */
MUTEX(slot)   /* global lock for thread slot table */
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


#ifdef __cplusplus
} /* extern "C" */
#endif
