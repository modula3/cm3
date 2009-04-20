/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#include <assert.h>
#include "ThreadPThreadC.h"

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

/* establish synonyms for two level names */

#define PushEFrame              RTHooks__PushEFrame
#define PopEFrame               RTHooks__PopEFrame
#define LockHeap                RTOS__LockHeap
#define UnlockHeap              RTOS__UnlockHeap
#define BroadcastHeap           RTOS__BroadcastHeap
#define WaitHeap                RTOS__WaitHeap
#define GetCurrentHandlers      ThreadF__GetCurrentHandlers
#define SetCurrentHandlers      ThreadF__SetCurrentHandlers
#define allThreads              ThreadPThread__allThreads
#define GetActivation           ThreadPThread__GetActivation
#define SignalHandler           ThreadPThread__SignalHandler
#define SetActivation           ThreadPThread__SetActivation
#define GetActivation           ThreadPThread__GetActivation
#define Self                    ThreadPThread__Self
#define InitActivations         ThreadPThread__InitActivations
#define SIG_SUSPEND             ThreadPThread__SIG_SUSPEND
#define sizeof_pthread_mutex_t  ThreadPThread__sizeof_pthread_mutex_t
#define sizeof_pthread_cond_t   ThreadPThread__sizeof_pthread_cond_t
#define SetupHandlers           ThreadPThread__SetupHandlers

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

static sigset_t mask;

/* Signal based suspend/resume */
static sem_t ackSem;

void SignalHandler(int, siginfo_t*, void* /* ucontext_t* */);

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

    act.sa_flags = SA_RESTART | SA_SIGINFO;
    act.sa_sigaction = SignalHandler;
    r = sigfillset(&act.sa_mask); assert(r == 0);
    r = sigaction(SIG_SUSPEND, &act, &oact); assert(r == 0);
}

int ThreadPThread__sem_wait(void) { return sem_wait(&ackSem); }
int ThreadPThread__sem_post(void) { return sem_post(&ackSem); }
int ThreadPThread__sem_getvalue(int* value) { return sem_getvalue(&ackSem, value); }
int ThreadPThread__sigsuspend(void) { return sigsuspend(&mask); }

#else /* APPLE */

void ThreadPThread__SetupHandlers(void) { APPLE_ASSERT_FALSE }
int  ThreadPThread__sem_wait(void) { APPLE_ASSERT_FALSE }
int  ThreadPThread__sem_post(void) { APPLE_ASSERT_FALSE }
int  ThreadPThread__sem_getvalue(int* value) { APPLE_ASSERT_FALSE }
int  ThreadPThread__sigsuspend(void) { APPLE_ASSERT_FALSE }

#endif /* APPLE */

#define M3_MAX(x, y) (((x) > (y)) ? (x) : (y))

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

/* activeMu slotMu initMu perfMu heapMu heapCond */

MUTEX(active) /* global lock for list of active threads */
MUTEX(slot)   /* global lock for thread slot table */
MUTEX(init)   /* global lock for initializers */
MUTEX(perf)
static pthread_mutex_t heapMu = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t heapCond = PTHREAD_COND_INITIALIZER;
THREAD_LOCAL(activations)

EXTERN_CONST int sizeof_pthread_mutex_t = sizeof(pthread_mutex_t);
EXTERN_CONST int sizeof_pthread_cond_t = sizeof(pthread_cond_t);

int
ThreadPThread__pthread_mutex_destroy(
    pthread_mutex_t* mutex)
{
#if defined(__hpux) || defined(__osf)
    /* workaround Tru64 5.1 and HP-UX bug
    pthread_mutex_destroy() intermittently returns
    EBUSY even when there are no threads accessing the mutex. */
    int e;
    while ((e = pthread_mutex_destroy(mutex)) == EBUSY) { }
    return e;
#else
    return pthread_mutex_destroy(mutex);
#endif
}

typedef enum ActState_t { Starting, Started, Stopping, Stopped } ActState_t;

typedef struct _Frame_t {
    void* next; /* exception handling support */
} Frame_t;

struct _Activation_t {

    Frame_t* frame; /* exception handling support */

#if 0 /* not yet -- in particular need to deal with floatState, heapState */

    /* global doubly-linked, circular list of all active threads */
    Activation_t* next;      /* LL = activeMu */
    Activation_t* prev;      /* LL = activeMu */

    /* thread handle */
    union {
        pthread_t pthread;  /* LL = activeMu */
        void* pad;
    } handle;
    /* base of thread stack for use by GC */
    ADDRESS stackbase;          /* LL = activeMu */
    ADDRESS sp;                 /* LL = activeMu */
    INTEGER size;               /* LL = activeMu */

    /* What is the size of this? */
    ActState_t state;           /* LL = activeMu */

    /* index into global array of active, slotted threads */
    INTEGER slot;               /* LL = slotMu */

    /* state that is available to the floating point routines */
    /* floatState : FloatMode.ThreadState; */

    /* state that is available to the heap routines */
    /* heapState : RTHeapRep.ThreadState; */
#endif
};

/* LL=activeMu */
Activation_t* allThreads = NULL; /* global list of active threads */

void
SetActivation(
    Activation_t* act)
/* LL = 0 */
{
    int r;
    if (allThreads == NULL) InitActivations();
    r = pthread_setspecific(activations, act); assert(r == 0);
}

Activation_t*
GetActivationUnsafeFast(
    void)
/* If not the initial thread and not created by Fork, returns NIL */
/* LL = 0 */
{
    return (Activation_t*)pthread_getspecific(activations);
}

Activation_t*
GetActivation(
    void)
/* If not the initial thread and not created by Fork, returns NIL */
/* LL = 0 */
{
    if (allThreads == NULL) InitActivations();
    return GetActivationUnsafeFast();
}

#if 0 /* not yet: slots not accessible */

void*
Self(
    void)
/* If not the initial thread and not created by Fork, returns NIL */
/* LL = 0 */
{
    void* me = GetActivation();
    void* t;
    int r;

    if (me == NULL) return NULL;
    r = pthread_mutex_lock(&slotMu); assert(r == 0);
        t = slots[me->slot];
    r = pthread_mutex_unlock(&slotMu); assert(r == 0);
    assert((t->act == me) || !"thread with bad slot!");
    return t;
}

#endif

/*------------------------------------------------------------- collector ---*/
/* These procedures provide synchronization primitives for the allocator
   and collector. */

static Activation_t* holder;
static INTEGER inCritical;

void LockHeap(void)
{
    Activation_t* me = GetActivation();

    if (holder != me)
    {
        int r = pthread_mutex_lock(&heapMu); assert(r == 0);
        holder = me;
    }
    INC(inCritical);
}

void UnlockHeap(void)
{
    int r;
    Activation_t* me = GetActivation();

    assert(holder == me);
    if (DEC(inCritical) == 0)
    {
        holder = NULL;
        r = pthread_mutex_unlock(&heapMu); assert(r == 0);
    }
}

void WaitHeap(void)
{
    int r;
    Activation_t* me = GetActivation();

    assert(holder == me);
    DEC(inCritical);
    assert(inCritical == 0);
    r = pthread_cond_wait(&heapCond, &heapMu); assert(r == 0);
    holder = me;
    assert(inCritical == 0);
    INC(inCritical);
}

void BroadcastHeap(void)
{
    int r = pthread_cond_broadcast(&heapCond); assert(r == 0);
}

/*--------------------------------------------- exception handling support --*/

ADDRESS GetCurrentHandlers(void)
{
    return GetActivation()->frame;
}

void SetCurrentHandlers(Frame_t* h)
/* Note: This is almost identical to PopEFrame. */
{
    GetActivation()->frame = h;
}

void PushEFrame(Frame_t* f)
{
    Activation_t* me = GetActivation();

    f->next = me->frame;
    me->frame = f;
}

void PopEFrame(Frame_t* f)
/* Note: This is almost identical to SetCurrentHandlers. */
{
    GetActivationUnsafeFast()->frame = f;
}

/*---------------------------------------------------------------------------*/

#ifdef __cplusplus
} /* extern "C" */
#endif
