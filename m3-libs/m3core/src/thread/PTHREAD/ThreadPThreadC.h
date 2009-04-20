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

#ifdef __cplusplus
extern "C" {
#endif

void ThreadPThread__InitActivations(void);

void ThreadPThread__SetupHandlers(void);

int ThreadPThread__sem_wait(void);

int ThreadPThread__sem_post(void);

int ThreadPThread__sem_getvalue(int* value);

int ThreadPThread__sigsuspend(void);

typedef void* (*start_routine_t)(void*);

int
ThreadPThread__thread_create(
    pthread_t* pthread,
    size_t stackSize,
    start_routine_t start_routine,
    void* arg);

int
ThreadPThread__pthread_mutex_destroy(
    pthread_mutex_t* mutex);

#if defined(_WIN64)
#define INC(x) (InterlockedIncrement64(&(x)))
#define DEC(x) (InterlockedDecrement64(&(x)))
#elif defined(_WIN32)
#define INC(x) (InterlockedIncrement(&(x)))
#define DEC(x) (InterlockedDecrement(&(x)))
#elif 0 /* not tested __GNUC__ > 3 */
#define INC(x) (__sync_fetch_and_add(&(x), 1))
#define DEC(x) (__sync_fetch_and_add(&(x), -1))
#else
#define INC(x) ((x) += 1)
#define DEC(x) ((x) -= 1)
#endif

typedef void* ADDRESS;
typedef ptrdiff_t INTEGER;

struct _Activation_t;
typedef struct _Activation_t Activation_t;

Activation_t* ThreadPThread__GetActivation(void);
Activation_t* ThreadPThread__Self(void);
void RTOS___LockHeap(void);
void RTOS__UnlockHeap(void);
void RTOS__WaitHeap(void);

#ifdef __cplusplus
} /* extern "C" */
#endif
