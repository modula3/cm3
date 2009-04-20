/* Copyright (C) 1994, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */
/*                                                                 */
/* Portions Copyright 1996-2000, Critical Mass, Inc.               */
/* See file COPYRIGHT-CMASS for details.                           */

#include <assert.h>
#include <windows.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const" */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

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

#define PushEFrame              RTHooks__PushEFrame
#define PopEFrame               RTHooks__PopEFrame
#define LockHeap                RTOS__LockHeap
#define UnlockHeap              RTOS__UnlockHeap
#define BroadcastHeap           RTOS__BroadcastHeap
#define WaitHeap                RTOS__WaitHeap
#define GetCurrentHandlers      ThreadF__GetCurrentHandlers
#define SetCurrentHandlers      ThreadF__SetCurrentHandlers
#define cm                      ThreadWin32__cm
#define slotMu                  ThreadWin32__slotMu
#define activeMu                ThreadWin32__activeMu
#define idleMu                  ThreadWin32__idleMu
#define perfMu                  ThreadWin32__perfMu
#define threadIndex             ThreadWin32__threadIndex
#define handlersIndex           ThreadWin32__handlersIndex
#define cs                      ThreadWin32__cs
#define inCritical              ThreadWin32__inCritical
#define allThreads              ThreadWin32__allThreads
#define GetActivation           ThreadWin32__GetActivation
#define SetActivation           ThreadWin32__SetActivation
#define GetActivation           ThreadWin32__GetActivation

/*----------------------------------------- Exceptions, types and globals ---*/

static CRITICAL_SECTION perfMu_x;
EXTERN_CONST PCRITICAL_SECTION perfMu = &perfMu_x;

static CRITICAL_SECTION cm_x;
EXTERN_CONST PCRITICAL_SECTION cm = &cm_x;
    /* Global lock for internals of Mutex and Condition */

static CRITICAL_SECTION activeMu_x;
EXTERN_CONST PCRITICAL_SECTION activeMu = &activeMu_x;
    /* Global lock for list of active threads */
    /* It is illegal to touch *any* traced references while
       holding activeMu because it is needed by SuspendOthers
       which is called by the collector's page fault handler. */

static CRITICAL_SECTION idleMu_x;
EXTERN_CONST PCRITICAL_SECTION idleMu = &idleMu_x;
    /* Global lock for list of idle threads */

static CRITICAL_SECTION slotMu_x;
EXTERN_CONST PCRITICAL_SECTION slotMu = &slotMu_x;
    /* Global lock for thread slot table */

struct _Activation_t;
typedef struct _Activation_t Activation_t;

typedef struct _Frame_t {
    void* next; /* exception handling support */
} Frame_t;

struct _Activation_t
{
    Frame_t* frame; /* exception handling support */

#if 0 /* not yet */

    Activation_t* next;
    Activation_t* prev;
        /* LL = activeMu; global doubly-linked, circular list of all active threads */

    HANDLE handle;
        /* LL = activeMu; thread handle in Windows */

    ADDRESS stackbase;
        /* LL = activeMu; base of thread stack for use by GC */

    INTEGER slot;
        /* LL = slotMu;  index into global array of active, slotted threads */

    /* thread state */
    /* heapState: RTHeapRep.ThreadState; */

#endif
};

/*------------------------------------------------------------------ Self ---*/

DWORD threadIndex;
    /* read-only;  TLS (Thread Local Storage) index */

#if 0

void InitActivations(void)
{
    long r;
    Activation_t* me = (Activation_t*)calloc(1, sizeof(*me));
    assert(me != NULL);

    r = TlsSetValue(threadIndex, me);
    assert(r);
    r = DuplicatHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess, &m->handle, 0, 0, DUPLICATE_SAME_ACCESS);
    assert(r);

    me->next = me;
    me->prev = me;
    InterlockedExchange(&r, r); /* barrier -- writes through me complete before write to allThreads */
    assert(allThreads == NULL);
    allThreads = me;
    InterlockedExchange(&r, r); /* barrier -- write to allThreads completes before return */
}

#endif

void SetActivation(Activation_t* act)
  /* LL = 0 */
{
    int r = TlsSetValue(threadIndex, act);
    assert(r);
}

Activation_t* GetActivation(void)
  /* If not the initial thread and not created by Fork, returns NIL */
  /* LL = 0 */
{
    return (Activation_t*)TlsGetValue(threadIndex);
}

#if 0 /* not yet: slots not C-accessible */

T* Self(void)
  /* If not the initial thread and not created by Fork, returns NIL */
  /* LL = 0 */
{
    Activation_t* me = (Activation_t*)TlsGetValue(threadIndex);
    if (me == NULL) return NULL;

    EnterCriticalSection(slotMu);
        t = slots[me->slot];
    LeaveCriticalSection(slotMu);
    assert((t->act == me) || !"thread with bad slot!");
    return t;
}

#endif

static CRITICAL_SECTION csstorage;
EXTERN_CONST PCRITICAL_SECTION cs = &csstorage;

long inCritical; /* LL = cs */

void LockHeap(void)
{
    EnterCriticalSection(cs);
    INC(inCritical);
}

void UnlockHeap(void)
{
    DEC(inCritical);
    LeaveCriticalSection(cs);
}

void WaitHeap(void)
/* extra layer so that pthread can implement this in C directly */
{
    ThreadWin32__WaitHeap();
}

void BroadcastHeap(void)
/* extra layer so that pthread can implement this in C directly */
{
    ThreadWin32__BroadcastHeap();
}

/*--------------------------------------------- exception handling support --*/

DWORD handlersIndex; /* read-only */

ADDRESS GetCurrentHandlers(void)
{
#if 0 /* not yet */
    return GetActivation()->frame;
#else
    return TlsGetValue(handlersIndex);
#endif
}

void SetCurrentHandlers(Frame_t* h)
/* Note: This is identical to PopEFrame. */
{
#if 0 /* not yet */
    GetActivation()->frame = h;
#else
    TlsSetValue(handlersIndex, h);
#endif
}

void PushEFrame(Frame_t* f)
{
#if 0 /* not yet */
    Activation_t* me = GetActivation();

    f->next = me->frame;
    me->frame = f;
#else
    f->next = TlsGetValue(handlersIndex);
    TlsSetValue(handlersIndex, f);
#endif
}

void PopEFrame(Frame_t* f)
/* Note: This is identical to SetCurrentHandlers. */
{
#if 0 /* not yet */
    GetActivation()->frame = f;
#else
    TlsSetValue(handlersIndex, f);
#endif
}

static DWORD MyTlsAlloc(void)
{
    DWORD a = TlsAlloc();
    assert(a != TLS_OUT_OF_INDEXES);
    return a;
}

void ThreadWin32__InitC(void)
{
    InitializeCriticalSection(&cm_x);
    InitializeCriticalSection(&activeMu_x);
    InitializeCriticalSection(&idleMu_x);
    InitializeCriticalSection(&csstorage);
    InitializeCriticalSection(&slotMu_x);
    InitializeCriticalSection(&perfMu_x);

    threadIndex = MyTlsAlloc();
    handlersIndex = MyTlsAlloc();
}

#ifdef __cplusplus
} /* extern "C" */
#endif
