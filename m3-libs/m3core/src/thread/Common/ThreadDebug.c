/*
This file implements aggressive logging for ThreadWin32 for debugging purposes.
You have to edit ThreadWin32.m3 to enable it.
The intent is to view the data in the debugger.
A good command is:
    dt -oca10 m3core!ThreadDebug__Log
The code is also portable and could be used for ThreadPThread.m3 if desired.
*/

#ifdef _MSC_VER
#pragma warning(disable:4201) /* nonstandard extension (windows.h) */
#pragma warning(disable:4214) /* nonstandard extension (windows.h) */
#pragma warning(disable:4115) /* named type definition in paren (windows.h) */
#pragma warning(disable:4514) /* unreferenced inline function */
#pragma warning(disable:4127) /* expression is constant */
#pragma warning(disable:4100) /* unused parameter */
#endif

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef _WIN32
#include <windows.h>
#endif

#if defined(__GNUC__)
#include <x86intrin.h>
#elif defined(_MSC_VER)
#include <intrin.h>
#pragma intrinsic(__rdtsc)
#endif


#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32 /* #if 1 ok here */

/* enabled */

#ifndef _WIN32
static unsigned long GetCurrentThreadId(void) { return (unsigned long)(WORD_T)pthread_self(); }
static long InterlockedIncrement(volatile long* a) { return ++*a; }
#endif

typedef struct _ThreadDebug__LogEntry_t {
/* The names here are terse to produce more compact debugger output. */
    unsigned tm;        /* time */
    unsigned long tid;  /* thread id */
    const char* f;      /* function */
    void* c;            /* condition */
    void* m;            /* mutex */
    void* t;            /* thread */
    void* e;            /* event */
    unsigned long to;   /* timeout */
} ThreadDebug__LogEntry_t;

static ThreadDebug__LogEntry_t ThreadDebug__Log[10]; /* size can be tuned for the scenario */
static volatile long ThreadDebug__LogCounter;

#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))

#ifndef ReadTimeStampCounter
#ifdef _M_IX86
static unsigned __int64
__cdecl
ReadTimeStampCounter(void)
{
   return __rdtsc();
}
#else
static unsigned BogusTimeStampCounter;
static unsigned ReadTimeStampCounter(void)
{
    return ++BogusTimeStampCounter;
}
#endif
#endif

static const char LockMutex[] = "LockMutex";
static const char UnlockMutex[] = "UnlockMutex";

static void __cdecl
ThreadDebug__LogEntry(const char* function, void* c, void* m, void* t, void* event, unsigned long timeout)
{
    if (function != LockMutex && function != UnlockMutex)
    {
        const unsigned Skip = 0; /* tuned for the scenario to debug */
        unsigned Counter = (unsigned)InterlockedIncrement(&ThreadDebug__LogCounter) - 1;
        if (Counter > Skip)
        {
            ThreadDebug__LogEntry_t* entry = &ThreadDebug__Log[(Counter - Skip) % NUMBER_OF(ThreadDebug__Log)];
            entry->tm = (unsigned)ReadTimeStampCounter();
            entry->tid = GetCurrentThreadId();
            entry->f = function;
            entry->c = c;
            entry->m = m;
            entry->t = t;
            entry->e = event;
            entry->to = timeout;
        }
    }
}

#define LOG(f) do { ThreadDebug__LogEntry(f, c, m, t, event, timeout); } while(0)

/* scope-based trick to log NULL for functions that don't have the corresponding parameter */
static void* const c = 0;
static void* const m = 0;
static void* const t = 0;
static void* const event = 0;
static unsigned long const timeout = 0;

#else

/* disabled */

#define LOG(f) /* nothing */

#endif

void __cdecl
ThreadDebug__LockMutex(void* m)
{
    LOG(LockMutex);
}

void __cdecl
ThreadDebug__UnlockMutex(void* m)
{
    LOG(UnlockMutex);
}

void __cdecl
ThreadDebug__InnerWait(void* m, void* c, void* t /* self */)
{
    LOG("InnerWait");
}

void __cdecl
ThreadDebug__Event_Wait(void* event, unsigned long timeout)
{
    LOG("Event_Wait");
}

void __cdecl
ThreadDebug__Event_Signal(void* event)
{
    LOG("Event_Signal");
}

void __cdecl
ThreadDebug__XWait(void* m, void* c, void* t /* self */)
{
    LOG("XWait");
}

void __cdecl
ThreadDebug__XPause(void* t /*self */, double n, int a /* alertable */)
{
    LOG("XPause");
}

void __cdecl
ThreadDebug__Pause(void* t /*self */, double n)
{
    LOG("Pause");
}

void __cdecl
ThreadDebug__AlertPause(void* t /*self */, double n)
{
    LOG("AlertPause");
}

void __cdecl
ThreadDebug__InnerTestAlert(void* t /* self */)
{
    LOG("InnerTestAlert");
}

void __cdecl
ThreadDebug__AlertWait(void* m, void* c)
{
    LOG("AlertWait");
}

void __cdecl
ThreadDebug__Wait(void* m, void* c)
{
    LOG("Wait");
}

void __cdecl
ThreadDebug__DequeueHead(void* c)
{
    LOG("DequeueHead");
}

void __cdecl
ThreadDebug__Signal(void* c)
{
    LOG("Signal");
}

void __cdecl
ThreadDebug__Broadcast(void* c)
{
    LOG("Broadcast");
}

void __cdecl
ThreadDebug__Alert(void* t)
{
    LOG("Alert");
}

void __cdecl
ThreadDebug__XTestAlert(void* t /* self */)
{
    LOG("XTestAlert");
}

void __cdecl
ThreadDebug__TestAlert(void)
{
    LOG("TestAlert");
}

void __cdecl
ThreadDebug__Join(void* t)
{
    LOG("Join");
}

void __cdecl
ThreadDebug__AlertJoin(void* t)
{
    LOG("AlertJoin");
}

void __cdecl
ThreadDebug__RunThread(void)
{
    LOG("RunThread");
}

void __cdecl
ThreadDebug__Fork(void)
{
    LOG("Fork");
}

void __cdecl
ThreadDebug__LockHeap(void)
{
    LOG("LockHeap");
}

void __cdecl
ThreadDebug__UnlockHeap(void)
{
    LOG("UnlockHeap");
}

void __cdecl
ThreadDebug__WaitHeap(void)
{
    LOG("WaitHeap");
}

void __cdecl
ThreadDebug__BroadcastHeap(void)
{
    LOG("BroadcastHeap");
}

#ifdef __cplusplus
} /* extern "C" */
#endif
