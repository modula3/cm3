/*
This file implements aggressive logging for ThreadWin32 for debugging purposes.
You have to edit ThreadWin32.m3 to enable it.
The intent is to view the data in the debugger.
A good command is:
dt -oca m3core!ThreadDebugWin32__Log
*/

#ifdef _MSC_VER
#pragma warning(disable:4201) /* nonstandard extension (windows.h) */
#pragma warning(disable:4214) /* nonstandard extension (windows.h) */
#pragma warning(disable:4115) /* named type definition in paren (windows.h) */
#pragma warning(disable:4514) /* unreferenced inline function */
#pragma warning(disable:4127) /* expression is constant */
#pragma warning(disable:4100) /* unused parameter */
#else
#ifndef __cdecl
#define __cdecl /* nothing */
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if 0

/* enabled */

#ifdef _WIN32
#include <windows.h>
#else
#include <stddef.h>
#include <pthread.h>
static unsigned long GetCurrentThreadId(void) { return (unsigned long)(size_t)pthread_self(); }
static long InterlockedIncrement(volatile long* a) { return ++*a; }
#endif

typedef struct _ThreadDebugWin32__LogEntry_t {
/* The names here are terse to produce more compact debugger output. */
    unsigned tm; /* time */
    unsigned long tid; /* thread id */
    const char* f; /* function */
    void* c; /* condition */
    void* m; /* mutex */
    void* t; /* thread */
} ThreadDebugWin32__LogEntry_t;

static ThreadDebugWin32__LogEntry_t ThreadDebugWin32__Log[28000]; /* size can be tuned for the scenario */
static volatile long ThreadDebugWin32__LogCounter;

#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))

#ifndef ReadTimeStampCounter
#ifdef _M_IX86
static unsigned __int64 ReadTimeStampCounter(void)
{
    __asm {
        _emit 0xF
        _emit 0x31
    }
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

static void ThreadDebugWin32__LogEntry(const char* function, void* c, void* m, void* t)
{
    if (function != LockMutex && function != UnlockMutex)
    {
        const unsigned Skip = 0; /* tuned for the scenario to debug */
        unsigned Counter = (unsigned)InterlockedIncrement(&ThreadDebugWin32__LogCounter) - 1;
        if (Counter > Skip)
        {
            {
                ThreadDebugWin32__LogEntry_t* entry = &ThreadDebugWin32__Log[(Counter - Skip) % NUMBER_OF(ThreadDebugWin32__Log)];
                entry->tm = (unsigned)ReadTimeStampCounter();
                entry->tid = GetCurrentThreadId();
                entry->f = function;
                entry->c = c;
                entry->m = m;
                entry->t = t;
            }
        }
    }
}

#define LOG(f) do { ThreadDebugWin32__LogEntry(f, c, m, t); } while(0)

/* scope-based trick to log NULL for functions that don't have the corresponding parameter */
static void* const c;
static void* const m;
static void* const t;

#else

/* disabled */

#define LOG(f) /* nothing */

#endif

void __cdecl ThreadDebugWin32__LockMutex(void* m)
{
    LOG(LockMutex);
}

void __cdecl ThreadDebugWin32__UnlockMutex(void* m)
{
    LOG(UnlockMutex);
}

void __cdecl ThreadDebugWin32__InnerWait(void* m, void* c, void* t /* self */)
{
    LOG("InnerWait");
}

void __cdecl ThreadDebugWin32__InnerTestAlert(void* t /* self */)
{
    LOG("InnerTestAlert");
}

void __cdecl ThreadDebugWin32__AlertWait(void* m, void* c)
{
    LOG("AlertWait");
}

void __cdecl ThreadDebugWin32__Wait(void* m, void* c)
{
    LOG("Wait");
}

void __cdecl ThreadDebugWin32__DequeueHead(void* c)
{
    LOG("DequeueHead");
}

void __cdecl ThreadDebugWin32__Signal(void* c)
{
    LOG("Signal");
}

void __cdecl ThreadDebugWin32__Broadcast(void* c)
{
    LOG("Broadcast");
}

void __cdecl ThreadDebugWin32__Alert(void* t)
{
    LOG("Alert");
}

void __cdecl ThreadDebugWin32__XTestAlert(void* t /* self */)
{
    LOG("XTestAlert");
}

void __cdecl ThreadDebugWin32__TestAlert(void)
{
    LOG("TestAlert");
}

void __cdecl ThreadDebugWin32__Join(void* t)
{
    LOG("Join");
}

void __cdecl ThreadDebugWin32__AlertJoin(void* t)
{
    LOG("AlertJoin");
}

void __cdecl ThreadDebugWin32__RunThread(void)
{
    LOG("RunThread");
}

void __cdecl ThreadDebugWin32__Fork(void)
{
    LOG("Fork");
}

void __cdecl ThreadDebugWin32__LockHeap(void)
{
    LOG("LockHeap");
}

void __cdecl ThreadDebugWin32__UnlockHeap(void)
{
    LOG("UnlockHeap");
}

void __cdecl ThreadDebugWin32__WaitHeap(void)
{
    LOG("WaitHeap");
}

void __cdecl ThreadDebugWin32__BroadcastHeap(void)
{
    LOG("BroadcastHeap");
}

#ifdef __cplusplus
} /* extern "C" */
#endif
