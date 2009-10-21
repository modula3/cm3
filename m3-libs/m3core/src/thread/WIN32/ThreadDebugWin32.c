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
#endif
#include <windows.h>
WINBASEAPI
BOOL
WINAPI
IsDebuggerPresent(
    VOID
    );

#ifdef __cplusplus
extern "C" {
#endif

#if 1
    
/* enabled */

typedef struct _ThreadDebugWin32__LogEntry_t {
/* The names here are terse to produce more compact debugger output. */
    UINT tm; /* time */
    UINT tid; /* thread id */
    PCSTR f; /* function */
    PVOID c; /* condition */
    PVOID m; /* mutex */
    PVOID t; /* thread */
} ThreadDebugWin32__LogEntry_t;

static ThreadDebugWin32__LogEntry_t ThreadDebugWin32__Log[4000]; /* size can be tuned for the scenario */
static LONG ThreadDebugWin32__LogCounter;

#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))

#ifndef ReadTimeStampCounter
unsigned __int64 ReadTimeStampCounter(VOID)
{
    __asm {
        _emit 0xF
        _emit 0x31
    }
}
#endif

static const char LockMutex[] = "LockMutex";
static const char UnlockMutex[] = "UnlockMutex";

static VOID ThreadDebugWin32__LogEntry(PCSTR function, PVOID c, PVOID m, PVOID t)
{
    const UINT Skip = 1201000; /* tuned for the scenario to debug */
    UINT Counter = (UINT)InterlockedIncrement(&ThreadDebugWin32__LogCounter);
    if (Counter > Skip)
    {
        /*if (function != LockMutex && function != UnlockMutex)*/
        {
            ThreadDebugWin32__LogEntry_t* entry = &ThreadDebugWin32__Log[(Counter - Skip) % NUMBER_OF(ThreadDebugWin32__Log)];
            entry->tm = (UINT)ReadTimeStampCounter();
            entry->tid = GetCurrentThreadId();
            entry->f = function;
            entry->c = c;
            entry->m = m;
            entry->t = t;
        }
    }
}

#define LOG(f) do { ThreadDebugWin32__LogEntry(f, c, m, t); } while(0)

/* scope-based trick to log NULL for functions that don't have the corresponding parameter */
static const PVOID c;
static const PVOID m;
static const PVOID t;

#else

/* disabled */

#define LOG(f) /* nothing */

#endif

VOID __cdecl ThreadDebugWin32__LockMutex(PVOID m)
{
    LOG(LockMutex);
}

VOID __cdecl ThreadDebugWin32__UnlockMutex(PVOID m)
{
    LOG(UnlockMutex);
}

VOID __cdecl ThreadDebugWin32__InnerWait(PVOID m, PVOID c, PVOID t /* self */)
{
    LOG("InnerWait");
}

VOID __cdecl ThreadDebugWin32__InnerTestAlert(PVOID t /* self */)
{
    LOG("InnerTestAlert");
}

VOID __cdecl ThreadDebugWin32__AlertWait(PVOID m, PVOID c)
{
    LOG("AlertWait");
}

VOID __cdecl ThreadDebugWin32__Wait(PVOID m, PVOID c)
{
    LOG("Wait");
}

VOID __cdecl ThreadDebugWin32__DequeueHead(PVOID c)
{
    LOG("DequeueHead");
}

VOID __cdecl ThreadDebugWin32__Signal(PVOID c)
{
    LOG("Signal");
}

VOID __cdecl ThreadDebugWin32__Broadcast(PVOID c)
{
    LOG("Broadcast");
}

VOID __cdecl ThreadDebugWin32__Alert(PVOID t)
{
    LOG("Alert");
}

VOID __cdecl ThreadDebugWin32__XTestAlert(PVOID t /* self */)
{
    LOG("XTestAlert");
}

VOID __cdecl ThreadDebugWin32__TestAlert(VOID)
{
    LOG("TestAlert");
}

VOID __cdecl ThreadDebugWin32__Join(PVOID t)
{
    LOG("Join", t);
}

VOID __cdecl ThreadDebugWin32__AlertJoin(PVOID t)
{
    LOG("AlertJoin", t);
}

VOID __cdecl ThreadDebugWin32__RunThread(VOID)
{
    LOG("RunThread");
}

VOID __cdecl ThreadDebugWin32__Fork(VOID)
{
    LOG("Fork");
}

#ifdef __cplusplus
} /* extern "C" */
#endif
