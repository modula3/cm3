/* Copyright (C) 1994, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */
/*                                                                 */
/* Portions Copyright 1996-2000, Critical Mass, Inc.               */
/* See file COPYRIGHT-CMASS for details.                           */

#ifdef _MSC_VER
struct IRpcStubBuffer;        /* warning 4115: named type definition in parentheses */
#pragma warning(disable:4201) /* nonstandard extension: nameless struct/union */
#pragma warning(disable:4209) /* nonstandard extension: benign re-typedef */
#pragma warning(disable:4214) /* nonstandard extension: bitfield other than int */
#pragma warning(disable:4514) /* unused inline function removed */
#if _MSC_VER <= 1100
#pragma warning(disable:4024) /* volatile mismatch on Interlocked */
#pragma warning(disable:4090) /* volatile mismatch on Interlocked */
#endif
#endif

#include <windows.h>
#include <assert.h>

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

#define LOCK(name) \
static CRITICAL_SECTION name##Lock; \
PCRITICAL_SECTION ThreadWin32__##name##Lock; \

LOCK(active) /* global lock for list of active threads */
LOCK(slot)   /* global lock for thread slots table which maps untraced to traced */
LOCK(giant)
LOCK(heap)
LOCK(perf)

DWORD ThreadWin32__threadIndex = TLS_OUT_OF_INDEXES;

static void InitLock(PCRITICAL_SECTION* pp, PCRITICAL_SECTION p)
{
    assert(*pp == NULL || *pp == p);
    if (*pp)
        return;
    InitializeCriticalSection(p);
    *pp = p;
}

static void DeleteLock(PCRITICAL_SECTION* pp, PCRITICAL_SECTION p)
{
    assert(*pp == NULL || *pp == p);
    if (!*pp)
        return;
    DeleteCriticalSection(p);
    *pp = 0;
}

BOOL __cdecl ThreadWin32__InitC(void)
{
    InitLock(&ThreadWin32__activeLock, &activeLock);
    InitLock(&ThreadWin32__giantLock, &giantLock);
    InitLock(&ThreadWin32__heapLock, &heapLock);
    InitLock(&ThreadWin32__perfLock, &perfLock);
    InitLock(&ThreadWin32__slotLock, &slotLock);

    if (ThreadWin32__threadIndex == TLS_OUT_OF_INDEXES)
        ThreadWin32__threadIndex = TlsAlloc(); /* This CAN fail. */

    return (ThreadWin32__threadIndex != TLS_OUT_OF_INDEXES);
}

void __cdecl ThreadWin32__Cleanup(void)
{
    DeleteLock(&ThreadWin32__activeLock, &activeLock);
    DeleteLock(&ThreadWin32__giantLock, &giantLock);
    DeleteLock(&ThreadWin32__heapLock, &heapLock);
    DeleteLock(&ThreadWin32__perfLock, &perfLock);
    DeleteLock(&ThreadWin32__slotLock, &slotLock);

    if (ThreadWin32__threadIndex != TLS_OUT_OF_INDEXES)
        TlsFree(ThreadWin32__threadIndex);

    ThreadWin32__threadIndex = TLS_OUT_OF_INDEXES;
}

#if 0
#if !defined(InterlockedExchangePointer) && defined(_X86_)
#define InterlockedExchangePointer InterlockedExchangePointer
PVOID InterlockedExchangePointer(PVOID* a, PVOID b)
{
    return (PVOID)InterlockedExchange((PLONG)a, (LONG)b);
}
#endif
#endif

#ifndef MemoryBarrier
void __cdecl WinNT__MemoryBarrier(void);
#define MemoryBarrier WinNT__MemoryBarrier
#endif

LONG __cdecl ThreadWin32__InterlockedRead(volatile LONG* a)
{ /* based on Boost */
    LONG b;
    MemoryBarrier();
    b = *a;
    MemoryBarrier();
    return b;
}

void __cdecl ThreadWin32__InterlockedIncrement(volatile LONG* a)
{
    InterlockedIncrement(a);
}

void __cdecl ThreadWin32__InterlockedDecrement(volatile LONG* a)
{
    InterlockedDecrement(a);
}

void __cdecl ThreadWin32__GetStackBounds(void** start, void** end)
{
    MEMORY_BASIC_INFORMATION info = { 0 };
    size_t a = VirtualQuery(&info, &info, sizeof(info));

    /* how far down has the stack been used so far */
    char* Used = (char*)info.BaseAddress;

    /* how far down the stack can grow */
    char* Available = (char*)info.AllocationBase;

    assert(a >= sizeof(info));
    assert(Available);
    assert(Used);
    assert(info.RegionSize);
    assert(((char*)&info) > Available);
    assert(((char*)&info) >= Used);
    assert(((char*)&info) < Used + info.RegionSize);

    /* verify it is readable
    NOTE: Do not verify *Available -- stack pages must be touched in order. */
    a = *(volatile unsigned char*)Used;
    a = *(volatile unsigned char*)(Used + info.RegionSize - 1);

    *start = Available;
    *end = Used + info.RegionSize;
}

PCRITICAL_SECTION __cdecl ThreadWin32__NewLock(void)
{
    PCRITICAL_SECTION lock = (PCRITICAL_SECTION)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(*lock));
    if (lock)
        InitializeCriticalSection(lock);
    return lock;
}

void __cdecl ThreadWin32__Lock(PCRITICAL_SECTION lock)
{
    EnterCriticalSection(lock);
}

void __cdecl ThreadWin32__Unlock(PCRITICAL_SECTION lock)
{
    LeaveCriticalSection(lock);
}

void __cdecl ThreadWin32__DeleteLock(PCRITICAL_SECTION lock)
{
    if (!lock)
        return;
    DeleteCriticalSection(lock);
    HeapFree(GetProcessHeap(), 0, lock);
}

BOOL __cdecl ThreadWin32__SetActivation(void* act)
  /* LL = 0 */
{
    assert(ThreadWin32__threadIndex != TLS_OUT_OF_INDEXES);
    return TlsSetValue(ThreadWin32__threadIndex, act); /* NOTE: This CAN fail. */
}

void* __cdecl ThreadWin32__GetActivation(void)
  /* If not the initial thread and not created by Fork, returns NIL */
  /* LL = 0 */
  /* This function is called VERY frequently. */
{
    assert(ThreadWin32__threadIndex != TLS_OUT_OF_INDEXES);
    return TlsGetValue(ThreadWin32__threadIndex);
}

#if 0

BOOL WINAPI DllMain(HANDLE DllHandle, DWORD Reason, PVOID Static)
{
    switch (Reason)
    {
    case DLL_THREAD_DETACH:
        break;

    case DLL_THREAD_ATTACH:
        /* SetActivation belongs here (and allocating it) */
        break;

    case DLL_PROCESS_DETACH:
        if (Static)
            return TRUE; /* no need for any cleanup */
        return ThreadWin32__Cleanup();

    case DLL_PROCESS_ATTACH:
        /* Module initializers belong here */.
        return ThreadWin32__InitC();
    }

    return TRUE;
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
