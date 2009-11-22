/* Copyright (C) 1994, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */
/*                                                                 */
/* Portions Copyright 1996-2000, Critical Mass, Inc.               */
/* See file COPYRIGHT-CMASS for details.                           */

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

#define CRITSEC(name) \
static CRITICAL_SECTION name##Lock; \
const PCRITICAL_SECTION ThreadWin32__##name##Lock = &name##Lock; \

CRITSEC(active)
CRITSEC(giant)
CRITSEC(heap)
CRITSEC(perf)
CRITSEC(slot)

#define THREAD_LOCAL(name) \
DWORD ThreadWin32__##name = TLS_OUT_OF_INDEXES; \
void* __cdecl ThreadWin32__TlsGetValue_##name(void) \
{ \
    if (ThreadWin32__##name == TLS_OUT_OF_INDEXES) \
        return 0; \
    return TlsGetValue(ThreadWin32__##name); \
} \
BOOL __cdecl ThreadWin32__TlsSetValue_##name(void* a) \
{ \
    if (ThreadWin32__##name == TLS_OUT_OF_INDEXES) \
        return 0; \
    return TlsSetValue(ThreadWin32__##name, a); \
}

THREAD_LOCAL(threadIndex)

void __cdecl ThreadWin32__InitC(void)
{
    assert(ThreadWin32__threadIndex == TLS_OUT_OF_INDEXES);
    InitializeCriticalSection(&activeLock);
    InitializeCriticalSection(&giantLock);
    InitializeCriticalSection(&heapLock);
    InitializeCriticalSection(&perfLock);
    InitializeCriticalSection(&slotLock);
    ThreadWin32__threadIndex = TlsAlloc();
    assert(ThreadWin32__threadIndex != TLS_OUT_OF_INDEXES);
}

#if !defined(InterlockedExchangePointer) && defined(_X86_)
#define InterlockedExchangePointer InterlockedExchangePointer
PVOID InterlockedExchangePointer(PVOID* a, PVOID b)
{
    return (PVOID)InterlockedExchange((PLONG)a, (LONG)b);
}
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
    *(volatile char*)Used;
    *(volatile char*)(Used + info.RegionSize - 1);

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


#ifdef __cplusplus
} /* extern "C" */
#endif
