/* Copyright (C) 1994, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */
/*                                                                 */
/* Portions Copyright 1996-2000, Critical Mass, Inc.               */
/* See file COPYRIGHT-CMASS for details.                           */

#include <windows.h>
#include <assert.h>

void __cdecl ThreadWin32__MemoryBarrier(void)
{
#if defined(__sun)
    membar_producer();
    membar_consumer();
#elif __GNUC__ >= 4
    __sync_synchronize();
#elif defined(_WIN32) && defined(MemoryBarrier)
    MemoryBarrier();
#elif defined(_WIN32) && defined(_M_IX86) && !defined(_M_CEE_PURE)
    LONG Barrier;
    __asm {
        xchg Barrier, eax
    }
#elif __GNUC__ >= 3 && __i386__
    long Barrier;
    asm volatile("xchg %0, %%eax"::"m"(Barrier):"%eax");
#else
#error ThreadWin32__MemoryBarrier not implemented.
#endif
}

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
CRITICAL_SECTION ThreadWin32__##name; \
void __cdecl ThreadWin32__EnterCriticalSection_##name(void) {EnterCriticalSection(&ThreadWin32__##name);} \
void __cdecl ThreadWin32__LeaveCriticalSection_##name(void) {LeaveCriticalSection(&ThreadWin32__##name);}

CRITSEC(activeMu)
CRITSEC(giant)
CRITSEC(heap)
CRITSEC(perfMu)
CRITSEC(slotMu)

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
    InitializeCriticalSection(&ThreadWin32__activeMu);
    InitializeCriticalSection(&ThreadWin32__giant);
    InitializeCriticalSection(&ThreadWin32__heap);
    InitializeCriticalSection(&ThreadWin32__perfMu);
    InitializeCriticalSection(&ThreadWin32__slotMu);
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


LONG __cdecl ThreadWin32__InterlockedRead(volatile LONG* a)
{ /* based on Boost */
    LONG b;
    ThreadWin32__MemoryBarrier();
    b = *a;
    ThreadWin32__MemoryBarrier();
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

#ifdef __cplusplus
} /* extern "C" */
#endif
