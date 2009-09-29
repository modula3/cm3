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
CRITICAL_SECTION ThreadWin32__##name; \
void __cdecl ThreadWin32__EnterCriticalSection_##name(void) {EnterCriticalSection(&ThreadWin32__##name);} \
void __cdecl ThreadWin32__LeaveCriticalSection_##name(void) {LeaveCriticalSection(&ThreadWin32__##name);}

CRITSEC(activeMu)
CRITSEC(cm)
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
    InitializeCriticalSection(&ThreadWin32__cm);
    InitializeCriticalSection(&ThreadWin32__heap);
    InitializeCriticalSection(&ThreadWin32__perfMu);
    InitializeCriticalSection(&ThreadWin32__slotMu);
    ThreadWin32__threadIndex = TlsAlloc();
    assert(ThreadWin32__threadIndex != TLS_OUT_OF_INDEXES);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
