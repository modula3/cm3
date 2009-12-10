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
#include <setjmp.h>

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const" */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

/* Sometimes setjmp saves signal mask, in which case _setjmp does not.
setjmp works, but _setjmp can be much faster. */
#ifndef __sun
#define M3_SETJMP _setjmp
#define M3_LONGJMP _longjmp
#else
#define M3_SETJMP setjmp
#define M3_LONGJMP longjmp
#endif

#if defined(__sparc) || defined(__ia64__) || defined(_M_IA64) || defined(_IA64_)
#define M3_REGISTER_WINDOWS
#endif

#if defined(_AMD64_)
#define STACK_REGISTER Rsp
#elif defined(_X86_)
#define STACK_REGISTER Esp
#elif defined(_IA64_)
#define STACK_REGISTER Rsp
#else
#error unknown architecture
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

static DWORD threadIndex = TLS_OUT_OF_INDEXES;

static void InitLock(PCRITICAL_SECTION* pp, PCRITICAL_SECTION p)
{
    assert(*pp == NULL || *pp == p);
    if (!*pp)
    {
        InitializeCriticalSection(p);
        *pp = p;
    }
}

static void DeleteLock(PCRITICAL_SECTION* pp, PCRITICAL_SECTION p)
{
    assert(*pp == NULL || *pp == p);
    if (*pp)
    {
        DeleteCriticalSection(p);
        *pp = 0;
    }
}

static BOOL stack_grows_down;

HANDLE __cdecl ThreadWin32__InitC(BOOL* bottom)
{
    HANDLE threadHandle = { 0 };
    BOOL success = { 0 };

    stack_grows_down = (bottom > &success);
    assert(stack_grows_down);

    InitLock(&ThreadWin32__activeLock, &activeLock);
    InitLock(&ThreadWin32__giantLock, &giantLock);
    InitLock(&ThreadWin32__heapLock, &heapLock);
    InitLock(&ThreadWin32__perfLock, &perfLock);
    InitLock(&ThreadWin32__slotLock, &slotLock);

    success = (threadIndex != TLS_OUT_OF_INDEXES);
    if (!success)
    {
        threadIndex = TlsAlloc(); /* This CAN fail. */
        success = (threadIndex != TLS_OUT_OF_INDEXES);
    }
    assert(success);
    if (!success)
        goto Exit;

    success = DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(), &threadHandle, 0, 0, DUPLICATE_SAME_ACCESS);
    assert(success);
Exit:
    return threadHandle;
}

void __cdecl ThreadWin32__Cleanup(void)
{
    DeleteLock(&ThreadWin32__activeLock, &activeLock);
    DeleteLock(&ThreadWin32__giantLock, &giantLock);
    DeleteLock(&ThreadWin32__heapLock, &heapLock);
    DeleteLock(&ThreadWin32__perfLock, &perfLock);
    DeleteLock(&ThreadWin32__slotLock, &slotLock);

    if (threadIndex != TLS_OUT_OF_INDEXES)
    {
        TlsFree(threadIndex);
        threadIndex = TLS_OUT_OF_INDEXES;
    }
}

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
    BOOL success = (threadIndex != TLS_OUT_OF_INDEXES);
    assert(success);
    success = TlsSetValue(threadIndex, act); /* NOTE: This CAN fail. */
    assert(success);
    return success;
}

void* __cdecl ThreadWin32__GetActivation(void)
  /* If not the initial thread and not created by Fork, returns NIL */
  /* LL = 0 */
  /* This function is called VERY frequently. */
{
    assert(threadIndex != TLS_OUT_OF_INDEXES);
    return TlsGetValue(threadIndex);
}

void* __cdecl ThreadWin32__StackPointerFromContext(CONTEXT* context)
{
  return (void*)context->STACK_REGISTER;
}

void __cdecl ThreadWin32__ProcessStopped(
    char* stackStart,
    char* stackEnd,
    CONTEXT* context,
    void (*p)(void* start, void* limit))
{
  volatile char assertReadable;
  char* top;

  /* stack bounds are not yet set or have been cleared;
     therefore the thread either doesn't yet have any traced
     references or no longer has any */

  if (!stackStart || !stackEnd)
      return;

  /* process stack */

  assert(context);
  assert(stackStart < stackEnd);
  assert(stack_grows_down);
  top = (char*)context->STACK_REGISTER;  ;
  assert(top <= stackEnd);
  assert(top);
  assertReadable = *(volatile char*)(stackEnd - 1);
  p(top, stackEnd);

 /* process registers */

#ifdef _X86_
  p(&context->Edi, &context->Eip); /* carefully pick out just a few registers */
#else
  p(context, context + 1); /* just do the whole thing */
#endif

    /* ?? handle register stack; don't know if this is correct ?? */

#ifdef _IA64_
  top = (char*)context->RsBSP;
  assert(stackEnd <= top);
  p(stackEnd, top);
#endif
}

void __cdecl ThreadWin32__ProcessLive(char *bottom, void (*p)(void *start, void *limit))
{
  jmp_buf jb;

  if (M3_SETJMP(jb) == 0) /* save registers to stack */
#ifdef M3_REGISTER_WINDOWS
    M3_LONGJMP(jb, 1); /* flush register windows */
  else
#endif
  {
    char *top = (char*)&top;
    assert(bottom);
    assert(stack_grows_down);
    assert(top < bottom);
    p(top, bottom);
    p(&jb, ((char *)&jb) + sizeof(jb));
#ifdef _IA64_
#error ia64?
    p(bottom, __getReg(?)); /* bsp? bspstore? try numbers until we find it? */
#endif
  }
}

PCONTEXT __cdecl ThreadWin32__NewContext(void)
{
    PCONTEXT context = (PCONTEXT)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(*context));
    if (context)
        context->ContextFlags = (CONTEXT_CONTROL | CONTEXT_INTEGER);
    return context;
}

void __cdecl ThreadWin32__DeleteContext(void* p)
{
    HeapFree(GetProcessHeap(), 0, p);
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
        return !!ThreadWin32__InitC();
    }

    return TRUE;
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
