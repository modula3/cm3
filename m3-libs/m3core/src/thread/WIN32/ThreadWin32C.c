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

#ifdef __cplusplus
extern "C" {
#endif

/*-------------------------------------------------------------------------*/
/* lock-free support */

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

/*-------------------------------------------------------------------------*/
/* LockE_t E = exclusive */

#if 0

/* see C:\src\jdk-6u14-ea-src-b05-jrl-23_apr_2009\hotspot\agent\src\os\win32\Monitor.cpp */

typedef struct _LockE_t {
    volatile long count;
    volatile DWORD owner;
    volatile HANDLE event;
} LockE_t;

#define LOCKE(name) \
static LockE_t name##Lock; \
LockE_t* ThreadWin32__##name##Lock; \

static void InitLockE(LockE_t** pp, LockE_t* p)
/* E = exclusive */
{
    MemoryBarrier();
    assert(*pp == NULL || *pp == p);
    if (!*pp && p)
    {
        HANDLE event;
        p->owner = 0;
        p->count = -1;
        event = CreateEventA(0, 0, 0, 0);
        if (event)
        {
            p->event = event;
            MemoryBarrier();
            *pp = p;
        }
    }
    MemoryBarrier();
}

static void DeleteLockE(LockE_t** pp, LockE_t* p)
/* E = exclusive */
{
    MemoryBarrier();
    assert(*pp == NULL || *pp == p);
    if (*pp && p)
    {
        HANDLE event = p->event;
        assert(p->owner == 0);
        assert(p->count == -1);
        if (event)
            CloseHandle(event);
        MemoryBarrier();
        *pp = 0;
    }
    MemoryBarrier();
}
 
void __cdecl ThreadWin32__DeleteLockE(LockE_t* lock)
/* E = exclusive */
{
    LockE_t* dummy;

    MemoryBarrier();
    dummy = lock;
    DeleteLockE(&dummy, lock);
    HeapFree(GetProcessHeap(), 0, lock);
    MemoryBarrier();
}
 
LockE_t* __cdecl ThreadWin32__NewLockE(void)
/* E = exclusive */
{
    LockE_t* dummy = 0;
    LockE_t* lock;

    MemoryBarrier();
    lock = (LockE_t*)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(*lock));
    InitLockE(&dummy, lock);
    if (!dummy)
        ThreadWin32__DeleteLockE(lock);
    MemoryBarrier();
    return dummy;
}
 
void __cdecl ThreadWin32__LockE(LockE_t* lock)
/* E = exclusive */
{
    MemoryBarrier();
    if (InterlockedIncrement(&lock->count) != 0)
        WaitForSingleObject(lock->event, INFINITE);
    assert(lock->owner == 0);
    lock->owner = GetCurrentThreadId();
    MemoryBarrier();
}
 
void __cdecl ThreadWin32__UnlockE(LockE_t* lock)
/* E = exclusive */
{
    MemoryBarrier();
    assert(lock->owner == GetCurrentThreadId());
    lock->owner = 0;
    if (InterlockedDecrement(&lock->count) >= 0)
        SetEvent(lock->event);
    MemoryBarrier();
}

#endif

/*-------------------------------------------------------------------------*/
/* LockRE_t RE = recursive/exclusive */

typedef CRITICAL_SECTION LockRE_t;

#define LOCKRE(name) \
static LockRE_t name##Lock; \
LockRE_t* ThreadWin32__##name##Lock; \

static void InitLockRE(LockRE_t** pp, LockRE_t* p)
{
    MemoryBarrier();
    assert(*pp == NULL || *pp == p);
    if (!*pp)
    {
        InitializeCriticalSection(p);
        MemoryBarrier();
        *pp = p;
    }
    MemoryBarrier();
}

static void DeleteLockRE(LockRE_t** pp, LockRE_t* p)
{
    MemoryBarrier();
    assert(*pp == NULL || *pp == p);
    if (*pp)
    {
        DeleteCriticalSection(p);
        MemoryBarrier();
        *pp = 0;
    }
    MemoryBarrier();
}

LockRE_t* __cdecl ThreadWin32__NewLockRE(void)
/* RE = recursive/exclusive */
{
    LockRE_t* lock;

    MemoryBarrier();
    lock = (LockRE_t*)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(*lock));
    if (lock)
        InitializeCriticalSection(lock);
    MemoryBarrier();
    return lock;
}

void __cdecl ThreadWin32__LockRE(LockRE_t* lock)
/* RE = recursive/exclusive */
{
    EnterCriticalSection(lock);
}

void __cdecl ThreadWin32__UnlockRE(LockRE_t* lock)
/* RE = recursive/exclusive */
{
    LeaveCriticalSection(lock);
}

void __cdecl ThreadWin32__DeleteLockRE(LockRE_t* lock)
/* RE = recursive/exclusive */
{
    MemoryBarrier();
    if (lock)
    {
        DeleteCriticalSection(lock);
        HeapFree(GetProcessHeap(), 0, lock);
    }
    MemoryBarrier();
}

/*-------------------------------------------------------------------------*/

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

/*-------------------------------------------------------------------------*/
/* some variables */

static DWORD threadIndex = TLS_OUT_OF_INDEXES;
static BOOL stack_grows_down;

/*-------------------------------------------------------------------------*/
/* context */

#if defined(_AMD64_)
#define STACK_REGISTER Rsp
#elif defined(_X86_)
#define STACK_REGISTER Esp
#elif defined(_IA64_)
#define STACK_REGISTER Rsp
#else
#error unknown architecture
#endif

void* __cdecl ThreadWin32__StackPointerFromContext(CONTEXT* context)
{
  return (void*)context->STACK_REGISTER;
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

/*-------------------------------------------------------------------------*/
/* process stopped and live stack and registers (garbage collection) */
 
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

/*-------------------------------------------------------------------------*/
/* activation (thread local) */

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

/*-------------------------------------------------------------------------*/
/* variables and initialization/cleanup */

LOCKRE(active) /* global lock for list of active threads */
LOCKRE(slot)   /* global lock for thread slots table which maps untraced to traced */
LOCKRE(giant)
LOCKRE(heap)
LOCKRE(perf)

HANDLE __cdecl ThreadWin32__InitC(BOOL* bottom)
{
    HANDLE threadHandle = { 0 };
    BOOL success = { 0 };

    stack_grows_down = (bottom > &success);
    assert(stack_grows_down);

    InitLockRE(&ThreadWin32__activeLock, &activeLock);
    InitLockRE(&ThreadWin32__giantLock, &giantLock);
    InitLockRE(&ThreadWin32__heapLock, &heapLock);
    InitLockRE(&ThreadWin32__perfLock, &perfLock);
    InitLockRE(&ThreadWin32__slotLock, &slotLock);

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
    DeleteLockRE(&ThreadWin32__activeLock, &activeLock);
    DeleteLockRE(&ThreadWin32__giantLock, &giantLock);
    DeleteLockRE(&ThreadWin32__heapLock, &heapLock);
    DeleteLockRE(&ThreadWin32__perfLock, &perfLock);
    DeleteLockRE(&ThreadWin32__slotLock, &slotLock);

    if (threadIndex != TLS_OUT_OF_INDEXES)
    {
        TlsFree(threadIndex);
        threadIndex = TLS_OUT_OF_INDEXES;
    }
}

/*-------------------------------------------------------------------------*/

BOOL ThreadWin32__InitMutexC(LockRE_t** lock)
/* Return TRUE if this thread allocated the lock and nobody beat it in a race.
FALSE can still mean the lock was allocated successfully, by another thread. */
{
    LockRE_t* newLock;

    if (*lock)
        return FALSE;

    newLock = ThreadWin32__NewLockRE();

    /* We failed, but in the mean time, somebody else may have succeeded. */
    if (!newLock)
    {
        if (*lock)
            return FALSE;
        /* try again after short delay */
        Sleep(1);
        newLock = ThreadWin32__NewLockRE();
    }

    if (!newLock)
        return FALSE;

    /* We succeeded, but in the mean time, somebody else may also have. */
#ifdef _WIN64
    if (InterlockedCompareExchangePointer((void**)lock, newLock, 0))
#else
    if (InterlockedCompareExchange((long*)lock, (long)newLock, 0))
#endif
    {
        ThreadWin32__DeleteLockRE(newLock);
        return FALSE;
    }

    return TRUE;
}

/*-------------------------------------------------------------------------*/

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

/*-------------------------------------------------------------------------*/

#ifdef __cplusplus
} /* extern "C" */
#endif
