/* Copyright (C) 1994, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */
/*                                                                 */
/* Portions Copyright 1996-2000, Critical Mass, Inc.               */
/* See file COPYRIGHT-CMASS for details.                           */

#if defined(_WIN32) && !defined(WIN32)
#define WIN32
#endif

#ifdef _MSC_VER
#pragma warning(disable:4616) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
#pragma warning(disable:4619) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
#pragma warning(disable:4115) /* named type definition in parentheses */
#pragma warning(disable:4100) /* unused parameter */
#pragma warning(disable:4201) /* nonstandard extension: nameless struct/union */
#pragma warning(disable:4214) /* nonstandard extension: bitfield other than int */
#pragma warning(disable:4514) /* unused inline function removed */
#pragma warning(disable:4705) /* statement has no effect for merely using assert() at -W4 */
#pragma warning(disable:4209) /* nonstandard extension: benign re-typedef */
#pragma warning(disable:4226) /* nonstandard extension: __export */
#pragma warning(disable:4820) /* padding inserted */
#pragma warning(disable:4255) /* () change to (void) */
#pragma warning(disable:4668) /* #if of undefined symbol */
#ifdef __cplusplus
#pragma warning(disable:4611) /* setjmp interaction with destructors is not portable */
#endif
#endif

#include <windows.h>
#include <assert.h>
#include <setjmp.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#define M3_FIELD_SIZE(type, field) (sizeof((type*)0)->field)

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
    /* 0x300 to workaround Windows 7 pre-SP1 bug
       GetThreadContext fails when using pageheap due to buffer overrun */
    typedef union {
        CONTEXT a;
        unsigned char b[0x300];
    } CONTEXT300;
    PCONTEXT context = (PCONTEXT)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(CONTEXT300));
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
  volatile char assertReadable = { 0 };
  char* top = { 0 };

  /* stack bounds are not yet set or have been cleared;
     therefore the thread either doesn't yet have any traced
     references or no longer has any */

  if (!stackStart || !stackEnd)
      return;

  /* process stack */

  assert(context);
  assert(stackStart < stackEnd);
  /* assert(stack_grows_down); */
  top = (char*)context->STACK_REGISTER;
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

  if (setjmp(jb) == 0) /* save registers to stack */
#ifdef _IA64_
    longjmp(jb, 1); /* flush register windows */
  else
#endif
  {
    char *top = (char*)&top;
    assert(bottom);
    /* assert(stack_grows_down); */
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
/* variables and initialization/cleanup */

/* implementing variables in C greatly increase debuggability (symbols work) */

#define threadIndex ThreadWin32__threadIndex

DWORD threadIndex = TLS_OUT_OF_INDEXES;
CRITICAL_SECTION ThreadWin32__activeLock;   /* global lock for list of active threads */
CRITICAL_SECTION ThreadWin32__slotLock;     /* global lock for thread slots table which maps untraced to traced */
CRITICAL_SECTION ThreadWin32__heapLock;
CRITICAL_SECTION ThreadWin32__perfLock;
CRITICAL_SECTION ThreadWin32__initLock;

/* widen to USHORT, etc. if needed */

typedef struct _ClonedHeaderCheckField_t {
    UCHAR offset;
    UCHAR size;
} ClonedHeaderCheckField_t;

typedef struct _ClonedHeaderCheck_t {
    UINT TlsOutOfIndexs;
    UCHAR sizeof_CRITICAL_SECTION;
    UCHAR sizeof_MEMORY_BASIC_INFORMATION;
    ClonedHeaderCheckField_t MEMORY_BASIC_INFORMATION_AllocationBase;
    ClonedHeaderCheckField_t MEMORY_BASIC_INFORMATION_BaseAddress;
    ClonedHeaderCheckField_t MEMORY_BASIC_INFORMATION_RegionSize;
} ClonedHeaderCheck_t;

#define FIELD_INFO(t, f) { offsetof(t, f), M3_FIELD_SIZE(t, f) }

const static ClonedHeaderCheck_t clonedHeaderCheck = {
    TLS_OUT_OF_INDEXES,
    sizeof(CRITICAL_SECTION),
    sizeof(MEMORY_BASIC_INFORMATION),
    FIELD_INFO(MEMORY_BASIC_INFORMATION, AllocationBase),
    FIELD_INFO(MEMORY_BASIC_INFORMATION, BaseAddress),
    FIELD_INFO(MEMORY_BASIC_INFORMATION, RegionSize),
};
void
__cdecl
ThreadWin32__ClonedHeaderCheck(
    const ClonedHeaderCheck_t* a,
    size_t aSize)
{
    assert(sizeof(*a) == aSize);
    assert(memcmp(a, &clonedHeaderCheck, sizeof(*a)) == 0);
}

#if 0
void
__cdecl
xThreadWin32__GetStackBounds(
    PBYTE* start,
    PBYTE* end
    )
{
    MEMORY_BASIC_INFORMATION info = { 0 };
    VirtualQuery(&info, &info, sizeof(info));
    *start = (PBYTE)info.AllocationBase;
    *end = (PBYTE)info.BaseAddress + info.RegionSize;
    fprintf(stderr, "ThreadWin32__GetStackBounds start:%p end:%p size:%u\n",
        *start, *end, (UINT)(*end - *start));
    exit(1);
}
#endif

#if 0
void __cdecl ThreadWin32__Cleanup(void)
{
    DeleteCriticalSection(&ThreadWin32__activeLock);
    DeleteCriticalSection(&ThreadWin32__heapLock);
    DeleteCriticalSection(&ThreadWin32__perfLock);
    DeleteCriticalSection(&ThreadWin32__slotLock);

    if (threadIndex != TLS_OUT_OF_INDEXES)
    {
        TlsFree(threadIndex);
        threadIndex = TLS_OUT_OF_INDEXES;
    }
}

/*-------------------------------------------------------------------------*/

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
        ThreadWin32__Cleanup();

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
