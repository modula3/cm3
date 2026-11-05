/* Copyright (C) 1994, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */
/*                                                                 */
/* Portions Copyright 1996-2000, Critical Mass, Inc.               */
/* See file COPYRIGHT-CMASS for details.                           */

#define _NO_CRT_STDIO_INLINE 1 /* Do not accidentally export printf. */

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

#include <assert.h>
#include <malloc.h>
#include <setjmp.h>
#include <stddef.h>
#include <string.h>
#include <windows.h>

#if 0
//#include <stdio.h>
#include <stdlib.h>
#undef assert
#define assert(expr) ((expr) || (ThreadWin32_AssertFailed(__FILE__, __LINE__, #expr), 0))
void ThreadWin32_AssertFailed(const char* file, unsigned long line, const char* expr)
{
	char buffer[1024] = { 0 };
	DWORD length = (DWORD)wsprintfA(buffer,  "assert failed:%s(%lu) %s\n", __FILE__, __LINE__, expr);
	WriteFile(GetStdHandle(STD_ERROR_HANDLE), buffer, length, &length, 0);
	OutputDebugStringA(buffer);
	if (IsDebuggerPresent()) DebugBreak();
	abort();
}
#endif

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

#if defined(_ARM_) || defined(_ARM64_) || defined(_ARM64EC_)
#define STACK_REGISTER Sp
#elif defined(_AMD64_)
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
  assert(context);
  assert(context->STACK_REGISTER);
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

void __cdecl ThreadWin32__DeleteContext(void** p)
{
	void* q = *p;
	*p = 0;
    HeapFree(GetProcessHeap(), 0, q);
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
  // Wrap in struct to avoid warning about taking address of array.
  struct {
    jmp_buf jb;
  } s;

  if (setjmp(s.jb) == 0) /* save registers to stack */
#ifdef _IA64_
    longjmp(s.jb, 1); /* flush register windows */
  else
#endif
  {
#ifdef __GNUC__
    char* top = (char*)__builtin_alloca(1);
#else
    char* top = (char*)_alloca(1);
#endif
    assert(bottom);
    if (top < bottom)
        p(top, bottom);
    else if (top > bottom)
        p(bottom, top);
    p(&s, 1 + &s);
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

EXTERN_CONST DWORD ThreadWin32__TLS_OUT_OF_INDEXES = TLS_OUT_OF_INDEXES;
EXTERN_CONST DWORD ThreadWin32__WAIT_OBJECT_0 = WAIT_OBJECT_0;
EXTERN_CONST DWORD ThreadWin32__WAIT_TIMEOUT = WAIT_TIMEOUT;
EXTERN_CONST DWORD ThreadWin32__CREATE_SUSPENDED = CREATE_SUSPENDED;
EXTERN_CONST DWORD ThreadWin32__DUPLICATE_SAME_ACCESS = DUPLICATE_SAME_ACCESS;
EXTERN_CONST DWORD ThreadWin32__INFINITE = INFINITE;

#if 1

/* Previously the size of CRITICAL_SECTION is exposed in Modula-3,
 * and paired with a boolean for on-demand initialization.
 * This was efficient, however required rewriting "windows.h"
 * in Modula-3 and is less abstracted than the pthread code.
 */

PCRITICAL_SECTION
__cdecl
ThreadWin32__NewCriticalSection(void)
{
    PCRITICAL_SECTION p = (PCRITICAL_SECTION)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(*p));
    if (p)
        InitializeCriticalSection(p);
    return p;
}

void
__cdecl
ThreadWin32__DelCriticalSection(
    PCRITICAL_SECTION * a
    )
{
    PCRITICAL_SECTION b;
    if (!a) return;
    b = *a;
    *a = 0;
    if (!b) return;
    DeleteCriticalSection(b);
    HeapFree(GetProcessHeap(), 0, b);
}

#endif

void
__cdecl
ThreadWin32__GetStackBounds(
    PBYTE* start,
    PBYTE* end
    )
{
    MEMORY_BASIC_INFORMATION info = { 0 };
    VirtualQuery(&info, &info, sizeof(info));
    *start = (PBYTE)info.AllocationBase;
    *end = (PBYTE)info.BaseAddress + info.RegionSize;
}

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
