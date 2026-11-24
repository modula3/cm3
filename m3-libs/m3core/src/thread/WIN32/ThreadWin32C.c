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
#include <stdlib.h>
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
#define PC_REGISTER Pc
#elif defined(_AMD64_)
#define STACK_REGISTER Rsp
#define PC_REGISTER Rip
#elif defined(_X86_)
#define STACK_REGISTER Esp
#define PC_REGISTER Eip
#elif defined(_IA64_)
#define STACK_REGISTER Rsp
#define PC_REGISTER StIIP
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

/* A thread can be stopped while its stack is growing: its stack pointer
   has already moved into the guard page below the committed stack, but it
   has not yet touched that page.  Reading the page from the collector would
   raise the guard-page exception in the collector's thread, and would also
   disarm the guard, so the stopped thread's stack could no longer grow
   (#1287).  An armed guard page has never been written, so it holds nothing
   to scan: start above it.

   Return the first address at or above top that is not in a guard page. */

static char* ThreadWin32__SkipGuardPages(char* top, char* stackEnd)
{
  MEMORY_BASIC_INFORMATION info = { 0 };

  while (top < stackEnd)
  {
    ZeroMemory(&info, sizeof(info));
    if (VirtualQuery(top, &info, sizeof(info)) == 0
        || info.State != MEM_COMMIT
        || (info.Protect & PAGE_GUARD) == 0)
      break;
    top = (char*)info.BaseAddress + info.RegionSize;
  }
  return (top < stackEnd) ? top : stackEnd;
}

/* Check that a stopped thread's stack, from where the scan starts up to
   stackEnd, is committed memory of that thread's stack.  If not, describe
   what was found and abort, instead of faulting inside NoteStackLocations
   with no clue why. */

static void ThreadWin32__PutText(char** out, const char* s)
{
  while (*s)
    *(*out)++ = *s++;
}

static void ThreadWin32__PutHex(char** out, UINT_PTR v)
{
  int i = { 0 };
  ThreadWin32__PutText(out, "0x");
  for (i = (int)sizeof(v) * 8 - 4; i >= 0; i -= 4)
    *(*out)++ = "0123456789abcdef"[(v >> i) & 15];
}

static void ThreadWin32__PutField(char** out, const char* name, UINT_PTR v)
{
  ThreadWin32__PutText(out, name);
  ThreadWin32__PutHex(out, v);
}

static void ThreadWin32__CheckStackRange(
    char* stackStart,
    char* stackEnd,
    char* top,
    CONTEXT* context)
{
  const char* problem = { 0 };
  char* a = top;
  MEMORY_BASIC_INFORMATION info = { 0 };
  char buffer[1024] = { 0 };
  char* out = buffer;
  DWORD length = { 0 };

  if (top < stackStart || top > stackEnd)
    problem = "stack pointer is outside the thread's stack";
  else
  {
    while (a < stackEnd)
    {
      ZeroMemory(&info, sizeof(info));
      if (VirtualQuery(a, &info, sizeof(info)) == 0)
      {
        problem = "VirtualQuery failed";
        break;
      }
      if (info.State != MEM_COMMIT)
      {
        problem = "range includes uncommitted memory";
        break;
      }
      if (info.Protect & (PAGE_GUARD | PAGE_NOACCESS))
      {
        problem = "range includes a guard or no-access page";
        break;
      }
      a = (char*)info.BaseAddress + info.RegionSize;
    }
  }
  if (!problem)
    return;

  ThreadWin32__PutText(&out, "\n*** ThreadWin32: bad stack range for a stopped thread: ");
  ThreadWin32__PutText(&out, problem);
  ThreadWin32__PutField(&out, "\n***   sp ", (UINT_PTR)context->STACK_REGISTER);
  ThreadWin32__PutField(&out, " scan from ", (UINT_PTR)top);
  ThreadWin32__PutField(&out, " pc ", (UINT_PTR)context->PC_REGISTER);
  ThreadWin32__PutField(&out, " ContextFlags ", (UINT_PTR)context->ContextFlags);
  ThreadWin32__PutField(&out, "\n***   stackStart ", (UINT_PTR)stackStart);
  ThreadWin32__PutField(&out, " stackEnd ", (UINT_PTR)stackEnd);
  ThreadWin32__PutField(&out, " collector thread ", (UINT_PTR)GetCurrentThreadId());
  if (top >= stackStart && top <= stackEnd)
  {
    ThreadWin32__PutField(&out, "\n***   region at ", (UINT_PTR)a);
    ThreadWin32__PutField(&out, ": base ", (UINT_PTR)info.BaseAddress);
    ThreadWin32__PutField(&out, " size ", (UINT_PTR)info.RegionSize);
    ThreadWin32__PutField(&out, " allocation ", (UINT_PTR)info.AllocationBase);
    ThreadWin32__PutField(&out, " state ", (UINT_PTR)info.State);
    ThreadWin32__PutField(&out, " protect ", (UINT_PTR)info.Protect);
  }
  else
  {
    ZeroMemory(&info, sizeof(info));
    VirtualQuery(top, &info, sizeof(info));
    ThreadWin32__PutField(&out, "\n***   region at sp: base ", (UINT_PTR)info.BaseAddress);
    ThreadWin32__PutField(&out, " size ", (UINT_PTR)info.RegionSize);
    ThreadWin32__PutField(&out, " allocation ", (UINT_PTR)info.AllocationBase);
    ThreadWin32__PutField(&out, " state ", (UINT_PTR)info.State);
    ThreadWin32__PutField(&out, " protect ", (UINT_PTR)info.Protect);
  }
  ThreadWin32__PutText(&out, "\n");
  WriteFile(GetStdHandle(STD_ERROR_HANDLE), buffer, (DWORD)(out - buffer), &length, 0);
  abort();
}

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
  if (top >= stackStart)
    top = ThreadWin32__SkipGuardPages(top, stackEnd);
  ThreadWin32__CheckStackRange(stackStart, stackEnd, top, context);
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
