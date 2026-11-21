/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/* This file implements the stack walking functions of
 * the RTStack interface.
 *
 * This is based on AMD64_LINUX, AMD64_DARWIN, and ARM64_DARWIN
 * which were materially identical, aside from comments.
 */
#include "m3core.h"
#include <stdlib.h>
/* Tested systems with libunwind.
 * This can likely be broadly expanded, i.e. Linux, BSD, etc.
 */
#if M3_HOST_AMD64_DARWIN || M3_HOST_ARM64_DARWIN || M3_HOST_AMD64_LINUX
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <signal.h>
#include <libunwind.h>
#define M3_LIBUNWIND 1
#else
#define M3_LIBUNWIND 0
#endif
/* This must agree with RTMachine.FrameInfo. */
typedef struct {
  ADDRESS pc;           /* program counter */
  ADDRESS sp;           /* stack pointer */
  INTEGER lock;         /* sentinel to detect cursor overun */
  ADDRESS exceptionRef; /* ref to the exception activation */
  INTEGER tTypeIndex;   /* tTypeIndex from exception table */
#if M3_LIBUNWIND
  unw_cursor_t *cursor; /* libunwind cursor to current frame */
#else
  INTEGER cursor;       /* filler */
#endif
  ADDRESS start_ip;     /* libunwin start ip of current proc */
  ADDRESS end_ip;       /* libunwin end ip of current proc */
  ADDRESS lsda;         /* language specific data area */
  ADDRESS persFn;       /* personality function (per language frame handler) */
  ADDRESS landingPad;   /* landing pad */
} Frame;

#ifdef __cplusplus
extern "C" {
#endif

#if M3_LIBUNWIND

/* External M3 allocator, avoids a memory leak in CurFrame. */
char * __cdecl RTException__AllocBuf (INTEGER size);

INTEGER __cdecl RTStack_Has_walker (void)
{
    return TRUE;
}

#define FrameLock 0x1234567890

/*---------------------------------------------------------------------------*/
/*
 * Populate proc_info fields (start_ip, end_ip, lsda, persFn) for the
 * frame identified by the libunwind cursor in *f.
 */

void __cdecl RTStack__GetProcInfo(Frame *f) {
  int res;
  unw_proc_info_t info;

  res = unw_get_proc_info(f->cursor, &info);
  if (res < 0) {
    printf("unw_get_proc_info error\n");
    abort();
  }
  f->start_ip = info.start_ip;
  f->end_ip   = info.end_ip;
  f->lsda     = info.lsda;
  f->persFn   = info.handler;
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
   Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. */

char* __cdecl RTStack__ProcName (Frame *f)
{
  int res;
  char *name;
  long name_len = 50;
  unw_word_t ofp;

  /* Small leak is acceptable — only called for debug/diagnostic output. */
  name = (char *) malloc(name_len);

  res = unw_get_proc_name(f->cursor, name, name_len, &ofp);
  if (res == 0) {
    return name;
  } else {
    return 0;
  }
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE GetThreadFrame (VAR f: Frame;  start: ADDRESS;  len: INTEGER);
   Return in "f" the frame of the thread whose machine state is in bytes
   [start .. start+len).  Returns with f.pc=NIL on failure. */

void __cdecl RTStack__GetThreadFrame (Frame *f, char *start, int len)
{
  /* Not implemented. */
  abort();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame)
 * Returns the frame that corresponds to the caller of this function. */

void __cdecl RTStack__CurFrame (Frame *f)
{
  unw_context_t *uc;
  unw_cursor_t *cursor;
  unw_word_t ip, sp = 0;

  /* Allocate from the M3 heap so the cursor lifetime is managed there. */
  uc     = (unw_context_t *) RTException__AllocBuf (sizeof(unw_context_t));
  cursor = (unw_cursor_t *)  RTException__AllocBuf (sizeof(unw_cursor_t));

  f->lock = FrameLock;
  unw_getcontext(uc);
  unw_init_local(cursor, uc);
  unw_get_reg(cursor, UNW_REG_IP, &ip);
  unw_get_reg(cursor, UNW_REG_SP, &sp);

  f->cursor = cursor;
  f->pc     = ip;
  f->sp     = sp;
  RTStack__GetProcInfo(f);

  if (f->lock != FrameLock) abort();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE PreviousFrame (READONLY callee: Frame;  VAR(*OUT*) caller: Frame)
   Return the stack frame that called "callee".  Returns with pc = NIL if
   "callee" is the first frame on the stack or its predecessor is ill-formed.
   */

void __cdecl RTStack__PrevFrame (Frame* callee, Frame* caller)
{
  unw_word_t ip, sp = 0;
  int res;

  if (!callee->cursor)           abort();
  if (callee->lock != FrameLock) abort();

  *caller = *callee;

  res = unw_step(caller->cursor);
  if (res > 0) {
    unw_get_reg(caller->cursor, UNW_REG_IP, &ip);
    unw_get_reg(caller->cursor, UNW_REG_SP, &sp);

    caller->pc = ip;
    caller->sp = sp;
    RTStack__GetProcInfo(caller);
  } else {
    caller->pc = 0;
    caller->sp = 0;
  }

  if (caller->lock != FrameLock) abort();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE Unwind (READONLY f: Frame);
   Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call. */

void __cdecl RTStack__Unwind (Frame *target)
{
  int res;

  if (!target->cursor)           abort();
  if (target->lock != FrameLock) abort();

  /* Set IP to the landing pad computed by RTEHScan.ScanEHTable. */
  unw_set_reg(target->cursor, UNW_REG_IP, target->landingPad);

  /* Pass the exception object pointer in EH data register 0. */
  unw_set_reg(target->cursor,
              __builtin_eh_return_data_regno(EHObjRegNo),
              target->exceptionRef);

  /* Pass the tTypeIndex (exception selector) in EH data register 1. */
  unw_set_reg(target->cursor,
              __builtin_eh_return_data_regno(EHTypeRegNo),
              target->tTypeIndex);

  res = unw_resume(target->cursor);
  /* unw_resume only returns on error. */
  if (res < 0) {
    printf("RTStack__Unwind - unw_resume error\n");
    abort();
  }
}

/*
//a test of generating a backtrace with libunwind.
//
void show_backtrace (void) {
  unw_cursor_t cursor; unw_context_t uc;
  unw_word_t ip, sp;

  int ret;
  char name[50];
  size_t name_len = 50;
  unw_word_t ofp;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);
  while (unw_step(&cursor) > 0) {
    unw_get_reg(&cursor, UNW_REG_IP, &ip);
    unw_get_reg(&cursor, UNW_REG_SP, &sp);
    printf ("ip = %lx, sp = %lx\n", (long) ip, (long) sp);

    ret = unw_get_proc_name(&cursor, name, name_len, &ofp);
    printf("name %s\n",name);
  }
}
*/

#else

INTEGER __cdecl RTStack_Has_walker (void)
{
    return FALSE;
}

/*---------------------------------------------------------------------------*/
/* Populate proc_info fields (start_ip, end_ip, lsda, persFn) for the
 * frame identified by the libunwind cursor in *f.
 */
void __cdecl RTStack__GetProcInfo(Frame *f) {
  ZERO_MEMORY (f);
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
   Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. */
char* __cdecl RTStack__ProcName (Frame *f)
{
  ZERO_MEMORY (f);
  return 0;
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE GetThreadFrame (VAR f: Frame;  start: ADDRESS;  len: INTEGER);
   Return in "f" the frame of the thread whose machine state is in bytes
   [start .. start+len).  Returns with f.pc=NIL on failure. */
void __cdecl RTStack__GetThreadFrame (Frame *f, char *start, int len)
{
  /* Not implemented. */
  abort();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame)
 * Returns the frame that corresponds to the caller of this function. */
void __cdecl RTStack__CurFrame (Frame *f)
{
  ZERO_MEMORY (f);
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE PreviousFrame (READONLY callee: Frame;  VAR(*OUT*) caller: Frame)
   Return the stack frame that called "callee".  Returns with pc = NIL if
   "callee" is the first frame on the stack or its predecessor is ill-formed.
   */
void __cdecl RTStack__PrevFrame (Frame* callee, Frame* caller)
{
  ZERO_MEMORY (callee);
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE Unwind (READONLY f: Frame);
   Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call. */
void __cdecl RTStack__Unwind (Frame *target)
{
  abort ();
}
#endif /* M3_LIBUNWIND */

#ifdef __cplusplus
} /* extern "C" */

/* _M3Exc is the C++ exception type thrown by RTStack__ThrowM3Exc and caught
   by the "catch (_M3Exc& _m3exc)" clauses that M3C.m3 generates in each
   translation unit.  It must be a proper C++ struct (not inside extern "C")
   so that C++ EH type-matching works correctly.  The identical definition
   appears in the M3C.m3 preamble for generated translation units; ODR is
   satisfied because the layout is the same everywhere. */
struct _M3Exc { void* act; };

/* RTStack__ThrowM3Exc is defined outside the extern "C" block so that the
   C++ compiler's exception-handling machinery is fully engaged (the throw
   expression needs C++ EH unwinding, not C abort semantics).  The
   extern "C" on the definition gives the symbol C linkage so that M3
   code can call it via the EXTERNAL pragma in RTStack.i3. */
extern "C" void RTStack__ThrowM3Exc(void* act) {
  throw _M3Exc{act};
}
#endif
