/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Thu May  4 09:34:11 PDT 1995 by kalsow     */
/*      modified on Tue May 18 13:21:11 PDT 1993 by muller     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the stack walking functions of
   the RTStack interface. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <signal.h>
#include <libunwind.h>


/* _M3Exc is the C++ exception type thrown by RTStack__ThrowM3Exc and caught
   by the "catch (_M3Exc& _m3exc)" clauses that M3C.m3 generates in each
   translation unit.  It must be a proper C++ struct (not inside extern "C")
   so that C++ EH type-matching works correctly.  The identical definition
   appears in the M3C.m3 preamble for generated translation units; ODR is
   satisfied because the layout is the same everywhere. */
#ifdef __cplusplus
struct _M3Exc { void* act; };
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Exception handling register numbers per the System V AMD64 ABI.
   Register 0 carries the exception object pointer; register 1 the
   selector (tTypeIndex).  __builtin_eh_return_data_regno() maps these
   abstract indices to the concrete DWARF register numbers for the target. */
#define EHObjRegNo  0
#define EHTypeRegNo 1

/* Personality function stub — the real dispatch is done in RTEHScan. */
void * __m3_personality_v0();

/* External M3 allocator — avoids a memory leak in CurFrame. */
extern char * RTException__AllocBuf(int size);

/*
  FrameInfo = RECORD
    pc  : ADDRESS;
    sp  : ADDRESS;
    bp  : ADDRESS;        (* base pointer — unused *)
    lock: INTEGER;        (* sentinel to detect cursor overrun *)
    excRef  : ADDRESS;    (* ref to the exception activation *)
    tTypeIndex : INTEGER; (* tTypeIndex from exception table *)
    cursor : ADDRESS;     (* libunwind cursor to cur frame *)
    startIP : ADDRESS;    (* libunwind start ip of current proc *)
    endIP : ADDRESS;      (* libunwind end ip of current proc *)
    lsda : ADDRESS;       (* libunwind lsda *)
    persFn : ADDRESS;     (* libunwind handler pers fn *)
    landingPad : ADDRESS; (* libunwind landing pad *)
  END;
 * The typedef below must agree with the definition above
 * and in RTMachine.i3.
 */

typedef struct {
  unsigned long pc;
  unsigned long sp;
  unsigned long bp;
  long lock;
  unsigned long exceptionRef;
  long tTypeIndex;
  unw_cursor_t *cursor;
  unw_word_t start_ip;
  unw_word_t end_ip;
  unw_word_t lsda;
  unw_word_t persFn;
  unw_word_t landingPad;
} Frame;

#define FrameLock 0x1234567890

void * __m3_personality_v0() {
  printf("m3 personality\n");
  return NULL;
}

/*---------------------------------------------------------------------------*/
/*
 * Populate proc_info fields (start_ip, end_ip, lsda, persFn) for the
 * frame identified by the libunwind cursor in *f.
 */

void RTStack__GetProcInfo(Frame *f) {
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

char* RTStack__ProcName (Frame *f)
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

void RTStack__GetThreadFrame (Frame *f, char *start, int len)
{
  /* Not implemented. */
  abort();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame)
 * Returns the frame that corresponds to the caller of this function. */

void RTStack__CurFrame (Frame *f)
{
  unw_context_t *uc;
  unw_cursor_t *cursor;
  unw_word_t ip, sp = 0;

  /* Allocate from the M3 heap so the cursor lifetime is managed there. */
  uc     = (unw_context_t *) RTException__AllocBuf(sizeof(unw_context_t));
  cursor = (unw_cursor_t *)  RTException__AllocBuf(sizeof(unw_cursor_t));

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

void RTStack__PrevFrame (Frame* callee, Frame* caller)
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

void RTStack__Unwind (Frame *target)
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

#ifdef __cplusplus
} /* extern "C" */
#endif

#ifdef __cplusplus
/* RTStack__ThrowM3Exc is defined outside the extern "C" block so that the
   C++ compiler's exception-handling machinery is fully engaged (the throw
   expression needs C++ EH unwinding, not C abort semantics).  The
   extern "C" on the definition gives the symbol C linkage so that M3
   code can call it via the EXTERNAL pragma in RTStack.i3. */
extern "C" void RTStack__ThrowM3Exc(void* act) {
  throw _M3Exc{act};
}
#endif
