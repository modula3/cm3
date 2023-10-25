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


#ifdef __cplusplus
extern "C" {
#endif

//Define the exception handling register number. 0 should be safe
//across most architectures.
#define EHObjRegNo  0
#define EHTypeRegNo 1

//declare a personality function
void * __m3_personality_v0();

//external M3 alloc memory
extern char * RTException__AllocBuf(int size);

/*
  FrameInfo = RECORD
    pc  : ADDRESS;
    sp  : ADDRESS;
    bp  : ADDRESS;        (* base pointer *)
    lock: INTEGER;        (* to ensure that cxt isn't overrun!! *)
    excRef  : ADDRESS;    (* ref to the exception activation *)
    tTypeIndex : INTEGER; (* tTypeIndex from exception table *)
    cursor : ADDRESS;     (* libunwind cursor to cur frame *)
    startIP : ADDRESS;    (* libunwind start ip of current proc *)
    endIP : ADDRESS;      (* libunwind end ip of current proc *)
    lsda : ADDRESS;       (* libunwind lsda *)
    persFn : ADDRESS;     (* libunwind handler pers fn *)
    landingPad : ADDRESS; (* libunwind landing pad *)
  END;
 * The typedef below must agree with the definition replicated here
 * and defined in RTMachine.i3
 */

typedef struct {
  unsigned long pc;
  unsigned long sp;
  unsigned long bp; //not used
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
 * Get the procedure info for this frame including the personality function
 * and language specific data
 *
 * The unw_get_proc_info() routine returns auxiliary information about the procedure that created the stack frame identified by argument cp. The pip argument is a pointer to a structure of type unw_proc_info_t which is used to return the information. 
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
  f->end_ip = info.end_ip;
  f->lsda = info.lsda;
  f->persFn = info.handler;
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

  name = (char *) malloc(name_len);
  //there is a small mem leak here but as it is only called
  //during debugex of the stack unwinder it should be fine.

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
  // not implemented 
  abort();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame)
 * returns the frame that corresponds to its caller. */

void RTStack__CurFrame (Frame *f)
{
  unw_context_t *uc;
  unw_cursor_t *cursor;
  unw_word_t ip, sp = 0;

  //we alloc in m3 otherwise a serious memory leak 
  //uc = (unw_context_t *) malloc(sizeof(unw_context_t));
  //cursor = (unw_cursor_t *) malloc(sizeof(unw_cursor_t));
  uc = (unw_context_t *) RTException__AllocBuf(sizeof(unw_context_t));
  cursor = (unw_cursor_t *) RTException__AllocBuf(sizeof(unw_cursor_t));

  f->lock = FrameLock;
  unw_getcontext(uc);
  unw_init_local(cursor, uc);
  unw_get_reg(cursor, UNW_REG_IP, &ip);
  unw_get_reg(cursor, UNW_REG_SP, &sp);
  
  f->cursor = cursor;
  f->pc = ip;
  f->sp = sp;
  RTStack__GetProcInfo(f);

  if (f->lock != FrameLock) abort ();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE PreviousFrame (READONLY callee: Frame;  VAR(*OUT*)caller: Frame)
   Return the stack frame that called "callee".  Returns with pc = NIL if
   "callee" is the first frame on the stack or its predecessor is ill-formed.
   */

void RTStack__PrevFrame (Frame* callee, Frame* caller)
{
  unw_word_t ip, sp = 0;
  int res;

  if (!callee->cursor) abort();

  if (callee->lock != FrameLock) abort ();
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
  if (caller->lock != FrameLock) abort ();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE Unwind (READONLY f: Frame);
   Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call. */

void RTStack__Unwind (Frame *target)
{
  int res;

  if (!target->cursor) abort();
  if (target->lock != FrameLock) abort ();
  
  //set the IP to landingPad 
  unw_set_reg(target->cursor, UNW_REG_IP, target->landingPad);
  
  //set the eh register zero to return the exception object
  unw_set_reg(target->cursor,
	      __builtin_eh_return_data_regno(EHObjRegNo),
	      target->exceptionRef);
 
  //set the eh register one to the tTypeIndex for gcc
  unw_set_reg(target->cursor,
	      __builtin_eh_return_data_regno(EHTypeRegNo),
	      target->tTypeIndex);

  res = unw_resume(target->cursor);
  //success means unreachable from here
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
