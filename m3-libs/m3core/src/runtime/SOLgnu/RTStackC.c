/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking    */

#include <stdio.h>
#include <signal.h>
#include <ucontext.h>
#include <sys/frame.h>

/*
TYPE FrameInfo = RECORD
  pc, sp, true_sp: ADDRESS;		 (* sp here is actually SPARC fp *)
  ctxt: Uucontext.ucontext_t;
  lock: INTEGER; (* to ensure that ctxt isn't overrun!! *)
END;
*/
/* On the SPARC local variables are referred to as an offset from the FP.
   However, the RTException model assumes offsets for local variables from the
   sp field of the RTStack.Frame structure, so we store the true FP in sp. */
typedef struct {
  void *pc;
  struct frame *fp;		/* true fp */
  struct frame *sp;		/* true sp */
  struct ucontext ctxt;
  int lock;
} Frame;

#define FrameLock 0x12345678

/*---------------------------------------------------------------------------*/
/* PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame)
 * returns the frame that corresponds to its caller. */

void RTStack__CurFrame (Frame *f)
{
  greg_t *reg = f->ctxt.uc_mcontext.gregs;
  struct frame *sp, *fp;
  void *pc;

  f->lock = FrameLock;
  if (getcontext(&(f->ctxt))) abort ();	/* getcontext flushes stack */
  pc = (void *)reg[REG_PC];
  sp = (struct frame *)reg[REG_SP];
  fp = sp->fr_savfp;
  /* now pick up previous frame */
  pc = (void *)sp->fr_savpc;
  sp = fp;
  fp = sp->fr_savfp;
  f->pc = pc;
  f->fp = fp;
  f->sp = sp;
  
  if (f->lock != FrameLock) abort ();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE GetThreadFrame (VAR f: Frame;  start: ADDRESS;  len: INTEGER);
   Return in "f" the frame of the thread whose machine state is in bytes
   [start .. start+len).  Returns with f.pc=NIL on failure. */

void RTStack__GetThreadFrame (Frame *f, void *start, int len)
{
  greg_t *reg = f->ctxt.uc_mcontext.gregs;

  f->lock = FrameLock;
  f->pc = 0;
  f->sp = 0;
  if (len == sizeof (ucontext_t)) {
    RTStack__Flush();

    f->ctxt = *(ucontext_t *)start;
    f->pc = (void *)reg[REG_PC];
    f->fp = (f->sp = (struct frame *)reg[REG_SP])->fr_savfp;
    if (f->lock != FrameLock) abort();
  }
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
   Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. */

char* RTStack__ProcName (Frame* f)
{
  /* No data - Always return nil. */
  return 0;
}

void (*RTHeapDep_Fault)();
void (*RTProcedureSRC_FromPC) (void *pc, void **p, char **file, char **name);

void RTStack__PrevFrame (Frame* callee, Frame* caller)
{
  ucontext_t *link;
  struct frame *link_sp, *link_fp;
  void *proc;
  char *file, *name;

  if (callee == 0) abort();
  if (caller == 0) abort();
  if (callee->lock != FrameLock) abort();

  RTStack__Flush();

  caller->lock = FrameLock;

  /* We must be careful when unwinding through signal trampolines that we also
     restore the signal mask of the previous context. */
  RTProcedureSRC_FromPC(callee->pc, &proc, &file, &name);
  if (proc == RTHeapDep_Fault) {
    /* SIGNAL HANDLER: previous frame information can be found in third
       argument of RTHeapDep_Fault */
    link = (ucontext_t *)callee->sp->fr_arg[2];
    caller->ctxt = *link;
    caller->pc = (void *)link->uc_mcontext.gregs[REG_PC];
    if (caller->pc) {
      caller->sp = (struct frame *)link->uc_mcontext.gregs[REG_SP];
      caller->fp = caller->sp->fr_savfp;
    } else
      caller->sp = caller->fp = 0;
  } else {
    caller->pc = (void *)callee->sp->fr_savpc;
    if (caller->pc) {
      caller->fp = (caller->sp = callee->fp)->fr_savfp;
    } else
      caller->sp = caller->fp = 0;
    caller->ctxt = callee->ctxt;
  }
  if (caller->lock != FrameLock) abort();
}

void RTStack__Unwind (Frame* target)
{
  struct frame *sp = target->sp;
  greg_t *reg = target->ctxt.uc_mcontext.gregs;

  RTStack__Flush();

  if (target->lock != FrameLock) abort();
  reg[REG_PC] = (int)target->pc + 8;/* for return address */
  reg[REG_nPC] = (int)reg[REG_PC] + 4;
  reg[REG_SP] = (int)target->sp;
  reg[REG_O7] = target->sp->fr_savpc;
  setcontext(&target->ctxt);
}
