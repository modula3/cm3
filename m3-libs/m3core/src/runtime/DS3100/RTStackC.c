/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Thu Jul  6 08:50:39 PDT 1995 by kalsow     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the stack walking functions of
   the RTStack interface. */

#ifdef DEBUG
#include <stdio.h>
#define dprintf(x)  fprintf x
#else
#define dprintf(x)
#endif

#include <string.h>
#include <setjmp.h>
#include <exception.h>
#include <sym.h>
void sigvec();

/* TYPE Frame = RECORD pc, sp: ADDRESS END; */
typedef struct {
  unsigned long pc;
  unsigned long sp;
} Frame;

typedef struct runtime_pdr *Proc;

/* !MAGIC! gleaned by disassembling the trampoline code that invokes
   a signal handler.... */
typedef struct {
  int unknown_1;
  int unknown_2;
  int unknown_3;
  int unknown_4;
  jmp_buf *signal_context;
} InterruptFrame;

Frame RTStack__CurFrame (); /* defined in RTStackASM.s */

/*---------------------------------------------------------------------------*/
/* RTStack__Find searches the runtime table for the entry corresponding
   to the given pc.  */

Proc RTStack__Find (unsigned long pc)
{
  int n = PSIZE;
  int lo = 0;
  int hi = n;
  int mid;

  while (lo < hi) {
    mid = (lo+hi) / 2;
    if (pc < _procedure_table[mid].adr) {
      hi = mid;
    } else {
      lo = mid+1;
    }
  }
  if (lo > 0) {
    lo--;
  }
  return (&_procedure_table[lo]);
}


/*---------------------------------------------------------------------------*/
/* PROCEDURE GetThreadFrame (VAR f: Frame;  start: ADDRESS;  len: INTEGER);
   Return in "f" the frame of the thread whose machine state is in bytes
   [start .. start+len).  Returns with f.pc=NIL on failure. */

void RTStack__GetThreadFrame (Frame *f, char *start, int len)
{
  jmp_buf *env;

  f->pc = 0;
  f->sp = 0;
  if (len == sizeof (jmp_buf)) {
    env = (jmp_buf *)start;
    f->pc = (*env)[JB_PC];
    f->sp = (*env)[JB_SP];
  }
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
   Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. */

char* RTStack__ProcName (Frame *f)
{
  Proc p = RTStack__Find (f->pc);
  if (!p) return 0;
  return (&_procedure_string_table[p->irpss]);
}


/*---------------------------------------------------------------------------*/
/* PROCEDURE PreviousFrame (READONLY f: Frame): Frame;
   Return the stack frame that called "f".  Returns with pc = NIL if
   "f" is the first frame on the stack or its predecessor is ill-formed. */

RTStack__PrevFrame (Frame* callee, Frame* caller)
{
  Proc p = RTStack__Find (callee->pc);
  Frame f;
  int i;
  int n;
  jmp_buf* context;

  if (callee == 0) abort ();
  if (caller == 0) abort ();

  /* update the SP */
  f.sp = callee->sp + p->frameoffset;

  /* update the PC */
  if (p->regmask & (1 << p->pcreg)) {
    /* standard frame */
    n = f.sp + p->regoffset;
    for (i = p->pcreg+1; i != 32; i++) {
      if (p->regmask & (1 << i)) {
        n -= 4;
      }
    }
    f.pc = *((unsigned long *) n);

  } else if ((p->adr == (unsigned long)sigvec) && (p->regmask == 0)) {
    /* ouch. we're trying to walk through a signal handler */
    context = ((InterruptFrame*)(callee->sp)) -> signal_context;
    f.pc = (*context)[JB_PC];
    f.sp = (*context)[JB_SP];

  } else if (p->pcreg == 0) {
    /* final frame */
    f.pc = 0;

  } else {
    /* we're lost... */
    f.pc = 0;
    /*** abort (); ***/
  }

  *caller = f;
}


/*---------------------------------------------------------------------------*/
/* PROCEDURE Unwind (READONLY f: Frame);
   Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call. */

#define RI(x)  ((x) - JB_REGS)
#define RF(x)  ((x) - JB_FREGS)

void RTStack__Unwind (Frame *target)
{
  Frame f;
  Proc p, target_proc;
  jmp_buf env;
  jmp_buf *context;
  int saw_signal_handler = 0;

  { 
    /*
     * Make a jmp_buf keeping the up-to-date register values,
     * keeping the old values of the PC, SP, sigmask, fpc_csr,
     * and the sigsave flag.
     */
    env[JB_ONSIGSTK] = 0; /* Ultrix setjmp doesn't set this field! */
    setjmp (env);  /* SEE _longjmp NOTE BELOW */

    env[JB_PC] = target->pc;
    env[JB_SP] = target->sp;
  }

  target_proc = RTStack__Find (target->pc);
  dprintf ((stderr, "target=%s  pc=%x  sp=%x\n",
	    RTStack__ProcName (target_proc), target->pc, target->sp));

  f = RTStack__CurFrame ();
  p = RTStack__Find (f.pc);
  if (!p) return;

  /* unwind the stack, storing saved registers into the jmp_buf */
  while (target_proc != p || f.sp != target->sp) {
    int i;
    int n;
    long m;
    int ro[32];
    unsigned long *r;

    dprintf ((stderr, "unwinding through %s\n", RTStack__ProcName (p)));
    dprintf ((stderr, "current pc=%x  sp=%x\n", f.pc, f.sp));

    if ((p->adr == (unsigned long)sigvec) && (p->regmask == 0)) {
      /* ouch. we're walking through a signal handler */
      context = ((InterruptFrame*)(f.sp)) -> signal_context;
      f.pc = (*context)[JB_PC];
      f.sp = (*context)[JB_SP];
      memcpy (env, context, sizeof (jmp_buf));
      env[JB_PC] = target->pc;
      env[JB_SP] = target->sp;
      env[JB_MAGIC] = JBMAGIC; /* apparently not set by kernel? */
      saw_signal_handler = 1;

    } else {
      /* a standard frame */
      f.sp += p->frameoffset;    /* fix SP */

      /* find offsets of saved integer registers */
      m = p->regmask;
      n = 0;
      for (i = 31; i != -1; i--) {
        if (m & (1 << i)) {
          ro[i] = n--;
        }
      }
      r = (unsigned long *) (f.sp + p->regoffset);

      /* restore integer registers */
      if (m & (1 << RI (JB_GP))) { env[JB_GP] = r[ro[RI (JB_GP)]]; }
      if (m & (1 << RI (JB_S0))) { env[JB_S0] = r[ro[RI (JB_S0)]]; }
      if (m & (1 << RI (JB_S1))) { env[JB_S1] = r[ro[RI (JB_S1)]]; }
      if (m & (1 << RI (JB_S2))) { env[JB_S2] = r[ro[RI (JB_S2)]]; }
      if (m & (1 << RI (JB_S3))) { env[JB_S3] = r[ro[RI (JB_S3)]]; }
      if (m & (1 << RI (JB_S4))) { env[JB_S4] = r[ro[RI (JB_S4)]]; }
      if (m & (1 << RI (JB_S5))) { env[JB_S5] = r[ro[RI (JB_S5)]]; }
      if (m & (1 << RI (JB_S6))) { env[JB_S6] = r[ro[RI (JB_S6)]]; }
      if (m & (1 << RI (JB_S7))) { env[JB_S7] = r[ro[RI (JB_S7)]]; }
      if (m & (1 << RI (JB_S8))) { env[JB_S8] = r[ro[RI (JB_S8)]]; }

      /* restore PC */
      if (!(m & (1 << p->pcreg))) { abort (); }
      f.pc = r[ro[p->pcreg]];
  
      /* find offsets of saved fp registers */
      m = p->fregmask;
      n = 0;
      for (i = 31; i != -1; i--) {
        if (m & (1 << i)) {
          ro[i] = n--;
        }
      }
      r = (unsigned long *) (f.sp + p->fregoffset);

      /* restore fp registers */
      if (m & (1 << RF(JB_F20))) { env[JB_F20] = r[ro[RF (JB_F20)]]; }
      if (m & (1 << RF(JB_F21))) { env[JB_F21] = r[ro[RF (JB_F21)]]; }
      if (m & (1 << RF(JB_F22))) { env[JB_F22] = r[ro[RF (JB_F22)]]; }
      if (m & (1 << RF(JB_F23))) { env[JB_F23] = r[ro[RF (JB_F23)]]; }
      if (m & (1 << RF(JB_F24))) { env[JB_F24] = r[ro[RF (JB_F24)]]; }
      if (m & (1 << RF(JB_F25))) { env[JB_F25] = r[ro[RF (JB_F25)]]; }
      if (m & (1 << RF(JB_F26))) { env[JB_F26] = r[ro[RF (JB_F26)]]; }
      if (m & (1 << RF(JB_F27))) { env[JB_F27] = r[ro[RF (JB_F27)]]; }
      if (m & (1 << RF(JB_F28))) { env[JB_F28] = r[ro[RF (JB_F28)]]; }
      if (m & (1 << RF(JB_F29))) { env[JB_F29] = r[ro[RF (JB_F29)]]; }
      if (m & (1 << RF(JB_F30))) { env[JB_F30] = r[ro[RF (JB_F30)]]; }
      if (m & (1 << RF(JB_F31))) { env[JB_F31] = r[ro[RF (JB_F31)]]; }
    }

    /* f has been advanced to the next frame, find the corresponding proc */
    if (f.pc == 0) { abort (); }
    p = RTStack__Find (f.pc);
  }

  longjmp (env, 1);
  /**************************************************************
    _longjmp NOTE:  The Ultrix version of _longjmp is buggy.
    It resets the stack pointer before it finishes reading values
    from the jump buffer.  So, when a signal is delivered in the
    middle of _longjmp, the jump buffer can get clobbered.  Most
    C programs don't suffer because the jump buffer is below the
    new stack pointer and protected from the signal handler.  In
    this code, the jump buffer is synthesized above the new stack
    pointer and therefore exposed to the danger....

    We'll use the full setjmp/longjmp instead.


  if (saw_signal_handler) {
    longjmp (env, 1);    (* do a full longjmp to destination *)
  } else {
    _longjmp (env, 1);   (* cheap longjmp to destination *)
  };
  ********************************************************************/
}



