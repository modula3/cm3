/* Copyright (C) 1995, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Thu May  4 09:13:59 PDT 1995 by kalsow     */

/* This file implements the default stack walking functions of
   the RTStack interface for platforms that don't support stack
   walking. */

#include <stdlib.h>

/* TYPE Frame = RECORD pc, sp: ADDRESS END; */
typedef struct {
  unsigned long pc;
  unsigned long sp;
} Frame;


/*---------------------------------------------------------------------------*/
/* PROCEDURE GetThreadFrame (VAR f: Frame;  start: ADDRESS;  len: INTEGER);
   Return in "f" the frame of the thread whose machine state is in bytes
   [start .. start+len).  Returns with f.pc=NIL on failure. */

void RTStack__GetThreadFrame (f, start, len)
     Frame *f;
     char *start;
     int  len;
{
  abort ();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame);
   Return in "f" the frame of its caller.  Returns with pc = NIL on failure. */

void RTStack__CurFrame (f)
     Frame *f;
{
  abort ();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE PreviousFrame (READONLY f: Frame): Frame;
   Return the stack frame that called "f".  Returns with pc = NIL if
   "f" is the first frame on the stack or its predecessor is ill-formed. */

RTStack__PrevFrame (callee, caller)
     Frame *callee;
     Frame *caller;
{
  abort ();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE Unwind (READONLY f: Frame);
   Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call. */

void RTStack__Unwind (target)
     Frame *target;
{
  abort ();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
   Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. */

char* RTStack__ProcName (f)
     Frame *f;
{
  return (char*)0;
}



