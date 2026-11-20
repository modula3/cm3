/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
#include <stdlib.h>

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

typedef struct {
  void* pc;
  void* sp;
} Frame;

/*---------------------------------------------------------------------------*/
/* PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
   Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. */
char* RTStack__ProcName (Frame *f)
{
  return 0;
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
  f->pc = f->sp = 0;
  /* This is only for printing stack traces, not for exception handling
  so does not really have to do anything. */
#if 0
  CONTEXT alignedContext={0};
  RtlCaptureContext(&alignedContext);
  f->context = alignedContext;
  GetCurrentThreadStackLimits(&f->StackLowLimit, &f->StackHighLimit);
#endif
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE PreviousFrame (READONLY callee: Frame;  VAR(*OUT*) caller: Frame)
   Return the stack frame that called "callee".  Returns with pc = NIL if
   "callee" is the first frame on the stack or its predecessor is ill-formed.
   */

void RTStack__PrevFrame (Frame* callee, Frame* caller)
{
  caller->pc = caller->sp = 0;
  /* This is only for printing stack traces, not for exception handling
  so does not really have to do anything. */
#if 0
  PFUNCTION_ENTRY functionEntry=0;
  UINT64 imageBase=0;
  UINT64 establisherFrame=0;
  UINT64 pc=0;
  UINT64 sp=0;
  *caller = *callee;
  pc = callee->context.Rip;
  sp = callee->context.sp;
  if ((sp & 7) || sp < callee->StackLowLimit || sp >= callee->StackHighLimit || pc == *(size_t*)sp) {
    callee->context.Rip = 0;
    callee->context.Rsp = 0;
    return;
  }
  functionEntry = RtlLookupFunctionEntry(callee->pc, &imageBase, 0);
  if (functionEntry) {
    RtlVirtualUnwind(0, imageBase, callee->pc, functionEntry, caller, 0, 0, &establisherFrame);
  } else {
    caller->context.Rip = *(size_t*)(caller->context->Rsp);
    caller->context.Rsp += sizeof(size_t);
  }
#endif
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE Unwind (READONLY f: Frame);
   Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call. */

void RTStack__Unwind (Frame *target)
{
    abort();
    /*RtlUnwind(&target->context, rip, exc, ret);*/
}

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
