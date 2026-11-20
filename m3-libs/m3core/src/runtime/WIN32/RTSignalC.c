/* Copyright 1996-2000, Critical Mass, Inc.   All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */
#include <windows.h>
#if __cplusplus
extern "C" {
#endif

void __stdcall RTSignal__SystemError(size_t, size_t, void*);

int RTSignalC_Raise(size_t rte, size_t err, size_t pc, PCONTEXT context)
{
/* Alter the thread's stack and PC so that when it resumes, it looks
   like it magically called "SystemError (rte, err, pc)" */

#if _M_IX86 || i386 || _X86_
  typedef struct Frame{size_t ret_pc, rte, err, pc;}Frame;
  volatile Frame*f=0; /* write to highest address first */
#elif _M_AMD64 || AMD64 || _AMD64_
  size_t* sp=0;
#endif

  if (!context) return 0;

#if _M_IX86 || i386 || _X86_
  f = (Frame*)(context->Esp - sizeof(Frame));
  if (IsBadWritePtr(f, sizeof(*f))) return 0;
#if !__GNUC__
  __try  {
#endif
    f->pc = pc;
    f->err = err;
    f->rte = rte; /* runtime error */
    f->ret_pc = pc;
#if !__GNUC__
  } __except(1)
  {
    /* it doesn't look like we can push the arguments on the stack */
    return 0;
  }
#endif

  context->Esp = (size_t)f;
  context->Eip = (size_t)&RTSignal__SystemError;

#elif _M_AMD64 || AMD64 || _AMD64_
  sp = (size_t*)context->Rsp;
  sp -= 1;
  if (IsBadWritePtr(sp, sizeof(*sp))) return 0;
#if !__GNUC__
  __try  {
#endif
    *sp = pc;
#if !__GNUC__
  } __except(1) {
    /* it doesn't look like we can push to stack */
    return 0;
  }
#endif
  context->Rsp = (size_t)sp;
  context->Rip = (size_t)&RTSignal__SystemError;
  context->Rcx = rte;
  context->Rdx = err;
  context->R8 = pc;

#elif _M_ARM64_
  context->Lr = pc; /* Dangerous? i.e. in prologue right before pushing? */
  context->Pc = (size_t)&RTSignal__SystemError;
  context->X0 = rte;
  context->X1 = err;
  context->X2 = pc;
#else
#error unknown 2
#endif
  return 1;
}
#if __cplusplus
}
#endif
