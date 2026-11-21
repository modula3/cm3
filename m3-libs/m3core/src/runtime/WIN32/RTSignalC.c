/* Copyright 1996-2000, Critical Mass, Inc.   All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */
#include <windows.h>
#if __cplusplus
extern "C" {
#endif

#if defined(_M_IX86) || defined(i386) || defined(_X86_)
#undef AMD64
#undef ARM64
#undef i386
#define i386 1
#elif defined(_M_AMD64) || defined(AMD64) || defined(_AMD64_) || defined(__x86_64__)
#undef AMD64
#undef ARM64
#undef i386
#define AMD64 1
#elif defined(_M_ARM64) || defined(ARM4) || defined(_ARM4_)
#undef AMD64
#undef ARM64
#undef i386
#define ARM64 1
#else
#error unknown architecture
#endif

void __stdcall RTSignal__SystemError(size_t, size_t, size_t);

size_t RTSignalC_Raise(size_t rte, size_t err, size_t pc, PCONTEXT context)
{
/* Alter the thread's stack and PC so that when it resumes, it looks
   like it magically called "SystemError (rte, err, pc)" */

#if i386
  typedef struct Frame{size_t ret_pc, rte, err, pc;}Frame;
  volatile Frame*f=0; /* write to highest address first */
#elif AMD64
  size_t volatile * sp=0;
#endif

  if (!context) return 0;

#if i386
  f = (Frame*)(context->Esp - sizeof(Frame));
#if __GNUC__
  if (IsBadWritePtr(f, sizeof(*f))) return 0;
#else
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

#elif AMD64
  sp = (size_t*)context->Rsp;
  sp -= 1;
#if __GNUC__
  if (IsBadWritePtr(sp, sizeof(*sp))) return 0;
#else
  __try  {
#endif
    *sp = pc;
#if !__GNUC__
  } __except(1) {
    /* it doesn't look like we can push to stack */
    return 0;
  }
#endif
  /* This is very dangerous and SystemError must not return.
   * It corrupts volatile registers mid-function.
   * The proper way to do this is to push a context,
   * or really just call SystemError from the unhandled exception filter.
   * TODO.
   */
  context->Rsp = (size_t)sp;
  context->Rip = (size_t)&RTSignal__SystemError;
  context->Rcx = rte;
  context->Rdx = err;
  context->R8 = pc;

#elif ARM4
  /* This is very dangerous and SystemError must not return.
   * It corrupts volatile registers mid-function.
   * The proper way to do this is to push a context,
   * or really just call SystemError from the unhandled exception filter.
   * TODO.
   */
  context->Lr = pc; /* Dangerous? i.e. in prologue right before pushing? */
  context->Pc = (size_t)&RTSignal__SystemError;
  context->X0 = rte;
  context->X1 = err;
  context->X2 = pc;
#endif
  return 1;
}
#if __cplusplus
}
#endif
