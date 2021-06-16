/* Copyright (C) 2018-2019 Intel Corporation */
/* SPDX-License-Identifier: BSD-3-Clause */
/* see the file COPYRIGHT-INTEL for more information */

/*
 * Coroutines for Modula-3 !
 *
 * Author : Mika Nystrom <mika.nystroem@intel.com>
 * April, 2018
 *
 * Much of this code is a hybrid of the PTHREADS code by Tony Hosking
 * and the POSIX code by DEC-SRC.
 */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

M3EXTERNC_BEGIN

/* expand C names */
#define Context ContextC__TValue  /* ContextC__T points to ContextC__TValue; see m3core.h */

void __cdecl Coroutine__RunInternal(void* inhibit);
void* __cdecl Coroutine__CallInternal(void* to, INTEGER* myId, void** me);

// This code has only been tested on Linux/amd64.
#if !(defined(__x86_64__) && defined(__linux))

BOOLEAN
__cdecl
Coroutine__Supported(void)
{
    return FALSE;
}

void
__cdecl
ContextC__SetLink(Context* target, Context* src)
{
    assert(0);
}

Context* // ContextC_T
__cdecl
ContextC__MakeContext(void    (*p)(void*),  // CoroutineUcontext__Entry
                      INTEGER words,
                      void    *arg)         // CoroutineUcontext__Arg
{
    assert(0);
    return 0;
}

void
__cdecl
ContextC__SwapContext(Context* from, Context* to)
{
    assert(0);
}

void
__cdecl
ContextC__DisposeContext(Context* c)
{
    assert(0);
}

Context*
__cdecl
ContextC__Current(void)
{
    assert(0);
    return 0;
}

void
__cdecl
ContextC__SetCurrentCoroutine(INTEGER* value)
{
    assert(0);
}

INTEGER*
__cdecl
ContextC__GetCurrentCoroutine(void)
{
    assert(0);
    return 0;
}

void
__cdecl
ContextC__InitC(int* stack)
{
    assert(0);
}

ADDRESS
__cdecl
ContextC__GetStackBase(Context* c)
{
    assert(0);
    return 0;
}

void
__cdecl
ContextC__PushContextForRun(void* inhibit, Context *c)
{
    assert(0);
}

void*
__cdecl
ContextC__PushContextForCall(void* to, INTEGER* myId, void** me, Context *c)
{
    assert(0);
    return 0;
}

#else

BOOLEAN
__cdecl
Coroutine__Supported(void)
{
    return TRUE;
}

#define M3_RETRY(expr)                                  \
  r = (expr);                                           \
  if (r == EAGAIN || r == ENOMEM || r == ENOSPC)        \
  {                                                     \
    /* try again right away */                          \
    r = (expr);                                         \
    if (r == EAGAIN || r == ENOMEM || r == ENOSPC)      \
    {                                                   \
      /* try again after short delay */                 \
      sleep(1);                                         \
      r = (expr);                                       \
    }                                                   \
  }

#define DEBUG 1

static int stack_grows_downward;

/* expand C names */
#define Closure ContextC__Closure /* This is not Coroutine__Closure, which exists, nearby, is something else. */

struct Closure; typedef struct Closure Closure;
struct Closure
{
  void (*p)(void*);
  void *arg;
};

struct Context
{
  void      *stackaddr;
  WORD_T     stacksize;
  void      *stackbase; /* this is the base reported to M3's GC */
  ucontext_t uc;
  ucontext_t pc; /* post context for cleanup */
  int        alive;
};

/* a truly clever implementation would:

   not store uc in the Context, but instead put it on the stack.  This
   ought to be possible: uc is used for two purposes.  One is to keep
   track of where we are in the coroutine so we can restart properly.
   The second is so that GC can scan the registers of the thread.
   Note that we need the uc on the stack at the same time (when the 
   coroutine is inactive).  When the coroutine is active, the code in
   ThreadPThread will *ensure* that the context is pushed anyhow. 

   We're not this clever yet.
*/

static void
trampoline(unsigned low32, unsigned high32)
{
  // make a pointer from two integers
  uintptr_t const low = low32;
  uintptr_t const high = high32;
  Closure* const cl = (Closure*)((high << 32) | low);
  
  cl->p(cl->arg);
}
  
static Context*
ContextC__New(void)
{
  Context *res=(Context*)calloc(1,sizeof(Context));
  res->alive = 1;
  return res;
}

void
ContextC__SetLink(Context *tgt, Context *src)
{
  tgt->uc.uc_link = &(src->uc);
}

static void
cleanup(unsigned low32, unsigned high32)
{
  uintptr_t const low = low32;
  uintptr_t const high = high32;
  /* this is the cleanup routine
     it is called when a context falls off the end (apply ends) 
  */
  Context* const c = (Context*)((high << 32) | low);

  c->alive = 0;

  assert(c->uc.uc_link);

  /* this is the only tricky part:
     the semantics of creating the context are that the followon context
     must be set when the context is created.

     this is not the semantics we are going for here.  In our implementation,
     when a coroutine "falls off the end", we resume with the context that
     called it (and none other!)  This next coroutine is not known when
     the dying coroutine is created, only when it is called.

     this is implemented by updating uc_link on every coroutine call.
     but it can't take effect like that, so we make a special catcher context 
     (in which we are here in cleanup) that jumps explicitly to that context.

     An alternate design would call in here to cleanup directly from the end
     of CoroutineUcontext.Run .  That would probably be simpler and work
     OK, too.
  */
  setcontext(c->uc.uc_link);
}

Context* // ContextC_T
ContextC__MakeContext(void    (*p)(void*),   // CoroutineUcontext__Entry
                      INTEGER words,
                      void*   arg)           // CoroutineUcontext__Arg
{
  /* from ThreadPosixC.c */
  Context *c = (Context *)calloc (1, sizeof(*c));
  INTEGER size = sizeof(void *) * words;
  INTEGER pagesize = getpagesize();
  char *sp = { 0 };
  INTEGER pages = { 0 };
  int er = { 0 };
  Closure *cl = (Closure *)calloc(1, sizeof(*cl));
  unsigned low, high;
  char *slim, *sbeg; /* limits of stack, for bookkeeping */
  
  if (c == NULL || cl == NULL)
    goto Error;

  c->alive = 1;
  
  if (size <= 0) return c;
  if (size < MINSIGSTKSZ) size = MINSIGSTKSZ;

  /* 
   * Round up to a whole number of pages, and
   * allocate three extra pages.  Two pages for the redzone and one for 
   * the arg pointer 
   */
  pages = (size + pagesize - 1) / pagesize + 3;
  size = pages * pagesize;
  sp = (char*)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);

  if (DEBUG) fprintf(stderr, "creating coroutine stack %p\n", sp);
  
  if (sp == NULL) goto Error;

  c->stackaddr = sp;
  c->stacksize = size;

  /* range for the stack is [sp, sp + size) */

  sbeg = sp;
  slim = sp + size;

  /*
   * redzone one page at the start
   * and one at the end, don't allow accessing
   * either one (catch stack overflow and underflow).
   */

  /* mprotect first and last page */
  if (mprotect(sbeg                , pagesize, PROT_NONE)) abort();
  if (mprotect(slim - pagesize     , pagesize, PROT_NONE)) abort();

  /* adjust ends of stack region accordingly */
  sbeg += pagesize;
  slim -= pagesize;

  /* the stack is now ready for use */
  if (stack_grows_downward) {
    c->stackbase = slim;
  } else {
    c->stackbase = sbeg;
  }
  
  /*
   * Modula-3 GC interaction tricks:
   * we need to push arg on the stack to pin it from the gc 
   * we do this by writing it at what could be the initial sp, and 
   * adjust the sp by the size of arg
   *
   * in other words, the sp given to the garbage collector will include
   * arg but the sp given to swapcontext will not 
   */
  if (stack_grows_downward) {
    *(void **)(slim - sizeof(void *)) = arg;

    /* adjust stack region accordingly */
    slim -= sizeof(void *);
  } else {
    *(void **)(sbeg) = arg;

    /* adjust stack region accordingly */
    sbeg += sizeof(void *);
  }
  
  if (getcontext(&(c->uc))) abort();
  if (getcontext(&(c->pc))) abort();
  
  c->uc.uc_stack.ss_sp   = sbeg;
  c->uc.uc_stack.ss_size = slim - sbeg;
  
  cl->p   = p;
  cl->arg = arg;

  /* define the post context for cleanup */
  c->pc.uc_stack = c->uc.uc_stack; /* reuse stack */
  c->pc.uc_link = (ucontext_t*)0;  /* will never go through here */
  
  c->uc.uc_link = &(c->pc);        /* set up cleanup linkage */

  low = (unsigned)(uintptr_t)c;
  high = (unsigned)(((uintptr_t)c) >> 32);
  makecontext(&(c->pc), (void (*)())cleanup, 2, low, high);
  
  low = (unsigned)(uintptr_t)cl;
  high = (unsigned)(((uintptr_t)cl) >> 32);
  makecontext(&(c->uc), (void (*)())trampoline, 2, low, high);

  return c;
Error:
  er = errno;
  free(c);
  free(cl);
  if (sp) munmap(sp, size);
  errno = er;
  return NULL;
}

void
ContextC__SwapContext (Context *from, Context *to)
{
  if(!to->alive) {
    fprintf(stderr,
            "WARNING: calling dead coroutine context %p\n",to);
    return;
  }
    
  if (swapcontext(&(from->uc), &(to->uc))) abort();
}

void
ContextC__DisposeContext (Context *c)
{
  if (munmap((c)->stackaddr, (c)->stacksize)) abort();
  free(c);
}

Context*
ContextC__Current(void)
{
  Context *context = ContextC__New();
  if (getcontext(&(context->uc))) abort();
  return context;
}

//TODO C++ [thread_local]
static pthread_key_t current_coroutine;

void
ContextC__SetCurrentCoroutine(INTEGER *value)
{
  int r = { 0 };
  M3_RETRY(pthread_setspecific(current_coroutine, (void *)value));
  assert(r == 0);
}

INTEGER *
ContextC__GetCurrentCoroutine(void)
{
  INTEGER *res = (INTEGER *)pthread_getspecific(current_coroutine);
  return res;
}

void
ContextC__InitC(int* stack)
{
  int r = { 0 };
  stack_grows_downward = (stack > &r);
#if defined(__APPLE__)   || \
    defined(__FreeBSD__) || \
    defined(__INTERIX)   || \
    defined(__i386__)    || \
    defined(__x86_64__)  || \
    defined(_AMD64_)     || \
    defined(_X86_)
    assert(stack_grows_downward);
#endif
  M3_RETRY(pthread_key_create(&current_coroutine, NULL)); assert(r == 0);
}

ADDRESS
ContextC__GetStackBase(Context *c)
{
  return (ADDRESS)c->stackbase;
}

M3_NO_INLINE
void
__cdecl
ContextC__PushContextForRun(void* inhibit, Context *c)
{
  // Store context in stack and continue.
  union {
    volatile unsigned char a[sizeof(ucontext_t)];
    volatile ucontext_t uc;
  } u;

  // C++ fails to assign volatile struct.
  memcpy((void*)&u.uc, &c->uc, sizeof(ucontext_t));
  Coroutine__RunInternal(inhibit);
  u.a[0]; // Keep uc around.
}

M3_NO_INLINE
void*
ContextC__PushContextForCall(void* to, INTEGER* myId, void** me, Context *c)
{
  // Store context in stack and continue.
  union {
    volatile unsigned char a[sizeof(ucontext_t)];
    volatile ucontext_t uc;
  } u;
  void* result;

  // C++ fails to assign volatile struct.
  memcpy((void*)&u.uc, &c->uc, sizeof(ucontext_t));
  result = Coroutine__CallInternal(to, myId, me);
  u.a[0]; // Keep uc around.
  return result;
}

void*
__cdecl
ContextC__Stack(void)
// Return roughly current stack pointer.
{
    return alloca(1);
}

#endif

M3EXTERNC_END

#undef Closure
#undef Context
