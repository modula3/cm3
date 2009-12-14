/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/* This is based on RTThread.m3, which is platform specific
and varied a lot. Some versions cached the mask to use
in allow/disallow, some recomputed it each time.
Some used sigaction(), some used signal().
The users of sigaction() vary as to which flags they use.
Some use BSD sigvec which is similar to sigaction.
*/

/* _XOPEN_SOURCE is required to get the correct MacOSX/x86
 * ucontext_t declaration. Otherwise getcontext overruns
 * its parameter.
 */
#define _XOPEN_SOURCE 500
#define _BSD_SOURCE
#define _XPG4_2
#define _DARWIN_C_SOURCE

#if (defined(__APPLE__) && defined(__x86_64__)) /*|| defined(__OpenBSD__)*/
/* see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
 * see http://www.engelschall.com/pw/usenix/2000/pmt-html/
 */
#define M3_USE_SIGALTSTACK
#endif

#include "m3core.h"
#include "ThreadPosix.h"
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <assert.h>
#include <setjmp.h>
#include <stddef.h>
#include <errno.h>
#ifdef __OpenBSD__
#include "context.h"
#else
#include <ucontext.h>
#endif
#include <sys/mman.h>

#ifdef __cplusplus
extern "C" {
#endif

#define SignalHandler1      ThreadPosix__SignalHandler1
#define setup_sigvtalrm     ThreadPosix__setup_sigvtalrm
#define allow_sigvtalrm     ThreadPosix__allow_sigvtalrm
#define disallow_sigvtalrm  ThreadPosix__disallow_sigvtalrm
#define MakeContext         ThreadPosix__MakeContext
#define SwapContext         ThreadPosix__SwapContext
#define DisposeContext      ThreadPosix__DisposeContext
#define ProcessContext      ThreadPosix__ProcessContext
#define InitC               ThreadPosix__InitC

static sigset_t *ThreadSwitchSignal = NULL;

#ifdef __CYGWIN__
#define SIG_TIMESLICE SIGALRM
#else
#define SIG_TIMESLICE SIGVTALRM
#endif

void setup_sigvtalrm(SignalHandler1 handler)
{
  static sigset_t tick;
  struct sigaction act, oact;

  ZeroMemory(&act, sizeof(act));
  ZeroMemory(&oact, sizeof(oact));

  sigemptyset(&tick);
  sigaddset(&tick, SIG_TIMESLICE);
  ThreadSwitchSignal = &tick;

  act.sa_handler = handler;
  act.sa_flags = SA_RESTART;
  sigemptyset(&(act.sa_mask));
  if (sigaction (SIG_TIMESLICE, &act, &oact)) abort();
}

void allow_sigvtalrm(void)
{
    int i = sigprocmask(SIG_UNBLOCK, ThreadSwitchSignal, NULL);
    assert(i == 0);
}

void disallow_sigvtalrm(void)
{
    int i = sigprocmask(SIG_BLOCK, ThreadSwitchSignal, NULL);
    assert(i == 0);
}

typedef struct {
  void *stackaddr;
  size_t stacksize;
  void *sp;
#ifdef M3_USE_SIGALTSTACK
  sigjmp_buf jb;
#else
  ucontext_t uc;
#endif
} Context, Context_t;

#ifdef M3_USE_SIGALTSTACK

#define xGetContext ThreadPosix__xGetContext
#define xSetContext ThreadPosix__xSetContext
#define xSwapContext ThreadPosix__xSwapContext
#define xMakeContext ThreadPosix__xMakeContext

#define GetContext(ctx)      \
do {                         \
    xGetContext(ctx);        \
    sigsetjmp((ctx)->jb, 1); \
} while(0)

#define SetContext(ctx)       \
do {                          \
    xSetContext(ctx);         \
    siglongjmp((ctx)->jb, 1); \
} while(0)

#define SWAP_CONTEXT(oldc, newc)        \
do {                                   \
    xSwapContext((oldc), (newc));      \
    if (sigsetjmp((oldc)->jb, 1) == 0) \
        siglongjmp((newc)->jb, 1);     \
} while (0)

void
xGetContext(
    Context_t* context)
{
}

void
xSetContext(
    Context_t* context)
{
}

void
xSwapContext(
    Context_t* oldContext,
    Context_t* newContext)
{
    xGetContext(oldContext);
    xSetContext(newContext);
}

static Context_t mctx_caller;
static sig_atomic_t volatile mctx_called;
static Context_t * volatile mctx_create;
static void (* volatile mctx_create_func)(void);
static sigset_t mctx_create_sigs;

static void mctx_create_boot(void)
{
    void (*mctx_start_func)(void);
    
    sigprocmask(SIG_SETMASK, &mctx_create_sigs, NULL);
    
    mctx_start_func = mctx_create_func;
    
    SWAP_CONTEXT(mctx_create, &mctx_caller);
    
    mctx_start_func();

    abort(); /* not reached */
}

static void mctx_create_trampoline(int sig)
{
    if (sigsetjmp(mctx_create->jb, 0) == 0)
    {
        mctx_called = 1;
        return;
    }
    
    mctx_create_boot();
}

void
xMakeContext( 
    Context_t *context, 
    void (*function)(void),
    void *stack,
    size_t stack_size) 
{
    struct sigaction sa = { 0 };
    struct sigaction osa = { 0 };
    stack_t ss = { 0 };
    stack_t oss = { 0 };
    sigset_t osigs = { 0 };
    sigset_t sigs = { 0 };

    sigemptyset(&sigs);
    sigaddset(&sigs, SIGUSR1);
    sigprocmask(SIG_BLOCK, &sigs, &osigs);
    
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = mctx_create_trampoline;
    sa.sa_flags = SA_ONSTACK;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGUSR1, &sa, &osa);
    
    ss.ss_sp = stack;
    ss.ss_size = stack_size;
    ss.ss_flags = 0;
    sigaltstack(&ss, &oss);
    
    mctx_create = context;
    mctx_create_func = function;
    mctx_create_sigs = osigs;
    mctx_called = 0;
    kill(getpid(), SIGUSR1);
    sigfillset(&sigs);
    sigdelset(&sigs, SIGUSR1);
    while (!mctx_called)
        sigsuspend(&sigs);
        
    sigaltstack(NULL, &ss);
    ss.ss_flags = SS_DISABLE;
    sigaltstack(&ss, NULL);
    if (!(oss.ss_flags & SS_DISABLE))
        sigaltstack(&oss, NULL);
    sigaction(SIGUSR1, &osa, NULL);
    sigprocmask(SIG_SETMASK, &osigs, NULL);
    
    SWAP_CONTEXT(&mctx_caller, context);
}

#endif /* M3_USE_SIGALTSTACK */

void *
MakeContext (void (*p)(void), int words)
{
  Context *c = calloc (1, sizeof(Context));
  size_t size = words * sizeof(void *);
  int pagesize = getpagesize();
  char *sp = NULL;
  int pages;
  int er;

  if (c == NULL)
    goto Error;
  if (size <= 0) return c;
  if (size < MINSIGSTKSZ) size = MINSIGSTKSZ;
  pages = (size + pagesize - 1) / pagesize + 2;
  size = pages * pagesize;
  sp = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (sp == NULL)
    goto Error;
  c->stackaddr = sp;
  c->stacksize = size;
  if (mprotect(sp, pagesize, PROT_NONE)) abort();
  if (mprotect(sp + size - pagesize, pagesize, PROT_NONE)) abort();

#ifdef M3_USE_SIGALTSTACK
  xMakeContext(c, p, sp + pagesize, size - 2 * pagesize);
#else
  if (getcontext(&(c->uc))) abort();
  c->uc.uc_stack.ss_sp = sp + pagesize;
  c->uc.uc_stack.ss_size = size - 2 * pagesize;
  c->uc.uc_link = 0;
  makecontext(&(c->uc), p, 0);
#endif /* M3_USE_SIGALTSTACK */

  return c;
Error:
  er = errno;
  if (c != NULL) free(c);
  if (sp != NULL) munmap(sp, size);
  errno = er;
  return NULL;
}

void SwapContext (Context *from, Context *to)
{
#ifdef M3_USE_SIGALTSTACK
  SWAP_CONTEXT(from, to);
#else
  if (swapcontext(&(from->uc), &(to->uc))) abort();
#endif
}

void DisposeContext (Context **c)
{
  if (munmap((*c)->stackaddr, (*c)->stacksize)) abort();
  free(*c);
  *c = NULL;
}

void
ProcessContext(Context *c, char *bottom, char *top,
	       void (*p) (void *start, void *limit))
{
  size_t xx;
  if (top == NULL) {
    /* live thread */
    /* do we need to flush register windows too? */
#ifdef M3_USE_SIGALTSTACK
    GetContext(c);
#else
    if (getcontext(&(c->uc))) abort();
#endif
    top = (char *)&xx;
  }
  if (bottom < top)
    p(bottom, top);
  else
    p(top, bottom);
#if defined(__APPLE__) && !defined(M3_USE_SIGALTSTACK)
  p(&(c->uc.uc_mcontext[0]), &(c->uc.uc_mcontext[1]));
#else
  p(&c[0], &c[1]);
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
