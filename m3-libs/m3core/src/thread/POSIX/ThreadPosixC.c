/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/* This is based on RTThread.m3, which is platform specific
 * and varied a lot. Some versions cached the mask to use
 * in allow/disallow, some recomputed it each time.
 * Some used sigaction(), some used signal().
 * The users of sigaction() vary as to which flags they use.
 * Some use BSD sigvec which is similar to sigaction.
 */

/* _XOPEN_SOURCE is required to get the correct MacOSX/x86
 * ucontext_t declaration. Otherwise getcontext overruns
 * its parameter.
 */
#define _XOPEN_SOURCE 500
//#define _BSD_SOURCE deprecated
#define _DEFAULT_SOURCE
#define _XPG4_2
#define _DARWIN_C_SOURCE

#ifdef __OpenBSD__
#error User threads not supported on OpenBSD (no get/set/make/swapcontext nor working sigaltstack?)
#endif

#if (defined(__APPLE__) && defined(__x86_64__)) \
    || (defined(__FreeBSD__) && (__FreeBSD__ < 5))
/* http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
 * http://www.engelschall.com/pw/usenix/2000/pmt-html/
 * Sigaltstack is more portable -- OpenBSD and Darwin/AMD64 do not
 * implement get/set/make/swapcontext. Ditto FreeBSD < 5.
 * But OpenBSD's sigaltstack seems to have problems.
 */
#define M3_USE_SIGALTSTACK
#endif

#if (defined(__FreeBSD__) && (__FreeBSD__ >= 5))
#define __BSD_VISIBLE 1
#endif

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#ifdef __DJGPP__
#include <sys/exceptn.h>
#include <dpmi.h>
#else
#include <ucontext.h>
#endif

M3_EXTERNC_BEGIN

typedef void (*SignalHandler1)(int signo);

#define setup_sigvtalrm     ThreadPosix__setup_sigvtalrm
#define allow_sigvtalrm     ThreadPosix__allow_sigvtalrm
#define allow_othersigs     ThreadPosix__allow_othersigs
#define disallow_signals    ThreadPosix__disallow_signals
#define MakeContext         ThreadPosix__MakeContext
#define SwapContext         ThreadPosix__SwapContext
#define DisposeContext      ThreadPosix__DisposeContext
#define ProcessContext      ThreadPosix__ProcessContext
#define InitC               ThreadPosix__InitC

static sigset_t ThreadSwitchSignal;
static sigset_t OtherSignals;
static sigset_t AllSignals;

#if !(defined (__CYGWIN__) || defined (__DJGPP__)) || defined (SIGVTALRM)
#define SIG_TIMESLICE SIGVTALRM
#else
#define SIG_TIMESLICE SIGALRM
#endif

#define Context ThreadPosix__Context
#define Stack ThreadPosix__Stack

struct Context;
typedef struct Context Context;

void
__cdecl
DisposeContext(Context** c);

void
__cdecl
setup_sigvtalrm(SignalHandler1 handler)
{
  struct sigaction act; /* (VT)ALRM signal */
  struct sigaction oct; /* other signals (specifically SIGCHLD) */

  ZERO_MEMORY(act);
  ZERO_MEMORY(oct);

  sigemptyset(&OtherSignals);
  sigemptyset(&ThreadSwitchSignal);
  sigemptyset(&AllSignals);
  sigemptyset(&act.sa_mask);
  sigemptyset(&oct.sa_mask);

  sigaddset(&OtherSignals, SIGSEGV);
  sigaddset(&AllSignals, SIGSEGV);
  sigaddset(&ThreadSwitchSignal, SIG_TIMESLICE);
  sigaddset(&AllSignals, SIG_TIMESLICE);
#if !defined (__DJGPP__) || defined (SIGCHLD)
  sigaddset(&OtherSignals, SIGCHLD);
  sigaddset(&AllSignals, SIGCHLD);
#endif

  oct.sa_handler = handler;
  act.sa_handler = handler;

#if !defined (__DJGPP__) || defined (SA_RESTART)
  oct.sa_flags = SA_RESTART;
  act.sa_flags = SA_RESTART;
#endif
  if (sigaction(SIG_TIMESLICE, &act, NULL)) abort();
#if !defined (__DJGPP__) || defined (SIGCHLD)
  if (sigaction(SIGCHLD, &oct, NULL)) abort();
#endif
  if (sigaction(SIGSEGV, &oct, NULL)) abort();
}

void
__cdecl
allow_sigvtalrm(void)
{
    int i = sigprocmask(SIG_UNBLOCK, &ThreadSwitchSignal, NULL);
    assert(i == 0);
}

void
__cdecl
allow_othersigs(void)
{
    int i = sigprocmask(SIG_UNBLOCK, &OtherSignals, NULL);
    assert(i == 0);
}

void
__cdecl
disallow_signals(void)
{
    int i = sigprocmask(SIG_BLOCK, &AllSignals, NULL);
    assert(i == 0);
}

struct Context {
    // Stack; storage here is overkill, but ok.
    char* free;
    char* map;
    size_t map_size;
    size_t pagesize;
    char* mprotect[2];

#if defined (__DJGPP__) || defined (M3_USE_SIGALTSTACK)
    sigjmp_buf jb;
#else
    ucontext_t uc;
#endif
};

#ifdef M3_USE_SIGALTSTACK

#define M3_SWAP_CONTEXT(oldc, newc)    \
do {                                   \
    if (sigsetjmp((oldc)->jb, 1) == 0) \
        siglongjmp((newc)->jb, 1);     \
} while (0)

#endif

#ifdef M3_USE_SIGALTSTACK

#define xMakeContext ThreadPosix__xMakeContext

static Context mctx_caller;
static sig_atomic_t volatile mctx_called;
static Context * volatile mctx_create;
static void (* volatile mctx_create_func)(void);
static sigset_t mctx_create_sigs;

static void __cdecl mctx_create_boot(void)
{
    void (*volatile mctx_start_func)(void) = { 0 };
    
    sigprocmask(SIG_SETMASK, &mctx_create_sigs, NULL);
    mctx_start_func = mctx_create_func;
    M3_SWAP_CONTEXT(mctx_create, &mctx_caller);
    mctx_start_func();
    abort(); /* not reached */
}

static void __cdecl mctx_create_trampoline(int sig)
{
    if (sigsetjmp(mctx_create->jb, 0) == 0)
    {
        mctx_called = 1;
        return;
    }
    
    mctx_create_boot();
}

void
__cdecl
xMakeContext( 
    Context *context, 
    void (*function)(void),
    void *stack,
    WORD_T stack_size) 
{
    struct sigaction sa;
    struct sigaction osa;
    stack_t ss;
    stack_t oss;
    sigset_t osigs;
    sigset_t sigs;

    ZERO_MEMORY(sa);
    ZERO_MEMORY(osa);
    ZERO_MEMORY(ss);
    ZERO_MEMORY(oss);
    ZERO_MEMORY(osigs);
    ZERO_MEMORY(sigs);

    sigemptyset(&sigs);
    sigaddset(&sigs, SIGUSR1);
    sigprocmask(SIG_BLOCK, &sigs, &osigs);
    
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
    
    M3_SWAP_CONTEXT(&mctx_caller, context);
}

#endif /* M3_USE_SIGALTSTACK */

#ifdef __DJGPP__
extern const unsigned _stklen;
//const unsigned _stklen = 1UL << 20; // 1MB todo verify this works for initial stack (stub is supposed to use this)
#endif

void *
__cdecl
MakeContext(void (*p)(void), INTEGER words)
{
  int err1 = 0;
  int err2 = 0;
  Context* c = (Context*)calloc (1, sizeof(*c));
  size_t size = sizeof(void *) * words;
  size_t pagesize = getpagesize();
  char* aligned_start = 0;
  char* aligned_end = 0;
  char* unaligned_start = 0;
  char* unaligned_end = 0;
  size_t pages = 0;
  size_t align = 0;

  if (c == NULL)
    goto Error;

  c->pagesize = pagesize;

#if !defined (__DJGPP__) || defined (MINSIGSTKSZ)
  if (size < MINSIGSTKSZ) size = MINSIGSTKSZ;
#else
  if (size < _stklen) size = _stklen; // 1MB
#endif

  /* Round up to a whole number of pages, and
   * allocate two extra pages, one at the start
   * and one at the end, and don't allow accessing
   * either one (catch stack overflow and underflow).
   */
  pages = (size + pagesize - 1) / pagesize + 2;
  // For djgpp we do not have an aligned allocator.
  // memalign and valloc fail. Therefore add two more pages.
  // We need a page at each end to align and a page at each
  // end to protect.
#ifdef __DJGPP__
  pages += 2;
#endif
  size = pages * pagesize;

  if (size <= 0)
    goto Error;

#ifdef __DJGPP__
  c->free = unaligned_start = (char*)malloc (size);
#else
  c->map = unaligned_start = aligned_start = (char*)mmap (NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  c->map_size = size;
#endif
  if (unaligned_start == NULL)
    goto Error;

  align = ((size_t)unaligned_start) % pagesize;
#ifndef __DJGPP__
  assert (align == 0);
#endif
  aligned_start = unaligned_start + (align ? pagesize : 0) - align;
  unaligned_end = unaligned_start + size;
  aligned_end =  unaligned_end - align;

  assert ((((size_t)aligned_start) % pagesize) == 0);
  assert ((((size_t)aligned_end) % pagesize) == 0);

  err1 = mprotect(aligned_start, pagesize, PROT_NONE);
  if (!err1)
    c->mprotect[0] = aligned_start;
  err2 = mprotect(aligned_end - pagesize, pagesize, PROT_NONE);
  if (!err2)
    c->mprotect[1] = aligned_end - pagesize;
  // mprotect is DPMI 1.0 and therefore often fails.
  // e.g. it works under MS-DOS (cwsdpmi) but not Windows 98.
#ifndef __DJGPP__
  if (err1 | err2)
  {
    DisposeContext (&c);
    c = NULL;
    abort();
    goto Error;
  }
#endif

#ifdef __DJGPP__
    // Get a reasonable context from current thread and set eip/esp manually.
    // TODO signal mask? It is easy to capture here, but we do not set it.
    if (sigsetjmp(c->jb, 0) == 0)
    {
        c->jb->__esp = (size_t)(aligned_end - pagesize);
        c->jb->__eip = (size_t)p;
        //*(size_t*)(c->jb->__esp -= sizeof(size_t)) = (size_t)arg;
        *(size_t*)(c->jb->__esp -= sizeof(size_t)) = 0; // return addres and alignment
        *(size_t*)(c->jb->__esp -= sizeof(size_t)) = 0; // return addres and alignment
        *(size_t*)(c->jb->__esp -= sizeof(size_t)) = 0; // return addres and alignment
        *(size_t*)(c->jb->__esp -= sizeof(size_t)) = 0; // return addres and alignment
    }
#elif defined(M3_USE_SIGALTSTACK)
  xMakeContext(c, p, aligned_start + pagesize, size - 2 * pagesize);
#else
  if (getcontext(&(c->uc))) abort();
  c->uc.uc_stack.ss_sp = aligned_start + pagesize;
  c->uc.uc_stack.ss_size = size - 2 * pagesize;
  c->uc.uc_link = 0;
  makecontext(&(c->uc), p, 0);
#endif /* M3_USE_SIGALTSTACK */

  return c;
Error:
  DisposeContext (&c);
  return NULL;
}

#ifdef __DJGPP__
static void CopyContext (jmp_buf to, jmp_buf from)
{
    // This is based on setjmp/longjmp.
    assert(from->__eip);
    to->__eip = from->__eip;
    to->__ebx = from->__ebx;
    to->__ecx = from->__ecx;
    to->__edx = from->__edx;
    to->__ebp = from->__ebp;
    to->__esi = from->__esi;
    to->__edi = from->__edi;
    to->__esp = from->__esp;
    to->__eflags = from->__eflags;
}
#endif

void
__cdecl
SwapContext(Context *from, Context *to)
{
#ifdef __DJGPP__
    // If we are in a exception handler, swap with its resume context.
    // longjmp out of exception handler does not work.
    if (__djgpp_exception_state_ptr)
    {
        CopyContext (from->jb, *__djgpp_exception_state_ptr);
        CopyContext (*__djgpp_exception_state_ptr, to->jb);
    }
    else if (sigsetjmp (from->jb, 1) == 0)
        siglongjmp (to->jb, 1);
#elif defined (M3_SWAP_CONTEXT)
  M3_SWAP_CONTEXT(from, to);
#else
  if (swapcontext(&(from->uc), &(to->uc))) abort();
#endif
}

void
__cdecl
DisposeContext(Context** pc)
{
    Context* c = *pc;
    if (c)
    {
        int i;
        int er = errno;
        *pc = 0;
        for (i = 0; i <= 1; ++i)
        {
            if (c->mprotect[i])
                mprotect(c->mprotect[i], c->pagesize, PROT_READ | PROT_WRITE);
        }
        free(c->free);
#ifndef __DJGPP__
        if (c->map)
            if (munmap(c->map, c->map_size)) abort();
#endif
        free(c);
        errno = er;
    }
}

M3_NO_INLINE // because alloca
void
__cdecl
ProcessContext(Context *c, char *bottom, char *top,
               void (*p) (void *start, void *limit))
{
  if (top == NULL)
  {
    /* live thread */
    /* do we need to flush register windows too? */
#if defined (M3_USE_SIGALTSTACK) || defined (__DJGPP__)
    sigsetjmp(c->jb, 0);
#else
    if (getcontext(&(c->uc))) abort();
#endif
    top = (char*)alloca(1);
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

int
__cdecl
ThreadPosix__SetVirtualTimer(void)
{
    struct timeval selected_interval;
    struct itimerval it;

    ZERO_MEMORY(selected_interval);
    ZERO_MEMORY(it);
    selected_interval.tv_sec = 0;
    selected_interval.tv_usec = 100 * 1000;
    it.it_interval = selected_interval;
    it.it_value    = selected_interval;
#if !defined (__DJGPP__) || defined (ITIMER_VIRTUAL)
    return setitimer(ITIMER_VIRTUAL, &it, NULL);
#else
    return setitimer(ITIMER_REAL, &it, NULL);
#endif
}

int ThreadPosix__DisableInterrupts (void)
{
#ifdef __DJGPP__
    return __dpmi_get_and_disable_virtual_interrupt_state ();
#else
    return 0;
#endif
}

void ThreadPosix__EnableInterrupts (int state)
{
#ifdef __DJGPP__
    (void)__dpmi_get_and_set_virtual_interrupt_state (state);
#endif
}

M3_EXTERNC_END
