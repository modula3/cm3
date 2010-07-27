#define _XOPEN_SOURCE 500
#define _BSD_SOURCE
#define _XPG4_2
#define _DARWIN_C_SOURCE

#include <assert.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <unistd.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/signal.h>

typedef ptrdiff_t INTEGER;
#define ZERO_MEMORY(a) (memset(&(a), 0, sizeof(a)))

typedef void (*SignalHandler1)(int signo);

sigset_t ThreadSwitchSignal;

#define SIG_TIMESLICE SIGVTALRM

void
setup_sigvtalrm(SignalHandler1 handler)
{
  struct sigaction act;

  ZERO_MEMORY(act);
  sigemptyset(&ThreadSwitchSignal);
  sigaddset(&ThreadSwitchSignal, SIG_TIMESLICE);

  act.sa_handler = handler;
  act.sa_flags = SA_RESTART;
  sigemptyset(&(act.sa_mask));
  if (sigaction (SIG_TIMESLICE, &act, NULL)) abort();
}

void
allow_sigvtalrm(void)
{
    int i = sigprocmask(SIG_UNBLOCK, &ThreadSwitchSignal, NULL);
    assert(i == 0);
}

void
disallow_sigvtalrm(void)
{
    int i = sigprocmask(SIG_BLOCK, &ThreadSwitchSignal, NULL);
    assert(i == 0);
}

typedef struct {
  void *stackaddr;
  size_t stacksize;
  void *sp;
  sigjmp_buf jb;
} Context;

#define SWAP_CONTEXT(oldc, newc)       \
do {                                   \
    if (sigsetjmp((oldc)->jb, 1) == 0) \
        siglongjmp((newc)->jb, 1);     \
} while (0)

Context mctx_caller;
sig_atomic_t volatile mctx_called;
Context * volatile mctx_create;
void (* volatile mctx_create_func)(void);
sigset_t mctx_create_sigs;

void mctx_create_boot(void)
{
    void (*volatile mctx_start_func)(void);

    fprintf(stderr, "mctx_create_boot\n");
    
    sigprocmask(SIG_SETMASK, &mctx_create_sigs, NULL);
    mctx_start_func = mctx_create_func;
    SWAP_CONTEXT(mctx_create, &mctx_caller);
    mctx_start_func();
    abort(); /* not reached */
}

void mctx_create_trampoline(int sig)
{
    fprintf(stderr, "mctx_create_trampoline\n");
    if (sigsetjmp(mctx_create->jb, 0) == 0)
    {
        mctx_called = 1;
        return;
    }
    
    mctx_create_boot();
}

void
xMakeContext( 
    Context *context, 
    void (*function)(void),
    void *stack,
    size_t stack_size) 
{
    struct sigaction sa;
    struct sigaction osa;
    stack_t ss;
    stack_t oss;
    sigset_t osigs;
    sigset_t sigs;

    fprintf(stderr, "xMakeContext\n");

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
    
    SWAP_CONTEXT(&mctx_caller, context);
}

void *
MakeContext (void (*p)(void), INTEGER words)
{
  Context *c = (Context *)calloc (1, sizeof(*c));
  INTEGER size = sizeof(void *) * words;
  INTEGER pagesize = getpagesize();
  char *sp = { 0 };
  INTEGER pages = { 0 };
  int er = { 0 };

  if (c == NULL)
    goto Error;
  if (size <= 0) return c;
  if (size < MINSIGSTKSZ) size = MINSIGSTKSZ;

  /* Round up to a whole number of pages, and
   * allocate two extra pages, one at the start
   * and one at the end, and don't allow accessing
   * either one (catch stack overflow and underflow).
   */
  pages = (size + pagesize - 1) / pagesize + 2;
  size = pages * pagesize;
  sp = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (sp == NULL)
    goto Error;
  c->stackaddr = sp;
  c->stacksize = size;
  if (mprotect(sp, pagesize, PROT_NONE)) abort();
  if (mprotect(sp + size - pagesize, pagesize, PROT_NONE)) abort();

  xMakeContext(c, p, sp + pagesize, size - 2 * pagesize);

  return c;
Error:
  er = errno;
  if (c) free(c);
  if (sp) munmap(sp, size);
  errno = er;
  return NULL;
}

void
SwapContext (Context *from, Context *to)
{
  SWAP_CONTEXT(from, to);
}

int
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
    return setitimer(ITIMER_VIRTUAL, &it, NULL);
}

void F1(void)
{
  fprintf(stderr, "F1\n");
}

int main()
{
  MakeContext(F1, 1000);
  return 0;
}

