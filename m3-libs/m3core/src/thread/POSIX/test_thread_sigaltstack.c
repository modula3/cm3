#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

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

#define ZERO_MEMORY(a) (memset(&(a), 0, sizeof(a)))

typedef struct {
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

void
__cdecl
mctx_create_boot(void)
{
    void (*volatile mctx_start_func)(void) = { 0 };

    fprintf(stderr, "mctx_create_boot\n");
    
    sigprocmask(SIG_SETMASK, &mctx_create_sigs, NULL);
    mctx_start_func = mctx_create_func;
    SWAP_CONTEXT(mctx_create, &mctx_caller);
    mctx_start_func();
    abort(); /* not reached */
}

void
__cdecl
mctx_create_trampoline(int sig)
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
__cdecl
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

    ZERO_MEMORY(sa);
    ZERO_MEMORY(osa);
    ZERO_MEMORY(ss);
    ZERO_MEMORY(oss);
    ZERO_MEMORY(osigs);
    ZERO_MEMORY(sigs);

    fprintf(stderr, "xMakeContext\n");

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
__cdecl
MakeContext (void (*p)(void), size_t  words)
{
  Context *c = (Context *)calloc (1, sizeof(*c));
  size_t size = sizeof(void *) * words;
  size_t pagesize = getpagesize();
  char *sp = { 0 };
  size_t pages = { 0 };
  int er = { 0 };

  if (c == NULL)
    goto Error;
  if (size <= 0)
    goto Error;
  if (size < MINSIGSTKSZ)
    size = MINSIGSTKSZ;

  /* Round up to a whole number of pages, and
   * allocate two extra pages, one at the start
   * and one at the end, and don't allow accessing
   * either one (catch stack overflow and underflow).
   */
  pages = (size + pagesize - 1) / (pagesize + 2);
  size = pages * pagesize;
  sp = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (sp == NULL)
    goto Error;
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
__cdecl
F1(void)
{
  fprintf(stderr, "F1\n");
}

int
__cdecl
main()
{
  MakeContext(F1, 1000);
  return 0;
}
