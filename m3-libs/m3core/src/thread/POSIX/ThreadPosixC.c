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

#define _XOPEN_SOURCE 500
#define _BSD_SOURCE
#define _XPG4_2
#define _DARWIN_C_SOURCE

#include "m3unix.h"
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
#define GetContext          ThreadPosix__GetContext
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
  void *sp;
  size_t size;
  ucontext_t uc;
} Context;

void *
MakeContext (void (*p)(void), int words)
{
  Context *c = (Context*)calloc (1, sizeof(Context));
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
  sp = (char*)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (sp == NULL)
    goto Error;
  c->sp = sp;
  c->size = size;
  if (mprotect(sp, pagesize, PROT_NONE)) abort();
  if (mprotect(sp + size - pagesize, pagesize, PROT_NONE)) abort();

  if (getcontext(&(c->uc))) abort();
  c->uc.uc_stack.ss_sp = sp + pagesize;
  c->uc.uc_stack.ss_size = size - 2 * pagesize;
  c->uc.uc_link = 0;
  makecontext(&(c->uc), p, 0);

  return c;
Error:
  er = errno;
  if (c != NULL) free(c);
  if (sp != NULL) munmap(sp, size);
  errno = er;
  return NULL;
}

void GetContext (Context *c)
{
  if (getcontext(&(c->uc))) abort();
}

void SwapContext (Context *from, Context *to)
{
  if (swapcontext(&(from->uc), &(to->uc))) abort();
}

void DisposeContext (Context **c)
{
  if (munmap((*c)->sp, (*c)->size)) abort();
  free(*c);
  *c = NULL;
}

void
ProcessContext(Context *c, char *bottom, char *top,
	       void (*p) (void *start, void *limit))
{
  int xx;

  if (top == NULL)
    top = &xx;
  if (bottom < top)
    p(bottom, top);
  else
    p(top, bottom);
#ifdef __APPLE__
  p(&(c->uc.uc_mcontext[0]), &(c->uc.uc_mcontext[1]));
#else
  p(&c[0], &c[1]);
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
