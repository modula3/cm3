/*
 * Coroutines for Modula-3 !
 *
 * Author : Mika Nystrom <mika.nystroem@intel.com>
 * April, 2018
 *
 * Much of this code is a hybrid of the PTHREADS code by Tony Hosking
 * and the POSIX code by DEC-SRC.
 */

#include <ucontext.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

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

#define INTEGER  long int
#define WORD_T   unsigned long int
#define ARG void *

typedef struct {
  void (*p)(ARG);
  void *arg;
} Closure;

typedef struct {
  void      *stackaddr;
  WORD_T     stacksize;
  stack_t    ss;
  ucontext_t uc;
  Closure    cl;
} Context;

static void
trampoline(int lo, int hi)
{
  /* take two 32-bit pointers and turn them into a 64-bit pointer */
  Closure *cl = (Closure *)(((long)hi << 32UL) | (long)lo);
  
  cl->p(cl->arg);
}
  
void *
ContextC__New(void)
{
  return calloc(1,sizeof(Context));
}

void *
ContextC__MakeContext(void      (*p)(ARG),
                      INTEGER     words,
                      Context    *resume,
                      void       *arg)
{
  /* from ThreadPosixC.c */
  Context *c = (Context *)calloc (1, sizeof(*c));
  INTEGER size = sizeof(void *) * words;
  INTEGER pagesize = getpagesize();
  char *sp = { 0 };
  INTEGER pages = { 0 };
  int er = { 0 };
  Closure *cl = (Closure *)calloc(1, sizeof(*cl));
  int lo, hi;

  if (c == NULL || cl == NULL)
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
  if (mprotect(sp                  , pagesize, PROT_NONE)) abort();
  if (mprotect(sp + size - pagesize, pagesize, PROT_NONE)) abort();

  if (getcontext(&(c->uc))) abort();
  c->uc.uc_stack.ss_sp = sp + pagesize;
  c->uc.uc_stack.ss_size = size - 2 * pagesize;
  c->uc.uc_link = 0;

  lo = (int)(long)cl;
  hi = (int)(((long)cl) >> 32UL);

  cl->p   = p;
  cl->arg = arg;
    
  makecontext(&(c->uc), (void (*)())trampoline, 2, lo, hi);

  return c;
Error:
  er = errno;
  if (c) free(c);
  if (cl) free(cl);
  if (sp) munmap(sp, size);
  errno = er;
  return NULL;
}

void
ContextC__SwapContext (Context *from, Context *to)
{
  if (swapcontext(&(from->uc), &(to->uc))) abort();
}

void
ContextC__DisposeContext (Context *c)
{
  if (munmap((c)->stackaddr, (c)->stacksize)) abort();
  free(c);
}

void *
ContextC__Current(void)
{
  Context *new=ContextC__New();
  if (getcontext(&(new->uc))) abort();
  return new;
}

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
ContextC__InitC(void) /* should be void *bottom? */
{
  int r = { 0 };
  M3_RETRY(pthread_key_create(&current_coroutine, NULL)); assert(r == 0);
}

void
ContextC__Dbg(INTEGER x)
{
  putc(x, stderr);
  putc('\n', stderr);
}

void
ContextC__DbgPtr(void *p)
{
  fprintf(stderr, "DbgPtr p=%#x\n", p);
}
