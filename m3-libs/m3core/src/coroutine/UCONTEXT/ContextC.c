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

#define DEBUG 1

typedef struct {
  void (*p)(ARG);
  void *arg;
} Closure;

typedef struct {
  void      *stackaddr;
  WORD_T     stacksize;
  stack_t    ss;
  ucontext_t uc;
  ucontext_t pc; /* post context for cleanup */
  Closure    cl;
  int        alive;
} Context;

static void
trampoline(int lo, int hi)
{
  /* take two 32-bit pointers and turn them into a 64-bit pointer */
  Closure *cl = (Closure *)(((unsigned long)(unsigned int)hi << 32UL) | (unsigned long)(unsigned int)lo);
  
  cl->p(cl->arg);
}
  
void *
ContextC__New(void)
{
  Context *res=calloc(1,sizeof(Context));
  res->alive = 1;
  return res;
}

void
ContextC__SetLink(Context *tgt, Context *src)
{
  tgt->uc.uc_link = &(src->uc);
}

static void
cleanup(int lo, int hi)
{
  /* this is the cleanup routine
     it is called when a context falls off the end (apply ends) 
  */
  
  Context *c = (Context *)(((unsigned long)(unsigned int)hi << 32UL) | (unsigned long)(unsigned int)lo);

  c->alive = 0;

  assert(c->uc.uc_link);

  /* this is the only tricky part:
     the semantics of creating the context are that the followon context
     must be set when the context is created.

     this is not the semantics we are going for here.  In our implementation,
     when a coroutine "falls off the end", we resume with the context that
     called it (and none other!).

     this is implemented by updating uc_link on every coroutine call.
     but it can't take effect like that, so we make a special catcher context 
     (in which we are here in cleanup) that jumps explicitly to that context
  */
  
  setcontext(c->uc.uc_link);
}

void *
ContextC__MakeContext(void      (*p)(ARG),
                      INTEGER     words,
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

  c->alive = 1;
  
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

  if (DEBUG) fprintf(stderr, "creating coroutine stack %#lx\n", sp);
  
  if (sp == NULL)
    goto Error;
  c->stackaddr = sp;
  c->stacksize = size;
  if (mprotect(sp                  , pagesize, PROT_NONE)) abort();
  if (mprotect(sp + size - pagesize, pagesize, PROT_NONE)) abort();

  if (getcontext(&(c->uc))) abort();
  if (getcontext(&(c->pc))) abort();

  c->uc.uc_stack.ss_sp = sp + pagesize;
  c->uc.uc_stack.ss_size = size - 2 * pagesize;

  cl->p   = p;
  cl->arg = arg;

  /* define the post context for cleanup */
  c->pc.uc_stack = c->uc.uc_stack; /* reuse stack */
  c->pc.uc_link = 0;               /* will never go through here */
  
  c->uc.uc_link = &(c->pc);        /* set up cleanup linkage */

  lo = (int)(unsigned long)c;
  hi = (int)(((unsigned long)c) >> 32UL);
  makecontext(&(c->pc), (void (*)())cleanup, 2, lo, hi);
  
  lo = (int)(unsigned long)cl;
  hi = (int)(((unsigned long)cl) >> 32UL);
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
  if(!to->alive) {
    fprintf(stderr,
            "WARNING: calling dead coroutine context %#lx\n",to);
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

void *
ContextC__GetStackBase(Context *c)
{
  return c->uc.uc_stack.ss_sp+c->uc.uc_stack.ss_size;
}

void *
stack_here(void)
{
  char *top=(char *)&top;
  return top;
}
      
void *
ContextC__PushContext(Context *c)
{
  ucontext_t uc=c->uc; /* write it on the stack */
  void *top;

  top = stack_here();
  return top;
}


