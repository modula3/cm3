/*
This is an implementation of makecontext/setcontext/swapcontext/getcontext
for systems that do not provide them.

It is highly system specific.

Systems without make/set/get/swapcontext include:
    OpenBSD (4.4)
    MacOS X prior to 10.5
    Cygwin
Systems with make/set/get/swapcontext include:
    Linux
    MacOS X 10.5
    NetBSD
    FreeBSD

There is much prior art to using setjmp/longjmp for similar purposes.
Layer over them.
*/

/* Cygwin defines its jmpbuf incorrectly. */
#ifdef __CYGWIN__
typedef unsigned char _JBTYPE __attribute__((aligned(4)));
#define _JBTYPE _JBTYPE
#endif
/* OpenBSD has typedef struct sigcontext_t ucontext_t which is wrong. */
#ifdef __OpenBSD__
#define ucontext_t openbsd_ucontext_t
#endif

#include <setjmp.h>
#include <stddef.h>
#include <signal.h>
#undef ucontext_t

struct _ucontext_t;
typedef struct _ucontext_t ucontext_t;

/* OpenBSD provides a correct stack_t */
#ifndef __OpenBSD__

struct _stack_t;
typedef struct _stack_t stack_t;

struct _stack_t
{
    void* ss_sp; /* stack base or pointer */
    size_t ss_size; /* stack size */
    int ss_flags;
};

#endif

typedef size_t mcontext_t[sizeof(jmp_buf) / sizeof(size_t)];

struct _ucontext_t
{
    mcontext_t uc_mcontext;
    stack_t uc_stack;
    ucontext_t* uc_link;
    sigset_t uc_sigmask;
};

int  getcontext(ucontext_t*);
int  setcontext(const ucontext_t*);
void makecontext(ucontext_t*, void(*)(), int, ...);
int  swapcontext(ucontext_t*, const ucontext_t*);
