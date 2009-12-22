/*
This is an implementation of makecontext/setcontext/swapcontext/getcontext
for systems that do not provide them.

Systems without make/set/get/swapcontext include:
    OpenBSD (4.4)
    MacOS X prior to 10.5
    Cygwin -- not going to work anyway, use kernel threads or possibly fibers
Systems with make/set/get/swapcontext include:
    Linux
    MacOS X 10.5
    NetBSD
    FreeBSD
    Solaris

see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
see http://www.engelschall.com/pw/usenix/2000/pmt-html/
*/

#if _MSC_VER > 1000
#pragma once
#endif

#ifndef M3_CONTEXT_SIGALTSTACK_INCLUDED
#define M3_CONTEXT_SIGALTSTACK_INCLUDED

/* Cygwin defines its jmpbuf incorrectly. */
#ifdef __CYGWIN__ /* not going to work anyway, use kernel threads or possibly fibers */
typedef unsigned char _JBTYPE __attribute__((aligned(4)));
#define _JBTYPE _JBTYPE
#endif

#include <setjmp.h>
#include <stddef.h>
#include <signal.h>
#include <errno.h>

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct {
    sigjmp_buf jb;
    int err; /* errno */
} Context_t;

void
xGetContext(
    Context_t* context);

void
xSetContext(
    Context_t* context);

void
xSwapContext(
    Context_t* oldContext,
    Context_t* newContext);

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

#define SwapContext(oldc, newc)        \
do {                                   \
    xSwapContext((oldc), (newc));      \
    if (sigsetjmp((oldc)->jb, 1) == 0) \
        siglongjmp((newc)->jb, 1);     \
} while (0)

void
MakeContext(
    Context_t* context,
    void (*function)(void*),
    void* parameter,
    void* stack,
    size_t stack_size);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
