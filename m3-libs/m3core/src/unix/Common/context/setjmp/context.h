/*
This is an implementation of makecontext/setcontext/swapcontext/getcontext
for systems that do not provide them.

It is highly system specific.

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

There is much prior art to using setjmp/longjmp for similar purposes.
Layer over them.
*/

#if _MSC_VER > 1000
#pragma once
#endif

#ifndef M3SETJMPCONTEXT_INCLUDED
#define M3SETJMPCONTEXT_INCLUDED

/* Cygwin defines its jmpbuf incorrectly. */
#ifdef __CYGWIN__ /* not going to work anyway, use kernel threads or possibly fibers */
typedef unsigned char _JBTYPE __attribute__((aligned(4)));
#define _JBTYPE _JBTYPE
#endif
/* OpenBSD has typedef struct sigcontext_t ucontext_t which is wrong. */
#ifdef __OpenBSD__
#define ucontext_t openbsd_ucontext_t
#endif
#ifdef __APPLE__
#define ucontext_t macosx_ucontext_t
#define mcontext_t macosx_mcontext_t
#endif

#include <setjmp.h>
#include <stddef.h>
#include <signal.h>
#undef ucontext_t
#undef mcontext_t

#ifdef __cplusplus
extern "C"
{
#endif

struct _ucontext_t;
typedef struct _ucontext_t ucontext_t;

/* OpenBSD, MacOS X provides a correct stack_t */
#if !defined(__OpenBSD__) && !defined(__APPLE__)

struct _stack_t;
typedef struct _stack_t stack_t;

struct _stack_t
{
    void* ss_sp; /* stack base or pointer */
    size_t ss_size; /* stack size */
    int ss_flags;
};

#endif

typedef union
{
    size_t a[sizeof(sigjmp_buf) / sizeof(size_t)];
    sigjmp_buf jb;
} mcontext_t;

struct _ucontext_t
{
    mcontext_t uc_mcontext;
    stack_t uc_stack;
    ucontext_t* uc_link;
};

int  Uucontext__getcontext(ucontext_t*);
int  Uucontext__setcontext(const ucontext_t*);
void Uucontext__makecontext(ucontext_t*, void(*)(), int, ...);
int  Uucontext__swapcontext(ucontext_t*, const ucontext_t*);

#define getcontext Uucontext__getcontext
#define setcontext Uucontext__setcontext
#define makecontext Uucontext__makecontext
#define swapcontext Uucontext__swapcontext

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
