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

Currently this only works for NT386.
NT386GNU shouldn't be hard -- adapt the inline assembly to gcc or use separate assembly files.
*/

#include <stddef.h>

typedef unsigned long sigset_t;

struct _ucontext_t;
typedef struct _ucontext_t ucontext_t;

struct _mcontext_t;
typedef struct _mcontext_t mcontext_t;

struct _stack_t;
typedef struct _stack_t stack_t;

struct _stack_t
{
    void* ss_sp; /* stack base or pointer */
    size_t ss_size; /* stack size */
    int ss_flags;
};

struct _mcontext_t
{
    int x;
};

struct _ucontext_t
{
    mcontext_t uc_mcontext;
    stack_t uc_stack;
    ucontext_t* uc_link;
    sigset_t uc_sigmask;
    void* fiber;
    void* delete_fiber;
    size_t argv[32];
    void (*function)();
};

int  getcontext(ucontext_t*);
int  setcontext(const ucontext_t*);
void makecontext(ucontext_t*, void(*)(), int, ...);
int  swapcontext(ucontext_t*, const ucontext_t*);
