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
*/

#include <stddef.h>

/*
OpenBSD has typedef struct sigcontext_t ucontext_t which is wrong.
*/
#ifdef __OpenBSD__
#define ucontext_t openbsd_ucontext_t
#endif
#include <signal.h>
#undef ucontext_t

struct _ucontext_t;
typedef struct _ucontext_t ucontext_t;

#if !defined(__OpenBSD__) || !defined(__i386__)

struct _mcontext_t;
typedef struct _mcontext_t mcontext_t;

#if !defined(__OpenBSD__)

struct _stack_t;
typedef struct _stack_t stack_t;

struct _stack_t
{
    /* assembly code depends on the layouts here so don't move stuff around */
    void* ss_sp; /* stack base or pointer */
    size_t ss_size; /* stack size */
    int ss_flags;
};

#endif

struct _mcontext_t
{
    /* This matches OpenBSD struct sigcontext -- at least prefix. */
	size_t	sc_gs;
	size_t	sc_fs;
	size_t	sc_es;
	size_t	sc_ds;
	size_t	sc_edi;
	size_t	sc_esi;
	size_t	sc_ebp;
	size_t	sc_ebx;
	size_t	sc_edx;
	size_t	sc_ecx;
	size_t	sc_eax;
	size_t	sc_eip;
	size_t	sc_cs;
	size_t	sc_eflags;
	size_t	sc_esp;
#if 0
    size_t	sc_ss;
	size_t	sc_onstack;
	size_t	sc_mask;
	size_t	sc_trapno;
	size_t	sc_err;
#endif
};

#else

typedef struct sigcontext mcontext_t;

#endif

struct _ucontext_t
{
    /* assembly code depends on the layouts here so don't move stuff around */
    mcontext_t uc_mcontext;
    stack_t uc_stack;
    ucontext_t* uc_link;
    sigset_t uc_sigmask;
};

int  getcontext(ucontext_t*);
int  setcontext(const ucontext_t*);
void makecontext(ucontext_t*, void(*)(), int, ...);
int  swapcontext(ucontext_t*, const ucontext_t*);
