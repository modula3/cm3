/* Copyright (C) 1992, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

/* derived from LINUXLIBC6 */

#undef _GNU_SOURCE
#define _GNU_SOURCE
#if defined(__INTERIX) && !defined(_ALL_SOURCE)
#define _ALL_SOURCE
#endif

#include <unistd.h>
#include <signal.h>
#include <assert.h>
typedef struct sigaction sigaction_t;
#if defined(__APPLE__)
/*
http://tinderbox.elegosoft.com/tinderbox/cgi-bin/gunzip.cgi\
  ?tree=cm3&brief-log=1258879870.10595#err9
/usr/include/ucontext.h:42:2: error: #error ucontext routines are
    deprecated, and require _XOPEN_SOURCE to be defined
http://duriansoftware.com/joe/PSA:-avoiding-the-%22ucontext-\
  routines-are-deprecated%22-error-on-Mac-OS-X-Snow-Leopard.html
*/
#include <sys/ucontext.h>
#elif !(defined(__OpenBSD__) || defined(__CYGWIN__))
/* OpenBSD 4.3: ucontext.h doesn't exist, ucontext_t is in signal.h
        Cygwin: no state provided to signal handler? */
#include <ucontext.h>
#endif

#if !defined(__INTERIX) && !defined(SA_SIGINFO)
#define SA_SIGINFO SA_SIGINFO
#endif

#include <string.h>
#define ZeroMemory(a,b) (memset((a), 0, (b)))

#ifdef __cplusplus
extern "C" {
#endif

/* EXPORTS RTSignalC */
void RTSignalC_InstallHandlers(void);
void RTSignalC_RestoreHandlers(void);
#define InstallHandlers RTSignalC_InstallHandlers
#define RestoreHandlers RTSignalC_RestoreHandlers

/* FROM RTSignalPrivate IMPORT MsgSegV, MsgAbort */
void RTSignalPrivate__MsgPCSegV (size_t);
void RTSignalPrivate__MsgPCAbort (size_t);
#define MsgPCSegV RTSignalPrivate__MsgPCSegV
#define MsgPCAbort RTSignalPrivate__MsgPCAbort

/* FROM RTProcess IMPORT OnInterrupt, InterruptHandler, InvokeExitors */
typedef void (*RTProcess__InterruptHandler)(void);
void RTProcess__InvokeExitors(void);
RTProcess__InterruptHandler RTProcess__OnInterrupt(RTProcess__InterruptHandler);
#define OnInterrupt RTProcess__OnInterrupt
#define InterruptHandler RTProcess__InterruptHandler
#define InvokeExitors RTProcess__InvokeExitors

#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))

static void ShutdownCommon(int Signal);

#ifdef SA_SIGINFO
#define SIGNAL_HANDLER_SIGNATURE (int Signal, siginfo_t* SignalInfo, void* Context)
#define SIGNAL_HANDLER_FIELD sa_sigaction
#define SIGNAL_HANDLER_TYPE SignalActionHandler
#else
#define SIGNAL_HANDLER_SIGNATURE (int Signal)
#define SIGNAL_HANDLER_FIELD sa_handler
#define SIGNAL_HANDLER_TYPE sighandler_t
#endif

static void SegV SIGNAL_HANDLER_SIGNATURE;
static void Shutdown SIGNAL_HANDLER_SIGNATURE;
static void Interrupt SIGNAL_HANDLER_SIGNATURE;
static void Quit SIGNAL_HANDLER_SIGNATURE;

static void InstallOneHandler(size_t i);
static void RestoreOneHandler(size_t i);

typedef void (*SignalActionHandler)(int, siginfo_t*, void*);

#if defined(__CYGWIN__) || defined(__INTERIX) \
|| (defined(__APPLE__) && defined(__arm))

/* Revisit Apple/arm */

#define GetPC(x) 0

#else

static size_t GetPC(void* xcontext)
/* PC: program counter aka instruction pointer, etc. */
{
    ucontext_t* context = (ucontext_t*)xcontext;

    if (context == NULL)
        return 0;

    return

#if defined(__APPLE__)

#if defined(__i386__)
#if __DARWIN_UNIX03
    context->uc_mcontext->__ss.__eip
#else
    context->uc_mcontext->sc.sc_eip
#endif
#elif defined(__x86_64__)
#if __DARWIN_UNIX03
    context->uc_mcontext->__ss.__rip
#else
    context->uc_mcontext->ss.rip
#endif
#elif defined(__ppc__) || defined(__ppc64__)
#if __DARWIN_UNIX03
    context->uc_mcontext->__ss.__srr0
#else
    context->uc_mcontext->ss.srr0
#endif
#else
/* arm is dealt with earlier */
#error Unknown __APPLE__ target
#endif

#elif defined(__sun) || defined(__sparc)
      context->uc_mcontext.gregs[REG_PC]

#elif defined(__linux)

#if defined(__i386)
      context->uc_mcontext.gregs[REG_EIP]
#elif defined(__amd64)
      context->uc_mcontext.gregs[REG_RIP]
#elif defined(__powerpc)
      context->uc_mcontext.uc_regs->gregs[PT_NIP]
#else
#error unknown __linux target
#endif

#elif defined(__NetBSD__)
    _UC_MACHINE_PC(context)

#elif defined(__OpenBSD__)
#if defined(__amd64)
    context->sc_rip
#elif defined(__powerpc)
    context->sc_frame.srr0
#else
    context->sc_pc
#endif

#elif defined(__FreeBSD__)
#if defined(__amd64)
    context->uc_mcontext.mc_rip
#elif defined(__i386)
    context->uc_mcontext.mc_eip
#else
    context->uc_mcontext.mc_pc
#endif

#elif defined(__mips)
    context->uc_mcontext.scp_pc.lo
#elif defined(__hppa)
    context->uc_mcontext.scp_pc
#elif defined(__i386)
    context->uc_mcontext.ss.eip
#elif defined(__amd64)
    context->uc_mcontext.ss.rip
#elif defined(__powerpc)
    context->uc_mcontext.ss.srr0
#else
#error unknown target
#endif
    ;
}

#endif

#define DefaultHandler ((SIGNAL_HANDLER_TYPE) SIG_DFL)
#define IgnoreSignal   ((SIGNAL_HANDLER_TYPE) SIG_IGN)

static const struct
{
    int Signal;
    SIGNAL_HANDLER_TYPE Handler;
}
Handlers[] =
{
    { SIGHUP,  Shutdown },
    { SIGINT,  Interrupt },
    { SIGQUIT, Quit },
    { SIGSEGV, SegV },
    { SIGPIPE, IgnoreSignal },
    { SIGTERM, Shutdown },
    { SIGBUS,  SegV },
};

static sigaction_t InitialHandlers[NUMBER_OF(Handlers)];

static void InstallOneHandler(size_t i)
{
    sigaction_t New;
    int r;
    int Signal = Handlers[i].Signal;

    ZeroMemory(&New, sizeof(New));
    r = sigemptyset(&New.sa_mask);
    assert(r == 0);

    /* What if Handler == SIG_IGN? */
    New.SIGNAL_HANDLER_FIELD = Handlers[i].Handler;
#ifdef SA_SIGINFO
    New.sa_flags = SA_SIGINFO;
#else
    New.sa_flags = 0;
#endif

    r = sigaction(Signal, &New, &InitialHandlers[i]);
    assert(r == 0);

    /* Don't override inherited, non-default handlers.
       That is, if the old handler is not the default handler, put it back. */
    if (InitialHandlers[i].SIGNAL_HANDLER_FIELD != DefaultHandler)
    {
        r = sigaction(Signal, &InitialHandlers[i], &New);
        assert(r == 0);
    }
}

void InstallHandlers(void)
{
    size_t i = { 0 };

    for (; i < NUMBER_OF(Handlers); ++i)
    {
        InstallOneHandler(i);
    }
}

static void RestoreOneHandler(size_t i)
{
    int Signal = Handlers[i].Signal;
    sigaction_t Old;

    ZeroMemory(&Old, sizeof(Old));
    sigaction(Signal, &InitialHandlers[i], &Old);
}

void RestoreHandlers(void)
{
    size_t i = { 0 };

    for (; i < NUMBER_OF(Handlers); ++i)
    {
        RestoreOneHandler(i);
    }
}

static void ShutdownCommon(int Signal)
{
    sigaction_t New;
    sigaction_t Old;

    ZeroMemory(&Old, sizeof(Old));
    ZeroMemory(&New, sizeof(New));
    New.sa_handler = SIG_DFL;
    New.sa_flags = 0;
    sigemptyset(&New.sa_mask);
    InvokeExitors();     /* flush stdio */
    sigaction(Signal, &New, &Old);  /* restore default handler */
    kill(getpid(), Signal);         /* and resend the signal */
}

static void Shutdown SIGNAL_HANDLER_SIGNATURE
{
    ShutdownCommon(Signal);
}

static void Interrupt SIGNAL_HANDLER_SIGNATURE
{
    InterruptHandler Handler = OnInterrupt(NULL);

    if (Handler == NULL)
    {
        ShutdownCommon(Signal);
    }
    else
    {
        OnInterrupt(Handler); /* reinstall the handler */
        Handler();
    }
}

static void Quit SIGNAL_HANDLER_SIGNATURE
{
    MsgPCAbort (GetPC(Context));
}

static void SegV SIGNAL_HANDLER_SIGNATURE
{
    MsgPCSegV (GetPC(Context));
}

#ifdef __cplusplus
} /* extern "C" */
#endif
