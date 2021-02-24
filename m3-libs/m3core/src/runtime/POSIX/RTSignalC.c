/* Copyright (C) 1992, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

/* derived from LINUXLIBC6 */

#undef _GNU_SOURCE
#define _GNU_SOURCE

#include "m3core.h"

#if !defined(__INTERIX) && !defined(__vms) && !defined(SA_SIGINFO)
#define SA_SIGINFO SA_SIGINFO
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* EXPORTS RTSignalC */
#define InstallHandlers RTSignalC__InstallHandlers
#define RestoreHandlers RTSignalC__RestoreHandlers
void InstallHandlers(void);
void RestoreHandlers(void);

/* FROM RTSignalPrivate IMPORT MsgSegV, MsgAbort */
#define MsgPCSegV RTSignalPrivate__MsgPCSegV
#define MsgPCAbort RTSignalPrivate__MsgPCAbort
void MsgPCSegV (WORD_T);
void MsgPCAbort (WORD_T);

/* FROM RTProcess IMPORT OnInterrupt, InterruptHandler, InvokeExitors */
#define OnInterrupt RTProcess__OnInterrupt
#define InterruptHandler RTProcess__InterruptHandler
#define InvokeExitors RTProcess__InvokeExitors
typedef void (*InterruptHandler)(void);
void InvokeExitors(void);
InterruptHandler OnInterrupt(InterruptHandler);

#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))

static void ShutdownCommon(int Signal);

#ifdef SA_SIGINFO
#define SIGNAL_HANDLER_SIGNATURE (int Signal, siginfo_t* SignalInfo, void* Context)
#define SIGNAL_HANDLER_FIELD sa_sigaction
typedef void (*SIGNAL_HANDLER_TYPE)(int, siginfo_t*, void*);
#else
#define SIGNAL_HANDLER_SIGNATURE (int Signal)
#define SIGNAL_HANDLER_FIELD sa_handler
#ifdef __INTERIX
typedef sighandler_t SIGNAL_HANDLER_TYPE;
#else
typedef void (*SIGNAL_HANDLER_TYPE)(int);
#endif
#endif

static void SegV SIGNAL_HANDLER_SIGNATURE;
static void Shutdown SIGNAL_HANDLER_SIGNATURE;
static void Interrupt SIGNAL_HANDLER_SIGNATURE;
static void Quit SIGNAL_HANDLER_SIGNATURE;

static void InstallOneHandler(WORD_T i);
static void RestoreOneHandler(WORD_T i);

#if defined(__CYGWIN__) || defined(__INTERIX) || defined(__vms)

/* Revisit VMS */

#define GetPC(x) 0

#else

static WORD_T GetPC(void* xcontext)
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
    context->uc_mcontext->ss.eip
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
#elif defined(__arm__) || defined(__arm64__)
#if __DARWIN_UNIX03
    __darwin_arm_thread_state64_get_pc(context->uc_mcontext->__ss)
#else
    __darwin_arm_thread_state64_get_pc(context->uc_mcontext->ss)
#endif
#else
#error Unknown __APPLE__ target
#endif

#elif defined(__osf__)
    context->uc_mcontext.sc_pc

#elif defined(__OpenBSD__)
#if defined(__amd64)
    context->sc_rip
#elif defined(__powerpc)
    context->sc_frame.srr0
#else
    context->sc_pc
#endif

#elif defined(__linux) && defined(__sparc) && __WORDSIZE == 64
      context->uc_mcontext.mc_gregs[REG_PC]

#elif defined(__sun) || defined(__sparc)
#if defined(REG_PC)
      context->uc_mcontext.gregs[REG_PC]
#elif defined(__sun) && defined(__i386) && (PC == 14)
      context->uc_mcontext.gregs[PC]
#elif defined(__sun) && defined(__sparc) && (PC == 1)
      context->uc_mcontext.gregs[PC]
#else
#error unknown __sun/__sparc target
#endif
#elif defined(__linux)
/* see /src/glibc-2.14/sysdeps/unix/sysv/linux/x/sigcontextinfo.h */
#if defined(__i386)
      context->uc_mcontext.gregs[REG_EIP]
#elif defined(__amd64)
      context->uc_mcontext.gregs[REG_RIP]
#elif defined(__powerpc)
      context->uc_mcontext.uc_regs->gregs[PT_NIP]
#elif defined(__arm__)
      context->uc_mcontext.arm_pc
#elif defined(__alpha__)
      context->uc_mcontext.sc_pc
#elif defined(__ia64__)
      context->uc_mcontext.sc_ip
#elif defined(__sh__)
#error untested __linux target
      context->uc_mcontext.sc_pc
#elif defined(__s390__)
#error untested __linux target
      context->uc_mcontext.sregs.regs.psw.addr
#else
#error unknown __linux target
#endif

#elif defined(__NetBSD__)
    _UC_MACHINE_PC(context)

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

#define DefaultHandler ((SIGNAL_HANDLER_TYPE)SIG_DFL)
#define IgnoreSignal   ((SIGNAL_HANDLER_TYPE)SIG_IGN)

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
    { SIGSEGV, SegV }, /* threading library may override this */
    { SIGPIPE, IgnoreSignal },
    { SIGTERM, Shutdown },
    { SIGBUS,  SegV },
};

static struct sigaction InitialHandlers[NUMBER_OF(Handlers)];

static void InstallOneHandler(WORD_T i)
{
    struct sigaction New;
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
    WORD_T i = { 0 };

    for (; i < NUMBER_OF(Handlers); ++i)
    {
        InstallOneHandler(i);
    }
}

static void RestoreOneHandler(WORD_T i)
{
    int Signal = Handlers[i].Signal;

    sigaction(Signal, &InitialHandlers[i], NULL);
}

void RestoreHandlers(void)
{
    WORD_T i = { 0 };

    for (; i < NUMBER_OF(Handlers); ++i)
    {
        RestoreOneHandler(i);
    }
}

static void ShutdownCommon(int Signal)
{
    struct sigaction New;

    ZeroMemory(&New, sizeof(New));
    New.sa_handler = SIG_DFL;
    New.sa_flags = 0;
    sigemptyset(&New.sa_mask);
    InvokeExitors();     /* flush stdio */
    sigaction(Signal, &New, NULL);  /* restore default handler */
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
