/* Copyright (C) 1992, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

/* derived from LINUXLIBC6 */

#undef _GNU_SOURCE
#define _GNU_SOURCE
#include <unistd.h>
#include <signal.h>
typedef struct sigaction sigaction_t;
#include <assert.h>
#if defined(__APPLE__) && defined(__arm)
/* nothing -- ucontext_t is from signal.h and #including ucontext.h gives #error */
#elif defined(__CYGWIN__)
typedef void ucontext_t;
#else
#include <ucontext.h>
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

typedef void (*SignalActionHandler)(int, siginfo_t*, void*);

#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))

static void SegV(int Signal, siginfo_t* SignalInfo, void* Context);
static void Shutdown(int Signal, siginfo_t* SignalInfo, void* Context);
static void Interrupt(int Signal, siginfo_t* SignalInfo, void* Context);
static void Quit(int Signal, siginfo_t* SignalInfo, void* Context);

static void InstallOneHandler(size_t Index);
static void RestoreOneHandler(size_t Index);
static size_t GetPC(void* Context);

static size_t GetPC(void* VoidContext)
/* PC: program counter aka instruction pointer, etc. */
{
    size_t pc = 0;
    ucontext_t *Context = (ucontext_t *)VoidContext;

    if (Context != NULL) {
#if defined(__APPLE__)
#if defined(__i386__)
#if __DARWIN_UNIX03
      pc = Context->uc_mcontext->__ss.__eip;
#else
      pc = Context->uc_mcontext->sc.sc_eip;
#endif
#elif defined(__x86_64__)
#if __DARWIN_UNIX03
      pc = Context->uc_mcontext->__ss.__rip;
#else
      pc = Context->uc_mcontext->ss.rip;
#endif
#elif defined(__ppc__) || defined(__ppc64__)
#if __DARWIN_UNIX03
      pc = Context->uc_mcontext->__ss.__srr0;
#else
      pc = Context->uc_mcontext->ss.srr0;
#endif
#elif defined(__arm)
      /* Apparently missing support? Revisit when I get an official toolset. */
#else
#error Unknown __APPLE__ target
#endif
#elif defined(__sparc)
      pc = Context->uc_mcontext.gregs[REG_PC];
#elif defined(__linux)
#if defined(__i386)
      pc = Context->uc_mcontext.gregs[REG_EIP];
#elif defined(__amd64)
      pc = Context->uc_mcontext.gregs[REG_RIP];
#else
#error Unknown __linux target
#endif
#endif
    }

#if 0 /* FUTURE, each or at least some of these need investigation and testing */
    ucontext_t* Context = (ucontext_t*) VoidContext;
    if (Context != NULL)
    {
#if defined(__CYGWIN__)
#error update GetPC
#elif defined(__amd64)
        pc = Context->uc_mcontext->ss.rip;
#elif defined(__i386) && defined(__linux)
        pc = Context->uc_mcontext.gregs[REG_EIP];
#elif defined(__i386) && defined(__FreeBSD__)
        pc = Context->uc_mcontext->scp_eip;
#elif defined(__powerpc)
        pc = Context->uc_mcontext->ss.srr0;
#elif defined(__mips)
        pc = Context->uc_mcontext->scp_pc.lo;
#elif defined(__hpux) && defined(__hppa)
        pc = Context->uc_mcontext->scp_pc;
#else
#error update GetPC
#endif
    }
#endif
    return pc;
}

#define DefaultHandler ((SignalActionHandler) SIG_DFL)
#define IgnoreSignal   ((SignalActionHandler) SIG_IGN)

static const struct
{
    int Signal;
    SignalActionHandler Handler;
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

static void InstallOneHandler(size_t Index)
{
    sigaction_t New;
    int i = { 0 };
    int Signal = Handlers[i].Signal;
    SignalActionHandler Handler = Handlers[i].Handler;

    ZeroMemory(&New, sizeof(New));
    i = sigemptyset(&New.sa_mask);
    assert(i == 0);

    /* What if Handler == SIG_IGN? */
    New.sa_sigaction = Handler;
    New.sa_flags = SA_SIGINFO;

    i = sigaction(Signal, &New, &InitialHandlers[Index]);
    assert(i == 0);

    /* Don't override inherited, non-default handlers.
       That is, if the old handler is not the default handler, put it back. */
    if (InitialHandlers[Index].sa_sigaction != DefaultHandler)
    {
        i = sigaction(Signal, &InitialHandlers[Index], &New);
        assert(i == 0);
    }
}

void InstallHandlers(void)
{
    size_t i = { 0 };

    for (i = 0 ; i != NUMBER_OF(Handlers) ; ++i)
    {
        InstallOneHandler(i);
    }
}

static void RestoreOneHandler(size_t Index)
{
    int Signal = Handlers[Index].Signal;
    sigaction_t Old;

    ZeroMemory(&Old, sizeof(Old));
    sigaction(Signal, &InitialHandlers[Index], &Old);
}

void RestoreHandlers(void)
{
    size_t i = { 0 };

    for (i = 0 ; i != NUMBER_OF(Handlers) ; ++i)
    {
        RestoreOneHandler(i);
    }
}

static void Shutdown(int Signal, siginfo_t* SignalInfo, void* Context)
{
    sigaction_t New;
    sigaction_t Old;

    ZeroMemory(&Old, sizeof(Old));
    ZeroMemory(&New, sizeof(New));
    New.sa_sigaction = DefaultHandler;
    New.sa_flags = 0;
    sigemptyset(&New.sa_mask);
    InvokeExitors();     /* flush stdio */
    sigaction(Signal, &New, &Old);  /* restore default handler */
    kill(getpid(), Signal);         /* and resend the signal */
}

static void Interrupt(int Signal, siginfo_t* SignalInfo, void* Context)
{
    InterruptHandler Handler = OnInterrupt(NULL);

    if (Handler == NULL)
    {
        Shutdown(Signal, SignalInfo, Context);
    }
    else
    {
        OnInterrupt(Handler); /* reinstall the handler */
        Handler();
    }
}

static void Quit(int Signal, siginfo_t* SignalInfo, void* Context)
{
    MsgPCAbort (GetPC(Context));
}

static void SegV(int Signal, siginfo_t* SignalInfo, void* Context)
{
    MsgPCSegV (GetPC(Context));
}

#ifdef __cplusplus
} /* extern "C" */
#endif
