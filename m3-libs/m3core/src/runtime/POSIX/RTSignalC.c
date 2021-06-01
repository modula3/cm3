/* Copyright (C) 1992, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

/* derived from LINUXLIBC6 */

#undef _GNU_SOURCE
#define _GNU_SOURCE

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

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

static void InstallOneHandler(size_t i);
static void RestoreOneHandler(size_t i);

#if defined(__CYGWIN__) || defined(__INTERIX) || defined(__vms)

/* Revisit VMS */

#define GetPC(x) 0

#else

static
WORD_T
__cdecl
GetPC(void* xcontext)
/* PC: program counter aka instruction pointer, etc. */
{
    ucontext_t* context = (ucontext_t*)xcontext;

    if (context == NULL)
        return 0;

    return GET_PC(context);
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

static void InstallOneHandler(size_t i)
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
    size_t i = { 0 };

    for (; i < NUMBER_OF(Handlers); ++i)
    {
        InstallOneHandler(i);
    }
}

static void RestoreOneHandler(size_t i)
{
    int Signal = Handlers[i].Signal;

    sigaction(Signal, &InitialHandlers[i], NULL);
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
