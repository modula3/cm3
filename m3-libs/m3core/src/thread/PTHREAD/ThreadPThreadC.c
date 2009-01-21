/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#include <assert.h>

#ifdef __APPLE__
/* MacOSX diverges in a good way and therefore this file is
just stubs for it, that other code dynamically choses
not to call (statically, but the compiler can't or won't tell). */
#define APPLE_ASSERT_FALSE assert(!"MacOS X should not get here.");
#else
#include <signal.h>
#include <semaphore.h>
#include <string.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif


#define SIG_SUSPEND ThreadPThreadC_SIG_SUSPEND
 
/* expected values for compat, if compat matters:
    Solaris: 17 (at least 32bit SPARC?)
    Cygwin: 19 -- er, but maybe that's wrong
    Linux: 64
    FreeBSD: 31
    OpenBSD: 31
    HPUX: 44
  Look at the history of Usignal and RTMachine to find more values.
  There was RTMachine.SIG_SUSPEND and SIG was aliased to it.
  Both SIG and SIG_SUSPEND were only defined for systems using pthreads.
  SIG was shorthand.
*/
#if defined(__APPLE__)
const int SIG_SUSPEND = 0;
#elif defined(__sun) || defined(__CYGWIN__) || defined(__FreeBSD__)
const int SIG_SUSPEND = SIGUSR2;
#elif defined(__linux)
const int SIG_SUSPEND = NSIG - 1;
#elif defined(SIGRTMAX)
/* This might be a function call, in which case try _SIGRTMAX or initializing it somewhere. */
const int SIG_SUSPEND = SIGRTMAX;
#elif defined(SIGUSR2)
const int SIG_SUSPEND = SIGUSR2;
#else
#error Unable to determine SIG_SUSPEND.
#endif

#ifndef __APPLE__

#define ZeroMemory(a, b) (memset((a), 0, (b)))

typedef struct sigaction sigaction_t;

#define SetupHandlers ThreadPThreadC_SetupHandlers
#define SignalHandler ThreadPThread__SignalHandler

static sigset_t mask;

/* Signal based suspend/resume */
static sem_t ackSem;

#ifdef __STDC__
void SignalHandler(int, siginfo_t*, void* /* ucontext_t* */);
#else
void SignalHandler();
#endif

#endif /* Apple */

void ThreadPThreadC_SetupHandlers()
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    sigaction_t act;
    sigaction_t oact;
    int r;

    ZeroMemory(&act, sizeof(act));
    ZeroMemory(&oact, sizeof(oact));

    r = sem_init(&ackSem, 0, 0);
    assert(r == 0);

    r = sigfillset(&mask);
    assert(r == 0);
    r = sigdelset(&mask, SIG_SUSPEND);
    assert(r == 0);
    r = sigdelset(&mask, SIGINT);
    assert(r == 0);
    r = sigdelset(&mask, SIGQUIT);
    assert(r == 0);
    r = sigdelset(&mask, SIGABRT);
    assert(r == 0);
    r = sigdelset(&mask, SIGTERM);
    assert(r == 0);

    act.sa_flags = SA_RESTART | SA_SIGINFO;
    act.sa_sigaction = SignalHandler;
    r = sigfillset(&act.sa_mask);
    assert(r == 0);
    r = sigaction(SIG_SUSPEND, &act, &oact);
    assert(r == 0);
#endif
}

int ThreadPThreadC_sem_wait()
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    return sem_wait(&ackSem);
#endif
}

int ThreadPThreadC_sem_post()
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    return sem_post(&ackSem);
#endif
}

int ThreadPThreadC_sem_getvalue(value)
    int* value;
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    return sem_getvalue(&ackSem, value);
#endif
}

int ThreadPThreadC_sigsuspend()
{
#ifdef __APPLE__
    APPLE_ASSERT_FALSE
#else
    return sigsuspend(&mask);
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
