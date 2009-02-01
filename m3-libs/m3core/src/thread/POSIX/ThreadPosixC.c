/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/* This is based on RTThread.m3, which is platform specific
and varied a lot. Some versions cached the mask to use
in allow/disallow, some recomputed it each time.
Some used sigaction(), some used signal().
The users of sigaction() vary as to which flags they use.
Some use BSD sigvec which is similar to sigaction.
*/

#include <signal.h>
#include "ThreadPosixC.h"
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ZeroMemory(address, size) (memset((address), 0, (size)))

#define SignalHandler1 ThreadPosixC_SignalHandler1
#define SignalHandler3 ThreadPosixC_SignalHandler3
#define setup_sigvtalrm ThreadPosixC_setup_sigvtalrm
#define allow_sigvtalrm ThreadPosixC_allow_sigvtalrm
#define disallow_sigvtalrm ThreadPosixC_disallow_sigvtalrm
#define Init ThreadPosixC_Init
#define ThreadSwitchSignal ThreadPosixC_ThreadSwitchSignal

sigset_t ThreadSwitchSignal;

void setup_sigvtalrm(SignalHandler1 handler)
{
#if 0
    sigaction_t sa;

    ZeroMemory(&sa, sizeof(sa));
    sa.sa_flags = SA_SIGINFO /* | SA_RESTART ? */;
    sa.sa_sigaction = handler;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGVTALRM, &sa, NULL);
#else
    void* old = signal(SIGVTALRM, handler);
    assert(old != SIG_ERR);
#endif
}

void allow_sigvtalrm(void)
{
    int i = sigprocmask(SIG_UNBLOCK, &ThreadSwitchSignal, NULL);
    assert(i == 0);
}

void disallow_sigvtalrm(void)
{
    int i = sigprocmask(SIG_BLOCK, &ThreadSwitchSignal, NULL);
    assert(i == 0);
}

void Init(void)
{
    int i = sigemptyset(&ThreadSwitchSignal);
    assert(i == 0);
    i = sigaddset(&ThreadSwitchSignal, SIGVTALRM);
    assert(i == 0);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
