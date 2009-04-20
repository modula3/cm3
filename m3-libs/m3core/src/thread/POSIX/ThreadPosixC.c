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

#include "ThreadPosix.h"
#include <signal.h>
#include <assert.h>
#include <setjmp.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ZeroMemory(address, size) (memset((address), 0, (size)))

#define SignalHandler1      ThreadPosix__SignalHandler1
#define setup_sigvtalrm     ThreadPosix__setup_sigvtalrm
#define allow_sigvtalrm     ThreadPosix__allow_sigvtalrm
#define disallow_sigvtalrm  ThreadPosix__disallow_sigvtalrm
#define InitC               ThreadPosix__InitC

static sigset_t ThreadSwitchSignal;

#ifdef __CYGWIN__
#define SIG_TIMESLICE SIGALRM
#else
#define SIG_TIMESLICE SIGVTALRM
#endif

void setup_sigvtalrm(SignalHandler1 handler)
{
    /* This should really use sigaction, right? */
    void (*old)(int) = signal(SIG_TIMESLICE, handler);
    assert(old != SIG_ERR);
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

void RTHooks__PushEFrame(void* frame)
{
    ThreadPosix__PushEFrame(frame);
}

void RTHooks__PopEFrame(void* frame)
{
    ThreadPosix__PopEFrame(frame);
}

void InitC(void)
{
    int i = sigemptyset(&ThreadSwitchSignal);
    assert(i == 0);
    i = sigaddset(&ThreadSwitchSignal, SIG_TIMESLICE);
    assert(i == 0);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
