/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#include <signal.h>
#include <semaphore.h>
#include <assert.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ZeroMemory(a, b) (memset((a), 0, (b)))

typedef struct sigaction sigaction_t;

#define mask ThreadPThreadC_mask
#define ackSem ThreadPThreadC_ackSem
#define SetupHandlers ThreadPThreadC_SetupHandlers

sigset_t mask;

/* Signal based suspend/resume */
sem_t ackSem;

void ThreadPThreadC_SetupHandlers(void* SignalHandler, int sig)
{
    sigaction_t act;
    sigaction_t oact;
    int r;

    ZeroMemory(&act, sizeof(act));
    ZeroMemory(&oact, sizeof(oact));

    r = sem_init(&ackSem, 0, 0);
    assert(r == 0);
    sigfillset(&mask);
    assert(r == 0);

    sigdelset(&mask, sig);
    sigdelset(&mask, SIGINT);
    assert(r == 0);
    sigdelset(&mask, SIGQUIT);
    assert(r == 0);
    sigdelset(&mask, SIGABRT);
    assert(r == 0);
    sigdelset(&mask, SIGTERM);
    assert(r == 0);

    act.sa_flags = SA_RESTART | SA_SIGINFO;
    act.sa_sigaction = SignalHandler;
    sigfillset(&act.sa_mask);
    assert(r == 0);
    sigaction(sig, &act, &oact);
    assert(r == 0);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
