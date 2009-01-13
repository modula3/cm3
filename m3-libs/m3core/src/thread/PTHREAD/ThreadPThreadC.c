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

#define SetupHandlers ThreadPThreadC_SetupHandlers

static sigset_t mask;

/* Signal based suspend/resume */
static sem_t ackSem;

void ThreadPThreadC_SetupHandlers(void* SignalHandler, int sig)
{
    sigaction_t act;
    sigaction_t oact;
    int r;

    ZeroMemory(&act, sizeof(act));
    ZeroMemory(&oact, sizeof(oact));

    r = sem_init(&ackSem, 0, 0);
    assert(r == 0);

    r = sigfillset(&mask);
    assert(r == 0);
    r = sigdelset(&mask, sig);
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
    r = sigaction(sig, &act, &oact);
    assert(r == 0);
}

int ThreadPThreadC_sem_wait(void)
{
    return sem_wait(&ackSem);
}

int ThreadPThreadC_sem_post(void)
{
    return sem_post(&ackSem);
}

int ThreadPThreadC_sem_getvalue(int* value)
{
    return sem_getvalue(&ackSem, value);
}

int ThreadPThreadC_sigsuspend (void)
{
    return sigsuspend(&mask);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
