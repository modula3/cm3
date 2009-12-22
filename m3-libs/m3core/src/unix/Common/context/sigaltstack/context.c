/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
see http://www.engelschall.com/pw/usenix/2000/pmt-html/
*/

#include "context.h"
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/signal.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif

static Context_t mctx_caller;
static sig_atomic_t volatile mctx_called;
static Context_t * volatile mctx_create;
static void (* volatile mctx_create_func)(void *);
static void * volatile mctx_create_arg;
static sigset_t mctx_create_sigs;

static void mctx_create_boot(void)
{
    void (*mctx_start_func)(void*);
    void* mctx_start_arg;
    
    sigprocmask(SIG_SETMASK, &mctx_create_sigs, NULL);
    
    mctx_start_func = mctx_create_func;
    mctx_start_arg = mctx_create_arg;
    
    SwapContext(mctx_create, &mctx_caller);
    
    mctx_start_func(mctx_start_arg);

    printf("end thread\n");
    while(1) sleep(100);
    abort(); /* not reached */
}

static void mctx_create_trampoline(int sig)
{
    if (sigsetjmp(mctx_create->jb, 0) == 0)
    {
        mctx_called = 1;
        return;
    }
    
    mctx_create_boot();
}

void
xGetContext(
    Context_t* context)
{
    context->err = errno;
}

void
xSetContext(
    Context_t* context)
{
    errno = context->err;
}

void
xSwapContext(
    Context_t* oldContext,
    Context_t* newContext)
{
    xGetContext(oldContext);
    xSetContext(newContext);
}

void
MakeContext( 
    Context_t *context, 
    void (*function)(void *),
    void *arg, 
    void *stack,
    size_t stack_size) 
{
    struct sigaction sa = { 0 };
    struct sigaction osa = { 0 };
    stack_t ss = { 0 };
    stack_t oss = { 0 };
    sigset_t osigs = { 0 };
    sigset_t sigs = { 0 };

    sigemptyset(&sigs);
    sigaddset(&sigs, SIGUSR1);
    sigprocmask(SIG_BLOCK, &sigs, &osigs);
    
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = mctx_create_trampoline;
    sa.sa_flags = SA_ONSTACK;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGUSR1, &sa, &osa);
    
    ss.ss_sp = stack;
    ss.ss_size = stack_size;
    ss.ss_flags = 0;
    sigaltstack(&ss, &oss);
    
    mctx_create = context;
    mctx_create_func = function;
    mctx_create_arg = arg;
    mctx_create_sigs = osigs;
    mctx_called = 0;
    kill(getpid(), SIGUSR1);
    sigfillset(&sigs);
    sigdelset(&sigs, SIGUSR1);
    while (!mctx_called)
        sigsuspend(&sigs);
        
    sigaltstack(NULL, &ss);
    ss.ss_flags = SS_DISABLE;
    sigaltstack(&ss, NULL);
    if (!(oss.ss_flags & SS_DISABLE))
        sigaltstack(&oss, NULL);
    sigaction(SIGUSR1, &osa, NULL);
    sigprocmask(SIG_SETMASK, &osigs, NULL);
    
    SwapContext(&mctx_caller, context);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
