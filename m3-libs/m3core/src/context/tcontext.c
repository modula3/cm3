/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
*/

#include <stdio.h>
#include "context.h"
#include <time.h>
#include <sys/time.h>
#include <assert.h>
typedef struct itimerval itimerval_t;
typedef struct timeval timeval_t;

static ucontext_t ctx[4];
unsigned preempt = 1;
volatile unsigned switches;
volatile unsigned current_thread;

typedef void (*SignalHandler)(int signo);

sigset_t ThreadSwitchSignal;

typedef timeval_t UTime;
UTime selected_interval = {0, 100 * 1000};

static void f1(int a, int b, int c)
{
    printf("start f1(%d, %d, %d)\n", a, b, c);
    if (preempt == 0)
    {
        swapcontext(&ctx[1], &ctx[3]);
    }
    else
    {
        printf("in f1(%d, %d, %d)\n", a, b, c);
        while (switches < 10) { }
    }
        
    printf("finish f1(%d, %d, %d)\n", a, b, c);
}

static void f2()
{
    printf("start f2\n");

    if (preempt == 0)
    {
        swapcontext(&ctx[2], &ctx[1]);
    }
    else
    {
        printf("in f2\n");
        while (switches < 10) { }
    }

    puts("finish f2");
}

static void f3(int a, int b)
{
    printf("start f3(%d, %d)\n", a, b);

    if (preempt == 0)
    {
        swapcontext(&ctx[3], &ctx[2]);
    }
    else
    {
        printf("in f3(%d, %d)\n", a, b);
        while (switches < 10) { }
    }

    printf("finish f3(%d, %d)\n", a, b);
}

void setup_sigvtalrm(SignalHandler handler)
{
    void* old = signal(SIGVTALRM, handler);
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

void init_ThreadSwitchSignal(void)
{
    int i = sigemptyset(&ThreadSwitchSignal);
    assert(i == 0);
    i = sigaddset(&ThreadSwitchSignal, SIGVTALRM);
    assert(i == 0);
}

void switch_thread(int sig)
{
    unsigned previous_thread = current_thread++;
    allow_sigvtalrm();
    switches += 1;
    swapcontext(&ctx[previous_thread & 3], &ctx[current_thread & 3]);
}

void StartSwitching(void)
{
    itimerval_t interval;
    itimerval_t old_interval;

    init_ThreadSwitchSignal();
    setup_sigvtalrm(switch_thread);
    interval.it_interval = selected_interval;
    interval.it_value = selected_interval;
    setitimer(ITIMER_VIRTUAL, &interval, &old_interval);
    allow_sigvtalrm();
}

int main(void)
{
    char st[3][8192];
    unsigned i;

    for (i = 0 ; i != 4 ; ++i)
    {
        getcontext(&ctx[i]);
        if (i != 0)
        {
            ctx[i].uc_stack.ss_sp = st[i - 1];
            ctx[i].uc_stack.ss_size = sizeof st[i - 1];
            ctx[i].uc_link = &ctx[i - 1];
        }
    }

    makecontext(&ctx[1], f1, 3, 1, 2, 3);
    makecontext(&ctx[2], f2, 0);
    makecontext(&ctx[3], f3, 2, 4, 5);

    if (preempt == 0)
    {
        swapcontext(&ctx[0], &ctx[3]);
    }
    else
    {
        StartSwitching();
        while (switches < 10)
        {
            sleep(0);
        }
    }

    printf("done\n");

    return 0;
}
