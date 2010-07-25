/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
*/

#include <stdio.h>
#include "context.h"
#include <time.h>
#include <sys/time.h>
#include <assert.h>
#ifdef __CYGWIN__
#include <windows.h>
#endif
#include <unistd.h>
typedef struct itimerval itimerval_t;
typedef struct timeval timeval_t;
typedef struct sigaction sigaction_t;
typedef struct sigcontext sigcontext_t;

ucontext_t ctx[4];
volatile unsigned preempt = 1;
volatile unsigned switches;
volatile unsigned current_thread;
volatile unsigned next_thread;
volatile unsigned done[4];

#define USE_YIELD defined(__CYGWIN__)
/*#undef USE_YIELD*/
/*#define USE_YIELD 1*/

void MyYield()
{
#if USE_YIELD
    if (current_thread != next_thread)
    {
        unsigned previous = current_thread;
        current_thread = next_thread;
        printf("switching from %u to %u\n", previous & 3, next_thread & 3);
        swapcontext(&ctx[previous & 3], &ctx[next_thread & 3]);
    }
#endif
}

#ifndef __CYGWIN__
#define ZeroMemory(address, size) (memset((address), 0, (size)))
#endif

typedef void (*SignalHandler1)(int signo);

sigset_t ThreadSwitchSignal;

struct timeval selected_interval = {0, 100 * 1000};

#ifdef __CYGWIN__
#define SIG_TIMESLICE SIGALRM
#define ITIMER_TIMESLICE ITIMER_REAL
#else
#define ITIMER_TIMESLICE ITIMER_VIRTUAL
#define SIG_TIMESLICE SIGVTALRM
#endif

void print_threadid(const char* function)
{
#ifdef __CYGWIN__
    printf("%s on thread %u\n", function, GetCurrentThreadId());
#endif
}

static void f1(int a, int b, int c)
{
    printf("start f1(%d, %d, %d)\n", a, b, c);
    print_threadid("f1");

    if (preempt == 0)
    {
        swapcontext(&ctx[1], &ctx[3]);
    }
    else
    {
        printf("in f1(%d, %d, %d)\n", a, b, c);
        while (switches < 10) { MyYield(); }
    }

    print_threadid("f1");        
    printf("finish f1(%d, %d, %d)\n", a, b, c);
    done[1] = 1;
}

static void f2()
{
    printf("start f2\n");
    print_threadid("f2");

    if (preempt == 0)
    {
        swapcontext(&ctx[2], &ctx[1]);
    }
    else
    {
        printf("in f2\n");
        while (switches < 10) { MyYield(); }
    }

    print_threadid("f2");
    puts("finish f2");
    done[2] = 1;
}

static void f3(int a, int b)
{
    printf("start f3(%d, %d)\n", a, b);
    print_threadid("f3");

    if (preempt == 0)
    {
        swapcontext(&ctx[3], &ctx[2]);
    }
    else
    {
        printf("in f3(%d, %d)\n", a, b);
        while (switches < 10) { MyYield(); }
    }

    print_threadid("f3");
    printf("finish f3(%d, %d)\n", a, b);
    done[3] = 1;
}

void setup_sigvtalrm(SignalHandler1 handler)
{
    signal(SIG_TIMESLICE, handler);
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
    i = sigaddset(&ThreadSwitchSignal, SIG_TIMESLICE);
    assert(i == 0);
}

void switch_thread(int signo)
{
    unsigned previous_thread = current_thread;
    allow_sigvtalrm();

    switches += 1;

#if USE_YIELD
    /* hasn't yielded yet */
    if (current_thread != next_thread)
        return;
#endif

    previous_thread = current_thread;
    next_thread = (current_thread + 1);
    while (done[next_thread & 3] == 1)
    {
        next_thread += 1;
    }

    print_threadid("switch_thread");
#if USE_YIELD
#else
    printf("switching from %u to %u\n", current_thread & 3, next_thread & 3);
    current_thread = next_thread;
    swapcontext(&ctx[previous_thread & 3], &ctx[next_thread & 3]);
#endif
}

void StartSwitching(void)
{
    struct itimerval interval;
    struct itimerval old_interval;

    init_ThreadSwitchSignal();
    setup_sigvtalrm(switch_thread);
    interval.it_interval = selected_interval;
    interval.it_value = selected_interval;
    setitimer(ITIMER_TIMESLICE, &interval, &old_interval);
    allow_sigvtalrm();
}

int main(void)
{
    static char st[3][256 * 1024];
    unsigned i;

    print_threadid("main");

    for (i = 0; i < 4; ++i)
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
        while (!done[1] || !done[2] || !done[3])
        {
            /*printf(".");*/
            MyYield();
            sleep(0);
        }
    }

    printf("done\n");

    return 0;
}
