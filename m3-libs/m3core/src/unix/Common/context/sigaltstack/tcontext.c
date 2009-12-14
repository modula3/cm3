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
#include <assert.h>
#include <unistd.h>
typedef struct itimerval itimerval_t;
typedef struct timeval timeval_t;
typedef struct sigaction sigaction_t;
typedef struct sigcontext sigcontext_t;

Context_t ctx[4];
volatile unsigned preempt = 1;
volatile unsigned switches;
volatile unsigned current_thread;
volatile unsigned next_thread;
volatile unsigned done[4];

typedef void (*SignalHandler1)(int signo);

sigset_t ThreadSwitchSignal;

#if 1 /* defined(__CYGWIN__) || defined(__APPLE__) */
#define SIG_TIMESLICE SIGALRM
#define ITIMER_TIMESLICE ITIMER_REAL
#else
#define ITIMER_TIMESLICE ITIMER_VIRTUAL
#define SIG_TIMESLICE SIGVTALRM
#endif

void print_threadid(int id)
{
#ifdef __CYGWIN__
    printf("thread %d:%u\n", id, GetCurrentThreadId());
#endif
}

static void F(int id, void* b)
{
    int a = (int)(size_t)b;

    printf("start f%d(%d)\n", id, a);
    print_threadid(id);

    printf("in f%d(%d)\n", id, a);
    while (switches < 10) { sleep(0); }

    print_threadid(id);
    printf("finish f%d(%d)\n", id, a);
    done[a] = 1;
}

static void f1(void* a)
{
    F(1, a);
}

static void f2(void* a)
{
    F(2, a);
}

static void f3(void* a)
{
    F(3, a);
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

    previous_thread = current_thread;
    next_thread = (current_thread + 1);
    while (done[next_thread & 3] == 1)
    {
        next_thread += 1;
    }

    print_threadid(-1);
    printf("switching from %u to %u\n", current_thread & 3, next_thread & 3);
    current_thread = next_thread;
    SwapContext(&ctx[previous_thread & 3], &ctx[next_thread & 3]);
}

void StartSwitching(void)
{
    itimerval_t interval = { 0 };
    int a = { 0 };
    
    init_ThreadSwitchSignal();
    setup_sigvtalrm(switch_thread);
    interval.it_interval.tv_sec = 0;
    interval.it_interval.tv_usec = 100 * 1000;
    interval.it_value = interval.it_interval;
    a = setitimer(ITIMER_TIMESLICE, &interval, NULL);
    assert(a == 0);
    allow_sigvtalrm();
}

int main(void)
{
    static size_t st[3][256 * 1024 / sizeof(size_t)];
    unsigned i;

    print_threadid(-2);

    GetContext(&ctx[0]);
    MakeContext(&ctx[1], f1, (void*)1, st[0], sizeof(st[0]));
    MakeContext(&ctx[2], f2, (void*)2, st[1], sizeof(st[1]));
    MakeContext(&ctx[3], f3, (void*)3, st[2], sizeof(st[2]));

    StartSwitching();
    while (!done[1] || !done[2] || !done[3])
    {
        /*printf(".")g;*/
        sleep(0);
    }

    printf("done\n");

    return 0;
}
