/*
Posix says interval timers are "reset" by fork.
By they don't define "reset".
Let's see.
*/

#include "m3core.h"

void __cdecl alarm_handler(int sig)
{
    char buffer[1000];

    memset(buffer, 0, sizeof(buffer));
    sprintf(buffer, "%d alarm\n", getpid());
    write(1, buffer, strlen(buffer));
}

typedef void (*SignalHandler1)(int signo);

static sigset_t ThreadSwitchSignal;

#define SIG_TIMESLICE SIGVTALRM

void __cdecl setup_sigvtalrm(SignalHandler1 handler)
{
  struct sigaction act;

  memset(&act, 0, sizeof(act));
  memset(&ThreadSwitchSignal, 0, sizeof(ThreadSwitchSignal));

  sigemptyset(&ThreadSwitchSignal);
  sigaddset(&ThreadSwitchSignal, SIG_TIMESLICE);

  act.sa_handler = handler;
  act.sa_flags = SA_RESTART;
  sigemptyset(&(act.sa_mask));
  if (sigaction (SIG_TIMESLICE, &act, NULL)) abort();
}

void __cdecl allow_sigvtalrm(void)
{
    int i = sigprocmask(SIG_UNBLOCK, &ThreadSwitchSignal, NULL);
    assert(i == 0);
}

void __cdecl disallow_sigvtalrm(void)
{
    int i = sigprocmask(SIG_BLOCK, &ThreadSwitchSignal, NULL);
    assert(i == 0);
}

int main()
{
    struct timeval tv;
    struct itimerval it;

    memset(&tv, 0, sizeof(tv));
    memset(&it, 0, sizeof(it));

    alarm_handler(0);

    tv.tv_sec = 1;
    it.it_value = tv;
    it.it_interval = tv;

    printf("setitimer: %d\n", setitimer(ITIMER_VIRTUAL, &it, NULL));
    setup_sigvtalrm(alarm_handler);
    allow_sigvtalrm();

    fork();

    alarm_handler(0);

    while (1) { }

    return 0;
}
