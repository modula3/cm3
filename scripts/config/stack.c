#define __EXTENSIONS__
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <limits.h>
#include <signal.h>
#ifdef __sun
#include <alloca.h>
#endif
#include <ucontext.h>
typedef unsigned U;

char* volatile Stack2;
char* volatile Stack1;
sig_atomic_t volatile SignalOccured;

void IgnoreSignal(int a, siginfo_t* b, void* c)
{
    SignalOccured = 1;
    _exit(2);
}

void* ThreadMain(void* a)
{
    Stack1 = (char*)&a;
    while (SignalOccured == 0)
    {
        Stack2 = (char*)alloca(1024);
        Stack2[0] = Stack2[1];
    }
    return 0;
}

int main()
{
    stack_t stack = { 0 };
    pthread_attr_t attr = { 0 };
    pthread_t thread = { 0 };
    size_t stacksize = 1024;
    struct sigaction siga = { 0 };
    int i = { 0 };
    char signal_stack[SIGSTKSZ];

    printf("SIGSTKSZ: %u %x\n", (U)SIGSTKSZ, (U)SIGSTKSZ);
    printf("PTHREAD_STACK_MIN: %u %x\n", (U)PTHREAD_STACK_MIN, (U)PTHREAD_STACK_MIN);

    stack.ss_size = SIGSTKSZ;
    stack.ss_sp = signal_stack;
    i = sigaltstack(&stack, NULL);
    printf("sigaltstack:%d\n", i);

    siga.sa_flags = SA_SIGINFO | SA_ONSTACK;
    siga.sa_sigaction = IgnoreSignal;
    i = sigaction(SIGSEGV, &siga, NULL);
    printf("sigaction:%d\n", i);

    pthread_attr_init(&attr);
    while (pthread_attr_setstacksize(&attr, stacksize) != 0)
    {
        stacksize += 1024;
    }
    printf("stacksize: %u %x\n", (U)stacksize, (U)stacksize);
    pthread_create(&thread, &attr, &ThreadMain, NULL);
    while (SignalOccured == 0)
    {
        sleep(1);
    }

    printf("%p %p\n", Stack1, Stack2);

    return 0;
}
