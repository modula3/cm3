#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>

char* signal_handler_stack[3];
unsigned signal_handler_stack_count;

void signal_handler(int sig)
{
    char a;
    signal_handler_stack[signal_handler_stack_count++] = &a;
}

void test_sigstack()
{
    struct sigstack stack;
    unsigned stack_size = 1024 * 1024 * 2;
    char* s = calloc(1, stack_size);
    struct sigaction sa;

    memset(&stack, 0, sizeof(stack));
    memset(&sa, 0, sizeof(sa));    

    sa.sa_flags = SA_ONSTACK;
    sa.sa_handler = signal_handler;
    if (sigaction(SIGUSR1, &sa, NULL))
        perror("sigaction");

    raise(SIGUSR1);
    raise(SIGUSR1);

    printf("test_sigstack: malloced stack %p - %p\n", s, s + stack_size);
    printf("test_sigstack: current stack %p\n", &s);
    stack.ss_onstack = 0;
    stack.ss_sp = s + stack_size / 2;
    if (sigstack(&stack, NULL) < 0)
        perror("sigstack");

    raise(SIGUSR1);

    printf("signal_handler_stack %p %p %p\n",
           signal_handler_stack[0],
           signal_handler_stack[1],
           signal_handler_stack[2]);
}

int main()
{
    test_sigstack();
    return EXIT_SUCCESS;
}
