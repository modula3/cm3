#define _INCLUDE_POSIX_SOURCE
#define _INCLUDE_HPUX_SOURCE
#define _FILE_OFFSET_BITS 64

#include <stdio.h>
#include <signal.h>

void Config()
{
    printf("SIGSUSPEND is SIGRTMAX if defined, else SIGUSR2\n");

#ifdef _NSIG
    printf("CONST _NSIG = %u;\n", _NSIG);
#else
    printf("_NSIG not defined\n");
#endif

#ifdef NSIG
    printf("CONST NSIG = %u;\n", NSIG);
#else
    printf("NSIG not defined\n");
#endif

#ifdef SIGRTMAX
    printf("CONST SIGRTMAX = %u;\n", SIGRTMAX);
#else
    printf("SIGRTMAX not defined\n");
#endif

#ifdef _SIGRTMAX
    printf("CONST _SIGRTMAX = %u;\n", _SIGRTMAX);
#else
    printf("_SIGRTMAX not defined\n");
#endif

#ifdef SIGUSR2
    printf("CONST SIGUSR2 = %u;\n", SIGUSR2);
#else
    printf("SIGUSR2 not defined\n");
#endif

#ifdef _SIGUSR2
    printf("CONST _SIGUSR2 = %u;\n", _SIGUSR2);
#else
    printf("_SIGUSR2 not defined\n");
#endif
}

int main()
{
    Config();
    return 0;
}
