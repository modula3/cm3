#include "TermC.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

static struct termios TermCooked;
static struct termios TermRaw;

static void TermC__cfmakeraw(struct termios* t)
{
#if defined(__CYGWIN__) || defined(__sun)
/* http://linux.about.com/library/cmd/blcmdl3_cfmakeraw.htm */
    t->c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON);
    t->c_oflag &= ~OPOST;
    t->c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
    t->c_cflag &= ~(CSIZE|PARENB);
    t->c_cflag |= CS8;
#else
    cfmakeraw(t);
#endif
}

static volatile int inited;

int __cdecl TermC__Inited(void)
{
    return inited;
}

void __cdecl TermC__Init(void)
{
    tcgetattr(STDIN_FILENO, &TermCooked);
    TermRaw = TermCooked;
    TermC__cfmakeraw(&TermRaw);
    /* memory barrier needed here, volatile does it for some compilers */
    inited = 1;
}

void* __cdecl TermC__GetTermRaw(void)
{
    return &TermRaw;
}

void* __cdecl TermC__GetTermCooked(void)
{
    return &TermCooked;
}

#else

void __cdecl TermC__Init(void)
{
}

void* __cdecl TermC__GetTermRaw(void)
{
    return 0;
}

void* __cdecl TermC__GetTermCooked(void)
{
    return 0;
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
