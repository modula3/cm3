#ifdef __cplusplus
extern "C"
#endif

#ifndef _WIN32

#include <termios.h>
#include <unistd.h> 
typedef struct termios termios_t;

static termios_t TermCooked;
static termios_t TermRaw;

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif

static void TermC__cfmakeraw(termios_t* t)
{
#ifdef __CYGWIN__
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

void TermC__Init(void)
{
    tcgetattr(STDIN_FILENO, &TermCooked);
    TermRaw = TermCooked;
    TermC__cfmakeraw(&TermRaw);
}

void* TermC__GetTermRaw(void)
{
    return &TermRaw;
}

void* TermC__GetTermCooked(void)
{
    return &TermCooked;
}

#else

void TermC__Init(void)
{
}

void* TermC__GetTermRaw(void)
{
    return 0;
}

void* TermC__GetTermCooked(void)
{
    return 0;
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
