#ifdef __cplusplus
extern "C" {
#endif

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const" */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

#ifndef _WIN32

#include <termios.h>
#include <unistd.h>
#include "TermC.h"

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif

EXTERN_CONST int Termios__Stdin = STDIN_FILENO;
EXTERN_CONST int Termios__Tcsanow = TCSANOW;

typedef struct termios termios_t;

void Termios__tcsetattr(int fd, int action, termios_t* t)
{
    if (TermC__Inited() && (t == TermC__GetTermRaw() || t == TermC__GetTermCooked()) && action == TCSANOW && fd == STDIN_FILENO)
    {
        tcsetattr(fd, action, t);
    }
}

#else

EXTERN_CONST int Termios__Stdin = 0;
EXTERN_CONST int Termios__Tcsanow = 0;

void Termios__tcsetattr(int fd, int action, void* t)
{
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
