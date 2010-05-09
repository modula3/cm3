#include "TermC.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

EXTERN_CONST int Termios__Stdin = STDIN_FILENO;
EXTERN_CONST int Termios__Tcsanow = TCSANOW;

void __cdecl Termios__tcsetattr(int fd, int action, struct termios* t)
{
    if (TermC__Inited() && (t == TermC__GetTermRaw() || t == TermC__GetTermCooked()) && action == TCSANOW && fd == STDIN_FILENO)
    {
        tcsetattr(fd, action, t);
    }
}

#else

EXTERN_CONST int Termios__Stdin = 0;
EXTERN_CONST int Termios__Tcsanow = 0;

void __cdecl Termios__tcsetattr(int fd, int action, void* t)
{
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
