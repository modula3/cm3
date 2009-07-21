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

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif

/* not used */
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif

/* not used */
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

EXTERN_CONST int Termios__Stdin = STDIN_FILENO;
EXTERN_CONST int Termios__Stdout = STDOUT_FILENO; /* not used */
EXTERN_CONST int Termios__Stderr = STDERR_FILENO; /* not used */
EXTERN_CONST int Termios__Tcsanow = TCSANOW;
EXTERN_CONST int Termios__Tcsadrain = TCSADRAIN; /* not used */
EXTERN_CONST int Termios__Tcsaflush = TCSAFLUSH; /* not used */

typedef struct termios termios_t;

void Termios__tcgetattr(int fd, termios_t* t)
{
    tcgetattr(fd, t);
}

void Termios__tcsetattr(int fd, int action, termios_t* t)
{
    tcsetattr(fd, action, t);
}

void Termios__cfmakeraw(termios_t* t)
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

#else

EXTERN_CONST int Termios__Stdin = 0;
EXTERN_CONST int Termios__Stdout = 0; /* not used */
EXTERN_CONST int Termios__Stderr = 0; /* not used */
EXTERN_CONST int Termios__Tcsanow = 0;
EXTERN_CONST int Termios__Tcsadrain = 0; /* not used */
EXTERN_CONST int Termios__Tcsaflush = 0; /* not used */

void Termios__tcgetattr(int fd, void* t)
{
}

void Termios__tcsetattr(int fd, int action, void* t)
{
}

void Termios__cfmakeraw(void* t)
{
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
