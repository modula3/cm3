#ifdef __cplusplus
extern "C"
#endif

#ifndef _WIN32

#include <termios.h>
#include <unistd.h> 
typedef struct termios termios_t;

void Termios__cfmakeraw(termios_t* t);

static termios_t TermCooked;
static termios_t TermRaw;

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif

void TermC__Init(void)
{
    tcgetattr(STDIN_FILENO, &TermCooked);
    TermRaw = TermCooked;
    Termios__cfmakeraw(&TermRaw);
}

void* TermC__GetTermRaw(void) { return &TermRaw; }
void* TermC__GetTermCooked(void) { return &TermCooked; }

#else

void TermC__Init(void) { }
void* TermC__GetTermRaw(void) { return 0; }
void* TermC__GetTermCooked(void) { return 0; }

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
