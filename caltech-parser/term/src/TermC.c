#ifdef __cplusplus
extern "C"
#endif

#ifndef _WIN32

#include <termios.h>
#include <unistd.h> 
typedef struct termios termios_t;

static termios_t TermCooked;
static termios_t TermRaw;

void* Term__GetTermRaw(void) { return &TermRaw; }
void* Term__GetTermCooked(void) { return &TermCooked; }

#else

void* Term__GetTermRaw(void) { return 0; }
void* Term__GetTermCooked(void) { return 0; }

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
