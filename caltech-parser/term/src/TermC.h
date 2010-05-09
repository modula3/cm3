#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifndef _WIN32
#include <termios.h>
#include <unistd.h>
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#endif

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const"
 */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

#ifdef __cplusplus
extern "C" {
#endif

void* __cdecl TermC__GetTermRaw(void);
void* __cdecl TermC__GetTermCooked(void);
int __cdecl TermC__Inited(void);

#ifdef __cplusplus
} /* extern "C" */
#endif
