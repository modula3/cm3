/*
 * Helper functions for accessing standard C's "errno" value.
 */

#if defined(__INTERIX) && !defined(_REENTRANT)
#define _REENTRANT
#endif

#include <errno.h>

int
SupErrno_GetErrno(void)
{
    return errno;
}

void
SupErrno_SetErrno(int e)
{
    errno = e;
}
