/*
 * Helper functions for accessing standard C's "errno" value.
 */

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
