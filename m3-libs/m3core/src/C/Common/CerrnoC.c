/*
 * Helper functions for accessing standard C's "errno" value.
 */

#include <errno.h>

int
Cerrno__GetErrno(void)
{
    return errno;
}

void
Cerrno__SetErrno(int e)
{
    errno = e;
}
