/*
 * Helper functions for accessing standard C's "errno" value.
 */

#include <errno.h>

int
m3_Cerrno_GetErrno(void)
{
    return errno;
}

void
m3_Cerrno_SetErrno(int e)
{
    errno = e;
}
