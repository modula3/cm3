/*
 * Helper functions for accessing standard C's "errno" value.
 */

#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _MSC_VER
#pragma optimize("gty", on)
#endif

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

#ifdef __cplusplus
}
#endif
