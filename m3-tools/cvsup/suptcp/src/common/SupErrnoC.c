/*
 * Helper functions for accessing standard C's "errno" value.
 */

#include "m3core.h"
#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

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

#ifdef __cplusplus
} /* extern "C" */
#endif
