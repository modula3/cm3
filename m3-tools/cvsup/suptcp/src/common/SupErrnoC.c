/*
 * Helper functions for accessing standard C's "errno" value.
 */

#include "m3core.h"
#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

int
__cdecl
SupErrno_GetErrno(void)
{
    return errno;
}

void
__cdecl
SupErrno_SetErrno(int e)
{
    errno = e;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
