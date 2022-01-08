/*
 * Helper functions for accessing standard C's "errno" value.
 */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

int
__cdecl
Cerrno__GetErrno(void)
{
    return errno;
}

void
__cdecl
Cerrno__SetErrno(int e)
{
    errno = e;
}

#ifdef __cplusplus
}
#endif
