/*
 * Helper functions for accessing standard C's "errno" value.
 */

#ifdef _MSC_VER
#undef _DLL
#ifndef _MT
#define _MT
#endif
#endif

#include "m3core.h"

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
