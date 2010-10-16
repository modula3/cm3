/*
 * Helper functions for accessing standard C's "errno" value.

copied from m3core/src/C/Common/Cerrno.i3 for bootstrapping against older releases 
 */

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#if defined(__INTERIX) && !defined(_REENTRANT)
#define _REENTRANT
#endif

#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

int
__cdecl
System__GetErrno(void)
{
    return errno;
}

#ifdef __cplusplus
}
#endif
