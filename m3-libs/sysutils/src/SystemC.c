/*
 * Helper functions for accessing standard C's "errno" value.

copied from m3core/src/C/Common/Cerrno.i3 for bootstrapping against older releases 
 */

#if defined(__INTERIX) && !defined(_REENTRANT)
#define _REENTRANT
#endif

#include <errno.h>

int
System__GetErrno(void)
{
    return errno;
}
