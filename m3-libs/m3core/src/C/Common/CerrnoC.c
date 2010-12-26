/*
 * Helper functions for accessing standard C's "errno" value.
 */

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

M3_DLL_EXPORT
int
__cdecl
Cerrno__GetErrno(void)
{
    return errno;
}

M3_DLL_EXPORT
void
__cdecl
Cerrno__SetErrno(int e)
{
    errno = e;
}

#ifdef __cplusplus
}
#endif
