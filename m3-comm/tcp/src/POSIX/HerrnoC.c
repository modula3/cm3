/*
 * Helper functions for accessing the "h_errno" value.
 */

#if defined(__INTERIX) && !defined(_REENTRANT)
#define _REENTRANT
#endif
#include <netdb.h>

#ifdef __cplusplus
extern "C" {
#endif

int
m3_Herrno_Get_h_errno(void)
{
    return h_errno;
}

void
m3_Herrno_Set_h_errno(int he)
{
    h_errno = he;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
