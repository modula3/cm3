/*
 * Helper functions for accessing the "h_errno" value.
 */

#include <netdb.h>

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
