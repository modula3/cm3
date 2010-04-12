/* Copyright 1992 Digital Equipment Corporation.             */
/* Distributed only by permission.                           */
/* Last modified on Thu Jun 20 17:23:58 PDT 1996 by heydon   */
/*      modified on Tue May  3 19:06:17 PDT 1994 by najork   */
/*      modified on Tue Dec 22 13:56:41 PST 1992 by mhb      */
/*      modified on Fri Nov 20 15:49:27 PST 1992 by sclafani */

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

#define DelayMicroseconds 250000

int
__cdecl
MidiLineServer__Select(int socket)
{
    fd_set fds;
    struct timeval timeout;

    ZERO_MEMORY(timeout);
    ZERO_MEMORY(fds);
    FD_ZERO(&fds);
    FD_SET(socket, &fds);
    timeout.tv_sec = 0;
    timeout.tv_usec = DelayMicroseconds;
    return select(socket + 1, &fds, NULL, NULL, &timeout);
}

#ifdef __cplusplus
}
#endif
