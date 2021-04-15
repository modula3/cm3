/* Copyright 1992 Digital Equipment Corporation.             */
/* Distributed only by permission.                           */
/* Last modified on Thu Jun 20 17:23:58 PDT 1996 by heydon   */
/*      modified on Tue May  3 19:06:17 PDT 1994 by najork   */
/*      modified on Tue Dec 22 13:56:41 PST 1992 by mhb      */
/*      modified on Fri Nov 20 15:49:27 PST 1992 by sclafani */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#include <poll.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DelayMicroseconds 250000

int
__cdecl
MidiLineServer__Poll(int fd)
{
    struct pollfd p;

    ZERO_MEMORY(p);
    p.fd = fd;
    p.events = POLLIN;
    p.revents = 0;
    return poll(&p, 1, DelayMicroseconds);
}

#ifdef __cplusplus
}
#endif
