/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3core.h"
#include <poll.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

enum {WaitResult_Ready, WaitResult_Error, WaitResult_FDError, WaitResult_Timeout};

#define MILLION (1000 * 1000)

int
__cdecl
ThreadInternal__Poll(int fd,
                     int/*boolean*/ read,
                     LONGREAL/*Time.T*/ m3timeout)
{
    int r;
    struct pollfd p;

    ZERO_MEMORY(p);
    p.fd = fd;
    p.events = POLLERR | (read ? POLLIN : POLLOUT);
    p.revents = 0;
    r = poll(&p, 1, (m3timeout < 0) ? -1 : (m3timeout * MILLION));
    assert(r == -1 || r == 0 || r == 1);
    if (r == -1)
        return WaitResult_Error;
    else if (r == 0)
        return WaitResult_Timeout;
    else if ((read && (p.revents & POLLIN)) || ((!read) && (p.revents & POLLOUT)))
        return WaitResult_Ready;
    else
        return WaitResult_FDError;
}

int
__cdecl
ThreadInternal__Select(int nfds,
                       ADDRESS read,
                       ADDRESS write,
                       ADDRESS except,
                       LONGREAL/*Time.T*/ m3timeout)
{
    struct timeval timeout;
    double n = { 0 };

    if (m3timeout < 0)
        return select(nfds, read, write, except, NULL);

    ZERO_MEMORY(timeout);
    timeout.tv_usec = modf(m3timeout, &n) * MILLION;
    timeout.tv_sec = n;
    return select(nfds, read, write, except, &timeout);
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
