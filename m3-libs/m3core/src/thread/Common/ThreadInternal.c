/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#include <poll.h>

M3_EXTERNC_BEGIN

#ifndef _WIN32

enum {WaitResult_Ready, WaitResult_Error, WaitResult_FDError, WaitResult_Timeout};
/* ^Must match TYPE WaitResult declared in SchedularPosix.i3. */ 

#undef THOUSAND /* Support concatenating multiple .c files. */
#undef MILLION  /* Support concatenating multiple .c files. */
#define THOUSAND (1000)
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
    r = poll(&p, 1, (m3timeout < 0) ? -1 : (m3timeout * THOUSAND));
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
                       fd_set* read,
                       fd_set* write,
                       fd_set* xcept, /* except is keyword on OSF/1 */
                       LONGREAL/*Time.T*/ m3timeout)
{
    struct timeval timeout;
    double n = { 0 };

    if (m3timeout < 0)
        return select(nfds, read, write, xcept, NULL);

    ZERO_MEMORY(timeout);
    timeout.tv_usec = modf(m3timeout, &n) * MILLION;
    timeout.tv_sec = n;
    return select(nfds, read, write, xcept, &timeout);
}

#endif

M3_EXTERNC_END
