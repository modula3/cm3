/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3core.h"
#include <poll.h>

#ifdef __cplusplus
extern "C" {
#endif

M3_NO_INLINE static int
ThreadInternal__StackGrowsDownHelper (volatile char* a)
/* Inlining could potentially reverse the relative placements,
 * leading to an incorrect result! Recursion used to
 * help defeat inlining optimizer, though a smart compiler
 * could still inline. */
{
  volatile char b;
  return a ? (&b < a) : ThreadInternal__StackGrowsDownHelper (&b);
}

M3_DLL_LOCAL int __cdecl
ThreadInternal__StackGrowsDown (void)
{
  return ThreadInternal__StackGrowsDownHelper (0);
}

#ifndef _WIN32

enum {WaitResult_Ready, WaitResult_Error, WaitResult_FDError, WaitResult_Timeout};

#define MILLION (1000 * 1000)

M3_DLL_LOCAL
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

M3_DLL_LOCAL
int
__cdecl
ThreadInternal__Select(int nfds,
                       ADDRESS read,
                       ADDRESS write,
                       ADDRESS xcept, /* except is keyword on OSF/1 */
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

#ifdef __cplusplus
} /* extern "C" */
#endif
