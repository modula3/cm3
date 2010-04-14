/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

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

#ifdef __cplusplus
} /* extern "C" */
#endif
