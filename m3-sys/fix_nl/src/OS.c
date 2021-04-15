/* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

#define MILLION (1000 * 1000)

int
__cdecl
OS__UTimes(TEXT tpath, LONGREAL/*Time.T*/ m3time)
{
    int result;
    const char* path;

    if (tpath == NULL)
    {
        errno = EINVAL;
        return -1;
    }

    path = M3toC__SharedTtoS(tpath);
    {
#ifndef __INTERIX
        struct timeval tv[2];
        double n = { 0 };

        ZERO_MEMORY(tv);
        tv[0].tv_usec = modf(m3time, &n) * MILLION; /* last access time */
        tv[0].tv_sec = n;                           /* last access time */
        tv[1] = tv[0]; /* last modified time */
        result = utimes(path, tv);
#else
        struct utimebuf times;

        ZERO_MEMORY(times);
        times.actime = m3time;
        times.modtime = times.actime;
        result = utime(path, &times);
#endif
    }
    M3toC__FreeSharedS(tpath, path);
    return result;
}

#endif /* WIN32 */

#ifdef __cplusplus
} /* extern "C" */
#endif
