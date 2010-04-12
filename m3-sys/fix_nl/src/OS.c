/* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

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

        ZERO_MEMORY(tv);
        TimePosix__FloatSecondsToMicrosecondsStruct(m3time, &tv[0]); /* last accessed time */
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
