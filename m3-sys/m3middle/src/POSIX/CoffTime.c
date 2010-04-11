/* Copyright (C) 1994, Digital Equipment Corporation         */
/* All rights reserved.                                      */
/* See the file COPYRIGHT for a full description.            */

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

INTEGER /* should be LONGINT */
__cdecl
CoffTime__Now(void)
{
    struct timeval tv;

    ZERO_MEMORY(tv);
    gettimeofday(&tv, NULL);
    return tv.tv_sec;
}

INTEGER /* should be LONGINT */
__cdecl
CoffTime__OfFile(TEXT tpath)
{
    const char* path;
    stat_t st;
    int i;

    if (tpath == NULL)
        return 0;

    ZERO_MEMORY(st);
    path = M3toC__SharedTtoS(tpath);

    i = stat(path, (struct stat*)&st); /* cast is for Darwin/arm */
    M3toC__FreeSharedS(tpath, path);
    if (i) /* ignore error */
        return 0;
    return st.st_mtime;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
