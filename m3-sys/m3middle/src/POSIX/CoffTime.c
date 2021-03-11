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
CoffTime__OfFile(const char* path)
{
    struct stat st;
    int i;

    if (path == NULL)
        return 0;

    ZERO_MEMORY(st);

    i = stat(path, &st);
    if (i) /* ignore error */
        return 0;
    return st.st_mtime;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
