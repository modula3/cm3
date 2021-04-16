/* Copyright (C) 1992, Digital Equipment Corporation. */
/* All rights reserved. */
/* See the file COPYRIGHT for a full description. */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C"
{           
#endif

const char* FSPosixC__readdir_name(DIR* dir)
{
    struct dirent* e;

    if (dir && (e = readdir(dir)))
        return e->d_name;

    return 0;
}

int FSPosixC__SetModificationTime(const char* path, INTEGER updated, INTEGER accessed)
{
#ifdef __INTERIX
    struct utimbuf t;

    ZeroMemory(&t, sizeof(t));
    t.actime = accessed;
    t.modtime = updated;

    return utime(path, &t);
#else
    struct timeval t[2];

    ZeroMemory(t, sizeof(t));
    t[0].tv_sec = accessed;
    t[0].tv_usec = 0;
    t[1].tv_sec = updated;
    t[1].tv_usec = 0;

    return utimes(path, t);
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
