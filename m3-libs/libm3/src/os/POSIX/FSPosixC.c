/* Copyright (C) 1992, Digital Equipment Corporation. */
/* All rights reserved. */
/* See the file COPYRIGHT for a full description. */

#include "m3unix.h"

#ifdef __cplusplus
extern "C"
{           
#endif

typedef struct dirent dirent_t;

const char* FSPosixC__readdir_name(DIR* dir)
{
    dirent_t* e;

    if (dir && (e = readdir(dir)))
        return e->d_name;

    return 0;
}

int FSPosixC__SetModificationTime(const char* path, INTEGER updated, INTEGER accessed)
{
#ifdef __INTERIX
    utimbuf_t t;

    memset(&t, 0, sizeof(t));
    t.actime = accessed;
    t.modtime = updated;

    return utime(path, &t);
#else
    timeval_t t[2];

    memset(&t, 0, sizeof(t));
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
