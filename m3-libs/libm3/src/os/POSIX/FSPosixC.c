/* Copyright (C) 1992, Digital Equipment Corporation. */
/* All rights reserved. */
/* See the file COPYRIGHT for a full description. */

#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <dirent.h>
typedef struct dirent dirent_t;

const char* FSPosixC__readdir_name(DIR* dir)
{
    dirent_t* e;

    if (dir && (e = readdir(dir)))
        return e->d_name;

    return 0;
}
