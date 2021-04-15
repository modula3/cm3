/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/*
Writing part of libm3/os/POSIX/FilePosix.m3/RegularFileLock, RegularFileUnlock in C
saves us from having to declare struct flock, which is gnarled up in #ifdefs.

see http://www.opengroup.org/onlinepubs/009695399/functions/fcntl.html
*/

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

#define FALSE 0
#define TRUE 1

INTEGER FilePosixC__RegularFileLock(int fd)
{
    struct flock lock;
    int err;

    ZeroMemory(&lock, sizeof(lock));
    lock.l_type = F_WRLCK;
    lock.l_whence = SEEK_SET;

    if (fcntl(fd, F_SETLK, &lock) < 0)
    {
        err = errno;
        if (err == EACCES || err == EAGAIN)
            return FALSE;
        return -1;
    }
    return TRUE;
}

INTEGER FilePosixC__RegularFileUnlock(int fd)
{
    struct flock lock;

    ZeroMemory(&lock, sizeof(lock));
    lock.l_type = F_UNLCK;
    lock.l_whence = SEEK_SET;

    return fcntl(fd, F_SETLK, &lock);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
