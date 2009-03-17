/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/*
Writing part of libm3/os/POSIX/FilePosix.m3/RegularFileLock, RegularFileUnlock in C
saves us from having to declare struct flock, which is gnarled up in #ifdefs.

see http://www.opengroup.org/onlinepubs/009695399/functions/fcntl.html
*/

#define _FILE_OFFSET_BITS 64

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stddef.h>

typedef ptrdiff_t INTEGER;
typedef struct flock flock_t;
#define FALSE 0
#define TRUE 1
#define ZeroMemory(a,b) (memset((a), 0, (b)))

INTEGER RegularFileLockC(int fd)
{
    flock_t lock;
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

INTEGER RegularFileUnlockC(int fd)
{
    flock_t lock;

    ZeroMemory(&lock, sizeof(lock));
    lock.l_type = F_UNLCK;
    lock.l_whence = SEEK_SET;

    return fcntl(fd, F_SETLK, &lock);
}
