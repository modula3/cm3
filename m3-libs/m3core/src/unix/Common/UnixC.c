/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/*
Writing libm3/os/POSIX/FilePosix.m3/RegularFileLock, RegularFileUnlock in C
saves us from having to declare struct flock, which is gnarled up in #ifdefs.
*/

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

typedef struct flock flock_t;
typedef size_t BOOLEAN;
#define FALSE 0
#define TRUE 1

void OSErrorPosix__Raise(void);

BOOLEAN m3_RegularFileLock(int fd)
{
    flock_t lock;
    int err;

    memset(&lock, 0, sizeof(lock));
    lock.l_type = F_WRLCK;
    lock.l_whence = L_SET;

    if (fcntl(fd, F_SETLK, &flock) < 0)
    {
        err = errno;
        if (err == EACCESS || err == EAGAIN)
            return FALSE;
        OSErrorPosix__Raise();
    }
    return TRUE;
}

void m3_RegularFileUnlock(int fd)
{
    flock_t lock;

    memset(&lock, 0, sizeof(lock));
    lock.l_type = F_UNLCK;
    lock.l_whence = L_SET;

    if (fcntl(fd, F_SETLK, &flock) < 0)
    {
        OSErrorPosix__Raise();
    }
}
