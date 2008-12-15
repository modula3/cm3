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

#ifdef _WIN64
typedef __int64 INTEGER;
#else
typedef long INTEGER;
#endif

typedef struct flock flock_t;
#define FALSE 0
#define TRUE 1

INTEGER m3_RegularFileLock(int fd)
{
    flock_t lock;
    int err;

    memset(&lock, 0, sizeof(lock));
    lock.l_type = F_WRLCK;
    lock.l_whence = L_SET;

    if (fcntl(fd, F_SETLK, &lock) < 0)
    {
        err = errno;
        if (err == EACCES || err == EAGAIN)
            return FALSE;
        return -1;
    }
    return TRUE;
}

INTEGER m3_RegularFileUnlock(int fd)
{
    flock_t lock;

    memset(&lock, 0, sizeof(lock));
    lock.l_type = F_UNLCK;
    lock.l_whence = L_SET;

    if (fcntl(fd, F_SETLK, &lock) < 0)
    {
        return -1;
    }
}
