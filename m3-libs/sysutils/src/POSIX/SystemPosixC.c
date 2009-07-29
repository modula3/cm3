/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#define _FILE_OFFSET_BITS 64

#ifdef __INTERIX
#ifndef _ALL_SOURCE
#define _ALL_SOURCE
#endif
#ifndef _REENTRANT
#define _REENTRANT
#endif
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <stddef.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <dirent.h>
#include <grp.h>
#include <netdb.h>
#include <pthread.h>
#include <unistd.h>
#include <pwd.h>
#define ZeroMemory(a,b) (memset((a), 0, (b)))
#ifdef __INTERIX
#include <utime.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef ptrdiff_t INTEGER;
typedef INTEGER m3_pid_t;

m3_pid_t System__waitpid(m3_pid_t pid, int* status, int options)
{
    return waitpid(pid, status, options);
}

#ifdef __cplusplus
}
#endif
