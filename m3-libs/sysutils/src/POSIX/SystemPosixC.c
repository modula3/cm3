/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include <sys/wait.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef ptrdiff_t m3_pid_t;

m3_pid_t System__waitpid(m3_pid_t pid, int* status, int options)
{
    return waitpid(pid, status, options);
}

#ifdef __cplusplus
}
#endif
