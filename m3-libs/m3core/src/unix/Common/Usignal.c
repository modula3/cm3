/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

int Usignal__kill(pid_t pid, int sig)
{
    return kill(pid, sig);
}

#endif

#ifdef __cplusplus
}
#endif
