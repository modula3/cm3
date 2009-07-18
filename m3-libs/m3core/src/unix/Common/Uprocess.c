/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

m3_pid_t Uprocess__getpid(void)
{
#ifdef _WIN32
    return _getpid();
#else
    return getpid();
#endif
}

#ifdef __cplusplus
}
#endif
