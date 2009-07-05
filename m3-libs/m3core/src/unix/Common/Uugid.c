/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

gid_t Uugid__geteuid(void)
{
    return geteuid();
}

gid_t Uugid__getegid(void)
{
    return getegid();
}

#endif


#ifdef __cplusplus
}
#endif
