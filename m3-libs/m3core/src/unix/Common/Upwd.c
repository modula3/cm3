/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

typedef struct passwd passwd_t;

passwd_t* Upwd__getpwuid(m3_uid_t uid)
{
    return getpwuid(uid);
}

passwd_t* Upwd__getpwnam(char* name)
{
    return getpwnam(name);
}

#endif


#ifdef __cplusplus
}
#endif
