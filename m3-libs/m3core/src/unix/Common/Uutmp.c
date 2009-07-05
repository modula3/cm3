/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

char* Uutmp__getlogin(void)
{
    return getlogin();
}

#endif

#ifdef __cplusplus
}
#endif
