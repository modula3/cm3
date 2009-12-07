/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3core.h"

#ifndef _WIN32

typedef struct passwd passwd_t;
#define M3MODULE Upwd

M3WRAP1(passwd_t*, getpwuid, m3_uid_t)
M3WRAP1(passwd_t*, getpwnam, char*)

#endif
