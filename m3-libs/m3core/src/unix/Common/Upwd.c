/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifndef _WIN32

#define M3MODULE Upwd

M3WRAP1(struct passwd*, getpwuid, m3_uid_t)
M3WRAP1(struct passwd*, getpwnam, char*)

#endif
