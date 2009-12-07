/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

#define M3MODULE Uugid
M3WRAP0(m3_uid_t, geteuid)
M3WRAP2(int, setreuid, m3_uid_t, m3_uid_t)
M3WRAP0(m3_gid_t, getegid)

#endif

#ifdef __cplusplus
}
#endif
