/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3core.h"

#ifndef _WIN32

#define M3MODULE Usignal
M3WRAP2(int, kill, m3_pid_t, int)

#endif
