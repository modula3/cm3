/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifdef _MSC_VER
#pragma optimize("gt", on)
#pragma optimize("y", off)
#undef _DLL
#ifndef _MT
#define _MT
#endif
#endif

#include "m3core.h"

#define M3MODULE Uprocess
M3WRAP0_(m3_pid_t, getpid)
