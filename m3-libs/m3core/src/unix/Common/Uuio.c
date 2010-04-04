/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifdef _MSC_VER
#undef _DLL
#ifndef _MT
#define _MT
#endif
#endif

#include "m3core.h"

#define M3MODULE Uuio
M3WRAP3_(ssize_t, read, int, void*, size_t)
M3WRAP3_(ssize_t, write, int, const void*, size_t)
