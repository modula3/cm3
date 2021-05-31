/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Uuio
M3WRAP3_(ssize_t, read, int, void*, size_t)
M3WRAP3_(ssize_t, write, int, const void*, size_t)
