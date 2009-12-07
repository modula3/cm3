/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#define M3MODULE Uuio
M3WRAP3_(ssize_t, read, int, void*, size_t)
M3WRAP3_(ssize_t, write, int, const void*, size_t)

#ifdef __cplusplus
}
#endif
