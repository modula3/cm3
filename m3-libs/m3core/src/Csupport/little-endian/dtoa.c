/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#ifndef __cplusplus
#define KR_headers
#endif
#define IEEE_8087
#undef IEEE_MC68k

#define MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) CConvert__Acquire(n)
#define FREE_DTOA_LOCK(n) CConvert__Release(n)

#if defined(__STDC__) || defined(__cplusplus)

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

void CConvert__Acquire(size_t);
void CConvert__Release(size_t);

#ifdef __cplusplus
} /* extern C */
#endif

#endif /* STDC || C++ */

#include "dtoa.h"
