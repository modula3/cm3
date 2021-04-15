/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifndef __cplusplus
#define KR_headers
#endif
#define IEEE_8087
#undef IEEE_MC68k

#define MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) CConvert__Acquire(n)
#define FREE_DTOA_LOCK(n) CConvert__Release(n)

#ifdef __cplusplus
extern "C" {
#endif

void CConvert__Acquire(WORD_T);
void CConvert__Release(WORD_T);

#ifdef __cplusplus
} /* extern C */
#endif

#include "dtoa.h"
