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

/* WORD_T/INTEGER are always exactly the same size as a pointer.
 * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers.
 */
#if __INITIAL_POINTER_SIZE == 64
typedef unsigned __int64 WORD_T;
#else
typedef size_t WORD_T;
#endif

#ifdef __cplusplus
extern "C" {
#endif

void CConvert__Acquire(WORD_T);
void CConvert__Release(WORD_T);

#ifdef __cplusplus
} /* extern C */
#endif

#endif /* STDC || C++ */

#include "dtoa.h"
