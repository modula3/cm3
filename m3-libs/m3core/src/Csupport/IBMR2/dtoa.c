/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

/* Last modified on Tue May 12 15:35:00 PDT 1992 by muller                   */

#ifndef KR_headers
#define KR_headers
#endif

#ifndef IEEE_MC68k
#define IEEE_MC68k
#endif

#define MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) RTOS__LockHeap()
#define FREE_DTOA_LOCK(n) RTOS__UnlockHeap()

#include "dtoa.h"
