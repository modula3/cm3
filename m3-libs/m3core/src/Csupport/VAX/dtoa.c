/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

/* Last modified on Tue Feb 11 14:23:50 PST 1992 by muller                   */

#ifndef KR_headers
#define KR_headers
#endif

#ifndef VAX
#define VAX
#endif

#define MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) RTOS__LockHeap()
#define FREE_DTOA_LOCK(n) RTOS__UnlockHeap()

#include "dtoa.h"
