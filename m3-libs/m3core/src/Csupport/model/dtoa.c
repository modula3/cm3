/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

/* Last modified on Tue Feb 11 14:23:53 PST 1992 by muller                   */

#ifndef IEEE_8087
#define IEEE_8087
#endif

/* Enable this on machines with 32-bit int but 64-bit long: Alpha, x86_64 */
#ifdef 0
#define Long int
#endif

#define MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) RTOS__LockHeap()
#define FREE_DTOA_LOCK(n) RTOS__UnlockHeap()

#include "dtoa.h"
