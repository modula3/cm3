/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

/* Last modified on Mon Oct 12 13:50:54 PDT 1992 by muller                   */

#ifndef IEEE_8087
#define IEEE_8087
#endif

#define MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) RTOS__LockHeap()
#define FREE_DTOA_LOCK(n) RTOS__UnlockHeap()

#include "dtoa.h"
