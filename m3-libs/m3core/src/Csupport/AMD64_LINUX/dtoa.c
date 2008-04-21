/* Copyright (C) 1992, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

/* Last modified on Wed Apr  5 08:44:12 PDT 1995 by kalsow    */
/*      modified on Mon Oct 12 13:50:54 PDT 1992 by muller    */

/*
This file is the same or almost the same across all platforms.
Let's factor it.
*/

#ifndef IEEE_8087
#define IEEE_8087
#endif

#define Long int

#define MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) RTOS__LockHeap()
#define FREE_DTOA_LOCK(n) RTOS__UnlockHeap()

#include "dtoa.h"
