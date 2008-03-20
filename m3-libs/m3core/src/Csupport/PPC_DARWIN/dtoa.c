/* Copyright according to COPYRIGHT-CMASS. */
/* FIXME: copied from FreeBSD3 target. Probably needs to be changed. */

#ifndef IEEE_MC68k
#define IEEE_MC68k
#endif

#ifdef  IEEE_8087
#undef  IEEE_8087
#endif

#define MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) RTOS__LockHeap()
#define FREE_DTOA_LOCK(n) RTOS__UnlockHeap()

#include "dtoa.h"
