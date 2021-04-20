/* Copyright (C) 1989, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */

/*
Modula-3 Time.T is LONGREAL aka double counting seconds.
We use gettimeofday() which returns seconds and microseconds.
*/

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#undef THOUSAND /* Support concatenating multiple .c files. */
#define THOUSAND ((UINT64)1000)
#define MILLION (THOUSAND * THOUSAND)
#define BILLION (THOUSAND * MILLION)

#ifdef _TIME64_T
/* OSF/1 5.1 in particular */
typedef struct timeval64 m3_timeval_t;
#define m3_gettimeofday gettimeofday64
#else
typedef struct timeval m3_timeval_t;
#define m3_gettimeofday gettimeofday
#endif

static
LONGREAL/*Time.T*/
__cdecl
TimePosix__FromMicrotime(const m3_timeval_t* tv)
/* see also TimePosix__ToUtime */
{
    return ((LONGREAL)tv->tv_sec) + ((LONGREAL)tv->tv_usec) / (LONGREAL)MILLION;
}

LONGREAL/*Time.T*/
__cdecl
TimePosix__Now(void)
{
    m3_timeval_t tv;
    int i;

    ZERO_MEMORY(tv);
    i = m3_gettimeofday(&tv, NULL);
    assert(i == 0);
    return TimePosix__FromMicrotime(&tv);
}

#if defined(__osf__) /* defined(CLOCK_HIGHRES) || defined(CLOCK_REALTIME) */

static
LONGREAL/*Time.T*/
__cdecl
TimePosix__FromNanotime(const struct timespec* tv)
{
    return ((LONGREAL)tv->tv_sec) + ((LONGREAL)tv->tv_nsec) / (LONGREAL)BILLION;
}

static
LONGREAL/*Time.T*/
__cdecl
ComputeGrainViaClockGetRes(void)
{
    struct timespec res = { 0 };
    int i = -1;
#if defined(CLOCK_HIGHRES)
    i = clock_getres(CLOCK_HIGHRES, &res);
#elif defined(CLOCK_REALTIME)
    i = clock_getres(CLOCK_REALTIME, &res);
#else
#error no CLOCK_HIGHRES or CLOCK_REALTIME
#endif
    assert(i == 0);
    return TimePosix__FromNanotime(&res);
}

#else

static
LONGREAL/*Time.T*/
ComputeGrainViaSamplingOnce(void)
{
    /* Determine value of "Grain" experimentally.  Note that
     * this will fail if this thread is descheduled for a tick during the
     * loop below. Omitting volatile leads to the result is 0 on Cygwin if optimized.
     */
    volatile LONGREAL t0 = TimePosix__Now();
    while (1)
    {
        volatile LONGREAL t1 = TimePosix__Now();
        if (t1 != t0)
            return (t1 - t0);
    }
}

static
LONGREAL/*Time.T*/
__cdecl
ComputeGrainViaSampling(void)
{
    /* Compute a few and then takes the smallest value. */
    LONGREAL a = ComputeGrainViaSamplingOnce();
    LONGREAL b = ComputeGrainViaSamplingOnce();
    LONGREAL c = ComputeGrainViaSamplingOnce();
    a = (b < a ? b : a);
    a = (c < a ? c : a);
    return a;
}

#endif

LONGREAL/*Time.T*/
__cdecl
TimePosix__ComputeGrain(void)
{
/* ComputeGrainViaSampling has a strong propensity to hang on OSF/1.
   I don't know why. On other platforms, ComputeGrainViaClockGetRes and
   ComputeGrainViaSampling vary by a lot.
*/
#if defined(__osf__) /* defined(CLOCK_HIGHRES) || defined(CLOCK_REALTIME) */
  return ComputeGrainViaClockGetRes();
#else
  return ComputeGrainViaSampling();
#endif
}

#ifdef __cplusplus
} /* extern C */
#endif
