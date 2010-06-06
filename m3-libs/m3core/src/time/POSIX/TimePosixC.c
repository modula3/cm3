/* Copyright (C) 1989, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */

/*
Modula-3 Time.T is LONGREAL aka double counting seconds.
We use gettimeofday() which returns seconds and microseconds.
*/

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

#define MILLION (1000 * 1000)

static
LONGREAL/*Time.T*/
__cdecl
TimePosix__FromUtime(const struct timeval* tv)
/* see also TimePosix__ToUtime */
{
    return ((LONGREAL)tv->tv_sec) + ((LONGREAL)tv->tv_usec) / (LONGREAL)MILLION;
}

LONGREAL/*Time.T*/
__cdecl
TimePosix__Now(void)
{
    struct timeval tv;
    int i = { 0 };

    ZERO_MEMORY(tv);
    i = gettimeofday(&tv, NULL);
    assert(i == 0);

    return TimePosix__FromUtime(&tv);
}

static
LONGREAL/*Time.T*/
ComputeGrainOnce(void)
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

LONGREAL/*Time.T*/
__cdecl
TimePosix__ComputeGrain(void)
{
/* I have seen the value vary so let's go for a
 * few times in a row instead of just one or two.
 * Doing four checks always hangs on Cygwin, odd.
 */
    while (1)
    {
        LONGREAL a = ComputeGrainOnce();
        LONGREAL b = ComputeGrainOnce();
#ifdef __osf__ /* Three calls take a long time to coincide. */
        if (a == b)
            return a;
#else
        LONGREAL c = ComputeGrainOnce();
        if (a == b && a == c)
            return a;
#endif
    }
}

#ifdef __cplusplus
} /* extern C */
#endif

#if 0

int main()
{
    LONGREAL grain = ComputeGrain();

    printf("%f\n", grain);

    return 0;
}

#endif
