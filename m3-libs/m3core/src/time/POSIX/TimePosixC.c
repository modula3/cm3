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

typedef double T;

#define MILLION (1000 * 1000)

struct timeval
__cdecl
TimePosix__ToUtime(T t)
{
    struct timeval tv;
    double n = { 0 };

    ZERO_MEMORY(tv); 
    tv.tv_usec = modf(t, &n) * MILLION;
    tv.tv_sec = n;
    return tv;
}

T
__cdecl
TimePosix__FromUtime(const struct timeval* tv)
{
    return ((T)tv->tv_sec) + ((T)tv->tv_usec) / (T)MILLION;
}

T
__cdecl
Time__Now(void)
{
    struct timeval tv;
    int i = { 0 };

    ZERO_MEMORY(tv);
    i = gettimeofday(&tv, NULL);
    assert(i == 0);

    return TimePosix__FromUtime(&tv);
}

static
T
ComputeGrainOnce(void)
{
  /* Determine value of "Grain" experimentally.  Note that
   * this will fail if this thread is descheduled for a tick during the
   * loop below. Omitting volatile leads to the result is 0 on Cygwin if optimized.
   */
    volatile T t0 = Time__Now();
    while (1)
    {
        volatile T t1 = Time__Now();
        if (t1 != t0)
            return (t1 - t0);
    }
}

T
__cdecl
Time__ComputeGrain(void)
{
/* I have seen the value vary so let's go for a
 * few times in a row instead of just one or two.
 * Doing four checks always hangs on Cygwin, odd.
 */
    while (1)
    {
        T a = ComputeGrainOnce();
        T b = ComputeGrainOnce();
        T c = ComputeGrainOnce();
        if (a == b && a == c)
            return a;
    }
}

#ifdef __cplusplus
} /* extern C */
#endif

#if 0

#include <stdio.h>

int main()
{
    T grain = ComputeGrain();

    printf("%f\n", grain);

    return 0;
}

#endif
