/* Copyright (C) 1989, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */

/*
Modula-3 Time.T is LONGREAL aka double counting seconds.
We use gettimeofday() which returns seconds and microseconds.
*/

#include "m3unix.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef double T;

#define Now Time__Now
#define FromUtime TimePosix__FromUtime
#define ToUtime TimePosix__ToUtime
#define ComputeGrain Time__ComputeGrain

#define M (1000000)

#if defined(__STDC__) || defined(__cplusplus)
#define ANSI(x) x
#define KR(x) /* nothing */
#else
#define ANSI(x) /* nothing */
#define KR(x) x
#define const /* nothing */
#endif

T Now(ANSI(void));
timeval_t ToUtime(ANSI(T));
T FromUtime(ANSI(const timeval_t*));

T Now(ANSI(void))
{
    timeval_t tv;
    int i;

    i = gettimeofday(&tv, NULL);
    assert(i == 0);

    return FromUtime(&tv);
}

timeval_t ToUtime(ANSI(T) n)
    KR(T n;)
{
    timeval_t tv;
 
    tv.tv_sec = (time_t)n;
    tv.tv_usec = ((((INT64)n) * M) % M);

    return tv;
}

T FromUtime(ANSI(const timeval_t*) tv)
    KR(const timeval_t* tv;)
{
    return ((T)tv->tv_sec) + ((T)tv->tv_usec) / (T)M;
}

static T ComputeGrainOnce()
{
  /* Determine value of "Grain" experimentally.  Note that
     this will fail if this thread is descheduled for a tick during the
     loop below. Omitting volatile leads to the result is 0 on Cygwin if optimized. */
    volatile T t0 = Now();
    while (1)
    {
        volatile T t1 = Now();
        if (t1 != t0)
            return (t1 - t0);
    }
}

T ComputeGrain()
{
/* I have seen the value vary so let's go for a
few times in a row instead of just one or two.
Doing four checks always hangs on Cygwin, odd. */
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

#if 1

int main()
{
    T grain = ComputeGrain();

    printf("%f\n", grain);

    return 0;
}

#endif
