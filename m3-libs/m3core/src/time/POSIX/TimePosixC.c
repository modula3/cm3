/* Copyright (C) 1989, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */

/*
Modula-3 Time.T is LONGREAL aka double counting seconds.
We use gettimeofday() which returns seconds and microseconds.
*/

#include "m3core.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#if 0

typedef double T;

#define Now Time__Now
#define FromUtime TimePosix__FromUtime
#define ToUtime TimePosix__ToUtime
#define ComputeGrain Time__ComputeGrain

#define MILLION (1000 * 1000)

#if defined(__STDC__) || defined(__cplusplus)
#define ANSI(x) x
#define KR(x) /* nothing */
#else
#define ANSI(x) /* nothing */
#define KR(x) x
#define const /* nothing */
#endif

T __cdecl Now(ANSI(void));
struct timeval __cdecl ToUtime(ANSI(T));
T __cdecl FromUtime(ANSI(const struct timeval*));

T __cdecl Now(ANSI(void))
{
    struct timeval tv = { 0 };
    int i;

    i = gettimeofday(&tv, NULL);
    assert(i == 0);

    return FromUtime(&tv);
}

struct timeval __cdecl ToUtime(ANSI(T) n)
    KR(T n;)
{
    struct timeval tv = { 0 };
 
    tv.tv_sec = (time_t)n;
    tv.tv_usec = ((((INT64)n) * MILLION) % MILLION);

    return tv;
}

T __cdecl FromUtime(ANSI(const struct timeval*) tv)
    KR(const struct timeval* tv;)
{
    return ((T)tv->tv_sec) + ((T)tv->tv_usec) / (T)MILLION;
}

static T ComputeGrainOnce(ANSI(void))
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

T __cdecl ComputeGrain(ANSI(void))
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

#endif

NanosecondsStruct_t*
__cdecl
TimePosix__FloatSecondsToNanosecondsStruct(FloatSeconds m3time,
                                           NanosecondsStruct_t* nanotime)
{
/* fairly direct conversion from the Modula-3 ThreadPThread.ToNTime */
    nanotime->tv_sec = TRUNC(m3time);
    nanotime->tv_nsec = ROUND((m3time - FLOAT(nanotime->tv_sec, LONGREAL)) * 1.0E9);
    return nanotime;
}

MicrosecondsStruct_t*
__cdecl
TimePosix__FloatSecondsToMicrosecondsStruct(FloatSeconds m3time,
                                            MicrosecondsStruct_t* microtime)
{
/* fairly direct conversion from the Modula-3 ThreadPThread.UTimeFromTime */
    LONGINT sec = FLOOR(m3time);
    microtime->tv_sec = sec;
    microtime->tv_usec = FLOOR(1.0E6 * (m3time - FLOAT(sec, LONGREAL)));
    return microtime;
}

#ifdef __cplusplus
} /* extern C */
#endif

#if 0

int main()
{
    T grain = ComputeGrain();

    printf("%f\n", grain);

    return 0;
}

#endif
