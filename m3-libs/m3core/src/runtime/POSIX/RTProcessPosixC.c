/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include <sys/types.h>
#include <time.h>
#include <sys/resource.h>
#include <assert.h>
typedef struct timeval timeval_t;
typedef struct rusage rusage_t;

#ifdef __cplusplus
extern "C" {
#endif

#define TimevalSecs RTProcessPosix__TimevalSecs

float TimevalSecs(timeval_t* t)
/* Return the number of seconds represented by "t" as a floating-
   point number. */
{
    return (((float) t->tv_sec) + (((float) t->tv_usec) / 1.0e6));
}

float RTProcessPosix__TimeUsed(void)
{
    rusage_t usage;
    int ret = getrusage(RUSAGE_SELF, &usage);
    assert(ret == 0);
    return TimevalSecs(&usage.ru_utime) + TimevalSecs(&usage.ru_stime);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
