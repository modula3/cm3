#include "m3unix.h"
#include <time.h>
#include <assert.h>
#ifdef __CYGWIN__ /* temporary */
#include <sys/time.h>
typedef struct itimerval itimerval_t;
#endif

/*
wrap up global variables in functions until something else is done
*/

#ifndef __OpenBSD__

time_t m3_get_timezone(void)
{
#ifdef __CYGWIN__
    return _timezone;
#else
    return timezone;
#endif
}

/* Are these correct? */
#if !defined(__CYGWIN__) && !defined(__hpux) && !defined(__linux)

time_t m3_get_altzone(void)
{
    return altzone;
}

#endif

int m3_get_daylight(void)
{
#ifdef __CYGWIN__
    return _daylight;
#else
    return daylight;
#endif
}

#endif /* __OpenBSD__ */

const char* m3_get_tzname(unsigned a)
{
    assert((a == 0) || (a == 1));
    return tzname[a & 1];
}

#ifdef __CYGWIN__ /* temporary */

/*
Cygwin setitmer(other than ITIMER_REAL) always fails.
We need it to succeed, though it need not do anything else.
*/

int m3_setitimer (int Timer, const itimerval_t* NewValue, itimerval_t* OldValue)
{
#ifdef __CYGWIN__
    if (Timer != ITIMER_REAL)
    {
        return 0;
    }
#endif
    return setitimer(Timer, NewValue, OldValue);
}

#endif
