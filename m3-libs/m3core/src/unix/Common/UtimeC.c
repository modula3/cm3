#include "m3unix.h"
#include <time.h>
#include <assert.h>
#include <sys/time.h>
typedef struct itimerval itimerval_t;

#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__)
#define M3BSD
#endif

/*
wrap up global variables in functions until something else is done
*/

#ifndef M3BSD

time_t m3_get_timezone(void)
{
#ifdef __CYGWIN__
    return _timezone;
#else
    return timezone;
#endif
}

/* Are these correct? */
#if !defined(__hpux) && !defined(__linux) && 0

time_t m3_get_altzone(void)
{
#ifdef __CYGWIN__
    /* Is this correct? */
    return m3_get_timezone();
#else
    return altzone;
#endif
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

const char* m3_get_tzname(unsigned a)
{
    assert((a == 0) || (a == 1));
    return tzname[a & 1];
}

#endif /* M3BSD */

/*
Cygwin setitimer(other than ITIMER_REAL) always fails.
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
