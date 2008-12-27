#include "m3unix.h"
#include <time.h>

/*
wrap up global variables in functions until something else is done
*/

const char* get_tzname(unsigned a);

#ifndef __OpenBSD__

time_t get_timezone(void);
time_t get_altzone(void);
int get_daylight(void);

time_t get_timezone(void)
{
#ifdef __CYGWIN__
    return _timezone;
#else
    return timezone;
#endif
}

/* Are these correct? */
#if !defined(__CYGWIN__) && !defined(__hpux) && !defined(__linux)

time_t get_altzone(void)
{
    return altzone;
}

#endif

int get_daylight(void)
{
#ifdef __CYGWIN__
    return _daylight;
#else
    return daylight;
#endif
}

#endif /* __OpenBSD__ */

const char* get_tzname(unsigned a)
{
    return tzname[a];
}
