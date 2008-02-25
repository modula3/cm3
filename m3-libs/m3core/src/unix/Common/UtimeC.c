#include <time.h>

/*
wrap up global variables in functions until something else is done
*/

time_t get_timezone(void);
time_t get_altzone(void);
int get_daylight(void);
const char* get_tzname(unsigned a);

time_t get_timezone(void)
{
#ifdef __CYGWIN__
    return _timezone;
#else
    return timezone;
#endif
}

#if !defined(__CYGWIN__) && !defined(__hpux) /* is hpux correct? */

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

const char* get_tzname(unsigned a)
{
    return tzname[a];
}
