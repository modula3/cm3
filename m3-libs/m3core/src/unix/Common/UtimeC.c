#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

/* to address libtool: file: UtimeC.o has no symbols */
void UtimeC__dummy(void) { }

#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#define M3BSD
#endif

/*
wrap up global variables in functions until something else is done
*/

#ifndef M3BSD

time_t Utime__get_timezone(void)
{
#ifdef __CYGWIN__
    return _timezone;
#else
    return timezone;
#endif
}

#if defined(__CYGWIN__) || defined(__sun) || defined(__hpux) || defined(__INTERIX)

time_t Utime__get_altzone(void)
{
#ifdef __sun
    return altzone;
#else
    return Utime__get_timezone(); /* ? */
#endif
}

#endif /* cygwin | sun */

int Utime__get_daylight(void)
{
#ifdef __CYGWIN__
    return _daylight;
#else
    return daylight;
#endif
}

const char* Utime__get_tzname(unsigned a)
{
    assert((a == 0) || (a == 1));
    return tzname[a & 1];
}

#endif /* M3BSD */

int Utime__gettimeofday (timeval_t* t, timezone_t* z)
{
    return gettimeofday(t, z);
}

int Utime__settimeofday (timeval_t* t, timezone_t* z)
{
    return settimeofday(t, z);
}

int Utime__getitimer(int which, itimerval_t* value)
{
    return getitimer(which, value);
}

time_t Utime__time(time_t* tloc)
{
    return time(tloc);
}

time_t Utime__mktime(tm_t* tm)
{
    return mktime(tm);
}

char* Utime__ctime(time_t* clock)
{
    return ctime(clock);
}

tm_t* Utime__localtime(time_t* clock)
{
    return localtime(clock);
}

tm_t* Utime__gmtime(time_t* clock)
{
    return gmtime(clock);
}

char* Utime__ctime_r(time_t* clock, char* buffer)
{
    return ctime_r(clock, buffer);
}

tm_t* Utime__localtime_r(time_t* clock, tm_t* result)
{
    return localtime_r(clock, result);
}

tm_t* Utime__gmtime_r(time_t* clock, tm_t* result)
{
    return gmtime_r(clock, result);
}

int Utime__setitimer(int which, itimerval_t* new_value, itimerval_t* old_value)
{
    return setitimer(which, new_value, old_value);
}

int Utime__nanosleep(timespec_t* req, timespec_t* rem)
{
    return nanosleep(req, rem);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
