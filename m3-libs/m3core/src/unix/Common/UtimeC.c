#include "m3unix.h"
#include <time.h>
#include <assert.h>
#include <sys/time.h>
typedef struct itimerval itimerval_t;

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

#if defined(__CYGWIN__) || defined(__sun) || defined(__hpux)

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
