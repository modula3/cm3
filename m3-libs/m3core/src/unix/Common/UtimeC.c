#ifdef _MSC_VER
#undef _DLL
#ifndef _MT
#define _MT
#endif
#endif

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

void
__cdecl
Utime__Assertions(void)
{
    /* We won't ever be truncating. */
    M3_STATIC_ASSERT(sizeof(time_t) <= sizeof(INT64));
}

#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#define M3BSD
#endif

/*
wrap up global variables in functions until something else is done
*/

#if !defined(M3BSD) && !defined(_WIN32)

m3_time_t
__cdecl
Utime__get_timezone(void)
{
#ifdef __CYGWIN__
    return _timezone;
#else
    return timezone;
#endif
}

#if defined(__CYGWIN__) || defined(__sun) || defined(__hpux) || defined(__INTERIX)

m3_time_t
__cdecl
Utime__get_altzone(void)
{
#ifdef __sun
    return altzone;
#else
    /* based on Python's timemodule */
    return Utime__get_timezone() - 3600;
#endif
}

#endif /* cygwin | sun | hpux | interix */

int
__cdecl
Utime__get_daylight(void)
{
#ifdef __CYGWIN__
    return _daylight;
#else
    return daylight;
#endif
}

const char*
__cdecl
Utime__get_tzname(unsigned a)
{
    assert((a == 0) || (a == 1));
    return tzname[a & 1];
}

#endif /* M3BSD, WIN32 */

m3_time_t
__cdecl
Utime__time(m3_time_t* tloc)
{
    time_t b = tloc ? (time_t)*tloc : 0;
    time_t a = time(tloc ? &b : 0);
    if (tloc) *tloc = b;
    return a;
}

char*
__cdecl
Utime__ctime(const m3_time_t* m)
{
    time_t t = m ? (time_t)*m : 0;
    return ctime(m ? &t : 0);
}

void
__cdecl
Utime__tzset(void)
{
#ifdef _WIN32
    _tzset();
#else
    tzset();
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
