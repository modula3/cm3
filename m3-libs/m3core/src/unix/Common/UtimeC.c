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
#ifndef _WIN32
    /* Basically no 32bit system has a 64bit time_t, unfortunate. */
    M3_STATIC_ASSERT(sizeof(time_t) <= sizeof(void*));

    /* verify timeval (microtime) contains just the two fields we know about, in either order */
#if defined(__APPLE__) && defined(__LP64__)
/* AMD64_DARWIN has:
struct timeval
{
 int64 tv_sec;
 int32 tv_usec;
 4 bytes of padding
}; I do not see much we can do about this. We use copying wrappers and we
   want to be sure they are copying the entire struct.
*/
    { typedef struct timeval T1;
      typedef struct { time_t tv_sec; suseconds_t tv_usec; } T2;
      M3_STATIC_ASSERT(M3_FIELD_SIZE(T1, tv_sec) == 8);
      M3_STATIC_ASSERT(M3_FIELD_SIZE(T1, tv_usec) == 4);
      M3_STATIC_ASSERT(M3_FIELD_SIZE(T2, tv_sec) == 8);
      M3_STATIC_ASSERT(M3_FIELD_SIZE(T2, tv_usec) == 4);
      M3_STATIC_ASSERT(sizeof(T1) == 16);
      M3_STATIC_ASSERT(sizeof(T2) == 16);
    }
#else
    { typedef struct timeval T;
      M3_STATIC_ASSERT(sizeof(T) == M3_FIELD_SIZE(T, tv_sec) + M3_FIELD_SIZE(T, tv_usec)); }
#endif

    /* verify timezone is exactly as we expect */
    { typedef m3_timezone_t M;
      typedef struct timezone T;
      M3_STATIC_ASSERT(sizeof(T) == sizeof(M));
      M3_STATIC_ASSERT(sizeof(T) == 8);
      M3_STATIC_ASSERT(offsetof(T, tz_minuteswest) == 0);
      M3_STATIC_ASSERT(offsetof(T, tz_dsttime) == 4);
      M3_STATIC_ASSERT(offsetof(M, minuteswest) == 0);
      M3_STATIC_ASSERT(offsetof(M, dsttime) == 4);
    }
#endif
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

#endif /* cygwin | sun */

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

#ifndef _WIN32

static void timeval_to_m3(const struct timeval* t, m3_timeval_t* m)
{
    if (!m) return;
    assert(t);
    m->sec = t->tv_sec;
    m->usec = t->tv_usec;
}

int
__cdecl
Utime__gettimeofday(m3_timeval_t* m3t)
{
    struct timeval t;
    /* null is not valid here; gcc warns */
    int r = gettimeofday(&t, 0);
    timeval_to_m3(&t, m3t);
    return r;
}

m3_time_t
__cdecl
Utime__time(m3_time_t* tloc)
{
    time_t b = tloc ? (time_t)*tloc : 0;
    time_t a = time(tloc ? &b : 0);
    if (tloc) *tloc = b;
    return a;
}

m3_time_t
__cdecl
Utime__mktime(struct tm* tm)
{
    return mktime(tm);
}

char*
__cdecl
Utime__ctime(const m3_time_t* m)
{
    time_t t = m ? (time_t)*m : 0;
    return ctime(m ? &t : 0);
}

struct tm*
__cdecl
Utime__localtime(const m3_time_t* m)
{
    time_t t = m ? (time_t)*m : 0;
    return localtime(m ? &t : 0);
}

struct tm*
__cdecl
Utime__gmtime(const m3_time_t* m)
{
    time_t t = m ? (time_t)*m : 0;
    return gmtime(m ? &t : 0);
}

struct tm*
__cdecl
Utime__localtime_r(const m3_time_t* m3t, struct tm* result)
{
    time_t t = m3t ? *m3t : 0;
    return localtime_r(m3t ? &t : 0, result);
}

struct tm*
__cdecl
Utime__gmtime_r(const m3_time_t* m3t, struct tm* result)
{
    time_t t = m3t ? *m3t : 0;
    return gmtime_r(m3t ? &t : 0, result);
}

#endif /* _WIN32 */

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
