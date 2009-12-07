#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

void Utime__Assertions(void)
{
#ifndef _WIN32
    /* Basically no 32bit system has a 64bit time_t, unfortunate. */
    M3_STATIC_ASSERT(sizeof(time_t) <= sizeof(void*));

    /* verify itimerval contains just the two fields we know about, in either order */
    { typedef itimerval_t T;
      M3_STATIC_ASSERT(sizeof(T) == M3_FIELD_SIZE(T, it_value) + M3_FIELD_SIZE(T, it_interval)); }

    /* verify timespec (nanotime) contains just the two fields we know about, in either order */
    { typedef timespec_T T;
      M3_STATIC_ASSERT(sizeof(T) == M3_FIELD_SIZE(T, tv_sec) + M3_FIELD_SIZE(T, tv_nsec)); }

    /* verify timeval (microtime) contains just the two fields we know about, in either order */
#if defined(__APPLE__) && defined(__x86_64__)
/* AMD64_DARWIN has:
struct timeval
{
 int64 tv_sec;
 int32 tv_usec;
 4 bytes of padding
}; I do not see much we can do about this. We use copying wrappers and we
   want to be sure they are copying the entire struct.
*/
    { typedef timeval_t T1;
      typedef struct { time_t tv_sec; suseconds_t tv_usec; } T2;
      M3_STATIC_ASSERT(M3_FIELD_SIZE(T1, tv_sec) == 8);
      M3_STATIC_ASSERT(M3_FIELD_SIZE(T1, tv_usec) == 4);
      M3_STATIC_ASSERT(M3_FIELD_SIZE(T2, tv_sec) == 8);
      M3_STATIC_ASSERT(M3_FIELD_SIZE(T2, tv_usec) == 4);
      M3_STATIC_ASSERT(sizeof(T1) == 16);
      M3_STATIC_ASSERT(sizeof(T2) == 16);
    }
#else
    { typedef timeval_t T;
      M3_STATIC_ASSERT(sizeof(T) == M3_FIELD_SIZE(T, tv_sec) + M3_FIELD_SIZE(T, tv_usec)); }
#endif

    /* verify timezone is exactly as we expect */
    { typedef m3_timezone_t M;
      typedef timezone_t T;
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

#ifndef M3BSD

m3_time_t Utime__get_timezone(void)
{
#ifdef __CYGWIN__
    return _timezone;
#else
    return timezone;
#endif
}

#if defined(__CYGWIN__) || defined(__sun) || defined(__hpux) || defined(__INTERIX)

m3_time_t Utime__get_altzone(void)
{
#ifdef __sun
    return altzone;
#else
    /* based on Python's timemodule */
    return Utime__get_timezone() - 3600;
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

#ifndef _WIN32

static void timespec_to_m3(const timespec_T* t, m3_timespec_t* m)
{
    if (!m) return;
    assert(t);
    m->sec = t->tv_sec;
    m->nsec = t->tv_nsec;
}

static const timespec_T* timespec_from_m3(timespec_T* t, const m3_timespec_t* m)
{
    if (!m) return 0;
    assert(t);
    t->tv_sec = m->sec;
    t->tv_nsec = m->nsec;
    return t;
}

static void timeval_to_m3(const timeval_t* t, m3_timeval_t* m)
{
    if (!m) return;
    assert(t);
    m->sec = t->tv_sec;
    m->usec = t->tv_usec;
}

static timeval_t* timeval_from_m3(timeval_t* t, const m3_timeval_t* m)
{
    if (!m) return 0;
    assert(t);
    t->tv_sec = m->sec;
    t->tv_usec = m->usec;
    return t;
}

static void itimerval_to_m3(const itimerval_t* t, m3_itimerval_t* m)
{
    if (!m) return;
    assert(t);
    timeval_to_m3(&t->it_interval, &m->interval);
    timeval_to_m3(&t->it_value, &m->value);
}

static itimerval_t* itimerval_from_m3(itimerval_t* t, const m3_itimerval_t* m)
{
    if (!m) return 0;
    assert(t);
    timeval_from_m3(&t->it_interval, &m->interval);
    timeval_from_m3(&t->it_value, &m->value);
    return t;
}

int Utime__gettimeofday(m3_timeval_t* m3t)
{
    timeval_t t;
    /* null is not valid here; gcc warns */
    int r = gettimeofday(&t, 0);
    timeval_to_m3(&t, m3t);
    return r;
}

int Utime__getitimer(int which, m3_itimerval_t* m3t)
{
    itimerval_t t;
    int r = getitimer(which, m3t ? &t : 0);
    itimerval_to_m3(&t, m3t);
    return r;
}

#endif

m3_time_t Utime__time(m3_time_t* tloc)
{
    time_t b = tloc ? *tloc : 0;
    time_t a = time(tloc ? &b : 0);
    if (tloc) *tloc = b;
    return a;
}

m3_time_t Utime__mktime(tm_t* tm)
{
    return mktime(tm);
}

char* Utime__ctime(const m3_time_t* m)
{
    time_t t = m ? *m : 0;
    return ctime(m ? &t : 0);
}

tm_t* Utime__localtime(const m3_time_t* m)
{
    time_t t = m ? *m : 0;
    return localtime(m ? &t : 0);
}

tm_t* Utime__gmtime(const m3_time_t* m)
{
    time_t t = m ? *m : 0;
    return gmtime(m ? &t : 0);
}

#ifndef _WIN32

tm_t* Utime__localtime_r(const m3_time_t* m3t, tm_t* result)
{
    time_t t = m3t ? *m3t : 0;
    return localtime_r(m3t ? &t : 0, result);
}

tm_t* Utime__gmtime_r(const m3_time_t* m3t, tm_t* result)
{
    time_t t = m3t ? *m3t : 0;
    return gmtime_r(m3t ? &t : 0, result);
}

int Utime__setitimer(int which, const m3_itimerval_t* m3new, m3_itimerval_t* m3old)
{
    itimerval_t ne;
    itimerval_t old;
    int r = setitimer(which, itimerval_from_m3(&ne, m3new), m3old ? &old : 0);
    itimerval_to_m3(&old, m3old);
    return r;
}

#ifndef __INTERIX

int Utime__nanosleep(const m3_timespec_t* m3req, m3_timespec_t* m3rem)
{
    timespec_T req;
    timespec_T rem;
    int r = nanosleep(timespec_from_m3(&req, m3rem), m3rem ? &rem : 0);
    timespec_to_m3(&rem, m3rem);
    return r;
}

int Unix__utimes(const char* file, const m3_timeval_t* m3t)
{
    timeval_t t;
    return utimes(file, timeval_from_m3(&t, m3t));
}

#endif /* __INTERIX */

int Unix__select(int nfds, fd_set* readfds, fd_set* writefds, fd_set* exceptfds, m3_timeval_t* m3t)
{
    /* timeout sometimes is in-only, sometimes in-out */
    timeval_t t;
    int r = select(nfds, readfds, writefds, exceptfds, timeval_from_m3(&t, m3t));
    timeval_to_m3(&t, m3t);
    return r;
}

#endif /* _WIN32 */

void Utime__tzset(void)
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
