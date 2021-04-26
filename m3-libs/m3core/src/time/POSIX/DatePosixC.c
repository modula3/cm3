
#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#if __GNUC__ >= 4
#pragma GCC visibility push(hidden)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __CYGWIN__
#define M3_TIMEZONE _timezone
#define M3_DAYLIGHT _daylight
#else
#define M3_TIMEZONE timezone
#define M3_DAYLIGHT daylight
#endif

/* OSF/1 is BSD at least if you define _OSF_SOURCE, else POSIX. */
#if defined(__CYGWIN__) || defined(__hpux) || defined(__sun) || defined(__INTERIX)
#define DATE_POSIX
#else
#define DATE_BSD
#endif

#define Local 0
#define UTC 1

#ifdef __osf__
/* This works whether or not _OSF_SOURCE is defined. The headers say:
ifdef _OSF_SOURCE
    long    tm_gmtoff;
    char    *tm_zone;
define __tm_gmtoff tm_gmtoff
define __tm_zone   tm_zone
else
    long    __tm_gmtoff;
    char    *__tm_zone;
#endif
*/
#define m3_tm_gmtoff __tm_gmtoff
#define m3_tm_zone   __tm_zone
#else
#define m3_tm_gmtoff tm_gmtoff
#define m3_tm_zone   tm_zone
#endif

#ifdef _TIME64_T
static time64_t TimePosix__ToSeconds(LONGREAL/*Time.T*/ t)
#else
static time_t TimePosix__ToSeconds(LONGREAL/*Time.T*/ t)
#endif
{
    double n = 0;
    modf(t, &n);
    return n;
}

void
__cdecl
DatePosix__FromTime(double t, /*const*/ INTEGER* pzone, Date_t* date, TEXT unknown, TEXT gmt)
{
    struct tm* tm = 0;
#ifdef _TIME64_T
    time64_t sec = TimePosix__ToSeconds(t);
#else
    time_t sec = TimePosix__ToSeconds(t);
#endif
    ptrdiff_t zone = (pzone ? *pzone : Local);
    ZeroMemory(date, sizeof(*date));
    assert(zone == Local || zone == UTC);
#ifdef _TIME64_T
    tm = ((zone == Local) ? localtime64(&sec) : gmtime64(&sec));
#else
    tm = ((zone == Local) ? localtime(&sec) : gmtime(&sec));
#endif
    assert(tm != NULL);
    date->year = tm->tm_year + 1900;
    date->month = tm->tm_mon;
    date->day = tm->tm_mday;
    date->hour = tm->tm_hour;
    date->minute = tm->tm_min;
    date->second = tm->tm_sec;
    date->weekDay = tm->tm_wday;

#ifdef DATE_BSD
    /* The "tm->tm_gmtoff" field is seconds *east* of GMT, whereas
     * the "date.offset" field is seconds *west* of GMT, so negate.
     */
    date->offset = -tm->m3_tm_gmtoff;
    date->zone = M3toC__CopyStoT(tm->m3_tm_zone);
#else
    if (zone == Local)
    {
        if (tm->tm_isdst == 0)
        {
            date->offset = M3_TIMEZONE;
            date->zone = M3toC__CopyStoT(tzname[0]);
        }
        else if (tm->tm_isdst > 0 && M3_DAYLIGHT)
        {
#ifdef __sun
            date->offset = altzone;
#else
            date->offset = M3_TIMEZONE - 3600;
#endif
            date->zone = M3toC__CopyStoT(tzname[1]);
        }
        else
        {
            date->offset = 0;
            date->zone   = unknown;
        }
    }
    else
    {
        date->offset = 0;
        date->zone  = gmt;
    }
#endif
}

double
__cdecl
DatePosix__ToTime(/*const*/ Date_t* date)
{
    struct tm tm;
    double t = 0;
#ifdef DATE_BSD
    const unsigned SecsPerHour = 60 * 60;
#ifdef _TIME64_T
    time64_t now = 0;
#else
    time_t now = 0;
#endif
    struct tm* local_now = 0;
#endif

    /* prepare call to mktime(3) */
    ZERO_MEMORY(tm);
    tm.tm_sec    = date->second;
    tm.tm_min    = date->minute;
    tm.tm_hour   = date->hour;
    tm.tm_mday   = date->day;
    tm.tm_mon    = date->month;
    tm.tm_year   = date->year - 1900;
    /* tm.tm_wday ignored */
    tm.tm_isdst  = 0; /* tell mktime that DST is not in effect */
    /* tm_zone, tm_gmtoff ignored */
#ifdef _TIME64_T
    t = mktime64(&tm);
#else
    t = mktime(&tm);
#endif
#ifdef DATE_BSD
    if (t == -1)
        goto Exit;

    /* adjust result to reflect "date->offset" */
#ifdef _TIME64_T
    time64(&now);
    local_now = localtime64(&now);
#else
    time(&now);
    local_now = localtime(&now);
#endif
    assert(local_now != NULL);
    if (local_now->tm_isdst > 0)
      /* decrement the local time zone by one hour if DST is in effect */
      local_now->m3_tm_gmtoff -= SecsPerHour;

    /* As above, we must negate "date->offset" to account for the
       opposite sense of that field compared to Unix. */
    t -= ((-date->offset) - local_now->m3_tm_gmtoff);
Exit:
#endif
    return t;
}

void
__cdecl
DatePosix__TypeCheck(/*const*/ Date_t* d, WORD_T sizeof_DateT)
{
    assert(sizeof(Date_t) == sizeof_DateT);
    assert(d->year == 1);
    assert(d->month == 2);
    assert(d->day == 3);
    assert(d->hour == 4);
    assert(d->minute == 5);
    assert(d->second == 6);
    assert(d->offset == 7);
    assert(d->zone == (TEXT)8);
    assert(d->weekDay == 9);
}

#ifdef __cplusplus
} /* extern C */
#endif
