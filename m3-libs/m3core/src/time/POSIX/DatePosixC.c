
#include "m3core.h"

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const"
 */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__CYGWIN__) || defined(__hpux) || defined(__sun) || defined(__INTERIX)
#define DATE_POSIX
#else
#define DATE_BSD
#endif

#define Local 0
#define UTC 1

void
__cdecl
Date__FromTime(double t, ptrdiff_t* zone, Date_t* date)
{
    struct tm* tm;
    struct timeval tv;
    struct tm tm_storage;
    
    tzset();
    ZeroMemory(date, sizeof(*date));
    ZeroMemory(&tv, sizeof(tv));
    ZeroMemory(&tm_storage, sizeof(tm_storage));
 
    tv = TimePosix__ToUtime(t);
    if (zone == NULL || *zone == Local)
        tm = localtime_r(&tv.tv_sec, &tm_storage);
    else if (*zone == UTC)
        tm = gmtime_r(&tv.tv_sec, &tm_storage);
    else
        assert(0);
    date->year = tm->tm_year + 1900;
    date->month = tm->tm_mon;
    date->day = tm->tm_mday;
    date->hour = tm->tm_hour;
    date->minute = tm->tm_min;
    date->second = tm->tm_sec;
    date->weekDay = tm->tm_wday;

#ifdef DATE_BSD
    /* The "tm.tm_gmtoff" field is seconds *east* of GMT, whereas
     * the "date.offset" field is seconds *west* of GMT, so a
     * negation is necessary.
     */
    date->offset = -tm->tm_gmtoff;
    *zone = M3toC__CopyStoT(tm->tm_zone);
#else
    if (tm->tm_isdst == 0)
    {
        date->offset = Utime__get_timezone();
        date->zone = M3toC__CopyStoT(Utime__get_tzname(0));
    }
    else if (tm->tm_isdst > 0 && Utime__get_daylight())
    {
#ifdef __sun
        date->offset = Utime__get_altzone();
#else
        date->offset = Utime__get_timezone() - 3600;
#endif
        date->zone = M3toC__CopyStoT(Utime__get_tzname(1));
    }
    else
    {
      date->offset = 0;
      date->zone   = M3toC__CopyStoT("Unknown");
    }
#endif
}

double
__cdecl
DatePosix__ToTime(const Date_t* date)
{
    struct tm tm;
    double t = { 0 };
#ifdef DATE_BSD
    const unsigned SecsPerHour = 60 * 60;
    time_t now = { 0 };
    struct tm local_now;

    ZERO_MEMORY(local_now);
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
    t = mktime(&tm);
#ifdef DATE_BSD
    if (t == -1)
        return t;

    /* adjust result to reflect "date->offset" */
    time(&now);
    localtime_r(&now, &local_now);
    if (local_now.tm_isdst > 0)
      /* decrement the local time zone by one hour if DST is in effect */
      local_now.tm_gmtoff -= SecsPerHour;

    /* As above, we must negate "date->offset" to account for the
       opposite sense of that field compared to Unix. */
    t -= ((-date->offset) - local_now.tm_gmtoff);
#endif
    /* convert to a "Time.T" */
    return t;
}

#ifdef __cplusplus
} /* extern C */
#endif
