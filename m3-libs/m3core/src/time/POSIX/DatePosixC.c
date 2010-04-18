
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

#ifdef __CYGWIN__
#define M3_TIMEZONE _timezone
#define M3_DAYLIGHT _daylight
#else
#define M3_TIMEZONE timezone
#define M3_DAYLIGHT daylight
#endif

#if defined(__CYGWIN__) || defined(__hpux) || defined(__sun) || defined(__INTERIX)
#define DATE_POSIX
#else
#define DATE_BSD
#endif

#define Local 0
#define UTC 1

#define MILLION (1000 * 1000)

static
struct timeval
__cdecl
TimePosix__ToUtime(LONGREAL/*Time.T*/ t)
/* see also TimePosix__FromUtime */
{
    struct timeval tv;
    double n = { 0 };

    ZERO_MEMORY(tv); 
    tv.tv_usec = modf(t, &n) * MILLION;
    tv.tv_sec = n;
    return tv;
}

void
__cdecl
DatePosix__FromTime(double t, const ptrdiff_t* pzone, Date_t* date, TEXT unknown, TEXT gmt)
{
    struct tm* tm = { 0 };
    struct timeval tv = TimePosix__ToUtime(t);    
    ptrdiff_t zone = (pzone ? *pzone : Local);

    ZeroMemory(date, sizeof(*date));
    assert(zone == Local || zone == UTC);
    tm = ((zone == Local) ? localtime(&tv.tv_sec) : gmtime(&tv.tv_sec));
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
     * the "date.offset" field is seconds *west* of GMT, so a
     * negation is necessary.
     */
    date->offset = -tm->tm_gmtoff;
    date->zone = M3toC__CopyStoT(tm->tm_zone);
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
DatePosix__ToTime(const Date_t* date)
{
    struct tm tm;
    double t = { 0 };
#ifdef DATE_BSD
    const unsigned SecsPerHour = 60 * 60;
    time_t now = { 0 };
    struct tm* local_now = { 0 };
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
        goto Exit;

    /* adjust result to reflect "date->offset" */
    time(&now);
    local_now = localtime(&now);
    assert(local_now != NULL);
    if (local_now->tm_isdst > 0)
      /* decrement the local time zone by one hour if DST is in effect */
      local_now->tm_gmtoff -= SecsPerHour;

    /* As above, we must negate "date->offset" to account for the
       opposite sense of that field compared to Unix. */
    t -= ((-date->offset) - local_now->tm_gmtoff);
Exit:
#endif
    return t;
}

void
__cdecl
DatePosix__TypeCheck(const Date_t* d, size_t sizeof_DateT)
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
