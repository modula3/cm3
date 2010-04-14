
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

typedef struct
{
  size_t year;
  size_t month;
  size_t day;
  size_t hour;
  size_t minute;
  size_t second;
  ptrdiff_t offset;
  TEXT zone;
  size_t weekDay;
} Date_t;

#if defined(__CYGWIN__) || defined(__hpux) || defined(__sun) || defined(__INTERIX)
#define DATE_POSIX
#else
#define DATE_BSD
#endif

static const int Local = 0;
static const int UTC = 1;
EXTERN_CONST int const * const Date__Local = &Local;
EXTERN_CONST int const * const Date__UTC = &UTC;

void
__cdecl
Date__FromTime(double t, int* z, Date_t* date)
{
    struct tm* tm;
    struct timeval tv;
    struct tm tm_storage;
    
    tzset();
    ZeroMemory(date, sizeof(*date));
    ZeroMemory(&tv, sizeof(tv));
    ZeroMemory(&tm_storage, sizeof(tm_storage));
 
    tv = TimePosix__ToUtime(t);
    if (z == NULL || *z == Local)
        tm = localtime_r(&tv.tv_sec, &tm_storage);
    else if (*z == UTC)
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

#ifdef __cplusplus
} /* extern C */
#endif
