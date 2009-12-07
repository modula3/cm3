
#include "m3core.h"

enum TimeZone { TimeZone_Local, TimeZone_Utc, TimeZone_Unknown };

typedef struct
{
  int year;
  int month;
  int day;
  int hour;
  int minute;
  int second;
  ptrdiff_t offset;
  void* zone;
  int weekDay;
  int align;
} Date_t;

#if defined(__CYGWIN__) || defined(__hpux) || defined(__sun) || defined(__INTERIX__)
#define DATE_POSIX
#else
#define DATE_BSD
#endif

struct timeval TimePosix__ToUtime(double t);

void Date__FromTime(double t, int z, Date_t* date, void** zone)
{
    struct tm* tm;
    struct tm tm_storage;
    struct timeval tv;
    
    *zone = 0;
    ZeroMemory(date, sizeof(*date));
    ZeroMemory(&tv, sizeof(tv));
    ZeroMemory(&tm_storage, sizeof(tm_storage));
 
    tv = TimePosix__ToUtime(t);
#ifdef DATE_BSD
    if (z == TimeZone_Local)
        tm = localtime(&tv.tv_sec);
    else if (z == TimeZone_Utc)
        tm = gmtime(&tv.tv_sec);
    else
        assert(0);
#else
    if (z == TimeZone_Local)
        tm = localtime_r(&tv.tv_sec, &tm_storage);
    else if (z == TimeZone_Utc)
        tm = gmtime_t(&tv.tv_sec, &tm_storage);
    else
        assert(0);
#endif
    date->year = tm->tm_year + 1900;
    date->month = tm->tm_mon;
    date->day = tm->tm_mday;
    date->hour = tm->tm_hour;
    date->minute = tm->tm_min;
    date->second = tm->tm_sec;
    date->weekDay = tm->tm_wday;

#ifdef DATE_BSD
    /* The "tm.tm_gmtoff" field is seconds *east* of GMT, whereas
     the "date.offset" field is seconds *west* of GMT, so a
     negation is necessary. */
    date->offset = -tm->tm_gmtoff;
    *zone = tm->tm_zone;
#else
    if (tm->tm_isdst == 0)
    {
        date->offset = timezone;
        *zone = tzname[0];
    }
    else if (tm_tm_isdst > 0 && daylight)
    {
#ifdef __sun
        date->offset = altzone;
#else
        date->offset = timezone - 3600;
#endif
        *zone = tzname[1];
    }
#endif
}
