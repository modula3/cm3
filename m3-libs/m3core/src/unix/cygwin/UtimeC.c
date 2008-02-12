/*
The data in Utime.i3 should be changed to functions.
Until that time, workaround.
*/

#include <time.h>

time_t Utime__timezone;
int Utime__daylight;
char* Utime__tzname[2];

void Utime__init(void)
{
    tzset();
    Utime__timezone = _timezone;
    Utime__daylight = _daylight;
    Utime__tzname[0] = _tzname[0];
    Utime__tzname[1] = _tzname[1];
}
