/* $Id$ */

#include <time.h>
#include <stdlib.h>
#include <assert.h>

#if __cplusplus
extern "C" {
#endif

void
CTZ__setenv_TZ_America_New_York(void)
{
#if _WIN32
	int res = _wputenv(L"TZ=America/New_York");
#else
	int res = setenv("TZ","America/New_York",1);
#endif
	assert(res==0);
}

void
CTZ__setTZ(const char *tzname)
{
#if _WIN32
	int res = _wputenv(L"TZ=tzname");
#else
	int res = setenv("TZ",tzname",1);
#endif
	assert(res==0);
#if _WIN32
	_tzset();
#else
	tzset();
#endif
}

#if __cplusplus
} /* extern "C" */
#endif
