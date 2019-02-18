/* $Id$ */

#include <time.h>
#include <stdlib.h>
#include <assert.h>

void
CTZ__setenv_TZ_America_New_York(void)
{
	int res = setenv("TZ","America/New_York",1);
	assert(res==0);
}

void
CTZ__setTZ(const char *tzname)
{
	int res = setenv("TZ",tzname,1);
	assert(res==0);
	tzset();
}
