/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>

void
UtimeOpsC__Set_second (struct tm *t, int second )
{
	t->tm_sec = second ;
}

void
UtimeOpsC__Set_minute (struct tm *t, int minute )
{
	t->tm_min = minute ;
}

void
UtimeOpsC__Set_hour   (struct tm *t, int hour   )
{
	t->tm_hour   = hour   ;
}

void
UtimeOpsC__Set_day    (struct tm *t, int day    )
{
	t->tm_mday    = day    ;
}

void
UtimeOpsC__Set_month  (struct tm *t, int month  )
{
	t->tm_mon  = month  ;
}

void
UtimeOpsC__Set_year   (struct tm *t, int year   )
{
	t->tm_year   = year - 1900  ;
}

void
UtimeOpsC__Set_wday   (struct tm *t, int wday   )
{
	t->tm_wday   = wday   ;
}

/***********************************************************************/

int
UtimeOpsC__Get_second (const struct tm *t) 
{
	return t->tm_sec ;
}


int
UtimeOpsC__Get_gmtoff (const struct tm *t) 
{
	return t->tm_gmtoff ;
}


int
UtimeOpsC__Get_minute (const struct tm *t) 
{
	return t->tm_min ;
}

const char *
UtimeOpsC__Get_zone (const struct tm *t) 
{
	return t->tm_zone ;
}



int
UtimeOpsC__Get_hour   (const struct tm *t) 
{
	return t->tm_hour   ;
}



int
UtimeOpsC__Get_day    (const struct tm *t) 
{
	return t->tm_mday    ;
}



int
UtimeOpsC__Get_month  (const struct tm *t) 
{
	return t->tm_mon  ;
}



int
UtimeOpsC__Get_year   (const struct tm *t) 
{
	return t->tm_year + 1900  ;
}



int
UtimeOpsC__Get_wday   (const struct tm *t) 
{
	return t->tm_wday   ;
}


double
UtimeOpsC__mktime(struct tm *t)
{
	t->tm_isdst = -1;
	t->tm_gmtoff = 0;
	t->tm_wday   = 0;
	t->tm_yday   = 0;
  return mktime(t);
}

struct tm *
UtimeOpsC__localtime_r(double clock, struct tm *result) 
{
	time_t clocki=clock;

	struct tm *res= localtime_r(&clocki, result);
#if 0
	printf("clock: %f\n", clock);
	printf("tm: %d %d %d\n", res->tm_hour, res->tm_min, res->tm_sec);
	printf("tm: %s\n", res->tm_zone);
#endif
        return res;
}

struct tm *
UtimeOpsC__make_T() 
{
	return (struct tm*)malloc(sizeof(struct tm));
}

void
UtimeOpsC__delete_T(struct tm *t)
{
	free(t);
}

char *
UtimeOpsC__ctime_r(time_t *clock, char *buf)
{
	return ctime_r(clock, buf);
}

void
UtimeOpsC__check_types(void)
{
        /* we really should assert that a Modula-3 INTEGER
 	is equally sized to C's time_t */
	assert(sizeof(time_t) == sizeof(long));
}

void
UtimeOpsC__write_double_clock(double time, time_t *buf)
{
	*buf = time;
}
