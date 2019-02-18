#include <time.h>

time_t MyUtime__mktime(struct tm *x) { return mktime(x); }
