#include <time.h>

#if __cplusplus
extern "C" {
#endif

time_t MyUtime__mktime(struct tm *x) { return mktime(x); }

#if __cplusplus
} /* extern "C" */
#endif
