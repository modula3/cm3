#ifdef _MSC_VER
#undef _DLL
#ifndef _MT
#define _MT
#endif
#endif

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

/* We won't ever be truncating. */
M3_STATIC_ASSERT(sizeof(time_t) <= sizeof(m3_time_t));

m3_time_t
__cdecl
Utime__time(m3_time_t* tloc)
{
    time_t b = tloc ? (time_t)*tloc : 0;
    time_t a = time(tloc ? &b : 0);
    if (tloc) *tloc = b;
    return a;
}

char*
__cdecl
Utime__ctime(const m3_time_t* m)
{
    time_t t = m ? (time_t)*m : 0;
    return ctime(m ? &t : 0);
}

void
__cdecl
Utime__tzset(void)
{
#ifdef _WIN32
    _tzset();
#else
    tzset();
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
