#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* We won't ever be truncating on the way out.
 * We might be truncating on the way in.
 * time() doesn't really take an input.
 * ctime() needs to be replaced with a 64bit version or possibly removed.
 */
#ifdef _TIME64_T
M3_STATIC_ASSERT(sizeof(time64_t) <= sizeof(m3_time64_t));
#else
M3_STATIC_ASSERT(sizeof(time_t) <= sizeof(m3_time64_t));
#endif

M3_DLL_EXPORT
m3_time64_t
__cdecl
Utime__time(m3_time64_t* tloc)
{
#ifdef _TIME64_T
    time64_t b = tloc ? (time64_t)*tloc : 0;
    time64_t a = time64(tloc ? &b : 0);
#else
    time_t b = tloc ? (time_t)*tloc : 0;
    time_t a = time(tloc ? &b : 0);
#endif
    if (tloc) *tloc = b;
    return a;
}

M3_DLL_EXPORT
char*
__cdecl
Utime__ctime(const m3_time64_t* m)
{
#ifdef _TIME64_T
    time64_t t = m ? (time64_t)*m : 0;
    return ctime64(m ? &t : 0);
#else
    time_t t = m ? (time_t)*m : 0;
    return ctime(m ? &t : 0);
#endif
}

M3_DLL_EXPORT
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
