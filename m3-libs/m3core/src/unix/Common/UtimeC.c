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
M3_STATIC_ASSERT(sizeof(m3_time_t) <= sizeof(m3_time64_t));

M3_DLL_EXPORT
m3_time64_t
__cdecl
Utime__time(m3_time64_t* tloc)
{
    m3_time_t b = tloc ? (m3_time_t)*tloc : 0;
#ifdef _TIME64_T
    m3_time_t a = time64(tloc ? &b : 0);
#else
    m3_time_t a = time(tloc ? &b : 0);
#endif
    if (tloc) *tloc = b;
    return a;
}

M3_DLL_EXPORT
char*
__cdecl
Utime__ctime(/*TODO const*/ m3_time64_t* m)
{
    m3_time_t t = m ? (m3_time_t)*m : 0;
#ifdef _TIME64_T
    return ctime64(m ? &t : 0);
#else
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
