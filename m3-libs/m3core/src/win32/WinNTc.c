#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif


void m3_MemoryBarrier(void)
{
#if defined(_WIN32) || defined(__CYGWIN__)
    MemoryBarrier();
#endif
}


#ifdef __cplusplus
}
#endif
