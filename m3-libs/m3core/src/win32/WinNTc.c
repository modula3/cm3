#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_WIN32) || defined(__CYGWIN__)

void m3_MemoryBarrier(void)
{
#if defined(MemoryBarrier)
    /* IA64 and AMD64 make this a macro, good, we can test for it. */
    MemoryBarrier();
#else
    /* x86 MemoryBarrier is plain exchange, so InterlockedExchange should work */
    static volatile long a;
    InterlockedExchange(&a, a);
#endif
}

#else

void m3_MemoryBarrier(void)
{
}

#endif

#ifdef __cplusplus
}
#endif
