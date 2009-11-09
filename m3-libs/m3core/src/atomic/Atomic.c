#ifdef __cplusplus
extern "C" {
#endif

#if defined(__sun)
#include <atomic.h>
#elif defined(_WIN32)
#include <windows.h>
#endif

#if _MSC_VER
#pragma warning(disable:4793)
#endif

void Atomic__MemoryBarrier(void)
{
#if defined(__sun)
    membar_producer();
    membar_consumer();
#elif __GNUC__ >= 4
    __sync_synchronize();
#elif defined(_WIN32) && defined(MemoryBarrier)
    MemoryBarrier();
#elif defined(_WIN32) && defined(_M_IX86) && !defined(_M_CEE_PURE)
    LONG Barrier;
    __asm {
        xchg Barrier, eax
    }
#elif __GNUC__ >= 3 && __i386__
    long Barrier;
    asm volatile("xchg %0, %%eax"::"m"(Barrier):"%eax");
#else
#error Atomic__MemoryBarrier not implemented.
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
