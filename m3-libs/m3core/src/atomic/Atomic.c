#ifdef __cplusplus
extern "C" {
#endif

#if defined(__sun)

#include <atomic.h>

void Atomic__MemoryBarrier(void)
{
    membar_producer();
    membar_consumer();
}

#elif __GNUC__ >= 4

void Atomic__MemoryBarrier(void)
{
    __sync_synchronize();
}

#elif defined(_WIN32)

#include <windows.h>

#ifndef FORCEINLINE
#define FORCEINLINE
#endif

#if !defined(MemoryBarrier) && defined(_M_IX86) && !defined(_M_CEE_PURE)
#pragma warning(disable:4793)
VOID
FORCEINLINE
MemoryBarrier(
    VOID)
{
    LONG Barrier;
    __asm {
        xchg Barrier, eax
    }
}
#define MemoryBarrier MemoryBarrier
#endif

void Atomic__MemoryBarrier(void)
{
    MemoryBarrier();
}

#elif __GNUC__ >= 3 && __i386__

void Atomic__MemoryBarrier(void)
{
    long Barrier;
    asm volatile("xchg %0, %%eax"::"m"(Barrier):"%eax");
}
 
#else
 
#error Atomic__MemoryBarrier not implemented.
 
#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
