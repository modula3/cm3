#ifdef __cplusplus
extern "C" {
#endif

#if defined(__sun)
#include <atomic.h>
#elif defined(_WIN32)
#if _MSC_VER
struct IRpcStubBuffer;   /* warning 4115: named type definition in parentheses */
#pragma warning(disable:4201) /* nonstandard extension: nameless struct/union */
#pragma warning(disable:4214) /* nonstandard extension: bitfield other than int */
#pragma warning(disable:4514) /* unused inline function removed */
#endif
#include <windows.h>
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
