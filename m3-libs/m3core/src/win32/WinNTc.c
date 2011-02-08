#include <windows.h>

#ifdef __cplusplus
extern "C" {
#endif

void __cdecl WinNT__MemoryBarrier(void)
/*
This function ensure that all reads/writes before it
finish before any reads/writes after it.
It is both a compiler barrier and a processor barrier.
*/
{
#ifdef MemoryBarrier
    /* newer headers */
    MemoryBarrier();
#else
    /* older headers */
    volatile long a = 0;
    InterlockedExchange(&a, a);
#endif
}

#ifdef __cplusplus
}
#endif
