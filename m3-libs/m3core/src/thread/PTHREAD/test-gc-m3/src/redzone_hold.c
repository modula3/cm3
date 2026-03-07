/*
 * redzone_hold.c — C helper for M3 GC red zone test
 *
 * Stores a traced M3 heap pointer in the ARM64 red zone (below SP),
 * clears all registers, spins until signaled, then returns the
 * pointer from the red zone.
 *
 * If the GC scans [SP, top) without the red zone adjustment,
 * it will miss this pointer and may collect the object.
 */

#include <stdint.h>

#ifdef __arm64__

void *
RedZoneHold(void *obj, volatile int *flag)
{
    void *recovered = 0;

    /*
     * Store obj in the red zone at [SP-8] and [SP-16].
     * Clear the register so the ONLY live copy is below SP.
     */
    __asm__ volatile (
        "str %[ptr], [sp, #-8]\n\t"
        "str %[ptr], [sp, #-16]\n\t"
        "mov %[ptr], #0\n\t"
        : [ptr] "+r" (obj)
        : : "memory"
    );

    /* Spin until signaled — this is the GC window.
     * Other M3 threads are allocating heavily, triggering GC.
     * The GC will suspend this thread and scan its stack.
     * With M3_STACK_ADJUST=0, it won't look below SP. */
    while (*flag == 0) {
        __asm__ volatile("yield" ::: "memory");
    }

    /* Recover the pointer from the red zone */
    __asm__ volatile (
        "ldr %[out], [sp, #-8]\n\t"
        : [out] "=r" (recovered)
        : : "memory"
    );

    return recovered;
}

#else

/* Non-ARM64: just return the pointer (no red zone issue) */
void *
RedZoneHold(void *obj, volatile int *flag)
{
    while (*flag == 0) ;
    return obj;
}

#endif
