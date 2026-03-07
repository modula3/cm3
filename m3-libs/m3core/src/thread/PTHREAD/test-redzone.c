/*
 * test-redzone.c — Demonstrate ARM64 red zone visibility gap
 *
 * On macOS ARM64, leaf functions can use 128 bytes below SP (the "red zone")
 * without adjusting SP.  The CM3 GC scans [SP, stack_top) but with
 * M3_STACK_ADJUST=0 it misses the red zone.
 *
 * This test doesn't try to trigger a crash — it directly demonstrates that
 * data written to the red zone is invisible to the GC scan range.
 *
 * Approach: a worker thread calls a leaf function that stores pointers
 * in a local array (which the compiler places in the red zone at -O1).
 * A monitor thread suspends the worker, reads SP, and checks whether
 * those pointers fall below SP.
 *
 * Build:  cc -O1 -o test-redzone test-redzone.c -lpthread
 * Run:    ./test-redzone
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>

#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/thread_act.h>
#endif

static volatile int phase = 0;  /* 0=init, 1=ready, 2=done */

/*
 * We use inline asm to write a known pattern below SP without
 * modifying SP — simulating what a leaf function does.
 * This guarantees data is in the red zone.
 */

/* sentinel values we'll look for */
#define SENTINEL1 0xDEADBEEFCAFE0001ULL
#define SENTINEL2 0xDEADBEEFCAFE0002ULL
#define SENTINEL3 0xDEADBEEFCAFE0003ULL
#define SENTINEL4 0xDEADBEEFCAFE0004ULL

static void *worker_func(void *arg) {
#ifdef __arm64__
    /*
     * Write sentinel values into the red zone (below SP) and spin.
     * We use stp (store pair) to write 16 bytes at a time.
     * ARM64 red zone = 128 bytes below SP.
     *
     * We write at SP-16, SP-32, SP-48, SP-64.
     */
    __asm__ volatile (
        /* Load sentinel values into registers */
        "mov x9,  #0x0001\n\t"
        "movk x9, #0xCAFE, lsl #16\n\t"
        "movk x9, #0xBEEF, lsl #32\n\t"
        "movk x9, #0xDEAD, lsl #48\n\t"   /* x9  = SENTINEL1 */

        "mov x10, #0x0002\n\t"
        "movk x10, #0xCAFE, lsl #16\n\t"
        "movk x10, #0xBEEF, lsl #32\n\t"
        "movk x10, #0xDEAD, lsl #48\n\t"  /* x10 = SENTINEL2 */

        "mov x11, #0x0003\n\t"
        "movk x11, #0xCAFE, lsl #16\n\t"
        "movk x11, #0xBEEF, lsl #32\n\t"
        "movk x11, #0xDEAD, lsl #48\n\t"  /* x11 = SENTINEL3 */

        "mov x12, #0x0004\n\t"
        "movk x12, #0xCAFE, lsl #16\n\t"
        "movk x12, #0xBEEF, lsl #32\n\t"
        "movk x12, #0xDEAD, lsl #48\n\t"  /* x12 = SENTINEL4 */

        /* Store into red zone (below SP, no SP adjustment) */
        "stp x9,  x10, [sp, #-16]\n\t"    /* SP-16, SP-8  */
        "stp x11, x12, [sp, #-32]\n\t"    /* SP-32, SP-24 */
        "stp x9,  x10, [sp, #-48]\n\t"    /* SP-48, SP-40 */
        "stp x11, x12, [sp, #-64]\n\t"    /* SP-64, SP-56 */
        "stp x9,  x10, [sp, #-80]\n\t"    /* SP-80, SP-72 */
        "stp x11, x12, [sp, #-96]\n\t"    /* SP-96, SP-88 */
        "stp x9,  x10, [sp, #-112]\n\t"   /* SP-112, SP-104 */
        "stp x11, x12, [sp, #-128]\n\t"   /* SP-128, SP-120 */
        :
        :
        : "x9", "x10", "x11", "x12", "memory"
    );

    /* Signal ready and spin */
    phase = 1;
    while (phase == 1) {
        __asm__ volatile ("" ::: "memory"); /* prevent optimization */
    }
#endif
    return NULL;
}

#if defined(__APPLE__) && defined(__arm64__)

static int check_redzone(void) {
    pthread_t worker;
    pthread_create(&worker, NULL, worker_func, NULL);

    /* Wait for worker to be in the spin loop with red zone populated */
    while (phase != 1) { usleep(1000); }
    usleep(10000); /* let it settle */

    mach_port_t mach_thread = pthread_mach_thread_np(worker);

    /* Suspend the worker */
    kern_return_t kr = thread_suspend(mach_thread);
    if (kr != KERN_SUCCESS) {
        fprintf(stderr, "thread_suspend failed: %d\n", kr);
        phase = 2;
        pthread_join(worker, NULL);
        return -1;
    }
    kr = thread_abort_safely(mach_thread);
    if (kr != KERN_SUCCESS) {
        thread_resume(mach_thread);
        phase = 2;
        pthread_join(worker, NULL);
        return -1;
    }

    /* Read thread state to get SP */
    arm_thread_state64_t state;
    mach_msg_type_number_t count = ARM_THREAD_STATE64_COUNT;
    kr = thread_get_state(mach_thread, ARM_THREAD_STATE64,
                          (thread_state_t)&state, &count);
    if (kr != KERN_SUCCESS) {
        fprintf(stderr, "thread_get_state failed: %d\n", kr);
        thread_resume(mach_thread);
        phase = 2;
        pthread_join(worker, NULL);
        return -1;
    }

    uintptr_t sp = (uintptr_t)__darwin_arm_thread_state64_get_sp(state);

    printf("Worker SP = 0x%llx\n", (unsigned long long)sp);
    printf("\n--- Scanning red zone [SP-128, SP) ---\n");

    int sentinels_in_redzone = 0;
    for (int offset = -128; offset < 0; offset += 8) {
        uintptr_t addr = sp + offset;
        uint64_t val = *(uint64_t *)addr;
        if (val == SENTINEL1 || val == SENTINEL2 ||
            val == SENTINEL3 || val == SENTINEL4) {
            printf("  [SP%+d] = 0x%016llx  <-- SENTINEL (invisible to GC)\n",
                   offset, (unsigned long long)val);
            sentinels_in_redzone++;
        }
    }

    printf("\n--- Scanning above SP [SP, SP+128) ---\n");

    int sentinels_above_sp = 0;
    for (int offset = 0; offset < 128; offset += 8) {
        uintptr_t addr = sp + offset;
        uint64_t val = *(uint64_t *)addr;
        if (val == SENTINEL1 || val == SENTINEL2 ||
            val == SENTINEL3 || val == SENTINEL4) {
            printf("  [SP+%d] = 0x%016llx  <-- SENTINEL (visible to GC)\n",
                   offset, (unsigned long long)val);
            sentinels_above_sp++;
        }
    }

    printf("\n--- Scanning registers ---\n");

    /* Check all general-purpose registers in the thread state */
    int sentinels_in_regs = 0;
    for (int r = 0; r < 29; r++) {
        uint64_t val = state.__x[r];
        if (val == SENTINEL1 || val == SENTINEL2 ||
            val == SENTINEL3 || val == SENTINEL4) {
            printf("  x%d = 0x%016llx  <-- SENTINEL (visible via register scan)\n",
                   r, (unsigned long long)val);
            sentinels_in_regs++;
        }
    }

    printf("\n--- Summary ---\n");
    printf("Sentinels in red zone (below SP): %d\n", sentinels_in_redzone);
    printf("Sentinels above SP:               %d\n", sentinels_above_sp);
    printf("Sentinels in registers:           %d\n", sentinels_in_regs);

    if (sentinels_in_redzone > 0) {
        printf("\nBUG CONFIRMED: %d sentinel values in red zone are invisible\n",
               sentinels_in_redzone);
        printf("to GC stack scan with M3_STACK_ADJUST=0.\n");
        printf("\nWith M3_STACK_ADJUST=0:   GC scans [SP, top)   — misses %d values\n",
               sentinels_in_redzone);
        printf("With M3_STACK_ADJUST=128: GC scans [SP-128, top) — finds all values\n");
    }

    /* Resume and cleanup */
    thread_resume(mach_thread);
    phase = 2;
    pthread_join(worker, NULL);

    return sentinels_in_redzone > 0 ? 1 : 0;
}

#endif

int main(void) {
#if !defined(__APPLE__)
    printf("This test requires macOS (Mach thread API).\n");
    return 0;
#elif !defined(__arm64__)
    printf("This test targets ARM64.\n");
    printf("On x86_64, CM3 already uses M3_STACK_ADJUST=128.\n");
    return 0;
#else
    printf("=== ARM64 Red Zone GC Visibility Test ===\n");
    printf("ARM64 ABI: leaf functions may use 128 bytes below SP.\n");
    printf("CM3 GC (ThreadApple.c): M3_STACK_ADJUST=%d for __arm64__.\n\n", 0);

    int result = check_redzone();

    if (result > 0) {
        printf("\nFAILED: Data in red zone invisible to GC scan.\n");
        printf("Fix: #define M3_STACK_ADJUST 128  (in ThreadApple.c, __arm64__ section)\n");
        return 1;
    } else if (result == 0) {
        printf("\nPASSED: No sentinels found in red zone (unexpected).\n");
        return 0;
    } else {
        printf("\nERROR: Could not perform test.\n");
        return 2;
    }
#endif
}
