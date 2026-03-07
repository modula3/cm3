/*
 * test-gc-redzone.c — Trigger use-after-free from missed red zone scan
 *
 * Simulates the CM3 GC bug on ARM64: a "collector" thread suspends
 * workers, scans [SP, stack_top) for heap pointers, and "collects"
 * (poisons) any heap object not found.  Workers store heap pointers
 * in the red zone via leaf-function locals.  The collector misses them,
 * poisons the objects, and the workers crash on next access.
 *
 * Build:  cc -O1 -o test-gc-redzone test-gc-redzone.c -lpthread
 * Run:    ./test-gc-redzone
 *
 * With M3_STACK_ADJUST=0:   crashes (SIGBUS/SIGSEGV/SIGILL)
 * With M3_STACK_ADJUST=128: runs to completion
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>
#include <signal.h>
#include <setjmp.h>

#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/thread_act.h>
#endif

#if !defined(__APPLE__) || !defined(__arm64__)
int main(void) {
    printf("This test requires macOS ARM64.\n");
    return 0;
}
#else

/*--- Configuration ---*/

/* Toggle this to test the fix: 0 = buggy, 128 = fixed */
#ifndef M3_STACK_ADJUST
#define M3_STACK_ADJUST 0
#endif

#define NUM_WORKERS    4
#define HEAP_POOL_SIZE 256
#define MAGIC          0xCAFEBABE
#define POISON         0xDEADDEAD
#define GC_CYCLES      5000

/*--- Heap simulation ---*/

/* A "heap object" that workers allocate and use */
struct heap_obj {
    uint32_t magic;     /* MAGIC if live, POISON if collected */
    uint32_t value;
    uint64_t pad[6];    /* 64 bytes total, like a small M3 traced object */
};

static struct heap_obj heap_pool[HEAP_POOL_SIZE];
static volatile int pool_next = 0;

static struct heap_obj *heap_alloc(void) {
    int idx = __atomic_fetch_add(&pool_next, 1, __ATOMIC_SEQ_CST);
    idx = idx % HEAP_POOL_SIZE;
    heap_pool[idx].magic = MAGIC;
    heap_pool[idx].value = (uint32_t)idx;
    return &heap_pool[idx];
}

static int is_heap_ptr(uintptr_t val) {
    uintptr_t base = (uintptr_t)&heap_pool[0];
    uintptr_t end  = (uintptr_t)&heap_pool[HEAP_POOL_SIZE];
    return val >= base && val < end;
}

/*--- Worker threads ---*/

static volatile int workers_go = 0;
static volatile int workers_stop = 0;
static pthread_t worker_threads[NUM_WORKERS];
static volatile uintptr_t worker_stack_tops[NUM_WORKERS];
static volatile int worker_ready[NUM_WORKERS];
static volatile int worker_crashes = 0;

/*
 * Store a heap pointer in the red zone using inline asm, then spin.
 * The GC (with M3_STACK_ADJUST=0) won't see it below SP.
 * After GC runs, load it back and check if the object was poisoned.
 */
__attribute__((noinline))
static void worker_use_heap_in_redzone(int id) {
    struct heap_obj *obj = heap_alloc();
    struct heap_obj *recovered = NULL;

    /*
     * Store the heap pointer ONLY in the red zone.  We clobber the
     * register that held it so the ONLY live copy is below SP.
     * This forces the GC to find it via stack scan, not registers.
     */
    uintptr_t ptr_val = (uintptr_t)obj;
    __asm__ volatile (
        "str %[ptr], [sp, #-8]\n\t"
        "str %[ptr], [sp, #-16]\n\t"
        "mov %[ptr], #0\n\t"           /* clear the register! */
        : [ptr] "+r" (ptr_val)
        : : "memory"
    );
    /* ptr_val is now 0; obj may still be in a callee-saved reg.
       Clobber obj too. */
    obj = NULL;

    /* Signal ready and spin — GC window.
       The ONLY copy of the heap pointer is at [SP-8] and [SP-16]. */
    worker_ready[id] = 1;
    while (!workers_go) {
        __asm__ volatile("" ::: "memory");
    }

    /* Load the pointer back from the red zone */
    __asm__ volatile (
        "ldr %[out], [sp, #-8]\n\t"
        : [out] "=r" (recovered) : : "memory"
    );

    /* Check if GC poisoned our object */
    if (recovered->magic != MAGIC) {
        __atomic_fetch_add(&worker_crashes, 1, __ATOMIC_SEQ_CST);
        fprintf(stderr, "  Worker %d: CORRUPTED! magic=0x%08x (expected 0x%08x)\n",
                id, recovered->magic, MAGIC);
    }
}

static void *worker_func(void *arg) {
    int id = (int)(intptr_t)arg;

    /* Record stack top (approximate — high address on growdown stack) */
    volatile int anchor;
    worker_stack_tops[id] = (uintptr_t)&anchor;

    while (!workers_stop) {
        worker_ready[id] = 0;
        worker_use_heap_in_redzone(id);

        /* Brief pause before next round */
        for (volatile int i = 0; i < 10; i++) ;
    }
    return NULL;
}

/*--- GC simulation ---*/

/*
 * Simulate CM3's conservative stack scan: scan the stack range
 * for anything that looks like a heap pointer, and mark those
 * objects as "live".  Then poison everything not marked.
 */
static void gc_scan_range(uintptr_t lo, uintptr_t hi, int *live) {
    for (uintptr_t *p = (uintptr_t *)lo; (uintptr_t)p < hi; p++) {
        uintptr_t val = *p;
        if (is_heap_ptr(val)) {
            /* Mark this object as seen */
            int idx = (int)((struct heap_obj *)val - &heap_pool[0]);
            if (idx >= 0 && idx < HEAP_POOL_SIZE) {
                live[idx] = 1;
            }
        }
    }
}

static void gc_scan_registers(arm_thread_state64_t *state, int *live) {
    /* Scan general-purpose registers x0-x28 + fp + lr + sp */
    uintptr_t *regs = (uintptr_t *)state;
    size_t nregs = sizeof(*state) / sizeof(uintptr_t);
    for (size_t i = 0; i < nregs; i++) {
        uintptr_t val = regs[i];
        if (is_heap_ptr(val)) {
            int idx = (int)((struct heap_obj *)val - &heap_pool[0]);
            if (idx >= 0 && idx < HEAP_POOL_SIZE) {
                live[idx] = 1;
            }
        }
    }
}

static int gc_cycle(void) {
    int live[HEAP_POOL_SIZE];
    memset(live, 0, sizeof(live));

    /* Suspend all workers */
    mach_port_t mach_threads[NUM_WORKERS];
    int suspended[NUM_WORKERS];
    memset(suspended, 0, sizeof(suspended));

    for (int i = 0; i < NUM_WORKERS; i++) {
        mach_threads[i] = pthread_mach_thread_np(worker_threads[i]);
        kern_return_t kr = thread_suspend(mach_threads[i]);
        if (kr == KERN_SUCCESS) {
            kr = thread_abort_safely(mach_threads[i]);
            if (kr == KERN_SUCCESS) {
                suspended[i] = 1;
            } else {
                thread_resume(mach_threads[i]);
            }
        }
    }

    /* Scan each suspended worker's stack */
    for (int i = 0; i < NUM_WORKERS; i++) {
        if (!suspended[i]) continue;

        arm_thread_state64_t state;
        mach_msg_type_number_t count = ARM_THREAD_STATE64_COUNT;
        kern_return_t kr = thread_get_state(mach_threads[i], ARM_THREAD_STATE64,
                                            (thread_state_t)&state, &count);
        if (kr != KERN_SUCCESS) continue;

        uintptr_t sp = (uintptr_t)__darwin_arm_thread_state64_get_sp(state);
        uintptr_t top = worker_stack_tops[i];

        /* This is the key line — M3_STACK_ADJUST controls whether we
           see into the red zone or not */
        sp -= M3_STACK_ADJUST;

        /* Scan stack */
        if (sp && top) {
            if (sp < top)
                gc_scan_range(sp, top, live);
            else
                gc_scan_range(top, sp, live);
        }

        /* Scan registers (CM3 does this too) */
        gc_scan_registers(&state, live);
    }

    /* "Collect" unmarked objects by poisoning them */
    int collected = 0;
    for (int i = 0; i < HEAP_POOL_SIZE; i++) {
        if (!live[i] && heap_pool[i].magic == MAGIC) {
            heap_pool[i].magic = POISON;
            heap_pool[i].value = POISON;
            collected++;
        }
    }

    /* Resume all workers */
    for (int i = 0; i < NUM_WORKERS; i++) {
        if (suspended[i]) {
            thread_resume(mach_threads[i]);
        }
    }

    return collected;
}

/*--- Main ---*/

int main(void) {
    printf("=== ARM64 GC Red Zone Test ===\n");
    printf("M3_STACK_ADJUST = %d  (%s)\n\n",
           M3_STACK_ADJUST,
           M3_STACK_ADJUST == 0 ? "BUGGY — misses red zone" : "FIXED");

    int total_corruptions = 0;

    for (int cycle = 0; cycle < GC_CYCLES; cycle++) {
        /* Reset state */
        workers_go = 0;
        workers_stop = 0;
        worker_crashes = 0;
        pool_next = 0;
        for (int i = 0; i < HEAP_POOL_SIZE; i++) {
            heap_pool[i].magic = 0;
        }
        for (int i = 0; i < NUM_WORKERS; i++) {
            worker_ready[i] = 0;
        }

        /* Start workers */
        for (int i = 0; i < NUM_WORKERS; i++) {
            pthread_create(&worker_threads[i], NULL, worker_func,
                           (void *)(intptr_t)i);
        }

        /* Wait for all workers to be in their leaf function */
        for (int i = 0; i < NUM_WORKERS; i++) {
            while (!worker_ready[i]) {
                __asm__ volatile("" ::: "memory");
            }
        }

        /* Small delay to ensure workers are spinning in the leaf */
        for (volatile int j = 0; j < 50; j++) ;

        /* Run GC — scan stacks and poison unreachable objects */
        gc_cycle();

        /* Release workers — they'll try to use their objects */
        workers_go = 1;

        /* Brief pause for workers to check their objects */
        usleep(100);

        /* Stop workers */
        workers_stop = 1;
        for (int i = 0; i < NUM_WORKERS; i++) {
            pthread_join(worker_threads[i], NULL);
        }

        total_corruptions += worker_crashes;

        if (worker_crashes > 0) {
            printf("  Cycle %d: %d corruptions detected\n", cycle, worker_crashes);
        }

        if (cycle > 0 && cycle % 500 == 0) {
            printf("  ... %d/%d cycles, %d total corruptions\n",
                   cycle, GC_CYCLES, total_corruptions);
        }
    }

    printf("\n=== Results ===\n");
    printf("GC cycles:    %d\n", GC_CYCLES);
    printf("Corruptions:  %d\n", total_corruptions);

    if (total_corruptions > 0) {
        printf("\nFAILED: GC missed %d heap pointers in the red zone.\n", total_corruptions);
        if (M3_STACK_ADJUST == 0) {
            printf("Recompile with -DM3_STACK_ADJUST=128 to verify the fix.\n");
        }
        return 1;
    } else {
        printf("\nPASSED: No corruptions detected.\n");
        if (M3_STACK_ADJUST == 0) {
            printf("(Bug may not have triggered — pointer stayed in registers.)\n");
        }
        return 0;
    }
}

#endif /* __APPLE__ && __arm64__ */
