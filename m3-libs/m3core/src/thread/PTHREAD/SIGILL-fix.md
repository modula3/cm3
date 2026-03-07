# Fix: ARM64 Red Zone GC Stack Scan Bug

## Bug Summary

On macOS ARM64, `ThreadApple.c` defines `M3_STACK_ADJUST=0` for `__arm64__`.
The ARM64 ABI allows leaf functions to use 128 bytes below SP (the "red zone")
without adjusting SP.  The GC scans `[SP, stack_top)` and misses any heap
pointers stored in `[SP-128, SP)`.

`test-redzone.c` confirms this: 16 values written to the red zone are
completely invisible to the GC scan range.

`test-gc-redzone.c` goes further: it simulates the full GC cycle (suspend
workers, scan stacks, "collect" unreachable objects) and demonstrates that
with `M3_STACK_ADJUST=0`, heap pointers stored only in the red zone are
missed by the scan, and the objects are prematurely collected.  Results:

- **M3_STACK_ADJUST=0**: ~80% corruption rate (1626/2000 cycles corrupt)
- **M3_STACK_ADJUST=128**: 0% corruption rate (5000/5000 cycles clean)

If the GC misses a heap pointer that is only live in the red zone, it may
collect the object.  The subsequent use-after-free can manifest as SIGILL
(PAC authentication failure on corrupted pointers) or SIGSEGV.

`test-gc-m3/` is a Modula-3 stress test (8 threads x 100K iterations x
20-node linked lists).  It doesn't currently trigger the bug because the
C backend generates non-leaf functions for allocation-heavy code, but it
serves as a regression test after applying the fix.

## Fix 1: M3_STACK_ADJUST (critical, one-line fix)

**File:** `m3-libs/m3core/src/thread/PTHREAD/ThreadApple.c`, line 106

```c
// Before:
#define M3_STACK_ADJUST            0

// After:
#define M3_STACK_ADJUST            128
```

This makes `ProcessStopped` scan from `SP-128` instead of `SP`, covering
the full red zone.  This matches what x86_64 already does (line 91).

Note: PPC uses `C_RED_ZONE` from `<architecture/ppc/cframe.h>`.  ARM64
doesn't have an equivalent system header constant, so we hardcode 128
(defined by the ARM64 Procedure Call Standard, section 6.2.3).

## Fix 2: SIGILL signal handler (diagnostic improvement)

**File:** `m3-libs/m3core/src/runtime/POSIX/RTSignalC.c`, line 119

Add SIGILL to the handler table so crashes produce a backtrace instead of
silent termination:

```c
Handlers[] =
{
    { SIGHUP,  Shutdown },
    { SIGINT,  Interrupt },
    { SIGQUIT, Quit },
    { SIGSEGV, SegV },
    { SIGPIPE, IgnoreSignal },
    { SIGTERM, Shutdown },
#if !defined (__DJGPP__) || defined (SIGBUS)
    { SIGBUS,  SegV },
#endif
    { SIGILL,  SegV },    /* <-- ADD: PAC failures on ARM64 raise SIGILL */
};
```

## Fix 3: m3_fence (correctness on weak memory architectures)

**File:** `m3-sys/m3back/src/M3C.m3`, line 4883

The non-MSVC fence is a no-op:
```c
static void __stdcall m3_fence(void){}
```

On ARM64's weak memory model this is incorrect.  Replace with:
```c
#define m3_fence() __atomic_thread_fence(__ATOMIC_SEQ_CST)
```

This affects correctness of any code relying on memory ordering between
threads (e.g., the `inCritical` flag in `RTAllocator.m3`).

## Testing Plan

### Unit test: `test-redzone.c`
Already written.  Demonstrates that red zone data is invisible to GC scan.
Run before and after fix to confirm.

```sh
cc -O1 -o test-redzone test-redzone.c -lpthread
./test-redzone       # FAIL before fix
# apply fix 1
./test-redzone       # should still show sentinels in red zone
                     # (the test shows the ABI behavior, not the GC fix directly)
```

### Integration test: rebuild CM3 and stress-test cspc
After applying Fix 1:

```sh
# Rebuild m3core
cd cm3
scripts/do-pkg.sh buildship m3core

# Stress-test cspc with the MiniMIPS build (known to trigger SIGILL)
cd intel-async/async-toolkit/m3utils/m3utils/csp/mips
for i in $(seq 1 100); do
    bash build.sh 2>&1 | tail -1
    if [ $? -ne 0 ]; then
        echo "FAILED on attempt $i"
        break
    fi
done
echo "Completed 100 builds"
```

Before fix: expect ~5-15% crash rate (~5-15 failures in 100 runs).
After fix: expect 0 failures.

### Regression: full CM3 test suite
```sh
cd cm3/m3-sys/m3tests
cm3 -DHTML
# Check m3tests-results.xml for regressions
```

### Regression: cspbuild test suite
```sh
cd intel-async/async-toolkit/m3utils/m3utils/csp
# Run the 26-test cspbuild suite
make test
```

## Risk Assessment

**Fix 1** is safe.  It only makes the GC scan a larger region (128 extra
bytes per suspended thread).  The cost is negligible — scanning 16 extra
pointer-sized words per thread per GC cycle.  False positives (non-pointer
values in the red zone that look like heap addresses) are harmless — they
just prevent collection of those objects for one cycle (conservative GC
already tolerates this).

**Fix 2** is safe.  Adding a signal handler for SIGILL only improves
diagnostics.  The handler calls `MsgPCSegV` which prints the faulting PC
and aborts — same behavior as SIGSEGV/SIGBUS.

**Fix 3** has broader impact.  The fence affects all generated code on all
non-MSVC platforms.  It should be tested thoroughly.  However, the current
no-op is demonstrably wrong on ARM64 — it's surprising it doesn't cause
more problems.  Recommend applying Fix 3 separately with its own testing.
