# Porting CM3 to AMD64_DARWIN (x86_64 macOS)

This document describes the work done to bootstrap an `AMD64_DARWIN`
CM3 compiler on macOS, starting from an existing native `ARM64_DARWIN`
installation on Apple Silicon and producing a self-hosted x86_64 compiler
that runs under Rosetta 2.

**See also:**
- [`README-ARM64_DARWIN.md`](README-ARM64_DARWIN.md) — bootstrapping the native Apple Silicon target
- [`README-ex_stack.md`](README-ex_stack.md) — full technical reference for the C++ zero-cost EH implementation shared by both Darwin targets

---

## Starting Point

CM3 was already installed at `~/cm3` as a native `ARM64_DARWIN` binary on an
Apple Silicon Mac.  The canonical source tree is the git repository at
`~/git/cm3`.

The goal was a fully self-hosted `AMD64_DARWIN` CM3 compiler installed at
`~/cm3-AMD64_DARWIN`, with both the C backend and `ex_stack` support working
correctly under Rosetta 2.

---

## Issues Encountered and Fixed

### 1. CM3 target not overridable via `M3CONFIG` alone

The CM3 startup sequence in `m3-sys/m3quake/src/MxConfigC.m3` calls
`uname()` first and sets `HOST` to whatever the OS reports.  On Apple
Silicon, `uname` returns `arm64`, so `HOST = ARM64_DARWIN`.  The
`DefineIfNotDefined` call for `TARGET` runs before `cm3.cfg` is read,
so the usual `if not defined("TARGET")` guard in `cm3.cfg` is always
false — `M3CONFIG` alone cannot change `TARGET` during a cross-build.

**Fix**: Pass `-DTARGET=AMD64_DARWIN` explicitly on the `cm3` command
line, via the `BUILDARGS` environment variable:

```sh
export BUILDARGS="-DTARGET=AMD64_DARWIN"
```

This is required for the ARM64 cm3 cross-building for AMD64.  In
addition, the installed `~/cm3-AMD64_DARWIN/bin/cm3.cfg` must set:

```quake
TARGET = "AMD64_DARWIN"
```

**unconditionally**, with no `if not defined("TARGET")` guard.
Otherwise a cross-build launched from the ARM64 compiler can leave
`TARGET` stuck at `ARM64_DARWIN`.

### 2. `EXTENDED` float type: AMD64_DARWIN is 64-bit, not 128-bit

The Modula-3 type `EXTENDED` historically maps to x87 extended
precision on x86, and CM3 often models this via `_Float128` on
platforms that have it.  That is correct for `AMD64_LINUX`, but
**not** for `AMD64_DARWIN`: Apple clang on x86_64 macOS does not
support `_Float128`, so `EXTENDED` must be treated as a 64-bit
`double`, exactly as on `ARM64_DARWIN`.

This was the root cause of the long-standing cross-build failure:

```
bad version stamps: M3C.m3 / Target.Real mismatch
```

when compiling `AMD64_DARWIN` from an ARM64 host.

The float `m3makefile` logic was updated so that `AMD64_DARWIN`
selects `ext64` instead of `ext128`:

#### `m3-libs/m3core/src/float/IEEE/m3makefile`
```diff
-if equal (TARGET_OS, "NT") and not IsBackendLlvm ()
-  include_dir ("ext64")
-else
-  include_dir ("ext128")
-end
+if equal (TARGET, "AMD64_LINUX")
+  include_dir ("ext128")
+else
+  include_dir ("ext64")
+end
```

#### `m3-libs/m3core/src/float/IEEE-le/m3makefile`
Same change (selects `ExtendedRep.i3`).

#### `m3-libs/libm3/src/random/IEEE/m3makefile`
Same change (selects random float implementation).

### 3. `Target.Extended` size mismatch between compiler and runtime

`m3-sys/m3middle/src/Target.m3` sets `Target.Extended.size` at runtime
when the compiler initialises for a given target.  The default is 128
bits.  An override to 64 bits existed only for `WIN32`.

To make `AMD64_DARWIN` match the new `ext64` runtime layout,
`Target.m3` was refined so that `AMD64_DARWIN` is explicitly treated
as 64-bit `EXTENDED`:

```diff
-    IF (Text.Equal(OS_name, "WIN32") OR
-        TextUtils.StartsWith(system, "ARM64")) AND NOT LlvmSet THEN
+    IF (Text.Equal(OS_name, "WIN32") OR
+        TextUtils.StartsWith(system, "ARM64") OR
+        Text.Equal(system, "AMD64_DARWIN")) AND NOT LlvmSet THEN
       (* treat EXTENDED AS LONGREAL: Windows (no x87 ext), ARM64 (no _Float128),
          and AMD64_DARWIN (Apple Clang x86-64 does not support _Float128) *)
       Extended := Float_type{ ..., 64, 64, 8, ... };
```

### 4. Apple clang lacks `_Float128` and quad-precision math functions on x86_64 macOS too

`M3C.m3` generates a C++ preamble containing:

```c
# if defined(_MSC_VER)
typedef double EXTENDED;
# else
typedef _Float128 EXTENDED;
# endif
```

That is wrong on `AMD64_DARWIN`, because Apple clang x86_64 does not
support `_Float128`.  It also lacks `truncq`, `floorq`, `ceilq`, and
`llroundq`.

The Darwin-specific fix in `m3-sys/m3back/src/M3C.m3` was generalized
to cover both Apple Silicon and x86_64 macOS:

```diff
-# if defined(_MSC_VER) || defined(__aarch64__)
+# if defined(_MSC_VER) || defined(__aarch64__) || (defined(__x86_64__) && defined(__APPLE__))
 typedef double EXTENDED;
```

The same condition was added to all four conversion helper guards:

```diff
-#if defined(_WIN32) || defined(__aarch64__)
+#if defined(_WIN32) || defined(__aarch64__) || (defined(__x86_64__) && defined(__APPLE__))
 static INT64 __stdcall m3_round(EXTENDED f) { return llround(f); }
 ...
```

No compatibility header is needed.  The generated code now handles Apple
Darwin directly.

### 5. `Infinity` and `NaN` identifier conflicts

Older CM3-generated C++ sometimes uses `Infinity` and `NaN` as identifiers
without defining them first.  Newer generated code emits explicit preamble
definitions.

To allow old and new generated code to coexist during bootstrap,
`m3-sys/m3back/src/M3C.m3` now guards both definitions:

```diff
+#ifndef Infinity
 #define Infinity INFINITY
+#endif
+#ifndef NaN
 #define NaN NAN
+#endif
```

This fix applies equally to `AMD64_DARWIN` and `ARM64_DARWIN`.

### 6. `M3_BACKEND_MODE = "C"` is required, and must be set before includes

`Darwin.common` defaults to backend mode `"3"` (the native x86 assembler
backend).  That is not what we want here:

- the C backend is more portable for this port
- `ex_stack` requires the C backend
- the Apple-Darwin `EXTENDED` fixes live in `M3C.m3`

So `m3-sys/cminstall/src/config/AMD64_DARWIN` must set:

```quake
M3_BACKEND_MODE = "C"
```

**before** including `AMD64.common` and `Darwin.common`, so the guard in
`Darwin.common` sees the correct value.

### 7. `M3_PARALLEL_BACK = 1` is required under Rosetta 2, and must be set after includes

CM3 uses `M3_PARALLEL_BACK` to control how many parallel threads are spawned
to compile generated C++ files.  The default is 20.

Under Rosetta 2, `AMD64_DARWIN` builds can hang in thread creation and
pthread synchronization when too many translated threads are spawned at once.
For reliable bootstrap, the value must be:

```quake
M3_PARALLEL_BACK = 1
```

On `AMD64_DARWIN`, this assignment must come **after** the config includes,
because `AMD64.common` unconditionally sets it back to 20.

### 8. `ex_stack` runtime support had to be ported to AMD64_DARWIN

`AMD64_DARWIN` now has full native stack-walking exception handling support.

The following files were added:

- `m3-libs/m3core/src/runtime/AMD64_DARWIN/RTMachine.i3`
- `m3-libs/m3core/src/runtime/AMD64_DARWIN/RTStackC.c`
- `m3-libs/m3core/src/runtime/AMD64_DARWIN/m3makefile`

The implementation is effectively identical to `ARM64_DARWIN` because Apple's
libunwind API is architecture-independent at this level.  The
`HAS_STACK_WALKER` table in `m3core/src/runtime/m3makefile` was updated to
include:

```quake
"AMD64_DARWIN" : TRUE,
```

All `ex_stack` verification tests pass on `AMD64_DARWIN` under Rosetta 2.

### 9. `SYSTEM_LIBS{"UNWIND"} = []` is required

On macOS, libunwind is part of `libSystem.B.dylib`, so no extra link flags
are needed.  However, the runtime build still calls `import_sys_lib("UNWIND")`
when stack walking is enabled, so the config must define:

```quake
SYSTEM_LIBS{"UNWIND"} = []
```

### 10. `cm3 -ship` does not overwrite the running cm3 binary

`program ("cm3")` in `m3-sys/cm3/src/m3makefile` tells CM3 to build the
binary but not auto-ship it.  The ship step only installs interface and
library files.

**Workaround**: After each bootstrap stage, manually copy the new binary:

```sh
cp m3-sys/cm3/src/AMD64_DARWIN/cm3 ~/cm3-AMD64_DARWIN/bin/cm3
```

---

## Bootstrap Process

### Prerequisites

- Apple Silicon Mac with Rosetta 2 installed
- Existing native `ARM64_DARWIN` CM3 installation at `~/cm3`
- CM3 source tree: git repository at `~/git/cm3`
- Xcode Command Line Tools

### Stage 1 — Cross-compile for AMD64 using ARM64 cm3

The ARM64 cm3 binary is told to target `AMD64_DARWIN` by passing
`-DTARGET=AMD64_DARWIN` explicitly.

```sh
cd ~/git/cm3

export ROOT=$(pwd)
export CM3_TARGET=AMD64_DARWIN
export INSTALLROOT=$HOME/cm3-AMD64_DARWIN
export M3CONFIG=$HOME/cm3-AMD64_DARWIN/bin/cm3.cfg
export BUILDARGS="-DTARGET=AMD64_DARWIN"
export PATH=$HOME/cm3/bin:$PATH    # native ARM64 cm3

# Build all "front" packages (m3core through cm3)
./scripts/do-cm3-front.sh buildlocal

# Ship libraries to ~/cm3-AMD64_DARWIN/
./scripts/do-cm3-front.sh ship

# Manually install the cm3 binary (ship doesn't overwrite it)
cp m3-sys/cm3/src/AMD64_DARWIN/cm3 ~/cm3-AMD64_DARWIN/bin/cm3
```

Verify:

```sh
file ~/cm3-AMD64_DARWIN/bin/cm3
# → Mach-O 64-bit executable x86_64

arch -x86_64 ~/cm3-AMD64_DARWIN/bin/cm3 -version
# → host: AMD64_DARWIN, target: AMD64_DARWIN
```

### Stage 2 — Cross-compile again using ARM64 cm3, but with `buildglobal`

With the Stage 1 AMD64 binary in place, rebuild everything again using the
ARM64 compiler as the driver, but now with build+ship in one step.

```sh
cd ~/git/cm3

find . -name "AMD64_DARWIN" -type d | xargs rm -rf

export ROOT=$(pwd)
export CM3_TARGET=AMD64_DARWIN
export INSTALLROOT=$HOME/cm3-AMD64_DARWIN
export M3CONFIG=$HOME/cm3-AMD64_DARWIN/bin/cm3.cfg
export BUILDARGS="-DTARGET=AMD64_DARWIN"
export PATH=$HOME/cm3/bin:$PATH    # still using ARM64 cm3 as cross-compiler

./scripts/do-cm3-front.sh buildglobal

cp m3-sys/cm3/src/AMD64_DARWIN/cm3 ~/cm3-AMD64_DARWIN/bin/cm3
```

### Stage 3 — Self-hosted build using the new AMD64 cm3 under Rosetta 2

Now switch to the `AMD64_DARWIN` compiler itself and rebuild under Rosetta.

Because `~/cm3-AMD64_DARWIN/bin/cm3.cfg` sets `TARGET = "AMD64_DARWIN"`
unconditionally, no `BUILDARGS` override is needed here.

```sh
cd ~/git/cm3

find . -name "AMD64_DARWIN" -type d | xargs rm -rf

export ROOT=$(pwd)
export CM3_TARGET=AMD64_DARWIN
export INSTALLROOT=$HOME/cm3-AMD64_DARWIN
export PATH=$HOME/cm3-AMD64_DARWIN/bin:$PATH

arch -x86_64 ./scripts/do-cm3-front.sh buildglobal

cp m3-sys/cm3/src/AMD64_DARWIN/cm3 ~/cm3-AMD64_DARWIN/bin/cm3
```

### Bootstrap stability

The Stage 2 and Stage 3 `cm3` binaries were identical in size:

- Stage 2: **7,437,432 bytes**
- Stage 3: **7,437,432 bytes**

This confirms the bootstrap is stable.

---

## Summary of Changes

| File | Change |
|------|--------|
| `m3-sys/m3middle/src/Target.m3` | `Target.Extended` set to 64-bit for `AMD64_DARWIN` as well as ARM64 targets |
| `m3-sys/m3back/src/M3C.m3` | `typedef double EXTENDED` and simple conversion helpers for `(defined(__x86_64__) && defined(__APPLE__))`; `#ifndef` guards on `Infinity`/`NaN` |
| `m3-libs/m3core/src/float/IEEE/m3makefile` | Select `ext64/` for `AMD64_DARWIN` |
| `m3-libs/m3core/src/float/IEEE-le/m3makefile` | Select `ext64/` for `AMD64_DARWIN` |
| `m3-libs/libm3/src/random/IEEE/m3makefile` | Select `ext64/` for `AMD64_DARWIN` |
| `m3-libs/m3core/src/runtime/m3makefile` | Added `"AMD64_DARWIN" : TRUE` to `HAS_STACK_WALKER` |
| `m3-libs/m3core/src/runtime/AMD64_DARWIN/RTMachine.i3` | New `AMD64_DARWIN` stack-walker runtime description |
| `m3-libs/m3core/src/runtime/AMD64_DARWIN/RTStackC.c` | New libunwind-based stack walker and `RTStack__ThrowM3Exc` implementation |
| `m3-libs/m3core/src/runtime/AMD64_DARWIN/m3makefile` | New runtime package file for the target |
| `m3-sys/cminstall/src/config/AMD64_DARWIN` | New source config; `M3_BACKEND_MODE = "C"`; `M3_USE_STACK_WALKER = TRUE`; `M3_PARALLEL_BACK = 1`; `SYSTEM_LIBS{"UNWIND"} = []` |
| `~/cm3-AMD64_DARWIN/bin/cm3.cfg` | `TARGET = "AMD64_DARWIN"` must be unconditional |

---

## Notes on `m3cc`

The `m3cc` package (GCC-based backend) is not used on `AMD64_DARWIN`.
CM3 uses its own C backend (`m3back`, `M3_BACKEND_MODE = "C"`) on Darwin.

Any `m3cc`-specific failures during this port are expected and harmless.
```

---
