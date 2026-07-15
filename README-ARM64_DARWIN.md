# Porting CM3 to ARM64_DARWIN (Apple Silicon)

This document describes the work done to bootstrap a native ARM64_DARWIN
CM3 compiler on Apple Silicon Macs (M1/M2/M3/…), starting from an existing
AMD64_DARWIN installation running under Rosetta 2.

**See also:**
- [`README-AMD64_DARWIN.md`](README-AMD64_DARWIN.md) — bootstrapping the x86_64 Darwin target from ARM64_DARWIN
- [`README-ex_stack.md`](README-ex_stack.md) — full technical reference for the C++ zero-cost EH implementation shared by both Darwin targets

---

## Starting Point

CM3 was already installed at `~/cm3` as an AMD64_DARWIN binary.  On Apple
Silicon this binary runs transparently under Rosetta 2, which translates
x86_64 instructions to ARM64 at runtime.  The canonical source tree
is the git repository at `~/git/cm3`.

The goal was a fully self-hosted native ARM64 CM3 compiler installed at
`~/cm3`.

---

## Issues Encountered and Fixed

### 1. ARM64 target not auto-detected by build scripts

`scripts/sysinfo.sh` handles the Darwin case by inspecting `uname -p`.  It
handles `powerpc` and `i386` but has no branch for `arm` or `arm64`, so it
falls through without setting `CM3_TARGET`.  This causes the wrapper scripts
(`pkgmap.sh`, `do-cm3-front.sh`) to error with "does not know about uname -a".

**Fix**: Set `CM3_TARGET=ARM64_DARWIN` explicitly in the environment before
invoking any build script.

### 2. CM3 target not overridable via `M3CONFIG` alone

The CM3 startup sequence in `m3-sys/m3quake/src/MxConfigC.m3` calls
`uname()` first and sets `HOST` to whatever the OS reports.  Under Rosetta,
`uname` returns `x86_64`, so `HOST = AMD64_DARWIN`.  The `DefineIfNotDefined`
call for `TARGET` runs before `cm3.cfg` is read, so the `if not
defined("TARGET")` guard in `cm3.cfg` is always false — `M3CONFIG` alone
cannot change `TARGET`.

**Fix**: Pass `-DTARGET=ARM64_DARWIN` explicitly on the `cm3` command line,
via the `BUILDARGS` environment variable that `pkgcmds.sh` appends to the
build command:

```sh
export BUILDARGS="-DTARGET=ARM64_DARWIN"
```

This is only required for the AMD64 cm3 cross-building for ARM64.  Once the
native ARM64 binary is in use, `uname()` returns `arm64` and `TARGET` is set
correctly from `cm3.cfg` without any override.

### 3. `EXTENDED` float type: ARM64_DARWIN uses `ext64`, not `ext128`

The Modula-3 type `EXTENDED` historically maps to the x87 80-bit extended
precision type on x86.  CM3 implements this via one of two subdirectories:

- `ext128/` — 128-bit EXTENDED using GCC's `_Float128` type
- `ext64/` — EXTENDED = LONGREAL (64-bit double)

ARM64 has no x87 unit and no hardware support for any precision beyond 64-bit
double.  Apple clang also does not provide `_Float128` on Darwin.  The
existing condition that selected `ext64` only covered Windows
(`TARGET_OS = "NT"`); all other targets including ARM64 fell into `ext128`.

The float `m3makefile` logic was updated so that `ARM64_DARWIN` and
`AMD64_DARWIN` both select `ext64`:

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

### 4. `Target.Extended` size mismatch between compiler and runtime

`m3-sys/m3middle/src/Target.m3` sets `Target.Extended.size` at runtime when
the compiler initialises for a given target.  The default is 128 bits (quad
precision).  An override to 64 bits existed only for `WIN32`.  When the new
ARM64 cm3 binary compiled with `ext64` in m3core but the compiler still
believed `EXTENDED` was 128 bits, m3front detected the size mismatch and
errored.

The original ARM64-only fix was refined so that `AMD64_DARWIN` is also
handled explicitly:

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

### 5. Apple clang lacks `_Float128` and quad-precision math functions

`M3C.m3` generates a C++ preamble containing:

```c
# if defined(_MSC_VER)
typedef double EXTENDED;
# else
typedef _Float128 EXTENDED;
# endif
```

Apple clang on ARM64 does not support `_Float128`.  It also lacks the
quad-precision math functions `truncq`, `floorq`, `ceilq`, and `llroundq`.
Additionally, the conversion helper functions (`m3_round`, `m3_trunc`,
`m3_floor`, `m3_ceil`) used `#ifdef _WIN32` to select the simple
`llround(f)` / `trunc(f)` / `floor(f)` / `ceil(f)` forms, falling back to
the quad-precision `*q()` versions elsewhere.

The Darwin-specific fix now covers both Apple Silicon and x86_64 macOS:

```diff
-# if defined(_MSC_VER)
+# if defined(_MSC_VER) || defined(__aarch64__) || (defined(__x86_64__) && defined(__APPLE__))
 typedef double EXTENDED;
```

The same condition was added to all four conversion helper guards:

```diff
-#if defined(_WIN32)
+#if defined(_WIN32) || defined(__aarch64__) || (defined(__x86_64__) && defined(__APPLE__))
 static INT64 __stdcall m3_round(EXTENDED f) { return llround(f); }
 ...
```

### 6. `Infinity` and `NaN` identifier conflicts — no compat header required anymore

Older pre-built AMD64 CM3 binaries generate C++ files that use `Infinity`
and `NaN` as identifiers without ever `#define`-ing them.  Newer generated
code emits explicit definitions in the preamble.

The fix in `m3-sys/m3back/src/M3C.m3` is to guard both definitions:

```diff
+#ifndef Infinity
 #define Infinity INFINITY
+#endif
+#ifndef NaN
 #define NaN NAN
+#endif
```

With the `#ifndef` guards in place, and with the Apple-Darwin
`typedef double EXTENDED` / simple `m3_round`-family guards now covering
both `__aarch64__` and `(__x86_64__ && __APPLE__)`, the old injected
compatibility header is no longer needed.

The following cleanup was done:

- `~/cm3/include/m3_arm64_compat.h` was deleted
- `-include m3_arm64_compat.h` was removed from `~/cm3/bin/config/ARM64_DARWIN`
- the source config `m3-sys/cminstall/src/config/ARM64_DARWIN` now exists and
  does **not** reference any compat header

### 7. Rosetta 2 pthread deadlock with parallel compilation

CM3 uses `M3_PARALLEL_BACK` to control how many parallel threads are spawned
to compile generated C++ files.  The default is 20.

Under Rosetta 2, `pthread_create` from AMD64 translated code acquires
Rosetta's internal JIT/thread-registration lock.  Spawning many threads
simultaneously can deadlock this lock — the build stalls indefinitely.

**Fix** in the `ARM64_DARWIN` config:

```quake
M3_PARALLEL_BACK = 1
```

This forces sequential C compilation during the Rosetta-driven bootstrap
stage.  The source config file `m3-sys/cminstall/src/config/ARM64_DARWIN`
now exists and contains:

- `M3_BACKEND_MODE = "C"`
- `M3_USE_STACK_WALKER = TRUE`
- `M3_PARALLEL_BACK = 1`
- `SYSTEM_LIBS{"UNWIND"} = []`

It does not reference any compat header.

### 8. `cm3 -ship` does not overwrite the running cm3 binary

`program ("cm3")` in `m3-sys/cm3/src/m3makefile` (lowercase, not
`Program`) tells CM3 to build the binary but not auto-ship it.  The ship
step only installs interface and library files.  This is intentional — it
prevents accidentally overwriting the running compiler mid-bootstrap.

**Workaround**: After each bootstrap stage, manually copy the new binary:

```sh
cp m3-sys/cm3/src/ARM64_DARWIN/cm3 ~/cm3/bin/cm3
```

---

## Bootstrap Process

### Prerequisites

- Apple Silicon Mac (M1/M2/M3/…)
- Existing AMD64_DARWIN CM3 installation at `~/cm3` (runs under Rosetta 2)
- CM3 source tree: git repository at `~/git/cm3`
- Xcode Command Line Tools (provides `clang`, `ar`, etc.)

If you want to preserve the old Rosetta-based compiler tree, copy `~/cm3`
elsewhere before beginning.  The current bootstrap installs directly to
`~/cm3`; it does **not** use a staging directory such as
`~/cm3-ARM64_DARWIN`.

### Stage 1 — Cross-compile for ARM64 using AMD64 cm3 under Rosetta

The AMD64 cm3 binary is told to target `ARM64_DARWIN` by passing
`-DTARGET=ARM64_DARWIN` on the command line.  The build now installs
directly into `~/cm3`.

```sh
cd ~/git/cm3

export ROOT=$(pwd)
export CM3_TARGET=ARM64_DARWIN
export INSTALLROOT=$HOME/cm3
export BUILDARGS="-DTARGET=ARM64_DARWIN"
export PATH=$HOME/cm3/bin:$PATH    # AMD64 cm3 under Rosetta

# Build all "front" packages (m3core through cm3)
./scripts/do-cm3-front.sh buildlocal

# Ship libraries to ~/cm3
./scripts/do-cm3-front.sh ship

# Manually install the cm3 binary (ship doesn't overwrite it)
cp m3-sys/cm3/src/ARM64_DARWIN/cm3 ~/cm3/bin/cm3
```

Verify:

```sh
file ~/cm3/bin/cm3
# → Mach-O 64-bit executable arm64

~/cm3/bin/cm3 -version
# → host: ARM64_DARWIN, target: ARM64_DARWIN
```

### Stage 2 — Self-hosted build using the new native ARM64 cm3

With the Stage 1 ARM64 binary in place, rebuild everything natively.  No
`M3CONFIG` override or `-DTARGET` flag is needed: the native binary calls
`uname()` → `arm64` → `ARM64_DARWIN` automatically.

```sh
cd ~/git/cm3

# Clean stale build artifacts
find . -name "ARM64_DARWIN" -type d | xargs rm -rf

export ROOT=$(pwd)
export CM3_TARGET=ARM64_DARWIN
export INSTALLROOT=$HOME/cm3
export PATH=$HOME/cm3/bin:$PATH   # native ARM64 cm3

./scripts/do-cm3-front.sh buildglobal   # build + ship in one step

# Manually install the cm3 binary
cp m3-sys/cm3/src/ARM64_DARWIN/cm3 ~/cm3/bin/cm3
```

### Stage 3 — Stability verification (optional)

Repeat Stage 2 identically to confirm the output is stable.

```sh
find . -name "ARM64_DARWIN" -type d | xargs rm -rf
./scripts/do-cm3-front.sh buildglobal
cp m3-sys/cm3/src/ARM64_DARWIN/cm3 ~/cm3/bin/cm3
```

The Stage 2 and Stage 3 binaries may differ slightly in size due to embedded
`__DATE__`/`__TIME__` strings in `Version.c`, but the executable code is
identical.

---

## Summary of Changes

| File | Change |
|------|--------|
| `m3-sys/m3middle/src/Target.m3` | `Target.Extended` set to 64-bit for ARM64 and `AMD64_DARWIN` targets |
| `m3-sys/m3back/src/M3C.m3` | `typedef double EXTENDED` for `__aarch64__` and `(defined(__x86_64__) && defined(__APPLE__))`; `#ifndef` guards on `Infinity`/`NaN`; same condition in all four conversion helpers |
| `m3-libs/m3core/src/float/IEEE/m3makefile` | Select `ext64/` for ARM64 and `AMD64_DARWIN` |
| `m3-libs/m3core/src/float/IEEE-le/m3makefile` | Select `ext64/` for ARM64 and `AMD64_DARWIN` |
| `m3-libs/libm3/src/random/IEEE/m3makefile` | Select `ext64/` for ARM64 and `AMD64_DARWIN` |
| `m3-sys/cminstall/src/config/ARM64_DARWIN` | New source config; `M3_BACKEND_MODE = "C"`; `M3_USE_STACK_WALKER = TRUE`; `M3_PARALLEL_BACK = 1`; `SYSTEM_LIBS{"UNWIND"} = []`; no compat header reference |
| `~/cm3/bin/config/ARM64_DARWIN` | Removed the old `-include m3_arm64_compat.h` line |

---

## Notes on `m3cc`

The `m3cc` package (GCC-based backend) always fails on ARM64_DARWIN with:

```
GNU platform is not known for ARM64_DARWIN
```

This is expected and harmless.  CM3 uses its own C backend (`m3back`,
`M3_BACKEND_MODE = "C"`) on Darwin, not the GCC backend.

---
