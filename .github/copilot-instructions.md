# CM3 (Critical Mass Modula-3) — Copilot Instructions

## What This Repo Is

This is the source tree for [Critical Mass Modula-3](https://github.com/modula3/cm3), a self-hosting Modula-3 compiler and standard library collection. The compiler (`cm3`) is written in Modula-3. Building from source requires a working `cm3` binary (bootstrapped from a release tarball).

---

## Repository Layout

The tree is divided into package collections:
- **`m3-sys/`** — compiler core: `m3front` (parser/front-end), `m3back` (C code generator), `m3middle` (IR), `m3linker`, `m3quake` (build language), `cm3` (compiler driver), `cminstall` (platform configs)
- **`m3-libs/m3core`** — runtime library (threads, GC, exceptions, float types)
- **`m3-libs/libm3`** — standard library
- **`bootstrap/`** — cmake-based bootstrap tree (pre-compiled `.cpp` files for cold-start builds)
- **`scripts/`** — build orchestration; `concierge.py` is the primary build tool
- **`m3-sys/m3tests/`** — compiler validation test suite

Each package has a `src/m3makefile` written in Quake (cm3's build language). Packages are topologically ordered in `scripts/pkginfo.txt`.

---

## Build System: `concierge.py`

`scripts/concierge.py` is the main build tool. All commands are run from the repo root.

```sh
# Upgrade compiler (rebuild m3core → ... → cm3 in correct order)
scripts/concierge.py upgrade

# Upgrade compiler + all libraries
scripts/concierge.py full-upgrade

# Build a specific set of packages
scripts/concierge.py buildship m3core libm3

# Build full distribution tarball
scripts/concierge.py make-dist --target ARM64_DARWIN

# Prepare a bootstrap (C sources for cold-start)
scripts/concierge.py make-bootstrap
```

The legacy shell scripts in `scripts/` (e.g., `do-cm3-front.sh`) wrap `cm3` directly:

```sh
# Build and install the compiler frontend packages
./scripts/do-cm3-front.sh buildglobal

# Package actions: build | buildglobal | buildlocal | buildship | clean | realclean | ship
```

**`cm3 -ship` does NOT overwrite the running `cm3` binary.** After any rebuild of `m3-sys/cm3`, manually copy the binary:

```sh
cp m3-sys/cm3/ARM64_DARWIN/cm3 ~/cm3/bin/cm3
# (substitute your target name for ARM64_DARWIN)
```

---

## Running the Test Suite

```sh
cd m3-sys/m3tests
cm3 -DHTML
```

Results are written to `m3tests-results.xml` (JUnit format).

## Building a Standalone Test Program

Create a directory with your `Main.m3` and an `m3makefile`:

```quake
import("libm3")
implementation("Main")
program("test")
```

Then build and run:

```sh
cd /tmp/mytest
cm3 -build
./ARM64_DARWIN/test   # substitute your target name
```

Key `m3makefile` directives:
- `implementation("Foo")` — declares `Foo.m3` as a module implementation
- `interface("Foo")` — declares `Foo.i3` as an interface
- `import("libm3")` — imports a shipped library package
- `program("name")` — links an executable (note: lowercase; `Program()` has a different signature)

`cm3 -ship` is not needed for standalone test programs — just run the binary directly from the build directory.

---

## Platform Configs (Quake)

Each target has a config file at `m3-sys/cminstall/src/config/<TARGET>`. These are Quake scripts. Key variables:

| Variable | Purpose |
|---|---|
| `M3_BACKEND_MODE` | `"C"` = C code generator, `"3"` = native x86 assembler |
| `M3_USE_STACK_WALKER` | `TRUE` = use C++ EH (`ex_stack`); unset = use setjmp (`ex_frame`) |
| `M3_PARALLEL_BACK` | Parallel C compilations (set to `1` under Rosetta to avoid deadlock) |
| `SYSTEM_LIBS{"UNWIND"}` | Link flags for libunwind (empty `[]` on Darwin; needed on Linux) |

**Order matters**: `M3_BACKEND_MODE` and `M3_USE_STACK_WALKER` must be set **before** any `include(...)` calls so guards in included files fire correctly. On `AMD64_DARWIN`, `M3_PARALLEL_BACK = 1` must come **after** includes because `AMD64.common` resets it.

---

## Exception Handling: `ex_frame` vs `ex_stack`

CM3 has two EH implementations selected by `M3_USE_STACK_WALKER`:

- **`ex_frame`** (default): setjmp/longjmp. Every `TRY` pushes a `jmp_buf`. Portable but has overhead on every `TRY` entry.
- **`ex_stack`** (ARM64_DARWIN, AMD64_DARWIN, AMD64_LINUX): C++ zero-cost EH. `RAISE` calls `RTStack__ThrowM3Exc()`, which executes `throw _M3Exc{act}`. The C++ runtime unwinds via DWARF tables. Zero cost when no exception is raised.

`ex_stack` **requires** `M3_BACKEND_MODE = "C"`. The native x86 assembler backend does not implement `Start_try`/`End_try`/`Landing_pad`.

On Darwin, libunwind is part of `libSystem.B.dylib` — no extra link flags. On Linux, `libunwind-dev` must be installed separately.

The `ex_stack` selection is controlled by `m3-libs/m3core/src/runtime/m3makefile`:
```quake
readonly HAS_STACK_WALKER = {
  "AMD64_LINUX"  : TRUE,
  "AMD64_DARWIN" : TRUE,
  "ARM64_DARWIN" : TRUE,
}
```

---

## Bootstrap Tarballs

Bootstrap tarballs (`cm3-boot-<TARGET>-<VERSION>.tar.xz`) contain **pre-compiled `.cpp` files** generated from Modula-3 source by a prior compiler. The cmake build in `bootstrap/` compiles these `.cpp` files directly — it does **not** regenerate them from `.m3` sources.

This creates a chicken-and-egg constraint: to regenerate bootstrap `.cpp` files you need a working fixed compiler; to build the compiler you need the bootstrap. The proper workflow:

1. Extract old bootstrap, build it with cmake to get a compiler binary.
2. Use that binary with the fixed source tree to compile the packages.
3. Use `scripts/concierge.py make-bootstrap` to emit the new pre-compiled `.cpp` files.
4. Package and ship as a new bootstrap tarball.

The `make-release` CI workflow downloads a bootstrap from the GitHub release and runs `scripts/concierge.py upgrade` then `make-dist`. If the bootstrap `.cpp` files contain bugs, patching the `.m3` source alone does not fix CI — the bootstrap tarball itself must be regenerated and re-uploaded.

---

## Two-Phase Bootstrap: Transitioning to `ex_stack`

Enabling `ex_stack` on a target that previously used `ex_frame` requires two build phases because an old compiler binary (with old m3back statically linked) cannot compile `RTExStack.m3`.

The old m3back contains:
```
<* ASSERT NOT proc.is_RTException_Raise *>
```
which fires when the new `ex_stack` body of `RTException.Raise` (which calls `RTStack.ThrowM3Exc` inside a `RAISES ANY` scope) is compiled.

### Phase 1 — Build fixed compiler, still using `ex_frame`

**Goal**: new `cm3` binary with fixed m3back (noreturn detection, `start_try`/`end_try` generation), but still linked against the safe `ex_frame` runtime.

1. **Comment out** `M3_USE_STACK_WALKER = TRUE` in the platform config. This prevents `RTExStack.m3` from ever being compiled by the old binary.

2. Using the existing (old) `cm3`, rebuild in dependency order:
   ```sh
   cd m3-libs/m3core   && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-libs/libm3    && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3middle  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3linker  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3front   && cm3 -clean && cm3 -build && cm3 -ship   # TryStmt/LockStmt/TryFinStmt fixes
   cd m3-sys/m3quake   && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3objfile && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3back    && cm3 -clean && cm3 -build && cm3 -ship   # noreturn fix
   cd m3-sys/cm3       && cm3 -clean && cm3 -build && cm3 -ship
   ```

3. Install the new binary:
   ```sh
   cp m3-sys/cm3/<TARGET>/cm3 ~/cm3/bin/cm3
   ```

### Phase 2 — Rebuild with `ex_stack`, using Phase 1 binary

**Goal**: fully `ex_stack` `cm3` binary.

1. **Uncomment** `M3_USE_STACK_WALKER = TRUE` in the platform config.

2. Using the Phase 1 binary, rebuild in the same order:
   ```sh
   cd m3-libs/m3core   && cm3 -clean && cm3 -build && cm3 -ship   # now builds ex_stack runtime
   cd m3-libs/libm3    && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3middle  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3linker  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3front   && cm3 -clean && cm3 -build && cm3 -ship   # Compile1 path active
   cd m3-sys/m3quake   && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3objfile && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3back    && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/cm3       && cm3 -clean && cm3 -build && cm3 -ship
   ```

3. Install:
   ```sh
   cp m3-sys/cm3/<TARGET>/cm3 ~/cm3/bin/cm3
   ```

### Verification

After Phase 2, verify correctness with a test covering the five edge cases:

```modula3
MODULE Main;
IMPORT IO, OSError, Thread;
EXCEPTION LocalExc(TEXT);
BEGIN
  TRY RAISE LocalExc("hello") EXCEPT LocalExc(m) => IO.Put("caught: " & m & "\n") END;

  TRY
    TRY RAISE LocalExc("x") EXCEPT OSError.E => IO.Put("FAIL\n") END;
  EXCEPT LocalExc => IO.Put("reraise ok\n") END;

  VAR ran := FALSE;
  BEGIN
    TRY TRY RAISE LocalExc("f") FINALLY ran := TRUE END EXCEPT LocalExc => END;
    IF ran THEN IO.Put("finally ok\n") ELSE IO.Put("FAIL\n") END;
  END;

  VAR mu := NEW(MUTEX);
  BEGIN
    TRY LOCK mu DO RAISE LocalExc("lock") END EXCEPT LocalExc => IO.Put("lock ok\n") END;
  END;

  TRY
    Thread.Alert(Thread.Self()); Thread.AlertPause(0.0d0);
  EXCEPT Thread.Alerted => IO.Put("alerted ok\n") END;
END Main.
```

All five lines of output must appear. Cross-exception dispatch (the second test) is the most sensitive — a bug in `TryStmt.m3` will route `LocalExc` to the `OSError.E` handler and print `FAIL`.

---

## `EXTENDED` Float Type

`EXTENDED` maps to `double` (64-bit) on Darwin (ARM64 and x86_64) and Windows. It maps to `_Float128` (128-bit) only on `AMD64_LINUX`. Apple clang does not support `_Float128`.

This is implemented in:
- `m3-sys/m3back/src/M3C.m3`: C preamble uses `typedef double EXTENDED` for `__aarch64__` and `(defined(__x86_64__) && defined(__APPLE__))`
- `m3-sys/m3middle/src/Target.m3`: `Target.Extended.size` set to 64 bits for ARM64 and `AMD64_DARWIN`
- `m3-libs/m3core/src/float/IEEE/m3makefile` and `IEEE-le/m3makefile`: selects `ext64/` for all targets except `AMD64_LINUX`

---

## Key Source Files

| File | Role |
|---|---|
| `m3-sys/m3back/src/M3C.m3` | C code generator backend — generates `.cpp` files from M3 IR. Controls `noreturn`, `ex_stack` try/catch emission, `EXTENDED` typedef, `_M3Exc` struct |
| `m3-sys/m3front/src/stmts/TryStmt.m3` | Compiles `TRY/EXCEPT`; selects `Compile1` (ex_stack) or `Compile2` (ex_frame) based on `Target.Has_stack_walker` |
| `m3-sys/m3front/src/stmts/TryFinStmt.m3` | Compiles `TRY/FINALLY`; `End_try` placement is critical (must be before the FINALLY body label) |
| `m3-libs/m3core/src/runtime/ex_stack/RTExStack.m3` | `ex_stack` raise/resume; activation record must be heap-allocated before throw |
| `m3-libs/m3core/src/runtime/ARM64_DARWIN/RTStackC.c` | libunwind primitives + `RTStack__ThrowM3Exc` (ARM64) |
| `m3-libs/m3core/src/runtime/AMD64_DARWIN/RTStackC.c` | Same for x86_64 macOS |
| `m3-libs/m3core/src/runtime/AMD64_LINUX/RTStackC.c` | Same for x86_64 Linux |
| `m3-sys/cminstall/src/config/ARM64_DARWIN` | Platform config for Apple Silicon |
| `m3-sys/cminstall/src/config/AMD64_DARWIN` | Platform config for x86_64 macOS |
| `scripts/version` | Canonical version string (e.g., `CM3VERSION d5.12.1`) |
| `scripts/pkginfo.txt` | Topological package order used by concierge |

---

## CI Workflows

Workflows live in `.github/workflows/`. The `make-release.yml` fires on `push` to tags matching `d*.*.*`. It:
1. Downloads a bootstrap tarball from the GitHub release matching the tag.
2. Runs `concierge.py upgrade` then `concierge.py make-dist`.
3. Creates a draft release with the resulting distribution tarballs.

Per-target CI (e.g., `arm64_darwin.yml`) is called via `workflow_call` from `integration.yml`. These run `concierge.py full-upgrade` and then `cm3 -DHTML` in `m3-sys/m3tests`.

---

## ARM64_DARWIN / AMD64_DARWIN Notes

- `m3cc` (GCC backend) always fails on Darwin with "GNU platform is not known" — this is expected and harmless.
- Cross-compiling from AMD64 to ARM64 requires `export BUILDARGS="-DTARGET=ARM64_DARWIN"` because `uname` under Rosetta returns `x86_64`.
- Once a native ARM64 binary is installed, no override is needed.
- `M3_PARALLEL_BACK = 1` is set in both Darwin configs to prevent `pthread_create` deadlocks under Rosetta 2.
- `M3_USE_STACK_WALKER` is currently commented out in both Darwin configs pending a clean bootstrap rebuild with the fix.
