# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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

## Bootstrap Tarballs

Bootstrap tarballs (`cm3-boot-<TARGET>-<VERSION>.tar.xz`) contain **pre-compiled `.cpp` files** generated from Modula-3 source by a prior compiler. The cmake build in `bootstrap/` compiles these `.cpp` files directly — it does **not** regenerate them from `.m3` sources.

The proper workflow to regenerate a bootstrap:
1. Extract old bootstrap, build it with cmake to get a compiler binary.
2. Use that binary with the fixed source tree to compile the packages.
3. Use `scripts/concierge.py make-bootstrap` to emit the new pre-compiled `.cpp` files.
4. Package and ship as a new bootstrap tarball.

If the bootstrap `.cpp` files contain bugs, patching the `.m3` source alone does not fix CI — the bootstrap tarball itself must be regenerated and re-uploaded.

---

## Two-Phase Bootstrap: Transitioning to `ex_stack`

Enabling `ex_stack` on a target that previously used `ex_frame` requires two build phases because an old compiler binary (with old m3back statically linked) cannot compile `RTExStack.m3`.

### Phase 1 — Build fixed compiler, still using `ex_frame`

1. **Comment out** `M3_USE_STACK_WALKER = TRUE` in the platform config.
2. Using the existing (old) `cm3`, rebuild in dependency order:
   ```sh
   cd m3-libs/m3core   && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-libs/libm3    && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3middle  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3linker  && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3front   && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3quake   && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3objfile && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/m3back    && cm3 -clean && cm3 -build && cm3 -ship
   cd m3-sys/cm3       && cm3 -clean && cm3 -build && cm3 -ship
   ```
3. Install the new binary: `cp m3-sys/cm3/<TARGET>/cm3 ~/cm3/bin/cm3`

### Phase 2 — Rebuild with `ex_stack`, using Phase 1 binary

1. **Uncomment** `M3_USE_STACK_WALKER = TRUE` in the platform config.
2. Rebuild in the same order as Phase 1 using the Phase 1 binary.
3. Install: `cp m3-sys/cm3/<TARGET>/cm3 ~/cm3/bin/cm3`

---

## CI Workflows

Workflows live in `.github/workflows/`. The `make-release.yml` fires on `push` to tags matching `d*.*.*`. It:
1. Downloads a bootstrap tarball from the GitHub release matching the tag.
2. Runs `concierge.py upgrade` then `concierge.py make-dist`.
3. Creates a draft release with the resulting distribution tarballs.

Per-target CI (e.g., `arm64_darwin.yml`) is called via `workflow_call` from `integration.yml`. These run `concierge.py full-upgrade` and then `cm3 -DHTML` in `m3-sys/m3tests`.

---

## MSIR Development Notes

The `m3-sys/msir` package and `m3-sys/m3front/src/msir/` form the typed-SSA mid-level IR layer being built toward LLVM retargeting.

### Current Status

The end-to-end path is working: MSIR is emitted for a real module, lowered to LLVM IR, compiled to a native object, and linked into a passing test binary. The following features are implemented and tested (64/64 tests):

- Arithmetic, control flow (IF/WHILE/FOR/CASE/REPEAT/WITH/AND/OR)
- Records (by-value and by-ref), fixed and open arrays, enums, globals
- VAR/READONLY params, INC/DEC
- Exception handling: TRY/EXCEPT (UID-dispatch landingpad) and TRY/FINALLY (cleanup landingpad + resume)
- GC read barrier (nil/misaligned/gray-bit inline fast path + `RTHooks__CheckLoadTracedRef`)
- GC write barrier infrastructure (`GcStore` container operand, dirty-bit check + `RTHooks__CheckStoreTraced` for heap fields; globals are GC roots and need no barrier)
- RTLinker binder (`@Module_M3`) and `RT0.ModuleInfo` struct (`@Module_M3_info`) emitted in LLVM IR
- M3 symbol mangling (`Module.Proc` → `Module__Proc`), target triple/datalayout for LLVM 22
- `target triple` / `target datalayout` for ARM64_DARWIN, AMD64_DARWIN, AMD64_LINUX

### EH Model Requirement

MSIR's LLVM lowering uses the C++ EH personality model (`invoke`/`landingpad`/`resume` with `@__gxx_personality_v0`) exclusively.  This maps directly to `ex_stack` (C++ zero-cost EH) and cannot be used with `ex_frame` (setjmp/longjmp).  MSIR emission should only be enabled on `ex_stack` platforms (ARM64_DARWIN, AMD64_DARWIN, AMD64_LINUX).  On `ex_frame` platforms the C backend remains the only path.

### Enabling MSIR Emission

MSIR output is gated behind a runtime parameter so it doesn't slow normal builds. Pass `@M3m3front-msir` to the `cm3` process — the `@M3` prefix is consumed by `RTParams` and never reaches the compiled program's argument list:

```sh
cm3 '@M3m3front-msir' -build
```

This writes `<Module>.msir` and `<Module>.ll` to the build directory for every module compiled in that invocation.

### Build Order After Editing MSIR or m3front

When you change files in `m3-sys/msir/src/` or `m3-sys/m3front/src/msir/`, or any of the m3front files that touch MSIR (stmts, values, exprs), rebuild in this order:

```sh
# From the repo root, with ~/cm3/bin on PATH
cd m3-sys/msir   && cm3 -build && cm3 -ship
cd m3-sys/m3front && cm3 -build && cm3 -ship
cd m3-sys/cm3    && cm3 -build
cp m3-sys/cm3/ARM64_DARWIN/cm3 ~/cm3/bin/cm3
```

The `cm3` driver links m3front statically, so you must relink and reinstall the binary before the new MSIR code takes effect in compilations.

### End-to-End LLVM Link Test

The canonical test is in `m3-sys/msir/test/`:

```sh
# Requires LLVM clang on PATH (or export LLVM_PREFIX=$(brew --prefix llvm))
bash m3-sys/msir/test/run-llvm-link-test.sh
```

This script:
1. Builds `m3-sys/msir/test/smoke/Main.m3` with `@M3m3front-msir` → produces `Main.ll`
2. Compiles `Main.ll` via LLVM clang → `Main-llvm.o`
3. Links with the C test harness (`llvm_link_test.c`) and runs 64 checks

The harness provides C stubs for the few M3 runtime symbols it needs (`Fmt__Int`, `IO__Put`, GC barriers, `_ZTI6_M3Exc`) since it doesn't run the M3 runtime initialization.

To run as a full M3 program against the real runtime:
```sh
clang _m3main.cpp Main-llvm.o libm3core.a libm3.a -lc++ -o smoke-realrt
./smoke-realrt
```
The RTLinker calls `Main_M3(0)` to register the module, then `Main_M3(1)` to run the module body. The body currently fails on TEXT/IO operations (not yet fully supported in MSIR), but the binder and initialization sequence work correctly.

### Key Source Files

| File | Role |
|---|---|
| `m3-sys/msir/src/MSIR.i3/.m3` | IR types, values, ops, builders |
| `m3-sys/msir/src/MSIRToLLVM.m3` | Lowers MSIR → LLVM text IR; handles EH, GC barriers, RTLinker binder |
| `m3-sys/msir/src/MSIRPrinter.m3` | Prints MSIR text (`.msir` files) |
| `m3-sys/msir/src/MSIRVerifier.m3` | Structural checks on completed procs |
| `m3-sys/m3front/src/msir/MSIRBuilder.m3` | Per-proc builder state; `EmitCall` (invoke-inside-TRY); try-context stack |
| `m3-sys/m3front/src/msir/MSIREmit.m3` | Module-level gate; writes `.msir` and `.ll` at end of unit |
| `m3-sys/m3front/src/stmts/TryStmt.m3` | `CompileMSIR`: EH lowering for TRY/EXCEPT (UID comparison chain) |
| `m3-sys/m3front/src/stmts/TryFinStmt.m3` | `CompileMSIR`: EH lowering for TRY/FINALLY (cleanup landingpad) |
| `m3-sys/m3front/src/stmts/AssignStmt.m3` | `CompileMSIR`: fetches `CurrentBlock()` AFTER RHS to handle invoke-in-RHS |
| `m3-sys/m3front/src/stmts/BlockStmt.m3` | `CompileMSIR`: allocas + `CompileInitExprMSIR` for block-scope VAR init |
| `m3-sys/m3front/src/values/Variable.m3` | `CompileInitExprMSIR`: compiles VAR initializer expression for MSIR |
| `m3-sys/m3front/src/values/Procedure.m3` | Post-body: emits proc-scope VAR initializers before `Stmt.CompileMSIR` |
| `m3-sys/m3front/src/exprs/CallExpr.m3` | Uses `MSIRBuilder.EmitCall` (invoke-aware) instead of `MSIR.BuildCall` |
| `m3-sys/msir/test/smoke/Main.m3` | Comprehensive smoke test (arithmetic, arrays, EH, globals, …) |
| `m3-sys/msir/test/smoke/llvm_link_test.c` | 64-test C harness |
| `m3-sys/msir/test/run-llvm-link-test.sh` | End-to-end driver script |

### Known Limitations / Remaining Work

- **RAISE statement**: `Op.Raise` is defined but lowering not implemented; needs to build `RaiseActivation` on heap and call `RTHooks__Raise`
- **Exception value binding**: `EXCEPT E(v) =>` skipped (falls back to body-only); requires extracting arg from activation record
- **TYPECASE**: not implemented; calls `MSIRBuilder.Abandon`
- **Method dispatch**: not implemented
- **LOCK**: not implemented
- **Nested procedures**: up-level variable access not supported
- **TEXT / string literals**: `Fmt.Int` etc. return correct values but string concat/IO not fully supported
- **GC write barrier for heap fields**: `BuildGcStore(..., container)` infrastructure exists; activated when heap field stores are implemented
- **`imports` chain** in `RT0.ModuleInfo`: currently null; dependencies not transitively initialized via RTLinker
- **`type_cells`** in `RT0.ModuleInfo`: currently null; exception type lookups via typecode won't work
- **`var_map` / `gc_map`** in `RT0.ModuleInfo`: currently null; GC won't scan module globals as roots

### Cosmetic Issues in Emitted MSIR

- **Unreachable merge blocks**: when all branches of an IF end with `ret`, the `if.merge` block gets no predecessors. Harmless; LLVM DCE removes it.
- **Repeated block label names**: ELSIF chains reuse label hints (`if.then`, `if.next`) for each clause. The blocks are distinct objects; only the printed names collide. Fix: add a counter suffix in `NewBlock`.

---

## Darwin-Specific Notes

- `m3cc` (GCC backend) always fails on Darwin with "GNU platform is not known" — expected and harmless.
- Cross-compiling from AMD64 to ARM64 requires `export BUILDARGS="-DTARGET=ARM64_DARWIN"` because `uname` under Rosetta returns `x86_64`.
- Once a native ARM64 binary is installed, no `BUILDARGS` override is needed.
- `M3_PARALLEL_BACK = 1` is set in both Darwin configs to prevent `pthread_create` deadlocks under Rosetta 2.
- Cross-build from ARM64 to AMD64: set `export BUILDARGS="-DTARGET=AMD64_DARWIN"` and `export PATH=$HOME/cm3/bin:$PATH` (native ARM64 cm3). The installed `~/cm3-AMD64_DARWIN/bin/cm3.cfg` must set `TARGET = "AMD64_DARWIN"` **unconditionally** (no `if not defined` guard).
