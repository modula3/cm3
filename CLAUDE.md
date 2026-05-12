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

The end-to-end path is working: MSIR is emitted for a real module, lowered to LLVM IR, compiled to a native object, and linked into a passing test binary. The production binary (`smoke-realrt`) also runs to completion (exit 0) against the real CM3 runtime (`libm3core.a`/`libm3.a`). **Zero msir-abandon events across the full p0/p1/p2 test suite.** The following features are implemented and tested (86/86 smoke tests):

- Arithmetic, control flow (IF/WHILE/FOR/CASE/REPEAT/WITH/AND/OR)
- Records (by-value and by-ref), fixed and open arrays, enums, globals
- VAR/READONLY params, INC/DEC
- Exception handling: TRY/EXCEPT (UID-dispatch landingpad) and TRY/FINALLY (cleanup landingpad + resume)
- RAISE statement: per-exception `ExceptionDesc` static global (`{ uid, null, 0 }`), calls `RTHooks__Raise` via `HookProc(RaiseEx)`, emits `unreachable` after
- Exception value binding (`EXCEPT E(v) =>`): loads `act.arg` at `EA_arg = 8` bytes, stores to bound-variable alloca; `inttoptr`/`ptrtoint` for scalar arg packing
- RTLinker binder `@Module_I3` (interface view, returns same MI) and `@Module_M3`
- `RT0.ImportInfo` chain in `MI_imports`: linked list of `{ null, binder_fn, next }` records; `BuildImportLink` registers imports via `MSIREmit.RegisterImport`; RTHooks excluded (always pre-initialised by `InitRuntime`)
- GC read barrier (nil/misaligned/gray-bit inline fast path + `RTHooks__CheckLoadTracedRef`)
- GC write barrier infrastructure (`GcStore` container operand, dirty-bit check + `RTHooks__CheckStoreTraced` for heap fields; globals are GC roots and need no barrier)
- RTLinker binder (`@Module_M3`) and `RT0.ModuleInfo` struct (`@Module_M3_info`) emitted in LLVM IR
- M3 symbol mangling (`Module.Proc` → `Module__Proc`), target triple/datalayout for LLVM 22
- `target triple` / `target datalayout` for ARM64_DARWIN, AMD64_DARWIN, AMD64_LINUX
- **TypeCells**: `RefType.InitTypecellMSIR` / `ObjectType.InitTypecellMSIR` called from `Type.GenCells` alongside CG counterparts; driven by type *declarations*, not NEW sites
- **NEW(REF T)** and **NEW(OBJECT T)**: full support; `GenRefMSIR`/`GenObjectMSIR`/`CallAllocHook` in `New.m3`; vtable (`OTC_defaultMethods`) populated from `ObjectType.GetObjectTypeInfo`/`FillMethodNames`
- **NEW(REF ARRAY OF T, n)**: `GenOpenArrayMSIR` in `New.m3`; computes ATC parameters (dopeSize, elementSize, nDimensions) and calls `TypeDescValueForRefArray` eagerly at the call site (not deferred to `InitTypecellMSIR`) to handle the case where the type's UID is visible from an imported module; sizes struct layout `{ ptr &dim0, i64 ndims, i64 dim0 }`; stub in `raise_stub.cpp`
- **Vtable dispatch**: `ShapeDispatch(s)` correctly dispatches via `s.vtable[0](s)` in LLVM IR; `AllocateTracedObj` stub initialises vtable pointer from `OTC_defaultMethods`
- **Module global initialization**: variable initializers (user-specified and language-default zero-init) emitted in MSIR module body; traced globals use `BuildGcStore`
- **External/imported variable registration**: `DeclareGlobalsMSIR` in `Module.Compile` pre-registers all module-level variables and exception descriptors before proc bodies compile
- **TEXT literals**: static `TextLiteral.T` globals (`{ i64 gc_header, ptr method_list, i64 cnt, [len+1 x i8] chars }`); `ConstTextLit` value kind carries uid+chars for readable MSIR text; lowered to LLVM constant-expression GEP `getelementptr inbounds (i8, ptr @textlit_N, i64 8)`
- **TEXT concatenation**: `ConcatExpr.CompileMSIR` calls `RTHooks__Concat(a, b)` via `HookProc(RunTyme.Hook.Concat)`; module body now passes real TEXT values to `IO.Put`
- **GC write barrier for heap fields**: activated — `QualifyExpr.LValueMSIR` sets pending container (object pointer) via `MSIRBuilder.SetPendingContainer`; `AssignStmt.CompileMSIR` calls `TakePendingContainer` and passes it to `BuildGcStore`; traced object fields are retyped from `GcRef` to `GcSlot` so the barrier fires correctly
- **`var_map`/`gc_map`**: module globals embedded as trailing fields of `@Mod_M3_info` struct (after MI_SIZE=104 bytes); gc_map TipeMap byte sequence skips non-traced fields and emits `Op.Ref` for each traced global; LLVM aliases (`@Main__gCounter` etc.) preserve binary symbol compatibility with CG-compiled modules; GC now correctly scans MSIR module globals as roots
- **Nested procedures**: lambda-lifted — each captured up-level variable becomes an explicit `ptr` parameter in the inner proc's LLVM signature (`%__cap_0`, `%__cap_1`, …); outer proc's up-level vars are ordinary allocas whose addresses are passed as capture args; `Stmt.Capture` pre-scans the body to collect captures before `GenBodyMSIR`; `RegisterProc` stores the capture list so call sites build the right arg list; qualified LLVM name (`Main__NestedSum__Add`) avoids collision with module-level procs of the same base name; multi-level nesting supported naturally (inner proc passes its own capture params through to deeper procs)
- **Read-only scalar capture optimisation**: captures classified `written=FALSE` by `CaptureAnalysis` and of scalar MSIR type (integer, float, or untraced pointer) are passed by value instead of by pointer, giving LLVM's alias analysis better information; GcRef captures always pass by pointer so the conservative GC scanner keeps them on the stack
- **WIDECHAR text literals**: `M3WString.GetChar` provides raw code-point access; `MSIREmit` encodes each WIDECHAR as little-endian bytes (`Target.WideCharSize() DIV Target.Char.size` bytes); `MSIRToLLVM` emits the correct `[wcharBytes*len + wcharBytes x i8]` struct field with a wide null terminator; `cnt` is negative to distinguish from ASCII literals
- **TextLiteral vtable hooks**: the five `@textlit_methods` function pointers (`RTHooks__TextLitInfo` etc.) are resolved via `MSIRBuilder.HookProc`/`RunTyme.LookUpProc` in `MSIREmit.EndUnit` and stored in the MSIR module; `MSIRToLLVM` uses `LLSymbol(hook)` for names and `EmitDeclare` for signatures, eliminating all hardcoded strings and deriving correct types from the M3 type system
- **TypeCell alignment**: `InitTypecellMSIR` in `RefType.m3` and `ObjectType.m3` now correctly converts alignment from bits to bytes (divides by `Target.Byte`) before passing to `TypeDescValueForRef`/`TypeDescValueForRefArray`; `RTType__FinishTypecell` requires bytes in {1,2,4,8,16}
- **Fixed→open-array argument coercion**: `Formal.EmitArgMSIR` / `GenOpenArgMSIR` (in `Formal.m3`) build a stack dope vector `{ ptr data, i64 dim0, … }` when a fixed-size array actual is passed to a VAR/READONLY open-array formal; `UserProc.CompileMSIR` walks formals via `Formal.EmitArgMSIR` rather than the old Ptr-check heuristic; VALUE open-array formals abandon (not yet needed)
- **Procedure values**: `ProcExpr.CompileMSIR` returns `MSIR.ConstProcRef(proc)` — a `ptr @procname` constant; `NamedExpr.CompileMSIR` handles `Value.Class.Procedure` by folding to `ProcExpr`; `EqualExpr.CompileMSIR` handles procedure equality as `icmp eq ptr`; `MSIRType.Translate` maps `Type.Class.Procedure` to `TPtr(TVoid())`; `BindFormalMSIR` guards `Kind(EltType) ≠ Void` so proc formals are treated as by-value scalars
- **Float type conversions**: new cast ops `SIToFP`, `FPToSI`, `FPExt`, `FPTrunc`, `ZExt`, `SExt`, `Trunc` in `MSIR`; `Floatt.CompileMSIR` implements `FLOAT()` via `SIToFP` (int→float) or `FPExt`/`FPTrunc` (float→float)
- **TRUNC/FLOOR/CEILING/ROUND builtins**: new unary float ops `FPFloor`, `FPCeil`, `FPRound` in `MSIR`; lowered to `llvm.floor.*`, `llvm.ceil.*`, `llvm.roundeven.*` intrinsics (suffix `f32`/`f64`/`f128` from bit width); `Trunc.m3` emits direct `fptosi`; `Floor.m3`/`Ceiling.m3`/`Round.m3` emit the rounding op followed by `fptosi`; ROUND uses `llvm.roundeven.*` (NearestElseEven = `FloatMode.RoundDefault`); note `FloatMode.SetRounding` explicitly does not affect the ROUND builtin, so ROUND always uses `llvm.roundeven.*` regardless of the current FPU mode; the CG backend (M3C.m3) uses C `round()` (half-away-from-zero) which is a spec deviation, but MSIR is correct
- **Extern variable auto-registration**: `NamedExpr.CompileMSIR` calls `Variable.RegisterExternMSIR(vv)` on demand for `FROM X IMPORT y` style variables not pre-registered by `DeclareGlobalsMSIR`
- **EVAL / ASSERT / LOOP stmts**: `EvalStmt`, `AssertStmt`, `LoopStmt` have `CompileMSIR` implementations

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
3. Links with the C test harness (`llvm_link_test.c`) and runs 77 checks

The harness (`raise_stub.cpp`) provides C stubs for runtime symbols: `RTHooks__Raise`, `RTHooks__AllocateTracedRef`, `RTHooks__AllocateTracedObj`, `RTHooks__CheckLoadTracedRef`, `RTHooks__ScanTypecase`, import binder stubs (`Thread_I3`, `Fmt_I3`, `IO_I3`), and `RTHooks_M3`/`RTAllocator_M3` anti-pull-in stubs.

To run as a full M3 program against the real runtime:
```sh
clang _m3main.cpp Main-llvm.o libm3core.a libm3.a -lc++ -o smoke-realrt
./smoke-realrt
```
The RTLinker calls `Main_M3(0)` to register the module, then `Main_M3(1)` to run the module body. The module body runs to completion (exit 0) — all IO.Put / Fmt / Text calls in Main.m3 work correctly against the real runtime.

### Key Source Files

| File | Role |
|---|---|
| `m3-sys/msir/src/MSIR.i3/.m3` | IR types, values, ops, builders; `TypeDesc`, `ConstZero`, `TypeCellRef` |
| `m3-sys/msir/src/MSIRToLLVM.m3` | Lowers MSIR → LLVM text IR; handles EH, GC barriers, TypeCells, RTLinker binder |
| `m3-sys/msir/src/MSIRPrinter.m3` | Prints MSIR text (`.msir` files) |
| `m3-sys/msir/src/MSIRVerifier.m3` | Structural checks on completed procs |
| `m3-sys/m3front/src/msir/MSIRBuilder.m3` | Per-proc builder state; raw map helpers (`GlobalMapAdd`, `VarMapAdd`, `VarMapContains`); `EmitCall`/`EmitNestedCall`; try-context stack; `RegisterProc`/`GetProcCaptures` for lambda-lifted nested proc capture lists |
| `m3-sys/m3front/src/msir/MSIREmit.m3` | Module-level gate; writes `.msir` and `.ll` at end of unit |
| `m3-sys/m3front/src/stmts/TryStmt.m3` | `CompileMSIR`: EH lowering for TRY/EXCEPT (UID comparison chain) |
| `m3-sys/m3front/src/stmts/TryFinStmt.m3` | `CompileMSIR`: EH lowering for TRY/FINALLY (cleanup landingpad) |
| `m3-sys/m3front/src/stmts/AssignStmt.m3` | `CompileMSIR`: fetches `CurrentBlock()` AFTER RHS to handle invoke-in-RHS |
| `m3-sys/m3front/src/stmts/BlockStmt.m3` | `CompileMSIR`: calls `Scope.InitValues` (vars already registered by `BeginProc`) |
| `m3-sys/m3front/src/values/Variable.m3` | Owns MSIR declarations: `DeclareGlobalMSIR`, `RegisterExternMSIR`, `AddLocalMSIR` (with zero-init), `BindFormalMSIR`; MSIR init in `UserInit` |
| `m3-sys/m3front/src/values/Procedure.m3` | `GenBody`: `BeginProc` sets up MSIR proc; `Stmt.CompileMSIR`/`EndProc` follow CG body; `GenBodyMSIR`: MSIR-only inline compilation of nested procs |
| `m3-sys/m3front/src/values/Module.m3` | `DeclareGlobalsMSIR`: pre-registers globals + exception descs; `EmitBody`: module-init MSIR |
| `m3-sys/m3front/src/types/RefType.m3` | `InitTypecellMSIR`: registers MSIR TypeDesc; called from `Type.GenCells` |
| `m3-sys/m3front/src/types/ObjectType.m3` | `InitTypecellMSIR`: registers MSIR ObjectTypeDesc with vtable; `GetObjectTypeInfo`, `FillMethodNames` |
| `m3-sys/m3front/src/types/Type.m3` | `GenCells`: calls CG and MSIR `InitTypecell` together for each type cell |
| `m3-sys/m3front/src/builtinOps/New.m3` | `CompileMSIR`: dispatches to `GenRefMSIR`/`GenObjectMSIR`/`GenOpaqueMSIR`; `CallAllocHook` is common tail |
| `m3-sys/m3front/src/misc/CaptureAnalysis.i3/.m3` | Capture-analysis module: `Note(ca, v, written)` records up-level variable accesses; `GetCaptures` returns the set; `T` is the accumulator passed through `Stmt.Capture`/`Expr.Capture` walks |
| `m3-sys/m3front/src/misc/M3WString.m3` | Wide-char string representation; `GetChar(t, i)` gives raw code-point access used by `MSIREmit` to encode WIDECHAR literals as little-endian byte sequences |
| `m3-sys/m3front/src/exprs/CallExpr.m3` | Uniform `.methods` dispatch for `CompileMSIR` and `Capture` (capture analysis); `Capturer`/`CompilerMSIR` callback types; `CaptureDefault` (scan all args as reads) wired by `NewMethodList`; `SetMethodCapture`/`SetMethodMSIR` for per-builtin overrides |
| `m3-sys/m3front/src/types/UserProc.m3` | `CompileMSIR`: user-proc MSIR handler (direct, vtable, nested lambda); `Capture`: formal-mode scan; both wired onto `UserProc.Methods` in `Initialize` |
| `m3-sys/m3front/src/values/Formal.m3` | `EmitArgMSIR`: formal-aware arg-passing for MSIR call sites; `GenOpenArgMSIR`: builds stack dope vector when fixed array is passed to open-array formal |
| `m3-sys/msir/test/smoke/Main.m3` | Comprehensive smoke test (arithmetic, arrays, EH, globals, NEW, vtable dispatch, …) |
| `m3-sys/msir/test/smoke/llvm_link_test.c` | 76-test C harness |
| `m3-sys/msir/test/smoke/raise_stub.cpp` | C++ stubs: `RTHooks__Raise`, allocators, import binders, barriers |
| `m3-sys/msir/test/run-llvm-link-test.sh` | End-to-end driver script |

### Architecture: MSIR Declaration Lifecycle

MSIR declarations are co-located with CG declarations, not in separate passes:

| What | When | Where |
|---|---|---|
| Module globals (vars + exception descs) | `Module.Compile`, before type compilation | `Module.DeclareGlobalsMSIR` → `Variable.DeclareGlobalMSIR` / `RegisterExternMSIR` |
| Type cells (Ref + Object) | `Type.GenCells` in `GenLinkerInfo` | `RefType.InitTypecellMSIR` / `ObjectType.InitTypecellMSIR` alongside CG `InitTypecell` |
| Proc formals + locals | `MSIRBuilder.BeginProc`, before CG `Scope.InitValues` | `Variable.BindFormalMSIR` + `Variable.AddLocalMSIR` (zero-init if `InitCost > 0`) |
| Variable initializers | CG-path `Scope.InitValues` (guarded by `t.initDone`) | MSIR blocks inside `Variable.UserInit` fire here because `BeginProc` has set `curBlock` |
| Exception descriptors | `Module.DeclareGlobalsMSIR` | `MSIRBuilder.ExcDescValue` called upfront; lazy calls from TryStmt/RaiseStmt find existing desc |
| Nested proc body (MSIR) | `Procedure.LangInit` via `Scope.InitValues`, when `inline_nested_procs=FALSE` | `Stmt.Capture` pre-scans the body; `GenBodyMSIR(t)` calls `BeginProc` with the captures, registers the proc+captures; `ProcMapContains` guards `GenBody` against re-emitting MSIR on the second (CG-only) pass |

`Variable.m3` owns all MSIR registration for variables; `MSIRBuilder` exposes only raw map helpers (`GlobalMapAdd`, `VarMapAdd`, `VarMapContains`). The `Scope.InitValues` call in `GenBody`'s MSIR phase is intentionally absent — init fires during the CG-path call because `BeginProc` is already active.

**Nested proc note**: the C backend always sets `inline_nested_procs=FALSE` (via `-unfold_nested_procs` in `cm3cfg.common`). MSIR still calls `GenBodyMSIR` inline from `LangInit` — not because the outer proc's context is required (the old frame-struct reason), but to guarantee that the nested proc is registered in `procMap` before any call site in the outer body is compiled. Call sites call `LookupOrCreateProc` and `GetProcCaptures`; both must find the nested proc's MSIR.Proc and capture list already registered.

**Lambda-lifting: how nested procs work in MSIR**

Up-level variables are identified by `Stmt.Capture`, which dispatches through the `capture`/`captureLV` virtual methods on `Stmt.T` and `Expr.T` to walk the nested proc's AST before compilation and record each up-level variable reference. The nested proc gets one explicit `ptr` parameter per captured variable (`%__cap_0`, `%__cap_1`, …). In the outer proc, captured variables are ordinary `alloca` locals; their addresses are passed as capture arguments at each call site.

- `Stmt.Capture(body, ca)` — pre-pass that walks the AST via the `capture`/`captureLV` virtual methods on `Stmt.T` and `Expr.T`, recording `(Variable.T, written)` pairs in `ca`
- `MSIRBuilder.BeginProc(..., captures := ca)` — generates explicit capture params; binds each in the inner proc's varMap so `LookupVar`/`LookupVarAddr` work transparently
- `MSIRBuilder.RegisterProc(p, proc, caps)` — stores the capture list alongside the proc
- `MSIRBuilder.EmitNestedCall(name, callee, calleeVal, args)` — looks up captures for `calleeVal`, calls `LookupVarAddr(cap.var)` for each, prepends these to `args`

Multi-level nesting works naturally: if `Add` (nested in `NestedSum`) captures `acc`, and `SubAdd` (nested in `Add`) also uses `acc`, then `SubAdd`'s `BeginProc` runs inside `Add`'s varMap context where `acc` maps to `Add`'s `%__cap_0` param. `LookupVarAddr(acc)` returns `%__cap_0`, which is passed directly as `SubAdd`'s capture arg.

**Parameter explosion note**: a proc that captures many up-level variables acquires many extra pointer parameters. LLVM's inliner and middle-end optimisations (mem2reg, SROA) typically eliminate this overhead after inlining — the pointers are promoted back to registers and the indirections disappear. O16 in MSIR-design.md discusses frame-struct grouping as a future performance tuning step for hot paths where inlining does not apply.

### CallExpr MSIR Dispatch Architecture

`CallExpr.m3` dispatches both MSIR compilation and capture analysis through its `MethodList` mechanism, with no per-kind logic inside `CallExpr` itself.

**Types and setters** (in `CallExpr.i3`):
- `CompilerMSIR = PROCEDURE (t: T): MSIR.Value` — MSIR compilation callback
- `Capturer = PROCEDURE (t: T; ca: CaptureAnalysis.T)` — capture-analysis callback
- `SetMethodMSIR(ml, c)` / `SetMethodCapture(ml, s)` — wiring helpers
- `CaptureDefault` — set by `NewMethodList` on every `MethodList`; scans all args as reads

**Per-module callbacks**:
- `UserProc.m3`: `CompileMSIR` (direct call, vtable dispatch, nested lambda) + `Capture` (formal-mode scan); wired in `Initialize`
- `Inc.m3`, `Dec.m3`: each defines `CompileMSIR` (arithmetic) + `Capture` (ScanLV arg0, Scan rest); wired in `Initialize`
- Other builtins inherit `CaptureDefault` (all args read-only)

**`capture`/`captureLV` virtual methods**: defined on `Expr.T` (in `ExprRep.i3`) and `Stmt.T` (in `StmtRep.i3`). `Stmt.Capture(s, ca)` chains through `.next`; `Expr.Capture(e, ca)` dispatches to `e.capture(ca)`. Each concrete stmt/expr type overrides these to recurse into its sub-nodes. `Expr.CaptureLV` propagates the lvalue context so that directly-assigned variables are marked `written=TRUE` by `VarExpr` and `NamedExpr`.

### Known Limitations / Remaining Work

- **TEXT**: literals (ASCII and WIDECHAR), `&` concatenation, and TEXT-returning library calls (`Fmt.Bool`, `Text.Length`, etc.) all work — external calls are emitted correctly and the calling convention matches the C backend. Remaining gaps: `Fmt.Real` (floating-point formatting), `Text.Sub` and other TEXT manipulation operations not yet exercised in tests
- **GC write barrier for heap fields**: activated; see container protocol below
- **`var_map`/`gc_map`**: implemented; see architecture note below
- **NEW(REF open-array)**: `GenOpenArrayMSIR` supports 1-D open-array refs; multi-D untested
- **NEW(REF record with keyword args)**: `GenRefMSIR` abandons when `NUMBER(ce.args^) > 1`; plain `NEW(REF Record)` works
- **Opaque types**: `GenOpaqueMSIR` only handles REF revelation; OBJECT revelation deferred
- **VALUE open-array formals**: copy-in to a local dope vector not yet implemented; these still abandon
- **Tracers** (`<*TRACE*>` pragma): CG-only; MSIR-compiled code silently omits trace callbacks
- **Debug symbols**: no source locations reach LLVM IR; see below
- **SET type operations**: `IN` operator on small constant SETs works (word-size bit-mask extraction via `SetExpr.GetWordBitMask`, bitwise shift/and ops); SET literals, IN on non-constant/large sets, and set arithmetic (+/-/*/) not yet implemented
- **CONST array subscript with runtime index**: when a `CONST ARRAY` is subscripted at a non-constant index (e.g., `BoolToText[b]`, `reservedWords[i]`), MSIR abandons — requires materializing the CONST array as a static global

### GC Write Barrier Container Protocol

In the **CG path**, write barriers are explicit and emitted by the front-end before the store: `QualifyExpr.PrepLV` (and `DerefExpr.CompileLV`) call `RunTyme.EmitCheckStoreTraced()` when `Host.doGenGC` is true and the field is traced. That helper pops the container (object pointer) off the M3CG stack, emits a dirty-bit test inline, and conditionally calls `RTHooks__CheckStoreTraced` — all as M3CG ops before the subsequent `Store_indirect`. The C backend (`M3C.m3`) merely translates these M3CG ops to C; it does not independently decide when to add barriers.

In the **MSIR path**, LLVM IR is lower-level and barriers must be emitted explicitly. The protocol:

1. `QualifyExpr.LValueMSIR` (objField case) computes the base object pointer (`baseAddr`) before GEP-ing to the field. It calls `MSIRBuilder.SetPendingContainer(baseAddr)` as a side-effect and retypes the GEP result from `GcRef` to `GcSlot(elemType)` if the field is a traced ref.
2. `AssignStmt.CompileMSIR` calls `MSIRBuilder.TakePendingContainer()` immediately after `LValueMSIR`. The result is NIL for module globals (GC roots, no barrier) and the object pointer for heap fields.
3. `MSIR.BuildGcStore(block, slot, value, container)`: when `container != NIL`, emits the inline dirty-bit check + `RTHooks__CheckStoreTraced`.

The `SetPendingContainer`/`TakePendingContainer` side-channel avoids changing any expression interface while threading the container from the LValue expression to the assignment statement.

### TEXT Literal Architecture Note

`TextExpr.P` is extended with `cgOffset: INTEGER` to store the `Module.Allocate` result directly on the expression object. `LiteralTable = REF ARRAY OF P` (indexed by uid) is the single per-module registry for both CG and MSIR — no parallel tracking needed. `Split8`/`Split32` on `literals[uid]` gives string content; `literals[uid].cgOffset` gives the CG const-area offset. `ExpandLiterals` grows the array; `Reset` clears entries to NIL for reuse across modules. `MSIREmit.EndUnit` bridges the data to `MSIR.Module` since `MSIRToLLVM` (in the `msir` package) cannot import `TextExpr` from `m3front`.

### Module Global Layout and var_map/gc_map

Module globals in MSIR are **embedded as trailing fields of `@Mod_M3_info`** (after the standard 104-byte `RT0.ModuleInfo` header), matching the CG convention exactly. The GC walker `RTHeapMap.Walk(m, m.gc_map, v)` starts at `m` (the `@Mod_M3_info` address) and uses the TipeMap to find traced fields.

**Struct layout** (example: `gLock: MUTEX; gCounter: INTEGER; gRef: REFANY`):
```
@Main_M3_info = global %RT0_ModuleInfo_t {
  [13 standard MI fields, bytes 0..103]
  ptr null,  ; gLock (+104, traced MUTEX)
  i64 0,     ; gCounter (+112, untraced INTEGER)
  i64 0,     ; gBase (+120, untraced INTEGER)
  ptr null,  ; gRef (+128, traced REFANY)
}
```

**gc_map TipeMap** (`@Main_M3_gc_map`): byte sequence `SkipF_1(104), Ref, SkipF_1(16), Ref, Stop` — skips the header, visits `gLock`, skips the two untraced integers, visits `gRef`.

**LLVM aliases** (`@Main__gCounter = alias i64, ptr getelementptr ...`) preserve binary symbol compatibility so C code and CG-compiled modules can still access globals by their mangled names.

**Access in proc bodies**: `LookupVarAddr(g)` returns `getelementptr inbounds (i8, ptr @Main_M3_info, i64 <byteOffset>)` as a constant expression (a `StructFieldRef` value kind in MSIR).

### Debug Symbol Support (Future Work)

The MSIR → LLVM path currently emits no debug metadata. Adding DWARF support is self-contained additive work with no architectural changes required.

**What LLVM needs**: `!DICompileUnit`, `!DIFile`, `!DISubprogram` per proc, `!DILocalVariable` per local, `!DILocation` on every instruction, and `llvm.dbg.declare` intrinsics linking allocas to their variable descriptors.

**Natural hook points** (already in the code):
- `Scanner.offset` / `t.origin` carry (file, line) information throughout m3front
- `CG.Gen_location(offset)` is called at statement boundaries, proc entry, etc. — MSIR needs a parallel `MSIRBuilder.SetLocation(offset)` at the same sites
- `AddLocalMSIR` is where `llvm.dbg.declare` intrinsics would be emitted after each alloca
- `BeginProc` is where `!DISubprogram` would be attached to the proc definition

**Complication**: CM3's `Scanner.offset` packs file identity and line number into a single integer using m3front-internal encoding. Decoding back to a human-readable (file, line) requires `Scanner.Here` or equivalent — currently only callable during active scanning. A mapping table from file-id to path would need to be maintained during compilation.

**Right time to implement**: once the LLVM path is the primary production path. Debug metadata roughly doubles emitted LLVM IR size, so deferring until stability is established is reasonable.

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
