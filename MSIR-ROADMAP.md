# MSIR Roadmap: Current Status

Last updated: 2026-05-10 (msir branch)

## What's Working (69/69 tests pass)

The end-to-end path is live: MSIR emission → LLVM IR lowering → native object → linked binary.

### Emission (m3front → MSIR)
- [x] Arithmetic, comparisons, boolean short-circuit
- [x] IF/WHILE/FOR/CASE/REPEAT/WITH
- [x] VAR/READONLY/VALUE params, INC/DEC
- [x] Fixed and open arrays (elem-addr, size, subarray, relay)
- [x] Records (by-value and by-ptr)
- [x] Enums (byte-width scalars)
- [x] Module globals (non-traced and traced)
- [x] Floor DIV/MOD (Modula-3 semantics)
- [x] TRY/EXCEPT — UID-comparison dispatch chain, `invoke`/`landingpad`/`resume`
- [x] TRY/FINALLY — cleanup landingpad, alloca-saved lp value, resume
- [x] VAR initializers (proc-scope and block-scope, via `Variable.CompileInitExprMSIR`)
- [x] `EmitCall` promotes calls inside TRY to `invoke` automatically
- [x] `AssignStmt.CompileMSIR` re-fetches `CurrentBlock()` after RHS (invoke-in-RHS)
- [x] GC globals (gc_slot typed, GcLoad/GcStore ops)

### Lowering (MSIR → LLVM IR)
- [x] All scalar types, struct, fixed/open arrays, ptr/gc_ref
- [x] M3 symbol mangling (`Module.Proc` → `Module__Proc`)
- [x] `target triple` / `target datalayout` (ARM64_DARWIN, AMD64_DARWIN, AMD64_LINUX)
- [x] `invoke`/`landingpad`/`extractvalue`/`resume` (LLVM EH, ex_stack model)
- [x] `personality ptr @__gxx_personality_v0`, `@_ZTI6_M3Exc` extern
- [x] GcLoad inline read barrier (nil → misaligned → gray-bit → `RTHooks__CheckLoadTracedRef`)
- [x] GcStore write barrier infrastructure (`container` operand; dirty-bit check + `RTHooks__CheckStoreTraced` when heap field; globals skip)
- [x] `@Module_M3(i64 %mode)` RTLinker binder (mode=0: return MI; mode=1: run body+return MI)
- [x] `@Module_M3_info` RT0.ModuleInfo descriptor (binder and gc_flags set; other fields null)
- [x] GC barrier extern declarations (`RTHooks__CheckLoadTracedRef/CheckStoreTraced`)
- [x] RAISE statement: per-exception `ExceptionDesc` static (`{ uid, null, 0 }`), `RTHooks__Raise` via `HookProc(RaiseEx)`, Itanium ABI `__cxa_begin_catch` fix in catch landingpads
- [x] Exception value binding (`EXCEPT E(v) =>`): loads `act.arg` at EA_arg=8 bytes, `ptrtoint`/`inttoptr` for scalar packing; `AddLocal(h.var)` registers the handler-scope variable
- [x] `@Module_I3` interface binder (returns same MI as `@Module_M3`)
- [x] `RT0.ImportInfo` chain in `MI_imports`: `BuildImportLink` registers binders via `MSIREmit.RegisterImport`; RTHooks filtered (pre-initialised by `InitRuntime`); standalone binary now runs full transitive RTLinker init

---

## Remaining Work (prioritised)

### A. ~~Complete Runtime Linking~~ partially done
- [x] `imports` chain — emitted; transitive RTLinker init works
- [ ] `type_cells` — type/exception descriptors; needed for `NEW(T)` and typecode-based lookups
- [ ] `var_map` / `gc_map` — GC root map for traced globals
- [ ] Module body TEXT/IO — string concatenation and IO.Put crash; blocked on TEXT/TYPECASE support

### B. ~~RAISE statement~~ ✓ Done
- Ownership lifecycle correct: `__cxa_get_exception_ptr` for peeking (no ownership), `__cxa_begin_catch`/`__cxa_end_catch` strictly bracketing matched handler bodies (including before any `ret` via `ReturnStmt.CompileMSIR`), plain `resume` for no-match.

### C. ~~Exception value binding~~ ✓ Done

### D. ~~TYPECASE~~ ✓ Done

### E. ~~Method dispatch~~ ✓ Done

### F. ~~LOCK statement~~ ✓ Done

### G. Nested procedures / up-level access
- Up-level variable references call `Abandon`; blocked on static link support

### H. GC write barrier for heap fields
- Infrastructure is in place (`BuildGcStore(..., container)`)
- Activated automatically once heap field stores are wired in `CompileMSIR`
- Need: detect field-of-heap-object in LValueMSIR, pass container to BuildGcStore

---

## Test Infrastructure

```sh
# Full end-to-end LLVM link test (69 checks)
bash m3-sys/msir/test/run-llvm-link-test.sh

# Standalone M3 program (RTLinker path; body fails on TEXT/IO)
cd m3-sys/msir/test/smoke/ARM64_DARWIN
clang _m3main.cpp Main-llvm.o libm3core.a libm3.a -lc++ -o smoke-realrt
./smoke-realrt

# Just inspect the emitted IR
cd /any/m3-program && cm3 '@M3m3front-msir' -build
cat ARM64_DARWIN/Main.ll
```
