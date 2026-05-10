# MSIR Roadmap: Current Status

Last updated: 2026-05-10 (msir branch)

## What's Working (64/64 tests pass)

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

---

## Remaining Work (prioritised)

### A. Complete Runtime Linking
- [ ] `imports` chain in ModuleInfo — null now; without it, transitive module init via RTLinker won't work
- [ ] `type_cells` — exception type descriptors; needed for `Compiler.ThisException()` and runtime exception reporting
- [ ] `var_map` / `gc_map` — GC root map for traced globals; needed for correct GC under collection
- [ ] Module body TEXT/IO — `Fmt.Int` etc. call correctly but string concatenation and IO.Put crash without TYPECASE/TEXT support

### B. RAISE statement
- `Op.Raise` defined in MSIR but lowering not implemented
- Needs: allocate `RaiseActivation` on heap, fill UID+arg, call `RTHooks__Raise`
- Prerequisite for testing real exception flow end-to-end

### C. Exception value binding
- `EXCEPT E(v) =>` handler with bound variable skipped (falls back to body-only)
- Needs: extract `arg` field from `RaiseActivation` in the landing pad
- Three-level load chain: `exc_obj_ptr → act → arg`

### D. TYPECASE
- `Op.Typecase` defined in MSIR but `CompileMSIR` calls `Abandon`
- Needed for any code using object polymorphism
- Lowers to `RTHooks__ScanTypecase` dispatch

### E. Method dispatch
- `Op.Dispatch` in MSIR; `CompileMSIR` not implemented
- Needed for object-oriented code
- Lowers to vtable load + indirect call

### F. LOCK statement
- Not implemented; desugar as TRY/FINALLY wrapping Thread.Acquire/Release

### G. Nested procedures / up-level access
- Up-level variable references call `Abandon`; blocked on static link support

### H. GC write barrier for heap fields
- Infrastructure is in place (`BuildGcStore(..., container)`)
- Activated automatically once heap field stores are wired in `CompileMSIR`
- Need: detect field-of-heap-object in LValueMSIR, pass container to BuildGcStore

---

## Test Infrastructure

```sh
# Full end-to-end LLVM link test (64 checks)
bash m3-sys/msir/test/run-llvm-link-test.sh

# Standalone M3 program (RTLinker path; body fails on TEXT/IO)
cd m3-sys/msir/test/smoke/ARM64_DARWIN
clang _m3main.cpp Main-llvm.o libm3core.a libm3.a -lc++ -o smoke-realrt
./smoke-realrt

# Just inspect the emitted IR
cd /any/m3-program && cm3 '@M3m3front-msir' -build
cat ARM64_DARWIN/Main.ll
```
