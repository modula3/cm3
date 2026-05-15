# MSIR Roadmap: Current Status

Last updated: 2026-05-15 (msir branch)

## What's Working (149/149 tests pass)

The end-to-end path is live: MSIR emission → LLVM IR lowering → native object → linked binary.
**Zero msir-abandon events** across the full p0/p1/p2 compiler validation test suite.

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
- [x] RAISE statement: per-exception `ExceptionDesc` static global, `RTHooks__Raise`
- [x] Exception value binding (`EXCEPT E(v) =>`): loads `act.arg`, ptrtoint/inttoptr for scalar packing
- [x] TYPECASE
- [x] Method dispatch (vtable)
- [x] LOCK statement (via TRY/FINALLY lowering)
- [x] NEW(REF T) and NEW(OBJECT T): `GenRefMSIR`/`GenObjectMSIR`/`CallAllocHook`
- [x] NEW(REF record): same allocator hook as scalar (`NewTracedRef`), TypeCell carries record byte-size; `QualifyExpr.LValueMSIR` byte-offset GEP fallback for GcRef(Void) base (REF Record field access)
- [x] NEW(REF ARRAY OF T, n): `GenOpenArrayMSIR`; 1-D open arrays; multi-D untested
- [x] TypeCells: `RefType.InitTypecellMSIR` / `ObjectType.InitTypecellMSIR` from `Type.GenCells`
- [x] TEXT literals: static `TextLiteral.T` globals; `ConstTextLit` value kind
- [x] TEXT concatenation: `ConcatExpr.CompileMSIR` calls `RTHooks__Concat`
- [x] GC write barrier for heap fields: `QualifyExpr.LValueMSIR` sets pending container; `AssignStmt.CompileMSIR` calls `BuildGcStore` with container
- [x] GC write barrier for heap array elements: `SubscriptExpr.LValueMSIR` propagates pending container; array-element stores call `BuildGcStore`
- [x] `var_map`/`gc_map`: module globals embedded as trailing fields of `@Mod_M3_info`; TipeMap byte sequence for GC scanning; LLVM aliases for symbol compatibility
- [x] Nested procedures: **lambda-lifted** — `Stmt.Capture` pre-scans the body; each captured up-level variable becomes an explicit `ptr` param (`%__cap_0`, …); outer proc's up-level vars are ordinary allocas; multi-level nesting supported
- [x] Read-only scalar captures pass by value (not ptr): `CaptureAnalysis.written=FALSE` + scalar MSIR type → direct value param; GcRef stays by ptr for conservative GC
- [x] VALUE open-array formals: caller-side copy-in; `GenValueOpenArgMSIR` allocas element storage (eltType alignment), emits `@memcpy`, builds dope vector; fixed-size actuals and open actuals (dynamic element count, dynamic alloca) both supported
- [x] WIDECHAR text literals: encoded as little-endian bytes (`Target.WideCharSize()` per char); `[wcharBytes*len + wcharBytes x i8]` struct; negative `cnt` distinguishes from ASCII
- [x] TEXT library calls (`Fmt.Bool`, `Text.Length`, etc.): external calls emit correctly; calling convention matches C backend
- [x] Procedure values: `ProcExpr.CompileMSIR` → `MSIR.ConstProcRef(proc)` (`ptr @procname`); `NamedExpr.CompileMSIR` handles `Value.Class.Procedure` by folding to `ProcExpr`; auto-registers extern variables on demand for `FROM X IMPORT y` names; `EqualExpr.CompileMSIR` handles procedure equality as `icmp eq ptr`
- [x] Float type conversions: `FLOAT()` builtin via `SIToFP` (int→float) or `FPExt`/`FPTrunc` (float→float); cast ops `ZExt`/`SExt`/`Trunc` for integer widening/narrowing
- [x] EVAL, ASSERT, LOOP statements: `CompileMSIR` implementations
- [x] `MSIRType.Translate` maps `Type.Class.Procedure` to `TPtr(TVoid())`; `BindFormalMSIR` treats proc formals as by-value scalars (guards `Kind(EltType) ≠ Void`)
- [x] TRUNC/FLOOR/CEILING/ROUND builtins: `FPFloor`/`FPCeil`/`FPRound` unary float ops; lower to `llvm.floor.*`/`llvm.ceil.*`/`llvm.roundeven.*`; TRUNC emits direct `fptosi`; others emit rounding op then `fptosi`; ROUND uses `llvm.roundeven.*` (NearestElseEven = `FloatMode.RoundDefault`, per spec: `SetRounding` does not affect ROUND)
- [x] `IN` operator on SETs: `InExpr.CompileMSIR` emits `lshr(setVal, zext(elt - minOrd)) & 1 != 0`; works at any set width (single-word iN or IWide iN)
- [x] CONST array subscript: `NamedExpr.LValueMSIR` handles `Value.Class.Expr` for array types by calling `MSIRBuilder.MaterializeConstArray`; `ArrayExpr.EltCount`/`Elt` enumerate elements; per-element `Expr.CompileMSIR` yields constant MSIR values; result registered as `@constarray_N = private constant [N x T] [...]` global
- [x] Indirect (proc-variable) calls: `UserProc.CompileMSIR` handles non-literal, non-method case via `Expr.CompileMSIR(p.proc)` + `MSIRBuilder.EmitCallIndirect`; routes to `BuildCallIndirect` or `BuildInvokeIndirect` depending on active TRY context
- [x] CONST record field access: `QualifyExpr.CompileMSIR` folds `OK.rank` (CONST RECORD field) via `StripNamedCons + RecordExpr.Qualify` before attempting `LValueMSIR`
- [x] NEW(REF record, keyword args): `GenRefMSIR` initializes named fields after allocation via `KeywordExpr.Split + RecordType.LookUp + Field.Split + BuildPtrAdd + BuildStore`
- [x] ADR/BYTESIZE/BITSIZE/LOOPHOLE builtins; ADDRESS arithmetic via `PtrToInt`/`IntToPtr`/`PtrAdd`
- [x] SUBARRAY: fixed and open source arrays; WITH binding to sub-dope-vector; open-array slice via `MSIRBuilder.GenSubarray`
- [x] ISTYPE: `IsTypeExpr.CompileMSIR` via `RTHooks__CheckIsType`; type-link lookup
- [x] NARROW: `NarrowExpr.CompileMSIR` asserts type with `RTHooks__CheckIsType`; abandons and falls back on nil check failure
- [x] TYPECASE-with-var: binding variable tied to tested expression; block variable resolved from MSIR locals
- [x] TYPECODE: `TypecodeExpr.CompileMSIR` for both type forms and ref forms; type-link load via `MSIRBuilder.TypeLinkValue*`
- [x] SET type: constructor (`SetExpr.CompileMSIR` iterates `p.tree` constant ranges via lshr/shl/and/or + singleton OR for `p.others`); arithmetic (`+`, `-`, `*`, `/` → `or`, `and(a, not b)`, `and`, `xor`); comparison (`=`, `#`, `<=`, `<`); equality (`EqualExpr.CompileMSIR` icmp eq/ne); all widths supported via IWide `iN` (LLVM arbitrary-width integers); stored as `TI(info.size)` for ≤ 64 bits, `IWide(info.size)` for > 64 bits
- [x] Packed byte-array (BITS N FOR T): `SubscriptExpr.CompileMSIR` detects storage/natural-type width mismatch; emits load at storage type + ZExt/SExt/Trunc to natural type; `AssignStmt.CompileMSIR` emits Trunc when storing wider value to narrower slot
- [x] Compact subrange arrays (`[0..255]`, `[0..65535]`, BOOLEAN): `MSIRType.TranslateFixedArray` uses `ArrayType.EltPack(t)` to detect reduced storage width; element type in array IR uses storage width (e.g. `[N x i8]` for `[0..255]` elements); `SubscriptExpr.CompileMSIR` ZExt/SExt/Trunc on load to recover natural type
- [x] Records with compact/packed fields: `MSIRType.TranslateRecord` uses `fti.size` (storage bits) for LLVM struct field type when it differs from natural `Translate` result (e.g. `[0..255]` → i8, `[0..65535]` → i16, BOOLEAN → i8); `BitWidth > 0` guard prevents replacing traced-ref fields (TGcRef, BitWidth=-1) with `TI(64)`; `QualifyExpr.LValueMSIR` uses storage type for GEP pointer; `QualifyExpr.CompileMSIR` ZExt/SExt/Trunc via `LoadFieldValue` helper
- [x] Struct-by-value return (records, fixed arrays, large sets): hidden first `ptr` parameter (`_result_ptr`) — callee stores result through it and returns void; `BeginProc` detects `ProcType.LargeResult`, prepends hidden param, saves ptr as `curResultPtr`/`curResultType`; `ReturnStmt.CompileMSIR` stores through `curResultPtr` + `ret void`; `LookupOrCreateProc` mirrors convention for external stubs; `UserProc.CompileMSIR` allocas result slot, prepends to args, loads after call (nested large-result: Abandon)

### Lowering (MSIR → LLVM IR)
- [x] All scalar types, struct, fixed/open arrays, ptr/gc_ref
- [x] M3 symbol mangling (`Module.Proc` → `Module__Proc`)
- [x] `target triple` / `target datalayout` (ARM64_DARWIN, AMD64_DARWIN, AMD64_LINUX)
- [x] `invoke`/`landingpad`/`extractvalue`/`resume` (LLVM EH, ex_stack model)
- [x] `personality ptr @__gxx_personality_v0`, `@_ZTI6_M3Exc` extern
- [x] GcLoad inline read barrier (nil → misaligned → gray-bit → `RTHooks__CheckLoadTracedRef`)
- [x] GcStore write barrier (`container` operand; dirty-bit check + `RTHooks__CheckStoreTraced` for heap fields; globals skip)
- [x] `@Module_M3(i64 %mode)` RTLinker binder; `@Module_I3` interface binder
- [x] `@Module_M3_info` RT0.ModuleInfo descriptor with embedded module globals and gc_map
- [x] RAISE: `ExceptionDesc` static, `RTHooks__Raise`, Itanium ABI `__cxa_begin_catch` fix
- [x] `RT0.ImportInfo` chain in `MI_imports`: `BuildImportLink`; RTHooks filtered (pre-initialised)
- [x] TypeCell and ObjectTypeDesc (vtable) emission
- [x] TC_kind bytes: use `ORD(M3RT.TypeKind.*)` (was hardcoded 6/13); GcHeader and TEXT GEP offset use M3RT/Target constants
- [x] TextLiteral vtable method names resolved via `RunTyme.LookUpProc` → `MSIRBuilder.HookProc`; `EmitDeclare` derives signatures from M3 types (no hardcoded strings)
- [x] Procedure constants: `ConstProc` value kind lowers to `ptr @procname`
- [x] Cast instructions: `sitofp`, `fptosi`, `fpext`, `fptrunc`, `zext`, `sext`, `trunc`
- [x] Bitwise/shift instructions: `and`, `or`, `xor`, `shl`, `lshr`, `ashr` (via new `IAnd`/`IOr`/`IXor`/`IShl`/`ILShr`/`IAShr` ops)
- [x] `ProcSeen` in `CollectExterns` deduplicates by name (safety net against procMap overflow producing multiple stubs for the same external)

---

## Remaining Work (prioritised)

**Zero abandons** in the full p0/p1/p2 test suite. Remaining gaps are constructs
not exercised by p0/p1/p2 or architectural limitations.

### A. Large/runtime SET operations

Multi-word sets (> 64 bits) are fully implemented via `IWide` (`iN` LLVM type).
Runtime-element SET constructors (`T{lo..hi}` with non-constant bounds) emit an
`Abandon`; these are not exercised by p0/p1/p2.

### B. NEW(REF open-array): multi-dimensional

`GenOpenArrayMSIR` handles 1-D; multi-D untested.

### C. Opaque types

`GenOpaqueMSIR` handles only REF revelation; OBJECT revelation is deferred.

### D. Debug symbols

No source locations in emitted LLVM IR. See debug symbol architecture note in
`CLAUDE.md` for the natural hook points (`Scanner.offset`, `CG.Gen_location`,
`AddLocalMSIR`, `BeginProc`).

### E. TEXT: remaining cases

- `Fmt.Real` (floating-point formatting) — not yet exercised in tests
- `Text.Sub` and other TEXT manipulation operations — likely work (same pattern as `Fmt.Bool` / `Text.Length`) but not yet tested

---

## Known ABI Notes

- **Struct-by-value return (implemented)**: M3 procs with `ProcType.LargeResult` result (records, fixed arrays, large sets) use a hidden first `ptr` parameter — no `sret` attribute needed for M3-to-M3 calls. Mixed-width records (`ByteRec`, `MixedRec`) still use VAR output params because they would require `sret` for correct C interop; pure-M3 struct-return via hidden ptr is fully correct.
- **Nested large-result procs**: If a nested proc has a large result, its call site emits Abandon (the nested+large-result ordering of cap params + hidden ptr + explicit params is complex). Not exercised in the test suite.
- **procMap size**: `MaxProcMap = 2048` in `MSIRBuilder.m3`. For extremely large modules (>2000 unique external callees + internal procs), this could still overflow. The safe fix is to make procMap dynamic; increasing the constant is a stopgap.

---

## Test Infrastructure

```sh
# Full end-to-end LLVM link test (149 checks)
bash m3-sys/msir/test/run-llvm-link-test.sh

# Standalone M3 program (RTLinker path)
clang _m3main.cpp Main-llvm.o libm3core.a libm3.a -lc++ -o smoke-realrt
./smoke-realrt

# Just inspect the emitted IR
cd /any/m3-program && cm3 '@M3m3front-msir' -build
cat ARM64_DARWIN/Main.ll

# Full p0/p1/p2 abandon sweep
cd m3-sys/m3tests && rm -f /tmp/msir-debug.txt
for d in src/p0/p0?? src/p1/p1?? src/p2/p2??; do
  [ -d "$d" ] && (cd "$d" && cm3 '@M3m3front-msir' -build 2>/dev/null)
done
grep -c 'abandon' /tmp/msir-debug.txt 2>/dev/null || echo "0 abandons"
```
