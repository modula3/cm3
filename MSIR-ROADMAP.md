# MSIR Roadmap: Current Status

Last updated: 2026-05-17 (msir branch)

## What's Working (149/149 tests pass)

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
- [x] READONLY scalar formals: addressable via alloca spill — `BindFormalMSIR` spills all non-aggregate-pointer formals (VALUE and READONLY scalar) to an alloca; `t.indirect` guard prevents VALUE formals of pointer type (e.g. `p: IntPtr`) from being misclassified as aggregate-by-reference
- [x] `MSIRVerifier`: relaxed store/icmp pointer checks — all `Ptr`/`GcRef`/`GcSlot` pointer kinds are compatible in LLVM opaque-pointer mode; cross-kind pointer stores (e.g. `store alloca-result, ADDRESS-slot`) and pointer comparisons no longer emit false-positive type-mismatch errors
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
- [x] Sub-byte BITS-N-FOR-T packed fields (field offset or size not a byte multiple): `MSIRType.ByteArrayFallback` represents the enclosing record/array as `[N x i1]` (one i1 per bit, sentinel distinguishing it from byte arrays); field reads via `MSIRBuilder.ExtractBitField` (one or two `i8` loads, shift+mask, ZExt/SExt to natural M3 type); field writes via `MSIRBuilder.InsertBitField` (read-modify-write on one or two bytes); `QualifyExpr.LValueMSIR` returns NIL silently for sub-byte fields; `QualifyExpr.CompileMSIR` and `AssignStmt.CompileMSIR` detect the nil lvalue and route to the bitfield helpers
- [x] Packed record constructors: `RecordExpr.CompileMSIR` detects `[N x i1]` result type (ByteArrayFallback), zero-fills the byte array, then inserts each field value via `MSIRBuilder.InsertBitField` (sub-byte) or direct byte-aligned store (byte-multiple); nested packed aggregate fields are copied byte-by-byte via `i8` load/insert loops
- [x] Nested arrays of aggregate element types (`ARRAY OF ARRAY OF gc_ref`, etc.): `MSIRType.TranslateFixedArray` guards the EltPack storage-width override with `MSIR.BitWidth(eltMsir) > 0`; prevents aggregate element types (GcRef, FixedArray, …) with `BitWidth = -1` from being collapsed to `IWide(eltPack)`, which would break inner subscripts
- [x] `MSIRBuilder.ExtractBitField` / `MSIRBuilder.InsertBitField`: shared bitfield helpers exported from the builder layer so `QualifyExpr`, `RecordExpr`, and future callers can use them without circular imports; use `curBlock` directly (no `b` parameter)

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
- [x] Portability: all LLVM integer widths derive from `Target.Integer.size` / `Target.Address.size` (no hardcoded `i64`); OpenArray size fields, GC header/barrier operands, alloca count, GEP byte offsets, binder signatures all parameterised; TypeCell struct uses `i{IP}` for INTEGER fields, `i64` only for the always-64-bit fingerprint; padding field `[N x i8]` where `N = Target.Integer.bytes - 4` (omitted entirely when N=0 on 32-bit targets); verified field-by-field against `M3RT.m3` layout formulas and CG `Init_intt` call sites in `RefType.m3` / `ObjectType.m3`
- [x] TextLiteral vtable method names resolved via `RunTyme.LookUpProc` → `MSIRBuilder.HookProc`; `EmitDeclare` derives signatures from M3 types (no hardcoded strings)
- [x] Procedure constants: `ConstProc` value kind lowers to `ptr @procname`
- [x] Cast instructions: `sitofp`, `fptosi`, `fpext`, `fptrunc`, `zext`, `sext`, `trunc`
- [x] Bitwise/shift instructions: `and`, `or`, `xor`, `shl`, `lshr`, `ashr` (via new `IAnd`/`IOr`/`IXor`/`IShl`/`ILShr`/`IAShr` ops)
- [x] `ProcSeen` in `CollectExterns` deduplicates by name (safety net against procMap overflow producing multiple stubs for the same external)

---

## Remaining Work (prioritised)

**Zero abandons** in the full p0/p1/p2 test suite. Remaining gaps are constructs
not exercised by p0/p1/p2 or architectural limitations.

### A. Large/runtime SET operations — complete

Multi-word sets (> 64 bits) are fully implemented via `IWide` (`iN` LLVM type).
Runtime-element SET constructors (`T{lo..hi}` with non-constant bounds) are now
implemented via `lshr(ones,(size-1)-hi) AND shl(ones,lo)` — naturally yields 0
for empty ranges (lo > hi).  Verified with a standalone test for S{lo..hi} with
non-constant lo/hi.

### B. NEW(REF open-array): multi-dimensional — complete

`GenOpenArrayMSIR` correctly handles any number of open dimensions: the sizes
struct `{ ptr elt_ptr, i64 count, i64 dim0, ... }` is filled in by a
`FOR i := 1 TO ndims` loop; `elt_ptr` points to `dim0` inside the struct,
giving `RTHooks__AllocateOpenArray` a valid `RTHooks__ArrayShape*`.

Also fixed: `Subarray.m3` `LValueMSIR`/`CompileMSIR` used hardcoded SSA names
(`"sa.base"`, `"sa.off"`, `"sa.elt"`, `"sa.dope"`) that collided when two
SUBARRAY expressions appeared in the same procedure.  All hints replaced with
`""` so the builder auto-numbers them uniquely.  Verified with p260 (1-D through
4-D NEW, multiple SUBARRAY in one proc), all pass end-to-end.

### C. Opaque types — complete

`GenOpaqueMSIR` now handles both revelation kinds:
- REF revelation → `GenRefMSIR(x, r, ce)`
- OBJECT revelation → `GenObjectMSIR(x, ce)` (dispatched to concrete type)
- NIL or unknown revelation → Abandon with descriptive message

`MSIRType.Translate` for `Type.Class.Opaque` (and `Object`) now uses `info.isTraced`
to emit `TGcRef(void*)` for traced types and `TPtr(void*)` for untraced types.

### D. Debug symbols

No source locations in emitted LLVM IR. See debug symbol architecture note in
`CLAUDE.md` for the natural hook points (`Scanner.offset`, `CG.Gen_location`,
`AddLocalMSIR`, `BeginProc`).

### E. TEXT: remaining cases — complete

All TEXT library calls (`Fmt.Real`, `Fmt.LongReal`, `Text.Sub`, `Text.Equal`,
`Text.Cat`, etc.) work via the existing external-call pattern.

Also fixed: `MSIRToLLVM.EmitFloatHex` was emitting 32-bit float IEEE bits
zero-padded into the high 4 bytes of a 64-bit hex literal, which LLVM rejects
for `float` type (the double value would be out of float range).  LLVM requires
that a `float` hex literal be the 64-bit double representation of the same
value.  `EmitFloatHex` now properly widens float32 → double64 (rebias exponent
by 896, extend mantissa by 29 bits) before emitting the 16-hex-digit literal.
Verified: `Fmt.Real(3.14)`, `Fmt.Real(0.0)`, `Fmt.Real(-1.5)`, `Fmt.Real(1.0e10)`
all produce correct output; `Text.Sub`, `Text.Equal`, `Text.Length` also confirmed.

---

## Known ABI Notes

- **Struct-by-value return**: M3 procs with `ProcType.LargeResult` result (records, fixed arrays, large sets) use a hidden first `ptr` parameter — no `sret` attribute needed for M3-to-M3 calls. Mixed-width records (`ByteRec`, `MixedRec`) still use VAR output params because they would require `sret` for correct C interop; pure-M3 struct-return via hidden ptr is fully correct.
- **Nested large-result procs**: If a nested proc has a large result, its call site emits Abandon (the nested+large-result ordering of cap params + hidden ptr + explicit params is complex). Not exercised in the test suite.
- **Nested procedure calling convention**: Lambda-lifted nested procs have capture-pointer arguments prepended before explicit parameters: `proc(%__cap_0: ptr, %__cap_1: ptr, …, arg0, arg1, …)`. Read-only scalar captures pass by value; GcRef captures always pass by pointer (conservative GC stack-scan requirement). Call sites reconstruct the capture arg list via `MSIRBuilder.GetProcCaptures`. Taking a `PROCEDURE` value of a nested proc is not yet supported (Abandon).
- **Open-array wire format**: The dope vector is `{ ptr data, i64 nElts }` for rank-1, extended to `{ ptr data, i64 nElts, i64 dim0, … }` for rank-N. `data` points to the first element. This matches the RT0 open-array descriptor that `RTHooks__AllocateOpenArray` and `openarray.size`/`openarray.data` expect. VALUE open-array formals copy-in at the call site: the caller allocas element storage, emits a `memcpy`, then builds a fresh dope vector; the callee receives an independent stack copy.
- **EH personality — `ex_stack` platforms only**: MSIR uses the Itanium C++ personality (`@__gxx_personality_v0`, `invoke`/`landingpad`/`resume`). This is ABI-compatible only with the `ex_stack` runtime (`M3_USE_STACK_WALKER = TRUE`). Linking MSIR-compiled modules against an `ex_frame` (`setjmp`/`longjmp`) runtime is incorrect; the `TRY` frames will not be registered and exceptions will not unwind through MSIR frames.
- **READONLY and VALUE scalar formals are spilled to alloca**: `BindFormalMSIR` spills all non-aggregate-pointer formals (both VALUE and READONLY scalars) to an alloca so that `ADR(formalParam)` is always valid — matching M3 semantics. The C backend may keep scalars in registers, so the generated IR differs for READONLY params even when `ADR` is never taken.
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

# p2xx abandon sweep — capture baseline once, then use incremental check
cd m3-sys/msir/test
python3 sweep.py baseline      # run all 89 test dirs; write sweep-baseline.json
python3 sweep.py check         # re-run only tests with prior abandons; report Δ
python3 sweep.py summary       # tabulate abandon counts by message
python3 sweep.py grep <msg>    # list tests containing a specific abandon text
```
