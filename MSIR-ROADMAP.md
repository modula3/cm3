# MSIR Roadmap: Current Status

Last updated: 2026-05-18 (msir branch — language gap sweep complete)

## What's Working

**288/288 p0/p1/p2 tests compile with zero MSIR abandons.**
**149/149 end-to-end LLVM link test checks pass.**

The remaining 3 baseline entries are runtime timeouts (p161: million-element
sieve; p224: 303-thread mutex test expected to run minutes; p267: intentional
infinite GC loop) — not code-generation failures.

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
- [x] NEW(REF ARRAY OF T, n): `GenOpenArrayMSIR`; 1-D and multi-D open arrays
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
- [x] Non-constant `FOR` step: step expression compiled once and spilled to alloca; direction analysis via `Expr.GetBounds` — single header when sign is statically known, three-block runtime dispatch (`for.header → for.pos_test / for.neg_test`) for unknown-sign steps
- [x] `BITSIZE`/`BYTESIZE` of open array: `BuildOpenArraySize` per dimension, multiply dims, scale by `EltPack`; rounding-up division when `EltPack` not a multiple of `unit`
- [x] `MSIRType.Translate` maps `Type.Class.Procedure` to `TPtr(TVoid())`; `BindFormalMSIR` treats proc formals as by-value scalars (guards `Kind(EltType) ≠ Void`)
- [x] READONLY scalar formals: addressable via alloca spill — `BindFormalMSIR` spills all non-aggregate-pointer formals (VALUE and READONLY scalar) to an alloca; `t.indirect` guard prevents VALUE formals of pointer type (e.g. `p: IntPtr`) from being misclassified as aggregate-by-reference
- [x] `MSIRVerifier`: relaxed store/icmp pointer checks — all `Ptr`/`GcRef`/`GcSlot` pointer kinds are compatible in LLVM opaque-pointer mode; cross-kind pointer stores and pointer comparisons no longer emit false-positive type-mismatch errors
- [x] TRUNC/FLOOR/CEILING/ROUND builtins: `FPFloor`/`FPCeil`/`FPRound` unary float ops; lower to `llvm.floor.*`/`llvm.ceil.*`/`llvm.roundeven.*`; TRUNC emits direct `fptosi`; ROUND uses `llvm.roundeven.*` (NearestElseEven per spec)
- [x] `ABS` on float types: `Abs.AbsMSIR` emits `MSIR.BuildFPAbs` (`Op.FPAbs`); lowers to `llvm.fabs.f32` / `llvm.fabs.f64`
- [x] Non-scalar record/array equality: `EqualExpr.CompileMSIR` handles `Kind.Complex` for `Record`/`Array` via a byte-comparison loop; short-circuits on first differing byte
- [x] `IN` operator on SETs: `InExpr.CompileMSIR` emits `lshr(setVal, zext(elt - minOrd)) & 1 != 0`; works at any set width (single-word iN or IWide iN)
- [x] CONST array subscript: `NamedExpr.LValueMSIR` handles `Value.Class.Expr` for array types by calling `MSIRBuilder.MaterializeConstArray`; per-element `Expr.CompileMSIR` yields constant MSIR values; result registered as `@constarray_N = private constant [N x T] [...]` global
- [x] Indirect (proc-variable) calls: `UserProc.CompileMSIR` handles non-literal, non-method case via `Expr.CompileMSIR(p.proc)` + `MSIRBuilder.EmitCallIndirect`; routes to `BuildCallIndirect` or `BuildInvokeIndirect` depending on active TRY context
- [x] CONST record field access: `QualifyExpr.CompileMSIR` folds `OK.rank` (CONST RECORD field) via `StripNamedCons + RecordExpr.Qualify` before attempting `LValueMSIR`
- [x] NEW(REF record, keyword args): `GenRefMSIR` initializes named fields after allocation via `KeywordExpr.Split + RecordType.LookUp + Field.Split + BuildPtrAdd + BuildStore`
- [x] ADR/BYTESIZE/BITSIZE/LOOPHOLE builtins; ADDRESS arithmetic via `PtrToInt`/`IntToPtr`/`PtrAdd`
- [x] SUBARRAY: fixed and open source arrays; WITH binding to sub-dope-vector; open-array slice via `MSIRBuilder.GenSubarray`
- [x] ISTYPE: `IsTypeExpr.CompileMSIR` via `RTHooks__CheckIsType`; type-link lookup
- [x] NARROW: `NarrowExpr.CompileMSIR` asserts type with `RTHooks__CheckIsType`
- [x] TYPECASE-with-var: binding variable tied to tested expression; block variable resolved from MSIR locals
- [x] TYPECODE: `TypecodeExpr.CompileMSIR` for both type forms and ref forms; type-link load via `MSIRBuilder.TypeLinkValue*`
- [x] SET type: constructor, arithmetic (`+`,`-`,`*`,`/`), comparison, equality; all widths via IWide `iN`; stored as `TI(info.size)` for ≤ 64 bits, `IWide(info.size)` for > 64 bits
- [x] Packed byte-array (BITS N FOR T): storage/natural-type width mismatch → ZExt/SExt/Trunc on load; Trunc on store to narrower slot
- [x] Compact subrange arrays (`[0..255]`, `[0..65535]`, BOOLEAN): `ArrayType.EltPack` detects reduced storage width; element type uses storage width; ZExt/SExt/Trunc on load
- [x] Records with compact/packed fields: `MSIRType.TranslateRecord` uses `fti.size` (storage bits) when it differs from natural `Translate` result; `BitWidth > 0` guard prevents traced-ref fields from being collapsed; `QualifyExpr.CompileMSIR` ZExt/SExt/Trunc via `LoadFieldValue`
- [x] Struct-by-value return (records, fixed arrays, large sets): hidden first `ptr` parameter (`_result_ptr`) — callee stores result through it and returns void; caller allocas result slot, prepends to args, loads after call
- [x] Sub-byte BITS-N-FOR-T packed fields: `MSIRType.ByteArrayFallback` (`[N x i1]` sentinel); field reads via `MSIRBuilder.ExtractBitField`; writes via `MSIRBuilder.InsertBitField` (read-modify-write on one or two bytes); `QualifyExpr.LValueMSIR` returns NIL silently for sub-byte fields
- [x] Packed record constructors: `RecordExpr.CompileMSIR` detects `[N x i1]` ByteArrayFallback, zero-fills, inserts each field via `InsertBitField` or byte-aligned store
- [x] Nested arrays of aggregate element types: `MSIRType.TranslateFixedArray` guards EltPack override with `MSIR.BitWidth(eltMsir) > 0`; prevents aggregate types (GcRef, FixedArray) with `BitWidth = -1` from being collapsed
- [x] Sub-byte packed-element array subscript (`ARRAY OF BITS N FOR T`, eltPack ∈ {1,2,4}): `MSIRBuilder.ExtractBitFieldDyn`/`InsertBitFieldDyn` compute dynamic byte/bit offsets; eltPack ∈ {1,2,4} (divides 8) only; others Abandon
- [x] LONGINT eliminated from both `msir/src` and `m3front/src/msir/`: all uid fields, constant APIs, pointer-offset APIs, and array-length APIs use INTEGER; `MSIRBuilder.ConstInt(t, v: INTEGER)` replaces the former `MSIR.ConstInt(t, v: LONGINT)` pattern throughout

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
- [x] Portability: all LLVM integer widths derive from `Target.Integer.size` / `Target.Address.size`; TypeCell struct uses `i{IP}` for INTEGER fields, `i64` only for the always-64-bit fingerprint; padding field `[N x i8]` omitted entirely when N=0 on 32-bit targets
- [x] TextLiteral vtable method names resolved via `RunTyme.LookUpProc` → `MSIRBuilder.HookProc`
- [x] Procedure constants: `ConstProc` value kind lowers to `ptr @procname`
- [x] Cast instructions: `sitofp`, `fptosi`, `fpext`, `fptrunc`, `zext`, `sext`, `trunc`
- [x] Bitwise/shift instructions: `and`, `or`, `xor`, `shl`, `lshr`, `ashr`
- [x] `ProcSeen` in `CollectExterns` deduplicates by name (prevents multiple stubs for the same external)

---

## Remaining Work (prioritised)

### 1. Remaining language gaps

Live `Abandon` paths that real programs outside the p0/p1/p2 suite can hit.
Items marked [done] are fixed on the msir branch.

- [done] **`WITH` unhandled kinds**: all 4 `Kind` values handled; dead ELSE removed.
- [done] **`array-type store mismatch`**: `AssignStmt.CompileMSIR` now handles
  FixedArray←OpenArray (memcpy) and OpenArray←FixedArray (load data_ptr + store)
  for any element type combination.
- [done] **`SUBARRAY` of rank > 1 open source**: `LValueMSIR` now computes
  stride = eltBytes * size[1] * … * size[N-1], allocates rank-N result dope, and
  copies inner dimension sizes from source.
- [done] **`NEW(REF record, >1 keyword arg)`**: `GenRefMSIR` already iterates all
  keyword args (fixed in an earlier commit; never an active bug in this session).
- [done] **VALUE open-array partial depth coercion** (`actDepth < formDepth`):
  `GenValueOpenArgMSIR` now loads `actDepth` dynamic dims from the actual dope and
  reads the remaining `formDepth - actDepth` dims from M3 type constants.
- **Nested proc `PROCEDURE` values**: taking a `PROCEDURE` value of a
  lambda-lifted nested proc requires a trampoline or `{proc_ptr, env_ptr}` closure.
  The nested proc's MSIR function has extra capture params that the PROCEDURE type
  doesn't expose; a call through the proc variable would use the wrong ABI.
  Deferred: requires heap-allocated trampolines (GC-visible) or a closure ABI change.
- **`eltPack` not divisible by 8 in sub-byte array subscript**: only
  eltPack ∈ {1,2,4} handled; other values Abandon.  Elements with eltPack ∈ {3,5,6,7}
  can straddle byte boundaries, requiring a 2-byte load or a dynamic branch.
  Deferred: extremely rare in practice.

### 2. Make MSIR the default backend on ex_stack platforms

The biggest architectural step: when `M3_USE_STACK_WALKER = TRUE`,
cm3 should drive MSIR → LLVM instead of M3CG → C by default.

Concretely:
- Wire `@M3m3front-msir` into the build driver so it activates
  automatically on ex_stack targets without a manual flag.
- Drive `opt` + `llc` (or `clang`) in the build pipeline in place of
  the C compiler invocation.
- The C backend (`M3C.m3`) remains intact for non-ex_stack platforms.

This is the gating item for LLVM optimizer integration and bootstrap.

### 3. Debug symbols

No source locations in emitted LLVM IR. Natural hook points:
`Scanner.offset` (current source position), `CG.Gen_location` (CG path
equivalent), `MSIRBuilder.BeginProc` (function-level DISubprogram),
`Variable.AddLocalMSIR` (DILocalVariable + dbg.declare). Self-contained
additive work; does not affect correctness. Prerequisite for usable
`lldb` integration.

### 4. Dynamic procMap

`MaxProcMap = 2048` in `MSIRBuilder.m3` is a fixed-size array. Replacing
it with a heap-allocated table removes a latent overflow risk for large
modules with many unique external callees. Mechanical change; no IR impact.

### 5. LLVM optimizer integration

**Deferred until MSIR is the default backend (item 2 above).**

Optimization level must be controlled by cm3's existing flags (`-O`, `-O2`,
`-O3` / `M3_OPTIMIZE` in the platform config), not bolted on independently.
Recommended first step: pipe `.ll` through `opt -On | llc` in the build
driver; then migrate to LLVM-C API bitcode emission to eliminate the text
round-trip.

### 6. Statepoint / precise GC

MVP uses conservative stack scanning (Bartlett interior-pointer pinning).
LLVM's statepoint intrinsics (`gc.statepoint`, `gc.relocate`, `gc.result`)
enable precise stack maps, eliminating the conservative scan and the
`addrspacecast` at open-array ABI boundaries.

This is a transformation pass over already-built MSIR: insert
`gc.statepoint` wrappers around calls, thread `gc.relocate` for every
live `gc_ref` across call sites. CM3's mostly-copying collector is closer
to the statepoint model than typical framings suggest — the to-space stack
invariant already guarantees the collector never moves an object that a
conservative interior pointer points into.

---

## Known ABI Notes

- **Struct-by-value return**: M3 procs with `ProcType.LargeResult` result (records, fixed arrays, large sets) use a hidden first `ptr` parameter — no `sret` attribute needed for M3-to-M3 calls.
- **Nested large-result procs**: If a nested proc has a large result, its call site emits Abandon (cap params + hidden ptr + explicit params ordering is complex). Not exercised in the test suite.
- **Nested procedure calling convention**: Lambda-lifted nested procs have capture-pointer arguments prepended before explicit parameters: `proc(%__cap_0: ptr, %__cap_1: ptr, …, arg0, arg1, …)`. Read-only scalar captures pass by value; GcRef captures always pass by pointer (conservative GC stack-scan requirement). Call sites reconstruct the capture arg list via `MSIRBuilder.GetProcCaptures`. Taking a `PROCEDURE` value of a nested proc is not yet supported (Abandon).
- **Open-array wire format**: The dope vector is `{ ptr data, i64 nElts }` for rank-1, extended to `{ ptr data, i64 nElts, i64 dim0, … }` for rank-N. `data` points to the first element. VALUE open-array formals copy-in at the call site.
- **EH personality — `ex_stack` platforms only**: MSIR uses the Itanium C++ personality (`@__gxx_personality_v0`, `invoke`/`landingpad`/`resume`). Linking MSIR-compiled modules against an `ex_frame` runtime is incorrect.
- **READONLY and VALUE scalar formals are spilled to alloca**: `BindFormalMSIR` spills all non-aggregate-pointer formals to an alloca so that `ADR(formalParam)` is always valid — matching M3 semantics.
- **procMap size**: `MaxProcMap = 2048` in `MSIRBuilder.m3`. For extremely large modules (>2000 unique external callees + internal procs), this could overflow. The safe fix is a dynamic table; increasing the constant is a stopgap.

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

# p0/p1/p2 abandon sweep (288 tests)
cd m3-sys/msir/test
python3 sweep.py baseline      # run all 288 test dirs; write sweep-baseline.json
python3 sweep.py check         # re-run only tests with prior abandons; report Δ
python3 sweep.py summary       # tabulate abandon counts by message
python3 sweep.py grep <msg>    # list tests containing a specific abandon text
```
