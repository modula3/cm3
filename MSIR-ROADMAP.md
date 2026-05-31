# MSIR Roadmap: Current Status

Last updated: 2026-05-26 (FOR-loop upper-bound snapshot; global const-init in module body; p163/p185 cleared)

## What's Working

**288/288 p0/p1/p2 tests compile with zero MSIR abandons.**
**181/181 end-to-end LLVM link test checks pass.**

The remaining 2 baseline entries are runtime timeouts (p161: million-element
sieve; p267: intentional infinite GC loop) — not code-generation failures.

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
- [done] **Nested proc `PROCEDURE` values**: stack-allocated M3RT fat-pointer closure
  `{CL_marker=-1, CL_proc=shim, CL_frame=env}` at offsets 0/IP/IP+AP; a generated
  closure shim bridges the lambda-lifted ABI to the plain `PROCEDURE` ABI; indirect
  calls emit a CL_marker runtime check to dispatch closure vs plain proc pointer.
  Large-result nested procs also supported (hidden result ptr placed at arg 0 before
  captures). See D15 in MSIR-design.md for design rationale.
- **`eltPack` not divisible by 8 in sub-byte array subscript**: only
  eltPack ∈ {1,2,4} handled; other values Abandon.  Elements with eltPack ∈ {3,5,6,7}
  can straddle byte boundaries, requiring a 2-byte load or a dynamic branch.
  Deferred: extremely rare in practice.

### 2. Activate MSIR via explicit M3_BACKEND_MODE — **DONE**

**Complete as of 2026-05-30.**  All plumbing is committed on the `msir` branch.

What was done:
- `Target.i3`: `M3BackendMode_t` extended with `MSIRObj` ("11") and `MSIRAsm` ("12");
  `BackendMSIRSet`, `BackendModeStrings`, and `BackendSet` updated accordingly.
- `Builder.m3`: `CompileM3` plan block handles `MSIRObj`/`MSIRAsm` — sets
  `llvmIRName`, `DoRunLlc`, etc.; `GetConfig` enforces `M3_USE_STACK_WALKER = TRUE`
  when the mode is in `BackendMSIRSet`; `ResetEnv` calls `MSIREmit.SetLLOutPath`.
- `MSIREmit.m3`: `IsEnabled` checks `Target.BackendMode IN Target.BackendMSIRSet`
  in addition to the `@M3m3front-msir` RTParam; `SetLLOutPath` routes `.ll` output
  to the builder-supplied path.
- `cm3cfg.common`: `MSIRObj`/`MSIRAsm` included in the recognised backend-mode set.
- `M3CG_DoNothing.m3`: no-op backend discards CG output in MSIR mode; no `.cpp`
  file is opened.

Pipeline (MSIRObj):
```
.m3 → RunM3Front (MSIR emission → <Module>.ll) → RunLlcBack (llc) → <Module>.o
```

What remains for full activation: flip `M3_BACKEND_MODE = "MSIRObj"` in the
**installed** config (`~/cm3/bin/config/ARM64_DARWIN`) and rebuild the full
stack (see §2a Self-hosting below).  The source-tree config
(`m3-sys/cminstall/src/config/ARM64_DARWIN`) still ships with `"C"` so
bootstrap tarballs built from HEAD continue to work.

Constraint: MSIR requires `ex_stack` — `M3_BACKEND_MODE = "MSIRObj"` without
`M3_USE_STACK_WALKER = TRUE` is a fatal configuration error caught at build startup.

### 2a. Self-hosting

The decisive next milestone: compile the full compiler stack in MSIRObj mode
and verify that the resulting `cm3` binary passes the test suite.

**Why this matters**: p0/p1/p2 cover the language well in isolation but the
compiler itself is a much larger Modula-3 program — it uses generic modules,
deep object hierarchies, wide exception handling, and complex control flow that
the test suite does not exercise at the same scale.  A self-hosted binary is
the production-readiness proof.

**How to attempt it**:

```sh
# 1. Enable MSIRObj in the installed config
#    Edit ~/cm3/bin/config/ARM64_DARWIN:
#      M3_BACKEND_MODE = "MSIRObj"   (uncomment; comment out "C" line)

# 2. Make sure llc is on PATH
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

# 3. Rebuild the full compiler stack
scripts/concierge.py upgrade      # m3core → libm3 → m3middle → … → cm3
# Or just the front-end packages:
./scripts/do-cm3-front.sh buildglobal

# 4. Install and sign
cp m3-sys/cm3/ARM64_DARWIN/cm3 ~/cm3/bin/cm3
codesign -s - ~/cm3/bin/cm3

# 5. Run the test suite
cd m3-sys/m3tests && ~/cm3/bin/cm3 -DHTML
```

**What to watch for**:
- New MSIR abandons in compiler packages (m3front, m3back, m3middle, cm3, m3quake).
  These are the gaps that self-hosting will flush out.
- Linker errors due to stale `.o` files.  Do a full `cm3 -clean` in each package
  before the MSIRObj build.
- EH mode mismatches (duplicate d/D `_*_M3_info` symbols from leftover C-mode
  objects).  Check with `nm ~/cm3/bin/cm3 | grep "_M3_info" | grep " d "`.
- `MaxProcMap` overflow in large modules (see §6).

**Build-order trap (2026-05-31):** `cm3` links the *shipped* `libm3front.a`,
not the local `m3-sys/m3front/ARM64_DARWIN` build.  After editing m3front you
must `cm3 -build && cm3 -ship` it *before* rebuilding cm3, or the new cm3
silently links the old front-end.  A stale/inconsistent shipped libm3front.a
makes the fresh cm3 SIGSEGV at startup in `M3Front.Initialize → Scanner.Push`
(NIL deref on the indirect large-global pointer for `Scanner.stack`).  Existing
binaries keep working only because their m3front is statically baked in.  If a
freshly built cm3 crashes in Initialize, re-ship m3front from a clean tree and
rebuild cm3 before suspecting a source bug.

**CORRECTION (2026-05-31): MSIRObj m3core compiles+links but does NOT run yet.**
The earlier "m3core self-hosts" claim was wrong.  `cm3 -build` of m3core in
MSIRObj "succeeds" (archives `libm3core.a`) but `msir-verify` errors are
**non-fatal warnings** and `llc` tolerates/keeps stale `.o`, so broken modules
are silently archived.  The milestone `hello` test linked the *C-mode* m3core
dylib (m3core was built but not shipped at that point), so MSIRObj m3core was
never actually executed.  When MSIRObj m3core IS shipped and run, any dynamically
linked program (and a freshly built cm3, which statically embeds m3core) crashes
— e.g. `hello` SIGSEGVs in `RTLinker__InitRuntime`.  Recovery: rebuild+ship
C-mode m3core and rebuild cm3 against it (a backup cm3 that statically embeds a
good m3core is needed to bootstrap out, since the broken one crashes on -build).

Building m3core in MSIRObj surfaces **many masked codegen bugs**, all reported by
`msir-verify` but currently ignored: empty blocks in TRY / CASE / loop constructs
(`RuntimeError__Self`, `RTHeapMap__Walk`, `RTCollector__GrowHeap`, …) and `icmp
operand type mismatch` (`RTHeapStats__Scan*`).  **Action items:** (a) make
`msir-verify` failures FATAL in MSIRObj mode (and/or fail the build on `llc`
verifier errors) so broken modules can't be silently shipped; (b) fix the
empty-block control-flow emission (likely a shared root cause across TRY/CASE/
loop — blocks left unterminated/unpopulated) and the icmp width mismatch.

**Two fixes already landed toward this:**

1. *Interface-variable ownership.*  An interface variable has a single owner: the
   interface unit that declares it.  It appears in the interface's localScope and
   in every importing/re-exporting module's importScope.  The interface unit now
   DEFINES the storage in its `@<Intf>_M3_info` at the front-end byte offset
   (`t.offset`); the MSIR `ModuleAllocGlobal` layout coincides with `t.offset`
   because both reserve the same `MI_nFields * Address.bytes` header (= `MI_SIZE`)
   and lay out localScope globals in the same order with the same sizes/aligns —
   so the import chain (which uses `t.offset`) addresses the right field.  Modules
   route the variable to `RegisterExternMSIR`'s `imported AND NOT external` import
   chain, discriminated by the **External wrapper's** `imported` flag (mirroring
   `External.Declare`'s flag copy in `Module.DeclareGlobalsMSIR`).  The old
   `IF MSIR.ModuleIsInterface(m) THEN RETURN` guard in
   `Variable.DeclareGlobalMSIR` was lifted so the interface becomes the owner.
   This removed all the `_RTHeapRep__p0`/`_max_heap`/… duplicate definitions.

2. *Weak `_I3` binder.*  A `MODULE Foo` paired with a separate `INTERFACE Foo`
   must not emit a second strong `@Foo_I3`.  The `i3InImports` heuristic fails for
   `RTHooks` because `MSIREmit.RegisterImport` deliberately excludes `RTHooks_I3`
   from the import chain, so `MODULE RTHooks` re-defined `@RTHooks_I3` and clashed
   with the interface.  A non-interface unit now emits its own `@<Mod>_I3` as LLVM
   `weak` (MSIRToLLVM.m3), so the interface's strong definition always wins and a
   genuinely standalone module still keeps its own.

3. *objTypeMethod static supercall (2026-05-31).*  `T.m(obj, …)` (a method named
   through its type, not an instance — a supercall to T's binding of `m`) is now
   lowered by `UserProc.CompileMSIR`: when `QualifyExpr.LhsExpr = NIL` it loads
   the proc from T's typecell `OTC_defaultMethods` table (`TypeLinkValueForObject`
   + `M3RT.OTC_defaultMethods`/`Char.size` + method byte offset) and emits an
   indirect call (`EmitClosureCall`), the receiver being the first explicit
   actual.  New accessor `QualifyExpr.ObjTypeMethod`.  This clears the
   `BuiltinSpecials2` (`Special.write`) abandon AND the m3core
   `FloatMode`/`RealFloat`/`LongFloat` empty-block abandons.  Verified: smoke
   181/181; correct IR (loads typecell, OTC table, proc; indirect call).  KNOWN
   RISK (unvalidated, since MSIRObj m3core doesn't run): `TypeLinkValueForObject`
   resolves at RTLinker link time, so a supercall reached *before* the owner's
   TypeLink is resolved (very early init) would read an unresolved cell — if such
   a case appears, switch to a static `ObjectTypeCellRef`.

**Next:** the immediate blocker is no longer "extend up the stack" but **make
MSIRObj m3core actually run**: turn on fatal verification, then fix the
empty-block (TRY/CASE/loop) and icmp-width codegen bugs it exposes.  Only once a
shipped MSIRObj m3core runs can libm3 → … → cm3 self-hosting proceed (libm3 also
needs the `Capability__New` terminator-mid-block fix and the `FmtBuf`
const-array / uncaught-emitter-exception / void-value fixes).  Watch for
`MaxProcMap` overflow (§6).

**libm3 blocker — `objTypeMethod` supercall (2026-05-31):** building libm3 in
MSIRObj stops at `BuiltinSpecials2.m3` with `msir-abandon: method call: cannot
get receiver` (→ verifier "empty block", since an abandon mid-proc in MSIRObj
mode leaves malformed IR — there is no C fallback).  The construct is a *static
supercall* through a type: `Special.write(sp, r, pwr)` where `sp: Special` is a
parameter and `Special <: SpecialPublic` is opaque.  `T.m(obj, …)` denotes type
T's own binding of method `m` (the default/inherited body), NOT `obj`'s dynamic
override — the receiver is passed explicitly as the first actual.

  The C path (`QualifyExpr.Compile`, `Class.objTypeMethod`, lines ~455-468) loads
  the proc from the **type's** default-methods table: `Type.LoadInfo(objType,
  M3RT.OTC_defaultMethods, addr:=TRUE)` then `Load_indirect(method.offset)`, then
  an indirect call.  This works for opaque types because the table is reached via
  the typecell at runtime.  MSIR has no handler: `UserProc.CompileMSIR` abandons
  at `QualifyExpr.LhsExpr = NIL` ("cannot get receiver"), and
  `QualifyExpr.CompileMSIR` has no `objTypeMethod` case.

  *Fix recipe* (mirrors `MSIRBuilder.EmitMethodCall`, which loads a proc from the
  object vtable): in `UserProc.CompileMSIR`'s `objExpr = NIL` branch — (1) get the
  object type + holder + `method.offset` (add a `QualifyExpr` accessor); (2) typecell
  ptr via `MSIRBuilder.TypeLinkValueForObject(objType)`; (3) load the
  `OTC_defaultMethods` field (byte offset `M3RT.OTC_defaultMethods`) to get the
  table; (4) load the proc at word index `method.offset DIV Address.size`
  (+ `ObjectType.MethodOffset(holder)` when ≥ 0; abandon the runtime-`methodOffset`
  case for now); (5) `EmitClosureCall(fn, rtype, args)` with `args` = `p.args`
  (receiver already included).  **Prerequisite to verify first:** that MSIR's
  emitted object typecell (`@tc_obj_<uid>`) lays out `OTC_defaultMethods` at the
  RT0 byte offset the load assumes.  NOTE: a tempting `methodInfo.dfault`
  static-resolution shortcut is wrong — `dfault` is the declaration-site default,
  not type T's binding (differs under override), and is NIL for opaque types.

  This is expected to be the first of several codegen gaps libm3 exercises that
  m3core did not (libm3 is larger and uses more of the object system).

**Success criterion**: `cm3 -DHTML` in `m3-sys/m3tests` passes at least as many
tests as the C-backend baseline (currently 286/288 — the 2 TIMEOUTs are not
codegen failures).

### 3. Debug symbols

**Phase 1 (complete, 2026-05-19):** `DICompileUnit` / `DIFile` /
`DISubprogram` per proc. `DW_LANG_Modula3` language tag. `DILocation(line:0)`
stub on call/invoke instructions satisfies LLVM verifier. `llvm-as` accepts
IR with zero warnings; `llvm-dwarfdump` shows correct `DW_TAG_subprogram`
entries. Enables function-name backtraces and function-entry breakpoints in
`lldb`.

**Phase 2 (complete, 2026-05-19):** Variable declarations. What works:

- `DISubprogram` now carries separate `name` (short display: `"Sum"`) and
  `linkageName` (mangled linker: `"Main__Sum"`); LLDB shows the display name
  in backtraces and function listings.
- One `DINamespace(name: "Main", scope: !cu)` per module; all subprograms
  scoped to it.  Enables `Main::Sum` lookups in LLDB.
- Nine fixed `DIBasicType` nodes: `INTEGER` (DW_ATE_signed 64),
  `CARDINAL` (DW_ATE_unsigned 64), `INTEGER32` / `CARDINAL32` (32-bit
  variants), `BOOLEAN` (DW_ATE_boolean 1), `REAL` (DW_ATE_float 32),
  `LONGREAL` (DW_ATE_float 64), `CHAR` (DW_ATE_unsigned_char 8),
  `ADDRESS` (DW_ATE_address 64).
- `DILocalVariable` + `@llvm.dbg.declare` for allocas whose element type maps
  to one of the nine basic types.  `frame variable` in LLDB shows variable
  names, values, and types for scalar and pointer-typed locals.
- Internal MSIR temporaries filtered: `%__xxx` (double-underscore helpers) and
  `%t<digits>` (freshName-generated unnamed allocas) are excluded so they do
  not appear as spurious variables in LLDB.

**Known Phase 2 gaps** (tracked for Phase 3):

| Gap | Detail |
|---|---|
| Untracked alloca types | `Struct`, `OpenArray`, `HeapArray`, `FixedArray`, `ProcType`, `Set`, `Object`, `Subrange`, `IWide`, `I8/I16/W16` — all yield btIdx = -1, no DILocalVariable emitted |
| VAR/READONLY aggregate params | Passed as raw pointers into the proc; no alloca created, so invisible to LLDB (`frame variable` can't show them without a `DW_AT_location` expression) |
| Per-variable declaration line | All variables in a proc use the proc's source line as their `decl_line`. `Variable.T.origin` carries the real scanner offset but is not threaded through MSIR yet |
| `_result` variable | The implicit return-value alloca appears in `frame variable` as `_result`. This is technically correct (it IS a named M3 variable) but may surprise users expecting C-style return semantics |
| `AllocaDyn` | Dynamic stack allocas (VLAs) are not tracked |
| LLVM 22 constraint | `DILocalVariable` without a `type:` field crashes `DwarfCompileUnit::createAndAddScopeChildren` in LLVM 22. All tracked vars must have a basic type — hence the `ADDRESS` fallback for pointer kinds |

**Phase 3 (complete, 2026-05-19):** Per-statement `DILocation`. What works:

- `MSIR.SetCurrentSrcLine(line)` sets a module-global current source line;
  `addInsn` stamps every instruction with `i.srcLine := currentSrcLine`.
- `MSIRBuilder.GenLocation()` calls `Scanner.Here` and calls
  `MSIR.SetCurrentSrcLine` — called per-statement in `Stmt.CompileMSIR`.
- `BuildDebugInfo` fourth pass scans all instruction srcLines and allocates
  unique `(spIdx, line)` pairs as pre-numbered `DILocation` metadata nodes.
- `EmitInsn` selects the per-instruction DILocation (via `InsnDbgLocIdx`) on
  every call/invoke instruction; falls back to the proc's `line:0` DILocation
  when the instruction has no srcLine.
- `EmitDebugMetadata` emits all per-line `!DILocation(line: N, scope: !SP)`
  nodes at the module tail.
- Result: `lldb` source-level single-stepping works — `step` and `next`
  advance to distinct source lines instead of staying at the function entry.
  `thread backtrace` shows file:line for each frame.
- 149/149 LLVM link tests still pass; zero sweep regressions.

**Known Phase 3 gaps** (tracked for Phase 4):
- `DILocation` line numbers come from `Scanner.offset` at statement granularity.
  Sub-statement expressions within a single statement all share that line.
  This is the same resolution as the C backend.
- `MSIR.SetCurrentSrcLine` is not saved/restored across nested-proc context
  switches. In practice each statement calls `GenLocation()` which overwrites
  the global, so no stale lines escape.

**Phase 4 (complete, 2026-05-19):** Composite DWARF types. What works:

- `MSIR.Field` now carries `offset: INTEGER` (bit offset from M3 record layout);
  `MSIRType.TranslateRecord` fills `msirFields[i].offset := finfo.offset`.
- `TotalBitsOf(t)` recursively computes struct/array total bit size from field
  layout for `DW_AT_byte_size` on composite type nodes.
- `GetDbgTypeRef` dispatches to `GetOrBuildStructType` / `GetOrBuildFixedArrayType`
  / `GetOrBuildEnumType` / `GetOrBuildOpenArrayDvType` for composite MSIR types;
  falls back to ADDRESS for others.
- `GetOrBuildStructType`: emits `!DICompositeType(tag: DW_TAG_structure_type,
  name: "...", size: N, elements: !tuple)` with one `!DIDerivedType(tag:
  DW_TAG_member, name: "f", baseType: !T, size: N, offset: B)` child per field.
  Pre-reserves metadata indices before recursing into field types to handle nested
  structs without a visited-set.
- `GetOrBuildFixedArrayType`: emits `!DICompositeType(tag: DW_TAG_array_type,
  size: N, baseType: !elt, elements: !tuple)` with one `!DISubrange(count: N,
  lowerBound: lo)` child; lower bound taken from `MSIR.ArrayLo(t)`.
- `GetOrBuildEnumType`: emits `!DICompositeType(tag: DW_TAG_enumeration_type)`
  with one `!DIEnumerator(name: "...", value: N)` child per enum member.
  Enum label names recovered via `EnumExpr` walk at `BuildDebugInfo` time.
- `GetOrBuildOpenArrayDvType(rank)`: emits `!DICompositeType(tag: DW_TAG_structure_type,
  name: "__dope_N")` with fields `{data: ADDRESS, count: INTEGER}` (rank 1) or
  `{data, count0, count1, ...}` (rank N).  READONLY/VAR open-array formals are
  ptr-typed params (no alloca); `@llvm.dbg.declare` on the param value lets LLDB
  show the dope vector.  Param declares are injected inside the `entry:` block
  (inlined block-0 handling in `EmitProc`).
- `UniqueLocalName` in `MSIRBuilder` appends `.<N>` suffix when two locals share
  a name (e.g. two FOR-loop counters both named `i`); `StripVarName` in
  `MSIRToLLVM` strips the suffix for DWARF so both appear as `"i"`.
- Variables of `Struct`, `FixedArray`, `Enum`, and `OpenArray` MSIR type now
  appear in `frame variable` with correct composite-type descriptors.
- 181/181 LLVM link tests pass; zero sweep regressions.

**Known Phase 4 gaps** (tracked for future phases):

None — all composite-type DWARF is now emitted correctly.

OBJECT types (complete, 2026-05-19): Variables of OBJECT type now get
`DW_TAG_pointer_type` → `DW_TAG_structure_type` with a flat field list:
`__vtable` at bit 0, all inherited fields (in super-chain order), then own
fields; vtable and TEXT fields use ADDRESS. Type names are resolved via
`ObjectType.UserName` (the `user_name` recorded by `NoteRefName`) so `Dog`
and `Animal` appear as real names rather than hex UIDs. `GetOrBuildObjectStructType`
(kind=4) and `GetOrBuildObjectPtrType` (kind=5) handle the two-level metadata.

**Phase 5 — Optimized builds:**

**Phase 5 — Optimized builds:**
When `opt -O1+` is enabled (gated on MSIR becoming the default backend),
mem2reg removes allocas and `llvm.dbg.declare` calls become invalid.
Must switch to `llvm.dbg.value` annotation at every SSA update point.
This is an opt-in concern; Phase 1–4 target `-O0` equivalents only.

**LLDB fallback behaviour:**
LLDB recognises `DW_LANG_Modula3`, finds no native TypeSystem for it, and
falls back to its C parser for expression evaluation. This means `expr x`
and `p x` invoke Clang and will reject M3 syntax.  Developers should use
`frame variable` (or `v`) which parses DWARF memory offsets directly and
bypasses Clang entirely.  A Python data-formatter script auto-loaded at
LLDB startup can pretty-print M3-specific `DW_TAG_structure_type` nodes
(e.g. open-array dope vectors, REF fields) — deferred post-MVP tooling.

### 4. Dynamic procMap

`MaxProcMap = 2048` in `MSIRBuilder.m3` is a fixed-size array. Replacing
it with a heap-allocated table removes a latent overflow risk for large
modules with many unique external callees. Mechanical change; no IR impact.

### 5. LLVM optimizer integration

**Unblocked** (§2 complete).  Gated in practice on self-hosting (§2a) being
stable — optimized builds surface DWARF issues (Phase 5, §3) and may expose
MSIR lowering bugs that `-O0` misses.

**Goal**: pipe the emitted `.ll` through `opt -On` before `llc`, giving LLVM's
full optimization pipeline (mem2reg, inlining, vectorization, …) to
MSIR-compiled code.

**Planned implementation** (`Builder.m3`):

1. Add a `RunOpt` step between the MSIR `.ll` output and `RunLlcBack`:
   ```
   RunM3Front → <Module>.ll → opt -O<n> → <Module>-opt.ll → llc → <Module>.o
   ```
   Use `LlvmIROptNameForUnit(u)` (already wired, returns `<Module>-opt.ll`) as
   the intermediate.  `DoRunOpt := (optLevel > 0)` guards the step.

2. Drive from CM3's existing optimization flags: `M3_OPTIMIZE` Quake variable
   (or `-O`/`-O2`/`-O3` command-line flags) sets `optLevel` in `State`;
   `CompileM3` passes `optLevel` to `RunOpt`.

3. `opt` binary: `/opt/homebrew/opt/llvm/bin/opt` (same LLVM install as `llc`).
   Builder already locates `llc` via `s.m3llvm_compile`; add parallel
   `s.m3llvm_opt` resolved from the same directory.

4. Default: `optLevel = 0` (no `opt` invocation, identical to current behaviour).
   Platform config opts in: `M3_OPTIMIZE = "2"` in `ARM64_DARWIN` enables `-O2`.

**DWARF constraint**: at `-O1+`, `mem2reg` removes allocas and
`@llvm.dbg.declare` becomes invalid.  Before enabling optimized builds, switch
debug-variable annotation to `@llvm.dbg.value` at every SSA update point (see
§3 Phase 5).  Without this, `llvm-as` will warn and LLDB variable display will
break.  Optimized builds without debug info (`-g0`) are unaffected.

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
- **Nested procedure calling convention**: Lambda-lifted nested procs have capture-pointer arguments prepended before explicit parameters: `proc(%__cap_0: ptr, %__cap_1: ptr, …, arg0, arg1, …)`. Read-only scalar captures pass by value; GcRef captures always pass by pointer (conservative GC stack-scan requirement). Call sites reconstruct the capture arg list via `MSIRBuilder.GetProcCaptures`. For large-result nested procs the hidden result `ptr` is placed at arg 0 before the capture args. `PROCEDURE`-typed values of nested procs use a stack-allocated fat-pointer closure (see D15 in MSIR-design.md).
- **Open-array wire format**: The dope vector is `{ ptr data, i64 nElts }` for rank-1, extended to `{ ptr data, i64 nElts, i64 dim0, … }` for rank-N. `data` points to the first element. VALUE open-array formals copy-in at the call site.
- **EH personality — `ex_stack` platforms only**: MSIR uses the Itanium C++ personality (`@__gxx_personality_v0`, `invoke`/`landingpad`/`resume`). Linking MSIR-compiled modules against an `ex_frame` runtime is incorrect.
- **READONLY and VALUE scalar formals are spilled to alloca**: `BindFormalMSIR` spills all non-aggregate-pointer formals to an alloca so that `ADR(formalParam)` is always valid — matching M3 semantics.
- **procMap size**: `MaxProcMap = 2048` in `MSIRBuilder.m3`. For extremely large modules (>2000 unique external callees + internal procs), this could overflow. The safe fix is a dynamic table; increasing the constant is a stopgap.

---

## Test Infrastructure

```sh
# Full end-to-end LLVM link test (181 checks)
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
