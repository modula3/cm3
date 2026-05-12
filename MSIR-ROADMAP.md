# MSIR Roadmap: Current Status

Last updated: 2026-05-12 (msir branch)

## What's Working (73/73 tests pass; 0 abandons across p0/p1/p2)

The end-to-end path is live: MSIR emission → LLVM IR lowering → native object → linked binary.
The full p0/p1/p2 compiler validation test suite compiles with zero `msir-abandon` events.

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
- [x] `var_map`/`gc_map`: module globals embedded as trailing fields of `@Mod_M3_info`; TipeMap byte sequence for GC scanning; LLVM aliases for symbol compatibility
- [x] Nested procedures: **lambda-lifted** — `Stmt.Capture` pre-scans the body; each captured up-level variable becomes an explicit `ptr` param (`%__cap_0`, …); outer proc's up-level vars are ordinary allocas; multi-level nesting supported
- [x] Read-only scalar captures pass by value (not ptr): `CaptureAnalysis.written=FALSE` + scalar MSIR type → direct value param; GcRef stays by ptr for conservative GC
- [x] WIDECHAR text literals: encoded as little-endian bytes (`Target.WideCharSize()` per char); `[wcharBytes*len + wcharBytes x i8]` struct; negative `cnt` distinguishes from ASCII
- [x] TEXT library calls (`Fmt.Bool`, `Text.Length`, etc.): external calls emit correctly; calling convention matches C backend
- [x] Procedure values: `ProcExpr.CompileMSIR` → `MSIR.ConstProcRef(proc)` (`ptr @procname`); `NamedExpr.CompileMSIR` handles `Value.Class.Procedure` by folding to `ProcExpr`; auto-registers extern variables on demand for `FROM X IMPORT y` names; `EqualExpr.CompileMSIR` handles procedure equality as `icmp eq ptr`
- [x] Float type conversions: `FLOAT()` builtin via `SIToFP` (int→float) or `FPExt`/`FPTrunc` (float→float); cast ops `ZExt`/`SExt`/`Trunc` for integer widening/narrowing
- [x] EVAL, ASSERT, LOOP statements: `CompileMSIR` implementations
- [x] `MSIRType.Translate` maps `Type.Class.Procedure` to `TPtr(TVoid())`; `BindFormalMSIR` treats proc formals as by-value scalars (guards `Kind(EltType) ≠ Void`)

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

---

## Remaining Work (prioritised)

### A. TEXT: remaining cases

- `Fmt.Real` (floating-point formatting) — not yet exercised in tests
- `Text.Sub` and other TEXT manipulation operations — likely work (same pattern as `Fmt.Bool` / `Text.Length`) but not yet tested

### B. VALUE open-array formals

Copy-in of open-array VALUE formals (building a local dope vector) is not yet implemented.
Relevant for procedures that take open arrays by value.

### C. SET type operations

SET literals, the `IN` operator, and set arithmetic (`+`, `-`, `*`, `/`) are not yet implemented.
These require either inline bit manipulation (for small sets) or runtime calls (for large sets).

### D. NEW(REF open-array): multi-dimensional

`GenOpenArrayMSIR` handles 1-D; multi-D untested.

### E. NEW(REF record with keyword args)

`GenRefMSIR` abandons when `NUMBER(ce.args^) > 1`; plain `NEW(REF Record)` works.

### F. Opaque types

`GenOpaqueMSIR` handles only REF revelation; OBJECT revelation is deferred.

### G. Debug symbols

No source locations in emitted LLVM IR. See debug symbol architecture note in
`CLAUDE.md` for the natural hook points (`Scanner.offset`, `CG.Gen_location`,
`AddLocalMSIR`, `BeginProc`).

### H. TRUNC/FLOOR/CEILING/ROUND builtins

These float-to-integer conversion builtins need `CompileMSIR` implementations using `FPToSI`
(with appropriate rounding mode for FLOOR/CEILING/ROUND).

---

## Test Infrastructure

```sh
# Full end-to-end LLVM link test (73 checks)
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
