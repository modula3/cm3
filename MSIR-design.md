# MSIR Design Spine

Running record of cross-cutting design decisions for the Modula-3 Semantic
IR. Companion to `MSIR.md` (which holds the strategic narrative for why we
are bypassing M3CG). This document captures *what we have decided* and
*what we have not yet decided*.

Format:

- **Axes** name the orthogonal concerns the IR must address coherently.
- **Decided** entries record commitments, with the reason and the practical
  consequence of holding to them.
- **Open** entries record questions deferred, with the latest we can defer
  them to.

Update on every walkthrough that forces a new commitment or surfaces a new
open question. If a decision turns out wrong, edit it in place and note
the rationale in the commit.

## Status

| Item                           | State                          |
| ------------------------------ | ------------------------------ |
| Type system kinds              | Pinned at low resolution       |
| Procedure / CFG / SSA shape    | Pinned                         |
| GC contract                    | Pinned (MVP)                   |
| EH contract                    | Pinned: structured envelopes (D8, D20, D25). **Not yet implemented**: the front end emits `invoke`/`landingpad` directly |
| Module / init contract         | Pinned                         |
| Intrinsic surface              | Pinned for OBJECT/EH/opens/sets/RTTI; growing |
| Procedure attributes           | Pinned                         |
| Non-local control              | Pinned                         |
| Opacity / visibility           | Pinned (D21); not yet wired    |
| Verifier                       | Sketched (see A9 / D20)        |
| `m3-sys/msir` v0 package       | Built; ships; 181/181 LLVM link test checks; 286/288 p0/p1/p2 tests clean in MSIRObj mode (0 genuine abandons, 2 TIMEOUTs); smoke test 124/124 exit 0 |
| `m3-sys/m3middle` MSIRObj build | **Clean** — 50/50 object files produced, zero crashes, zero llc errors |
| **CI on all three targets (2026-11-20)** | **All green; supersedes the two rows below.** `.github/workflows/msir.yml` runs `ci-ladder.sh` from a clean d5.12.0 bootstrap on each `ex_stack` target. Run 44617989732 (`b13b4b26f9`): **ARM64_DARWIN, AMD64_DARWIN and AMD64_LINUX** each pass m3tests 293/293, the smoke test, and the three-stage IR fixpoint (1414/1414 files). The day before (run 44512471173) AMD64_LINUX differed between stages 1 and 2 and AMD64_DARWIN failed the smoke test; see *CI results on the x86_64 targets* below. |
| **Clean re-validation (2026-11-19)** | **Superseded by the row above.** Fresh `ci-ladder.sh` runs on ARM64_DARWIN: m3tests under the stage-2 driver **290/292** before the master merge (p165 regression, stale e020 expected output); full three-stage IR fixpoint holds (1416/1416 `.mb`/`.ib` files). See *Merge from master (2026-11)*. |
| **Latest validation (2026-07-24)** | *Not reproducible from a clean build (see above).* **m3tests 292/292, 0 errors**, both with the C-backend driver and with the MSIR-built stage-2 driver, under live incremental + generational GC. Three-stage bootstrap reaches a fixpoint (see *Validation Ladder* below). Supersedes the 2026-07-14 "26 errors" figures in the rows below. Last run of the `msir` branch HEAD (`b28fd265c3`). |
| **Platform coverage**           | **CI on all three `ex_stack` targets, all green (2026-11-20).** The x86_64 code-generation bugs found on the way are fixed (see *CI results on the x86_64 targets*). |
| **Front end decoupled from M3CG** | **Not started; plan agreed (D25).** MSIR emission runs inside the M3CG pass's driver and relies on its link-information side effects (see O17). Needed before proposing for master. |
| **Merge from master (2026-11)** | **Merged and pushed** (`593cb56d0c`, then `15ab336899` for #1251, #1253, #1254). See *Merge from master (2026-11)* below. |
| **Optimised m3tests**           | **Done (2026-11-23).** MSIRObj and MSIRAsm are in `m3tests/src/m3makefile`'s `-O3` clause, as master's llvm backends are (`a03e61e15b`), so the LLVM ladder's m3tests run optimised. The first run failed only p280, which found B32; with B32 fixed all 304 pass. |
| **EXTENDED on AMD64_LINUX**     | **Failing (2026-11-19).** fp128 comparisons, negation and constants fixed (`544b07b7c9`), but the MSIR-built stage-2 compiler still differs from stage 1 in three files and p113/p118/p123/p131 fail: probably fp128 arithmetic in the compiler's own constant folding. Original note: **Unvalidated.** MSIR lowers EXTENDED to `fp128` there. Peter needed parameter-alignment (`99e4d7c0ca`) and pickle (`cf2fa27f90`) fixes for the 128-bit change, so the first AMD64_LINUX ladder run should include an EXTENDED test. Revisit when #1249 (upgrade across extended128) and #1251 (`_Float128` with older g++, cross-host size) merge. |
| **Full compiler self-hosting**  | **Achieved 2026-07-14**: m3middle+m3linker+m3front+m3quake+m3objfile+m3back+cm3 all built from 100% LLVM IR objects (MSIRObj mode). Resulting `cm3` binary runs, compiles M3 programs, and passes the same 292-test suite as the C-backend cm3 (26 errors in both — all pre-existing C-backend failures). Zero new regressions. |
| **Full stack self-hosting**     | **Achieved 2026-07-14**: m3core (311 objects) + libm3 (299 objects) also build clean in MSIRObj mode. Full m3tests with MSIRObj runtime+stdlib: 292 tests, 26 errors — identical to C-backend baseline. The entire CM3 system (runtime, stdlib, compiler) runs from LLVM IR. |
| **Bootstrap fixpoint stability** | **Confirmed 2026-07-14**: Two consecutive three-stage fixpoint tests pass. Stage-1 (C-backend cm3) and stage-2 (MSIRObj cm3) generate bit-for-bit identical .ll files for all 383 compiler-package modules (m3middle×26, m3linker×12, m3front×299, m3quake×18, m3objfile×4, cm3×15). Stage-3 repeats the result. The MSIR code generator is fully deterministic and self-consistent. |
| **`do-cm3-base` clean**         | **2026-07-14**: All 24 base packages (m3core, libm3, m3middle, m3quake, tcp, …) build and ship in MSIRObj mode with zero failures. |
| **`do-cm3-std` clean**          | **2026-07-14**: 195 of 196 attempted packages build and ship in MSIRObj mode. One failure (`caltech-other/voronoi`) is a pre-existing C89 `void*` cast rejected by clang — reproduces identically with the C backend; zero MSIR codegen failures. 3 packages omitted on Darwin (tapi, serial, tcl). |

Walkthroughs done (updated 2026-07-14): OBJECT + METHOD, TRY/EXCEPT/FINALLY, open arrays,
module init, nested procedures, VAR/READONLY, SUBARRAY,
NARROW/TYPECASE/ISTYPE, sets + subrange, packed/compact fields,
open-array equality, struct-by-value return,
BITS-N-FOR-T bitfield read/write (ByteArrayFallback + shift/mask helpers),
sub-byte packed-element array subscript (ExtractBitFieldDyn/InsertBitFieldDyn),
packed integer subrange return widening (ZExt/SExt/Trunc in ReturnStmt.CompileMSIR),
compact subrange coercion in global/local init stores and ICmp/FOR loop contexts,
LONGINT elimination (msir/src + m3front/src/msir — all INTEGER now),
IByte-width type coercion in INC/DEC/MIN/MAX builtins,
Enum/GcSlot zero constants and `..'` array constructor guard (ConstZero),
global variable deduplication (GlobalMapAddStruct dedup + EmitGlobal alias dedup),
non-constant ConstArray element guard (Abandon before emitting bad global init),
READONLY TEXT dispatch (CompileLValueMSIR alloca spill + Formal.EmitArgMSIR in virtual dispatch),
Word built-in ops (Plus/Minus/Times/Divide/Mod/Rotate/Extract/Insert),
import-chain variable access (GlobalMapAddImport + ImportChainAddr),
MSIRObj end-to-end smoke test (124/124 checks, exit 0 against real CM3 runtime).


### CI results on the x86_64 targets (2026-11-19)

First runs of the ladder on AMD64_DARWIN and AMD64_LINUX.  Fixes found and
pushed on the way:

| Problem | Target | Fix |
|---|---|---|
| Homebrew builds LLVM from source on Intel macOS; job hit the 4 h limit | AMD64_DARWIN | Take `llvm-tools` from conda-forge (`4c48ed7c66`) |
| Stack-walker build linked with `gcc`: undefined `__cxa_throw` etc. | AMD64_LINUX | `SYSTEM_CXXXC = g++` when `M3_USE_STACK_WALKER` (`a0092c5aa9`) |
| `=`, `#`, unary minus on EXTENDED emitted integer ops; fp128 constants printed as doubles | AMD64_LINUX | `MSIR.IsFloat`; `0xL` fp128 literals (`544b07b7c9`) |
| `llvm.roundeven` becomes a `roundevenf` libcall macOS lacks | AMD64_DARWIN | Lower ROUND with `llvm.rint` (`544b07b7c9`) |
| `constdope_N` globals duplicated across units at link | AMD64_LINUX | Internal linkage (`GlobalSetPrivate`, `5b8b606ed4`) |
| `_M3Exc` typeinfo emitted in the arm64 form (hidden, name pointer high bit) on all Darwin; x86_64 libc++abi matches by address, so a RAISE caught in a program linked to `libm3core.dylib` never matched (smoke) | AMD64_DARWIN | Emit clang's x86_64 form there: exported `linkonce_odr`, plain name pointer |
| Multi-word sets (`i128`, `i192`) loaded/stored with LLVM's 16-byte ABI alignment though M3 aligns them to 8; real x86_64 faults on `movaps` (ARM64 and Rosetta don't) | both x86_64 | `align 8` on IWide loads and stores |
| fp128 constants written high half first; LLVM's `0xL` form is low half first. `LAST(LONGREAL)` became -inf in the MSIR-built compiler (the stage-1/stage-2 diff) | AMD64_LINUX | Emit low 64 bits then high 64 bits |

Open failures as of run 44512471173, all since fixed (run 44617989732 is green on all three targets):

| Failure | Targets | Notes |
|---|---|---|
| Set tests p053, p156, p157, p159, p160, p162 (SEGV) and p280 | both x86_64 | Fixed: `align 8` on IWide loads and stores (`03cd4e515a`). |
| RAISE in a module body not caught (smoke) | AMD64_DARWIN | Fixed: x86_64 typeinfo form (`03cd4e515a`). |
| p223 (.M3SHIP Library) build failure | AMD64_DARWIN | Missing expected output (Darwin ships `.dylib`, the default expects `.so`); added on msir (`b13b4b26f9`) and master (`30e5bbacf9`) for both Darwin targets. |
| Stage 1 vs stage 2 IR differs (RTCollector, ThreadPThread, AutoFlushWr); p113, p118, p123, p131 | AMD64_LINUX | Fixed: fp128 constant half order (`03cd4e515a`). |
| p165 | all | Fixed: `MSIREmit.RegisterImport` skipped RTHooks' binder, so a program's import chain never reached RTHooks and its typecells went unregistered (`7067f7d3be`). |

ROUND semantics note: MSIR rounds ties to even (FloatMode.RoundDefault),
as the language definition requires; the C backend calls C `round()`,
which rounds ties away from zero, so `ROUND(2.5)` is 2 under MSIR and 3
under the C backend.

## Terminology: what "structured" means here

MSIR is structured *in its ops*, not in its control flow. The CFG is a
graph of basic blocks with block parameters; ordinary control flow
(`IF`/`WHILE`/`FOR`) lowers to `br`/`cond_br` terminators, just as in
Swift SIL or Rust MIR. What carries Modula-3 language meaning are the
*composite ops* — `typecase` with parameterized clause blocks,
`try { … } except { … }` envelopes, `dispatch`, `narrow`, `subarray`,
`range_check`, `gc.load`, and so on. Each is checked structurally by
the verifier (A9).

This split — graph CFG, structured ops — is what SIL and MIR actually
are. It is *not* a tree IR (STG, Wasm text, MLIR's structured
dialects). All Modula-3 safety properties we care about (typecase
exhaustiveness, narrowing safety, GC trace completeness, opacity,
cross-envelope control rules) are op-level or type-level invariants,
not control-flow-shape invariants. Making the surrounding flow
tree-shaped does not tighten any of these checks; it costs every
analysis pass an impedance mismatch with the optimization
infrastructure that follows. See D20.

---

## Axes

### A1. Type system

Three families, each with deterministic LLVM lowering:

1. **Scalar** — `i1`, `i{8,16,32,64}`, `word{8,16,32,64}`, `f{32,64,128}`,
   `char`, `enum<UID, n>`. Bare LLVM scalars.
2. **Aggregate** — `record { fields }`, `array[N] of T`, `set<lo..hi>`,
   `subrange<lo..hi> of T`, `openarray<rank> of T`. LLVM struct / array /
   chosen scalar / dope-shape (TBD per A6).
3. **Reference** — `gc_ref T`, `untraced ptr T`, `proc(sig)`, `text`,
   plus the *slot* form `gc_slot T` for in-memory traced storage.

Commitments:

- Subrange and enum are *distinct* from their representation type; carry
  range/cardinality for check-elimination and TYPECASE folding.
- Open arrays are *shaped*: rank is part of the type.
- `EXTENDED` is parameterized: `f64` everywhere except `AMD64_LINUX`,
  where it is `f128`. Same rule the runtime already applies.

### A2. Procedure / CFG / SSA shape

- Procedures are typed-SSA over basic blocks with **block parameters**,
  not phi nodes. (Trivially lowered to phis.)
- Ordinary control flow is a **flat CFG**: no nested IF/WHILE/FOR
  constructs in the IR.
- EH is a **structured envelope** (`try`, `try_finally`) wrapping CFG
  fragments. This is the *only* nesting MSIR carries.
- Plain `ret` and `br` may not cross an envelope boundary. Cross-envelope
  exits use `ret_through_envelope` and `unwind_to label, args`.

### A3. GC contract

- Traced refs live in **`addrspace(1)`** at the LLVM level.
- **`gc_ref T`** = register-resident traced ref (SSA value).
- **`gc_slot T`** = memory-resident traced location (heap field, global,
  or address-taken local of traced type).
- **`gc.load`** reads a `gc_slot`, returning a `gc_ref`. Lowers to a
  software barrier check (test forwarding, cold-call to forward on miss),
  consistent with the collector's existing read barrier and the to-space
  stack invariant established at GC start.
- **`gc.store`** writes a `gc_ref` into a `gc_slot`. Lowers to a write
  barrier (call or inlined card-mark) plus the slot store.
- **Plain `load`/`store` on a `gc_ref` value** (e.g. reading a non-traced
  field of a heap object) is unguarded once the `gc_ref` is in SSA.
- Stack roots remain **conservative** under the existing collector model.
  Statepoint-based stack precision is a later refinement.
- **`gcmap`s and type descriptors are reused unchanged** from m3front.
  MSIR emits them as constant globals via the same `init_*`-equivalent
  ops the C backend uses.

### A4. EH contract

- Two structured constructs: **`try { body } except { clauses }`** and
  **`try_finally { body } finally { cleanup }`**.
- **`raise E, value`** raises a named exception. Permitted only with an
  exception declared in the enclosing procedure's `raises {…}` set or
  covered by an enclosing `try`.
- Each `EXCEPTION` declaration emits one **identity global**. M3
  exception identity is the address of that global, not a C++ RTTI type.
- C++-level catch is always for **`_M3Exc`** (existing runtime type).
  M3-level dispatch on which exception matched is a `switch` on the
  loaded identity address.
- Handlers are **parameterized blocks**; value-binding clauses
  (`Bad(msg) =>`) become block parameters loaded from the activation.
- `try_finally` lowers with the FINALLY body **duplicated**: once on the
  normal/return path, once in a cleanup landingpad.
- Lowering is **target-conditional**: `Has_stack_walker` targets emit
  `invoke`/`landingpad`/`resume`; non-stack-walker targets fall back to
  setjmp scaffolding, with the same MSIR input.
- Personality function: **`__gxx_personality_v0`** (the standard Itanium
  C++ personality, used directly).  The originally-planned thin-wrapper
  `__m3_personality_v0` was not built; the C++ personality is sufficient
  because M3 exception identity is handled at the M3 dispatch level, not
  inside the personality function itself.

### A5. Module / init contract

- Compilation units have explicit `imports {…}` / `exports {…}` lists.
- Module-init order is delegated to **the existing RTLinker mechanism**.
  MSIR emits a per-module init procedure with the same name discipline
  M3CG already uses.
- **Globals containing traced refs** require both a write barrier on
  every store and registration with the runtime as a GC root. MSIR's
  `gc_slot T` global form encodes this; lowering emits the registration
  side-table and the barrier call.

### A6. Intrinsic surface

M3-distinctive ops that are *not* runtime calls in MSIR (so LLVM can fold
them):

- **Object/RTTI**: `dispatch obj, Type.method`, `narrow obj, Type`,
  `istype obj, Type`, `typecase` (lowered to chained `istype`+branch).
- **Bounds/checks**: `range_check value, lo, hi`, `nil_check ref`,
  `subscript_check index, bound`.
- **Open arrays**: `openarray.size oa, k`, `openarray.data oa`,
  `openarray.elem_addr oa, idx…`, `openarray.deref ref`,
  `openarray.new <T, rank>, sz…`.
- **Sets**: `set_union`, `set_intersect`, `set_difference`,
  `set_member`, `set_singleton`, `set_construct`.
- **Bitfields**: `BITS N FOR T` packed fields are lowered inline via
  shift/mask on `i8` loads/stores (no dedicated op); see ByteArrayFallback
  convention in the Implementation Notes section.
- **Conversions**: `convert from_type to_type` with a documented set of
  permissible (from, to) pairs.

Each has a documented LLVM lowering and a fallback runtime call when
inlining is not profitable.

### A7. Procedure attributes

- **`raises {E, …}`** — drives the `call`-vs-`invoke` decision at every
  call site.
- **`noreturn`** — for `RAISE`-only or HALT-only paths.
- **`cc cc_kind`** — calling convention (`m3`, `c`, `winapi`, …).
- **`gc "modula3"`** — present once we adopt an LLVM GC strategy; absent
  for MVP conservative-stack model.
- **`personality @__m3_personality_v0`** — present iff procedure
  contains a `try` envelope.

### A8. Non-local control

- **`ret_through_envelope value`** — return through enclosing FINALLYs.
- **`unwind_to label, args`** — non-local jump through enclosing
  envelopes (e.g. `EXIT` from `LOOP` inside a `TRY`).
- These are the only ops permitted to cross envelope boundaries.

### A9. Verifier

The verifier checks invariants the optimizer and lowering passes
assume. It is *the* place where MSIR's safety claims are mechanized;
without it, every downstream pass would have to defend itself.

- **Op-level structural invariants.** TYPECASE clauses must be
  exhaustive (or have ELSE); `try` envelopes must contain at least
  one handler or be `try_finally`; `narrow`'s target type must permit
  the source type after runtime check; `gc.load`'s operand must have
  type `gc_slot T`; `gc.store`'s operands must agree; `dispatch`'s
  selector must name a method present on the operand's static type
  or a supertype.
- **Type consistency.** Operand types match the op's signature;
  block-call argument types match the target's parameter list;
  procedure return types match `ret` operand types; address-space
  annotations propagate consistently through GEPs and bitcasts.
- **Cross-envelope control rules** (D8). Plain `ret`/`br` may not
  cross an envelope boundary; only `ret_through_envelope` and
  `unwind_to` may.
- **SSA dominance.** Every operand use must be dominated by its def
  (or be a constant, procedure parameter, or block parameter of the
  use's containing block).
- **GC contract** (D5, D6, D17). Traced types appear only in
  permitted positions; the gcmap and addrspace(1) annotations agree;
  `addrspacecast` from 1→0 happens only at documented ABI boundaries.
- **Procedure-attribute consistency** (D10, D16). Procedures with
  `try` envelopes carry the personality attribute; calls inside
  envelopes use `invoke` iff the callee may raise; reference
  parameters carry `nocapture`; etc.

The verifier may compute a *structured projection* of the CFG on
demand (raising to structured control flow in the LLVM / MLIR sense)
for checks that are easier to phrase tree-recursively — for example,
checking that nested TYPECASE clauses cover the type lattice. The IR
storage form is the CFG; the structured view is a verifier
projection, not a second storage form.

---

## Decided axioms

### D1. Object header is a single word holding a typecell pointer.

**Why:** Standard M3 layout; matches existing runtime; minimizes object
size; supports both dispatch and RTTI through one indirection.

**Consequence:** Dispatch lowers to plain `load` (typecell ptr) + `gep`
into method table + indirect `call`. NARROW/ISTYPE walk the super-chain
through this same field.

### D2. Typecell pointer is not a traced ref.

**Why:** Typecells are immutable globals; they do not move; barrier on
header reads would be pure overhead.

**Consequence:** Header reads are plain LLVM `load` with
`!invariant.load`, not `gc.load`. Method-table slot loads inherit this.

### D3. Subclass layout is prefix-extension.

**Why:** Single inheritance + standard M3 layout rule.

**Consequence:** `%Subclass = type { ...%Super, own_fields... }`. No
coercion code on subtype assignment; upcast is a no-op.

### D4. Method-table slot assignment is a lowering-pass concern.

**Why:** MSIR uses named selectors; slot indices depend on inheritance
traversal order. Keeping slot assignment out of MSIR allows MSIR-level
devirtualization to rewrite `dispatch` → direct `call` without committing
to numbers.

**Consequence:** Two lowering passes (debug vs release, or different
targets) may assign different slot numbers if the rule is deterministic;
runtime sees only computed selectors.

### D5. `gc_ref T` (register) is distinct from `gc_slot T` (memory).

**Why:** With software read barrier, the barrier check belongs at slot
reads, not at every use. Mixing the two would force unnecessary checks
or unsound elision.

**Consequence:** `gc.load` is the only barrier-emitting load op. Once a
traced ref is in SSA, uses are plain LLVM operations.

### D6. m3front-emitted gcmaps and typemaps reused unchanged.

**Why:** They are computed from semantic information m3front owns; format
is runtime-defined and orthogonal to backend choice.

**Consequence:** MSIR object layout must match m3front's expectations
(one-word header, prefix-extension fields, declaration-order packing).
Diverging from this layout requires regenerating maps.

### D7. M3 exception identity = global address; one C++ catch type.

**Why:** Matches existing `_M3Exc` runtime; scales to thousands of
declared exceptions; M3-level dispatch is a `switch`, which LLVM
optimizes well.

**Consequence:** Every M3 landingpad has the same C++ shape. Per-handler
work is M3-level SSA after the catch. `_Unwind_Resume` propagates when
no clause matches.

### D8. Plain `ret` / `br` may not cross EH envelope boundaries.

**Why:** Cross-envelope exits must run intervening FINALLYs. Forcing
explicit ops makes the lowering pass mechanical instead of clever.

**Consequence:** MSIR has `ret_through_envelope` and `unwind_to`; the
verifier rejects any `ret`/`br` that would cross an envelope.

### D9. FINALLY is duplicated, not shared.

**Why:** Each path (normal completion, return, propagation) has different
post-conditions. Sharing would require a join with a "what to do next"
parameter. Duplication is SSA-cleaner.

**Consequence:** Large FINALLY bodies inflate code size briefly;
outliner / SimplifyCFG recovers most of it.

### D10. `raises {…}` is a first-class procedure attribute.

**Why:** Drives the `call`-vs-`invoke` decision and the `personality`
attribute. m3front already computes it.

**Consequence:** Every cross-procedure call inside a `try` envelope can
be classified locally. No global EH analysis.

**Amended (2026-11-22):** what is implemented is the RAISES *check*.
`ProcSetRaises` takes the allowed exceptions' descriptors (`ExcDesc`), and
is called only when the front end decides the clause must be enforced, on
the same conditions as CG's `Procedure.StartRaises` (`doRaisesChk`, the
body may raise something else, not RAISES ANY); `ProcChecksRaises` says
whether it was.  Using the set for the call-versus-invoke choice is not
done: every call in a protected block is still treated as possibly
raising (see D26).  Tony to review the representation.

### D11. Stack-precise GC is not on the MVP critical path.

**Why:** Existing collector is conservative-on-stack by design; the
software barrier + to-space stack invariant make conservative scanning
correct. Statepoint precision is a refinement.

**Consequence:** MVP emits no `gc.statepoint`. `addrspace(1)` is present
on traced refs purely to forbid LLVM from confusing them with untraced
pointers. Statepoint adoption is a later pass that retains MSIR shape.

### D12. `addrspace(1)` for traced refs from day one.

**Why:** Costs nothing on MVP target; preserves every future GC option;
gives LLVM alias separation for free.

**Consequence:** Every MSIR-emitted LLVM type for a traced ref carries
`addrspace(1)`. Pointer arithmetic on traced refs is permitted only
through MSIR's GC ops or through the type system's prefix-extension
rule.

### D13. Open arrays are structural fat-pointer types.

**Why:** `openarray<rank> T = { data, sz0, ..., sz(rank-1) }` carries
shape in the type. `REF ARRAY OF T` is a heap object with a
typecell-header + size-prefix + inline data layout (single allocation).
Address arithmetic (`openarray.elem_addr`) and bounds checks
(`subscript_check`) are separate ops so range analysis can eliminate
checks without rewriting the address computation.

**Consequence:** Multi-dim is row-major with explicit per-dim sizes in
the dope. `openarray.deref` of a `REF ARRAY OF T` carries the heap's
`addrspace(1)` through to the data pointer. Size loads of `REF ARRAY
OF T` carry `!invariant.load`.

### D20. Structure lives in ops, not in control flow.

**Why:** The Modula-3 safety properties we care about (TYPECASE
exhaustiveness, narrowing safety, EH cross-region control, GC trace
completeness, opacity) are all op-level or type-level, not
control-flow-shape-level. Swift SIL and Rust MIR are both CFGs with
composite ops; they validate this design at the language-feature
level we care about (typed-SSA dispatch, structured pattern-match
dispatch, typed exception flow). Going full-tree IR would buy nothing
for these properties and would force every analysis pass into
structural recursion, foreign to the optimization infrastructure that
consumes the IR.

**Consequence:** Ordinary control flow (`IF`/`WHILE`/`FOR`) lowers in
m3front-to-MSIR translation to flat CFG with `br`/`cond_br` and block
parameters — same as SIL/MIR. M3-distinctive constructs (TYPECASE,
TRY/EXCEPT/FINALLY, SUBARRAY, dispatch, narrow, set ops, range /
subscript / nil checks, `gc.load`/`gc.store`) are *composite ops*
carrying their semantic content into the IR. The verifier (A9)
checks structured invariants on these composite ops; it may compute a
structured projection of the CFG on demand for checks that benefit
from tree-recursive phrasing, but the IR storage is CFG-shaped.
Source positions are carried diligently on every op so debugger and
IDE tooling can reconstruct source structure when needed.

### D19. Sets and subranges are typed in MSIR; range checks are explicit.

**Why:** Sets are *typed bit patterns* with a domain; subranges are
*typed integer-like values* with bounds. Encoding both in the type
system buys (a) constant set folding at compile time, (b) range-check
elimination via LLVM's range analysis when subrange types pin the
domain, (c) explicit policy on which ops are intrinsics vs runtime
calls.

**Consequence:** `set<T, lo, hi>` lowers to an integer scalar when the
domain fits in a word, to `[N x i64]` aggregate otherwise. Set
intrinsics (`set_union`, `set_intersect`, `set_difference`,
`set_member`, `set_singleton`, `set_construct`, `set_card`) lower
inline for small sets, runtime call for large. `subrange<T, lo, hi>`
is an MSIR type kind distinct from its representation; m3front picks
the representation. `range_check` is a separate op from `convert`,
emitted by m3front when narrowing semantics demand it (mirroring
`subscript_check` from D13). `convert` is unchecked. ORD/VAL are
convert ops; VAL on enum/subrange may carry a preceding `range_check`
that LLVM's range analysis eliminates when bounds are provable.
Cardinality is an explicit loop today; LLVM's loop-idiom recognizer
maps it to `llvm.ctpop` opportunistically. A dedicated `set_card`
intrinsic remains an option if recognition proves unreliable.

### D18. NARROW / ISTYPE / TYPECASE are first-class MSIR intrinsics.

**Why:** They are the place where Modula-3 semantics meet LLVM
reasoning. Encoding them as runtime calls (the C-backend approach)
hides everything from the optimizer; encoding them as IR ops lets
MSIR-level peepholes fold them when the static type pins the answer
and lets a chosen algorithm (chain walk / Schubert / Cohen) replace
itself wholesale without IR-level churn.

**Consequence:** `istype obj, Type`, `narrow obj, Type`, and
`typecase` (with parameterized clause blocks and a mandatory ELSE) are
MSIR ops. Lowering uses a single internal helper for the chain walk,
which a later pass can swap for Schubert numbering or Cohen's algorithm
without touching MSIR. NARROW failure is a runtime error (not a
declared exception); does not enter `raises {…}`. TYPECASE clause
order is first-match-wins, preserved as authored. Value-binding
clauses (`Dog(d)`) are parameterized blocks — the binding is a retype,
not a memory operation. Typecell pointer loads and super-chain link
loads carry `!invariant.load` so LLVM can CSE across calls and loops.

### D17. SUBARRAY is a single MSIR op; ABI uniform in addrspace(0).

**Why:** SUBARRAY constructs a new fat pointer over existing storage —
no allocation. M3 restricts it to 1D, which keeps the op simple. Open
arrays cross procedure boundaries with their data pointer in
`addrspace(0)`, regardless of source, so the ABI is single-shape.

**Consequence:** `subarray oa, lo, count` lowers to bounds-check + GEP +
struct construction. Result inherits source data-pointer address space
within the procedure; an `addrspacecast` from 1 to 0 happens at every
cross-procedure call carrying a heap-derived open. Heap-derived
subarrays rely on conservative interior-pointer pinning (Bartlett),
matching the C backend's existing equilibrium. Statepoint adoption
(O11) eliminates the addrspacecast and replaces conservative pinning
with precise tracking; the MSIR shape doesn't change.

### D16. VAR / READONLY parameters lower to `ptr` with explicit modes.

**Why:** M3 reference parameters cannot escape, but they can alias each
other (`Swap(x, x)` is legal). LLVM has the right attribute vocabulary
to express this precisely.

**Consequence:** MSIR carries `var` / `readonly` as parameter-level
modes; the verifier enforces them. Lowering emits `nocapture`
universally on M3 reference parameters; `readonly` when MSIR says so;
no default `noalias` (per-call-site only, when m3front proves
non-aliasing). `VAR` of a traced-ref type is a stack-located slot
(addrspace 0) whose contents are addrspace 1; writes go through
`gc.store_through`. Capture-by-pointer (D15) and `VAR` are the same
lowering pattern from two source-language entry points.

### D15. Nested procedures are lambda-lifted at MSIR construction.

**Why:** M3 §4.13 forbids nested-procedure values from escaping their
enclosing scope, which makes lambda-lifting safe (no closure ever
needs to be a first-class value). LLVM has no static-link concept;
lambda-lifting maps cleanly to ordinary parameters.

**Why not LLVM's `nest` attribute + trampolines?**
LLVM's `nest` attribute exists specifically to enable the
`llvm.init.trampoline` / `llvm.adjust.trampoline` intrinsics, which
lower to a libgcc/compiler-rt helper (`__trampoline_setup`) that writes
a small machine-code stub into a stack buffer and then makes it
executable. This requires a writable+executable stack — a W⊕X
violation. On **ARM64 Darwin** (Apple Silicon) W⊕X is enforced in
hardware via PAC and MMU page permissions; there is no supported path
to an executable stack short of `MAP_JIT` + the
`com.apple.security.cs.allow-jit` entitlement (a JIT engine privilege,
not appropriate here). The same restriction applies on **AMD64 Darwin**.
Trampolines work on AMD64 Linux but not on any of the Darwin targets,
so they cannot be the uniform strategy. Lambda-lifting is portable
across all three primary targets and produces better IR for LLVM to
optimize.

**Consequence:** Each nested proc becomes a top-level MSIR procedure
with `internal` linkage and a lexical-mangled name (`Outer.Inner`).
m3front's existing capture analysis classifies each uplevel reference
read-only vs read-write; read-only captures pass by value, read-write
captures pass by pointer (forcing the source local to be address-taken
in the outer). Traced-ref captures fall out of the same rule:
read-only by value (`ptr addrspace(1)`), read-write by pointer to a
stack slot containing the traced ref (which the conservative scan
finds). After inlining, mem2reg reverses any address-take introduced
purely for capture support.

**Procedure values** (first-class `PROCEDURE`-typed value of a nested proc):
represented as a stack-allocated M3RT fat-pointer closure struct
`{CL_marker: i64 = -1, CL_proc: ptr = shim, CL_frame: ptr = env}`
at byte offsets 0 / IP (8) / IP+AP (16) on 64-bit targets.
`CL_frame` points to a capture-env array `[nCaps x ptr]` where each
slot holds the `alloca` address of a captured outer-proc variable.
`CL_proc` points to a *closure shim*: a generated `internal` function
with signature `(ptr %__env, explicit_params...) → result` that unpacks
the env array and tail-calls the lambda-lifted nested proc.

Call sites that receive a `PROCEDURE`-typed value (indirect calls) emit a
runtime CL_marker check: load the first `i64` of the callee value, compare
to −1.  If equal: extract shim and env, call `shim(env, args...)`.  If not:
call the raw function pointer directly.  This is always emitted for indirect
calls — distinguishing closure from plain proc pointer requires only the
marker check, which is free at direct-call sites that prove the callee is
a plain proc.

Stack lifetime is guaranteed by M3 §4.13: nested-proc values cannot escape
their enclosing scope, so the closure struct and env array can safely live
on the outer proc's stack frame.

**Implementation status (2026-05-19): D15 is fully implemented,**
**including first-class nested-proc procedure values.**

**Design rationale — hybrid lambda-lifting + closure shim (vs universal env-ptr):**

An alternative design would unify all nested-proc calls under a single `ptr %__env`
ABI (no capture params; the env pointer carries all captures for every call,
whether direct or indirect).  This eliminates the shim entirely — procedure values
can be represented as a plain `(proc_ptr, env_ptr)` pair without a runtime marker
check.

The hybrid (lambda-lifting for direct calls, shim only for procedure values) is
better for the common M3 case:

- **Direct calls are the norm.** Most nested procs in M3 are called directly in
  the same enclosing scope.  Lambda-lifting passes captures as ordinary scalar or
  pointer arguments: readonly scalars go in registers, LLVM can inline freely,
  and mem2reg promotes away any remaining indirection.  No alloca+store+load
  per call.
- **Env-ptr imposes baseline overhead on ALL calls.** With universal env-ptr,
  even a direct `inner(x)` must spill captures to a stack struct and pass a
  pointer.  LLVM's SROA may recover this — but only when the call site is
  inlined.  For non-inlined nested procs (library callbacks, recursion), the
  overhead is permanent.
- **Register allocation.** Lambda-lifting surfaces captures as named SSA
  parameters; LLVM assigns them to registers via the normal calling convention.
  Readonly scalar captures benefit especially (no memory traffic at all for
  direct calls).  An env-ptr treats all captures as memory loads inside the
  callee regardless of access pattern.
- **Inlining and constant propagation.** When the outer proc is inlined into
  a call site that holds a constant capture value, LLVM can propagate that
  constant through the explicit parameter.  With env-ptr the constant is
  hidden behind a pointer, blocking propagation until IPSCCP loads the env
  field — a harder analysis.
- **Shim cost amortized.** A shim is generated only when the proc value is
  actually taken (`ProcExpr.CompileMSIR` detects `IsNested`).  Procs that are
  always called directly never generate a shim; the hybrid has zero overhead
  for the pure-direct-call case.

**Future optimisation:** if capture analysis can prove a nested proc *never*
escapes (its address is never stored in a PROCEDURE-typed slot), shim generation
can be suppressed entirely.  The proc stays purely lambda-lifted at zero
procedure-value overhead.  This is O16 territory — deferred until post-MVP.

Lambda-lifting replaced the earlier static-link / frame-struct approach. The initial
implementation used `%__env: ptr` with byte-offset GEPs and a back-patched frame
alloca — that approach diverged from D15 and introduced three structural costs
(forced inline compilation, chained `%__env` for multi-level nesting, opaque GEPs
limiting LLVM optimization). All three are now gone.

**Current implementation** (`CaptureAnalysis` + `MSIRBuilder`):
- `Stmt.Capture(body, ca)` pre-scans the nested proc's AST, recording each
  up-level variable reference as `(Variable.T, written: BOOLEAN)`.
- `MSIRBuilder.BeginProc(..., captures := ca)` generates one explicit `ptr` parameter
  per captured variable (`%__cap_0`, `%__cap_1`, …) and binds them in the inner
  proc's varMap so `LookupVar`/`LookupVarAddr` work transparently.
- `MSIRBuilder.RegisterProc(p, proc, caps)` stores the capture list alongside the proc.
- `MSIRBuilder.EmitNestedCall` looks up captures for the callee, passes
  `LookupVarAddr(cap.var)` for each capture, then the explicit args.
- Up-level variables in the outer proc are ordinary `alloca` locals; no frame struct.
- Multi-level nesting works naturally: inner-inner procs receive outer capture ptrs
  directly, without chaining.

**Parameter explosion note**: a proc that captures many up-level variables acquires
many extra pointer parameters. LLVM's inliner and middle-end (mem2reg, SROA)
typically eliminate this overhead after inlining — pointers are promoted back to
registers and indirections disappear. Frame-struct grouping (O16) remains available
as a future performance tuning step for hot paths where inlining does not apply.

**Read-only scalar capture optimisation** (implemented): captures classified
`written=FALSE` by `CaptureAnalysis` and of scalar MSIR type (integer, float, or
untraced pointer) are passed by value instead of by pointer, giving LLVM's alias
analysis better information; GcRef captures always pass by pointer so the
conservative GC scanner keeps them on the stack.

**Static links on the C path (2026-11-22).**  Lambda-lifting happens once,
in m3front while the MSIR is built, not in the lowering to LLVM.  Both
lowerings therefore receive lifted procedures, and the MSIR route to C
(MSIR -> M3CG -> M3C) needs no static link either:

- `MSIRToM3CG` declares every procedure at level 0
  (`cg.declare_procedure (..., 0, ...)`) and calls a lifted procedure as
  an ordinary call with its captures.  It emits none of M3CG's static-link
  operations or frame structs; in the lowered compiler's generated C, the
  only occurrences of "static_link" are the names of m3front's own
  procedures (`CG.Pop_static_link` and the like).
- Static links remain on the direct CG route (m3front -> M3CG -> M3C, as
  on master), with their frame structs and the undefined static link that
  #1243 hit at `-O2`.  They go only when step 6 of D25 makes MSIR -> M3CG
  the default and deletes the per-node CG methods.
- Until then the two conventions coexist and do not interoperate: a
  lowered unit calls a closure as `shim(env, args...)`, a CG nested
  procedure takes its static link last.  Per Tony (2026-11-21) this
  lowered/CG boundary is not bridged, so a build must be all lowered or
  all CG (a mixed build crashed in `Utils.WriteFile` calling
  `M3Build.InstallFile.Emit`).
- Costs: a procedure gains one parameter per capture.  LLVM removes most
  of that after inlining; a C compiler may recover less, not yet measured.
  O16 (grouping captures into a frame struct) is the fallback for hot
  paths that are not inlined.  Indirect calls keep the `CL_marker` test,
  which reads a code word (O18).

### D14. Module descriptors are first-class MSIR entities.

**Why:** Init order, root registration, type registration, and
exception registration are all data, not code. Making them declarative
in MSIR keeps them auditable and lets RTLinker do the ordering it
already does.

**Consequence:** Each module emits exactly one constructor (registers
the descriptor with `RTLinker__AddUnit`); RTLinker invokes per-module
init procedures in dependency order. Init procs have no internal
idempotency guard. Descriptor schema is runtime-defined; MSIR matches
the bytes the existing `RTLinker.Module` record expects, so the
runtime needs no changes. Traced globals listed in the descriptor are
GC-root-registered during the init walk — no separate `@llvm.used`
machinery required. Cross-module name mangling matches the C backend
(`Module__Proc`, etc.) so MSIR-compiled and C-compiled translation
units link together during incremental adoption.

### D21. Opacity is per-site visibility, not per-type identity.

**Why:** In M3, *type identity is global; revelation is unit-local*. By
the time m3front compiles a unit, every revelation visible to that unit
has been resolved, so the full layout is already in hand at the
front-end. What "opaque from here" actually controls is **which ops are
legal at this site** — `field_addr`, `dispatch` on a privately-added
method, layout queries — not whether the type's structure exists in the
global type table. Encoding opacity in the type itself would force the
same Modula-3 type to appear as two distinct MSIR types depending on
which unit was compiling, breaking type-identity-based reasoning
(NARROW, ISTYPE, TYPECASE, GC descriptor identity) that already works
correctly on identity alone.

**Consequence:** One MSIR `Type` per global type identity, carrying
whatever structure the *defining* module knows. Each MSIR module
carries per-import **visibility records** of the form `{ Full,
PartialAsSubtypeOf(U), Hidden }`. The verifier consults visibility for
layout-dependent ops (`field_addr`, `dispatch` on hidden methods,
`sizeof`); identity-only ops (`narrow`, `istype`, `typecase`,
assignment, NIL, equality) are always legal regardless of visibility.
TYPECASE exhaustiveness automatically goes conservative when an opaque
type appears in the lattice — the mandatory-ELSE rule (D18) already
forces this with no special case. Descriptors are emitted by the
*defining* module; opaque-importing units reference the descriptor by
symbol and let the linker resolve it. No "runtime-supplied tracer"
hand-off, and no Swift-style `sealed`/`open` attribute matrix —
opacity in M3 is binary at any given site (revealed-here or not), and
the partial-revelation supertype is the only additional bit needed.

---

## Open questions

### O1. Object header alignment / packed-object support.

**Defer to:** Walkthrough that introduces a packed record or pragma
alignment override.

**Constraint:** Cannot break existing gcmap byte offsets.

### O2. NARROW/TYPECASE algorithm.

Choices: super-chain walk (MVP), Schubert numbering, Cohen's algorithm.

**Defer to:** Performance work after MVP correctness.

**Constraint:** Per D18, the algorithm lives in a single internal
helper (`__istype_walk`). Replacement must be wholesale of that helper
plus its sibling fast paths; must not require IR-level changes. Must
coexist with the existing runtime's RTType layout (RTType is the
source of truth).

### O3. Open-array dope vector ABI.

**Resolved by D13.** Multi-dim and pass-by-reference are pinned. SUBARRAY
remains open (see O13).

### O4. Nested procedures: lambda-lift vs static link.

**Resolved by D15.** Lambda-lifting is implemented: `Stmt.Capture`
pre-scans nested proc bodies; explicit capture params replace `%__env`; multi-level
nesting works naturally. See D15 for implementation details and the parameter
explosion note.

### O5. LOCK statement.

**Resolved.** Falls out of TRY/FINALLY lowering; `[x]` in the ROADMAP
checklist.  No dedicated walkthrough needed.

### O6. Module init order.

**Resolved by D14.** Module descriptor + per-module constructor +
RTLinker-driven topological init. No runtime changes.

### O7. LLVM version pinning.

**Defer to:** Tooling phase, after MVP.

**Constraint:** Current bindings target LLVM 13/18/22. The MSIR backend
should adopt the binding with the most stable opaque-pointer support
(LLVM 18+).

### O8. Bootstrap path: when does m3core get compiled by MSIR/LLVM?

**Defer to:** After MVP runs at least one whole package
(a small subset of `m3-libs/m3core`).

**Constraint:** Two-phase bootstrap protocol from `CLAUDE.md` applies.
Until then, MSIR ships as a non-default backend mode.

### O9. Unsafe constructs: LOOPHOLE, ADR, ADDRESS arithmetic.

**Partially resolved.** `ADR`, `LOOPHOLE` (rvalue and lvalue), and
`ADDRESS` arithmetic (`PtrToInt`/`IntToPtr`/`PtrAdd`) are implemented
and tested (`[x]` in the ROADMAP checklist).  The `addrspace(1)`
invariant is preserved: LOOPHOLE between traced-ref and non-traced types
is encoded as an `addrspacecast` at the LLVM level and documented as
unsafe.  Full UNSAFE module compilation (arbitrary pointer casts, `BITS
FOR` overlays) has not been walked through end-to-end.

**Remaining constraint:** A dedicated unsafe-module walkthrough to shake
out any remaining edge cases (self-referential packed types, LOOPHOLE
into aggregate types, ADDRESS-of formal params in unsafe context).

### O10. VAR / READONLY parameter lowering.

**Resolved by D16.** `ptr` with `nocapture` always, `readonly` when M3
says so, `noalias` only at proven call sites.

### O11. Statepoint adoption strategy.

**Defer to:** Post-MVP performance work.

**Constraint:** Must be a *transformation pass* over already-built MSIR;
must not require rewriting the IR's GC shape.

### O12. GC liveness of `gc_ref` derivatives across calls.

The specific hazard is now the `addrspacecast` from 1→0 at open-array
ABI boundaries (D17): once the data pointer is treated as untraced,
LLVM may drop the originating `gc_ref` from registers. Bartlett-style
conservative interior-pointer pinning catches this — the resulting
addrspace(0) pointer still lands on a heap page on the stack and the
collector pins. Same mechanism the C backend already relies on.

**Proper fix:** Statepoint adoption (O11) eliminates the addrspacecast
and tracks the heap pointer precisely.

**Constraint:** MVP must not regress relative to current C-backend
behavior. Stress-test under heavy GC; if interior-pointer pinning
proves unreliable, fall back to per-call-site keepalive intrinsics
until statepoints land.

### O16. Frame-struct grouping for high-capture nested procs.

When a nested procedure has many captures, bundling them into a single
struct passed by pointer reduces parameter count, improves alias
analysis (one `noalias` pointer instead of N), and reduces register
pressure.

**Defer to:** Performance work after MVP correctness.

**Constraint:** Must produce identical observable behavior to the
per-parameter form. Probably triggered by a heuristic on capture count.

### O14. Unhandled exception during module init.

If `@Module__init` raises an exception not caught inside its own body,
RTLinker presumably catches it and terminates the process with a
diagnostic. MSIR's lowering must match whatever the existing runtime
does — the program-visible behavior cannot change.

**Defer to:** Runtime audit of `RTLinker.m3`'s init loop.

**Constraint:** No new behavior; preserve existing message format and
exit code.

### O15. Generic instantiations.

Each instantiation `MyMap = Map(Key, Value)` produces a distinct
module-like compilation unit with its own descriptor. How is its name
synthesized; how does init order treat it relative to the generic
itself; how is its typecell deduplicated across translation units that
instantiate the same combination?

**Defer to:** Generics walkthrough.

**Constraint:** Must match existing CM3 generic-instantiation discipline.
Linker-level deduplication is probably the path of least resistance
(weak symbols / COMDAT).

### O13. SUBARRAY semantics.

**Resolved by D17.** Single MSIR op; uniform `addrspace(0)` ABI;
heap-derived subarrays lean on conservative interior-pointer pinning
until statepoints replace it.

### O17. Decoupling MSIR emission from the M3CG pass.

**Open; scope not yet estimated.**  Today MSIR is produced *during* the
M3CG pass, not instead of it:

- **Driver interleaving.** `Module.Compile` calls `CG.Begin_unit` then
  `MSIREmit.BeginUnit`.  `Procedure.GenBody` brackets each procedure with
  both `CG.Begin_procedure` and `MSIRBuilder.BeginProc`, and runs
  `Stmt.Compile` (CG) and then `Stmt.CompileMSIR` over the same body.
  Nested procedures differ: CG defers them, MSIR compiles them inline
  (`GenBodyMSIR`, which guards against being triggered twice).
- **Shared layout state.** Module globals use the same offsets as the
  CG path (`t.offset`), so importers agree on them (MSIR's own dense
  packing collided).  These offsets are assigned during type checking
  (`Module.TypeCheck` → `Module.SetGlobals` → `Variable.AllocGlobalVarSpace`),
  not by the CG pass, so they are already backend-independent.  What does
  hang off the CG pass is `DeclareGlobalMSIR`, called from inside
  `Variable.Declare`.  The module GC map is built after `CompileModule`
  "so the offsets are final"; with offsets fixed at type check this
  ordering is probably unnecessary (to verify).
- **Type and runtime metadata.** Typecells and initProcs have MSIR paths
  (`Type.GenCellsMSIR`, RefType/ObjectType) but are kept at parity with
  the CG output, not generated independently.
- **Breadth.** About 97 m3front files outside `src/msir/` contain MSIR
  code, with about 174 `*MSIR` procedures; 36 expression files and 20
  statement files have `CompileMSIR` / `LValueMSIR` methods.

Decoupling likely means: (1) a layout phase that is independent of any
backend, assigning global, frame and typecell layout before emission;
(2) a separate driver path that runs only MSIR in MSIRObj mode; and
(3) unifying nested-procedure ordering.  The per-node `CompileMSIR`
methods are already separate from `Compile`, so most of the work is in
the driver and the layout state, not the per-node code generation.

**Survey (2026-11-20), MSIRObj mode.**  The per-procedure CG walk is
already skipped (`p.cg_proc = NIL`, so `GenBody` runs only its MSIR
half), but the module-level CG walk still runs into `M3CG_DoNothing`.
What MSIR takes from it, by static reading of the `*MSIR` procedures
(not yet confirmed by a build with the walk switched off):

- **(A) The driver itself.**  MSIR bodies are emitted inside CG's
  `Procedure.GenBody`, reached through `ProcBody.EmitAll` from
  `CompileModule`/`CompileInterface`; nested procedures through
  `LangInit` → `GenBodyMSIR` from `Scope.InitValues`.  The fragile
  ordering hangs off this: the `msirSkip`/`ProcMapContains` guard
  (`Procedure.GenBody`), `Variable.m3`'s "CG's ProcBody.EmitAll fires
  before GenBodyMSIR", the `msirInitDone` patch.
- **(B) Link information, needed by every backend.**  The `.m3x` /
  `.M3EXPORTS` notes are made from CG-side procedures: `note_type`
  (`Type.Compile`, via `Type.CompileAll`), `note_version_stamp`
  (`Value.Declare`), `note_revelation` (`Revelation.DeclareRevelation`),
  `note_opaque` (OpaqueType's `Compiler`), `note_opaque_magic`
  (`ObjectType.NoteOffsets`), `note_interface_use`
  (`External.GenInitLinks`), `note_unit` / `note_webinfo`
  (`Module.Compile`).
- **(C) Pure CG work, discarded.**  `DeclareGlobalData` segments, type
  `compile` methods (typecell declarations and their `Module.Allocate`
  offsets), revelation tables, coverage tables, `ProcBody` proc info,
  `Variable.GenGlobalMap`, `GenLinkerInfo`.  MSIR has its own
  equivalents (`DeclareGlobalsMSIR`, `GenCellsMSIR`, `SetModuleGcMap`,
  module hooks, the byte-producing `TipeMap`/`TipeDesc` entry points).
- **(D) Not dependencies.**  Global offsets (type check, above) and field
  offsets (type check).  No `*MSIR` procedure reads an offset the CG walk
  allocates (strings, constants, exception descriptors, typecells).

**Scoping (2026-11-22).**  A second reading confirms (A) to (D) and adds
one hidden dependency inside (B):

- **`used` marks.**  An imported value gets a version stamp, and a CG
  import declaration, only if it is marked `used`: `Value.Declare` returns
  early for an imported value that is not.  The marks come from two
  places.  `Scope.LookUp` sets `used` for any lookup made while
  `Scanner.in_main`.  That holds at type check (`Module.TypeCheck` sets
  it); whether it still holds during code generation is to check, as it
  decides whether runtime hooks are marked when looked up then.  MSIR
  reaches the hooks the same way as CG (`RunTyme.LookUpProc`, via
  `MSIRBuilder.HookProc`), so either way the two walks agree on them.
  But the CG load
  paths for variables also set it (`Variable.Load`, `LoadLValue`,
  `SetLValue`, `LocalCGName`), and their MSIR counterparts do not.  If the
  CG walk ever marks a variable that no lookup did, an MSIR-only walk
  would drop that import's version stamp, and the linker's consistency
  check would no longer cover it.  Probably rare (a variable reached
  without a name lookup), but not yet measured.
- Consequence for B: `Value.Declare` does two jobs, the link note
  (`note_version_stamp`) and the CG import declaration.  Decoupling needs
  them split, with the link note driven by `used` marks that both walks
  set the same way.

**Proposed first step (an experiment, not a change):**
1. Instrument the CG variable load paths to log each imported variable
   whose `used` mark they set for the first time (and log `in_main` at
   code generation); build the base packages and count.  Zero means the MSIR walk loses nothing; otherwise the
   cases show what the MSIR load paths must mark.
2. Behind a debug switch, skip the module-level CG walk in MSIRObj mode,
   keeping the link notes of (B), and compare the `.m3x`/`.M3EXPORTS`
   files and the `.msir` output byte for byte with the current build over
   the base packages.  This confirms or corrects the survey's static
   reading of (D).
   *Result (2026-11-22):* not zero.  Over the 13 base packages (plain C
   build at msir `27687db226`) the CG load paths are the first to mark 357
   distinct imported variables (`LoadLValue` 320, `Load` 38), all with
   `in_main` set, so `Scope.LookUp` does mark during code generation too.
   Every one is a variable of an interface that the compiling module
   exports (`TargetMap.CG_Size` in TargetMap.m3, `RTHeapRep.align` in
   RTCollector.m3, `Date.UTC` in DatePosix.m3), never a true import: the
   same variables as D28.  Not from `External.InitGlobals`: interface
   initializers must be constant, so CG puts an interface variable's
   initial value in the interface unit's data and the exporter's init
   procedure emits nothing for it (checked: a `[5..10]` variable exported
   by two modules).  The likely source is ordinary uses in the exporter's
   code (`CG_Size[t] := ...` in TargetMap.m3): an unqualified name
   resolves through the import scope to the `External` wrapper, so the
   lookup marks the wrapper, and only the CG load marks the variable.  So
   an MSIR-only walk would drop the exporter's version stamps for its own
   interface variables, weakening the linker's check that exporter and
   importers agree on them.  Fix, as part of B: the MSIR load paths mark
   the variable used, as the CG ones do; it has no effect until version
   stamps come from a link-notes pass rather than from `Value.Declare`,
   which records them only when the CG declaration (`t.declare ()`)
   succeeds.
   *Step 2, revised (2026-11-22):* skipping the module-level CG walk would
   also break MSIR output (`Variable.Declare` calls `DeclareGlobalMSIR`;
   `ProcBody.EmitAll` and `EmitBody` drive MSIR bodies), so instead every
   link note was logged with the compile phase that produced it, over the
   13 base packages with MSIR emission on (C mode).  Results:
   - `InitGlobal` emitted code 0 times: exporters never initialize
     interface variables at run time.  The 359 CG-only used marks come
     from procedure bodies (304) and module bodies (55).
   - `interface_use` (`GenLinkInfo`, `Interface.export`), `revelation`
     (`Revelation.Declare` only), `opaque_magic` (`GenLinkerInfo`,
     `Revelation.Declare`) and `generic_use` come from a few fixed places
     that do no code generation: easy to move.
   - Version stamps and type notes are a by-product of declaring values
     and types, recorded once per unit by whichever walk declares first,
     spread over about 12 phases (`Scope.Enter` 22k stamps,
     `ProcBody.EmitAll` 20k, `GenImports` 6k, `EndUnitMSIR` 3.5k, ...).
     The MSIR phases produce some of them already (`DeclareGlobalsMSIR`,
     `EndUnitMSIR`), because MSIR code calls `Value.Declare` and
     `Type.Compile`: the coupling runs both ways.
   - Limitation: first-declaration-wins means the log shows what B must
     cover, not what an MSIR-only walk would lose.
   Suggested direction for B: keep the declaration walk (`Scope.Enter`,
   `Type.CompileAll`, `Revelation.Declare`, the link-info calls) running
   against `M3CG_DoNothing`, as MSIRObj already does, rather than split
   `Value.Declare`; decouple only the code walk (A), so C is about
   procedure and module bodies.
   **Decided (Tony, 2026-11-22):** do not split `Value.Declare`.  Its link
   note and CG declaration are integrated by an explicit, long-standing
   decision; B keeps the declaration walk against `M3CG_DoNothing`, and
   the CG pieces are excised later (with step 6 of D25).
3. Only then estimate A to C.  If both checks are clean, A and C are
   mainly driver work in `Module.Compile`/`Procedure.GenBody`, and B is
   the `Value.Declare` split plus moving the other `note_*` calls out of
   the CG-side procedures.  Not yet measured: how much compile time the
   module-level walk costs.  (Superseded for B by the decision above.)

**Link info today (2026-11-22).**  The `.m3x` of m3middle built in
MSIRObj mode (CG bodies already skipped) against the same tree in C mode:
same 49 units; no variable stamp is missing, exported or imported
(TargetMap's own interface variables included), so the CG-only used marks
of experiment 1 do not in fact lose stamps.  The only differences are
runtime-hook imports: 2 missing (`RTHooks.ReportFault` in M3CG,
`RTHooks.CheckIsType` in M3CG_Tee, hooks only the CG code calls there)
and 122 extra (MSIR registers `CheckLoadTracedRef`, `CheckStoreTraced`
and `ScanTypecase` in every unit).  Import stamps that follow the code
actually emitted are harmless to the linker's checks.  Repeated over all
13 base packages (same tree, same compiler): no variable stamp is missing
anywhere.  Every missing stamp is an import of something only the CG code
refers to: runtime hooks (mostly `RTHooks.ReportFault`, `CheckIsType`,
also `AllocateOpenArray`), `Long.*` in Convert and DragonInt,
`Integer.Compare` in M3C, `CallExpr.BuiltinAlignDefault` (a procedure
used as a field default) in m3front's builtins, the constant
`M3x86Rep.NoStore`, and two anonymous types.  (Some MSIRObj builds then
fail to link against the C-built libraries of that install, by design of
the ABI guard; the `.m3x` is written before the link.)

**A: the plan (proposed 2026-11-22).**  With B decided, A is smaller than
the survey suggested, because in MSIRObj mode the per-body CG walk is
already off: `CG.Declare_procedure` returns NIL from `M3CG_DoNothing`, so
`Procedure.GenBody` and `Module.EmitBody` run only their MSIR halves, and
the CG-only bodies in the `ProcBody` queue (`Exceptionz` raisers,
`TryFinStmt` handler procedures) are scheduled only from CG statement code
that no longer runs.  What A still has to do:

1. *The `ProcBody` queue stays the driver.*  It is filled by the
   declaration walk, which B keeps, and it already reaches the MSIR bodies
   (`gen_body`).  No new walk is needed; A is the removal of CG work
   around it.
2. *Module init body.*  `EmitBody` still runs `Scope.InitValues` (CG
   compiles of module-global initializers, into `DoNothing`) and
   `External.InitGlobals` (which emits nothing: 0 times over the base
   packages).  Neither is needed for MSIR: its half of `EmitBody` already
   runs its own `Variable.InitValuesMSIR` (`LangInitMSIR`,
   `UserInitMSIR`, nested procedures) on the same scopes, and
   `InitGlobals` has no counterpart because there is nothing to do:
   interface initializers are constant, so interface variables are
   initialized in the interface unit's data, by CG and MSIR alike
   (`InitGlobal` finds `initDone` set).  `InitGlobals`/`InitGlobal` may be
   dead on master too; to confirm beyond the base packages before removing
   them with step 6.  Guard both on `x.cg_proc # NIL`, as the body already is,
   after checking that `Scope.InitValues` makes no link note the MSIR walk
   lacks (the inventory shows 27 stamps and 16 type notes there).
3. *Module-level CG tables.*  `ProcBody.EmitAll`'s procedure-name and
   proc-info tables, `Variable.GenGlobalMap`, `Coverage.GenerateTables`,
   `DeclareGlobalData` and the CG part of `GenLinkerInfo` produce only
   `DoNothing` output in MSIRObj mode.  Skip them there, keeping
   `GenLinkerInfo`'s link notes (`opaque_magic`, version stamps), which
   B needs.
4. *Nested procedures.*  With `inline_nested_procs` (the default), MSIR
   compiles nested bodies inline from `LangInit` (`GenBodyMSIR`), and the
   `msirSkip`/`ProcMapContains` guard in `GenBody` keeps the queue from
   emitting them again.  In MSIRObj mode that guard becomes the rule
   rather than a patch; the non-default `-inline_nested_procs` off path
   can stay as is.
5. *Validation.*  Both ladders; the `.m3x` comparison over all base
   packages (no lost stamps); and the compile-time measurement that C
   needs, before and after (MSIRObj, the base packages).

Size: a handful of guarded call sites in `Module`, `ProcBody` and
`Procedure`, none in the per-node `CompileMSIR` code.  The delicate part
is 4, and 2 needs the link-note check first.  C (skipping the discarded
work) is mostly delivered by 2 and 3.

Plan: see D25.

**Progress (2026-11-23): (B) parity.**  The link information (`.m3x`) of an
MSIRObj build is compared with the C build's by content
(`m3xcanon.py`: names and version stamps resolved, per unit).  Parity is
exact, except that MSIR must import at least the types C does (Tony,
2026-11-23): imported-type (`t`) records are used only by the linker's
"missing imported type" check, and C's set is a by-product of where its
prepare phase calls `PrepLValue`.  libm3 went from 1,279 differing records to
parity.  On the way: hooks looked up only when used (MSIREmit), and the MSIR
walk calls `Type.Compile` where the C walk does (lvalues, literals, locals,
formals, NEW, TYPECASE, `T.m`'s holder and object type).  The comparison
also found real bugs: B21 (RETURN range check), B22/B23 (NIL checks), B24
(VALUE argument range check), plus needless checks and barriers (B25, B26).
Remaining for (B): the same comparison across all packages.

**B3 folds into (A) under D29.**  The plan was to move the note calls out of
the CG-side procedures into a backend-neutral pass.  D29 says otherwise: the
procedures that make the notes (`Type.Compile`, `Value.Declare`,
`Revelation.DeclareRevelation`, `OpaqueType`'s compiler,
`ObjectType.NoteOffsets`, `External.GenInitLinks`, `Module.Compile`) are
master's entry points, which stay; at D25 step 6 their CG content becomes
MSIR's.  So the notes stay where they are, and what (B) needs is that the
MSIR walk reaches each of those entry points itself, as it now does for
`Type.Compile`.  That is (A): once the MSIR walk drives the unit, the
module-level CG walk is no longer needed in MSIRObj mode, and the parity
check confirms nothing was lost.

**A under D29: one driver, two payloads (decided, Tony, 2026-11-23).**
D29 rules out a separate MSIR walk with its own shape, so (A) is not "an
MSIR-only driver" in the sense of new code alongside master's.  It is
master's driver (`Module.Compile` → `CompileModule`/`CompileInterface` →
`ProcBody.EmitAll` → `Procedure.GenBody`/`Module.EmitBody`), each step doing
its C work, its MSIR work, or both, the way D29a made `Marker` serve both
walks.  At D25 step 6 the C halves go and the driver is unchanged.

- **Where things stand.**  In MSIRObj/MSIRAsm the per-body C walk is already
  off (`M3CG_DoNothing`), so `GenBody` and `EmitBody` run only their MSIR
  halves.  Since 2026-11-23 the module's MSIR declarations run where the C
  scope walk is, after `Revelation.Declare`, and `Variable.Declare` no
  longer calls into MSIR; EXIT and RETURN route through `Marker` in both
  walks; the `.m3x` matches C's in every base package except where a
  difference is decided (m3core's backend-selected units and `WINAPI`
  fingerprints; B31's empty constructor).
- **Decided: no mode predicate.**  The driver's C-only work
  (`DeclareGlobalData`, `Scope.InitValues` in `EmitBody`, `ProcBody`'s
  tables, `Variable.GenGlobalMap`, `Coverage.GenerateTables`, the CG part of
  `GenLinkerInfo`) keeps running into `DoNothing` in MSIR modes until D25
  step 6.  Guards would scatter transitional mode tests through master's
  code, and that work's side effects on front-end state (used marks, type
  compilation, link notes) are part of why the `.m3x` matches.  A step is
  dropped at step 6, so it is not guarded meanwhile.
- **Steps** (each checked by byte-identical `.msir` for libm3, m3quake and
  m3front, the `.m3x` comparison, then both ladders):
  1. *Nested procedures, a rule of the MSIR half.*  MSIR always compiles a
     nested body inline, from the MSIR walk of its parent's scope
     (`LangInitMSIR` → `GenBodyMSIR`; D29).  So the MSIR half of `GenBody`
     never compiles a nested procedure.  **Done**: this replaces the
     `ProcMapContains` guard and `GenBody`'s second, nested-only path (its
     own capture analysis for a nested body), whichever way
     `inline_nested_procs` is set.  MSIRObj output of libm3 and m3front is
     byte-identical (MSIRObj already went only through `GenBodyMSIR`); in
     the dual mode nested bodies are now emitted when MSIRObj emits them.
     `msirInitDone` stays: it is the MSIR walk's counterpart of
     `initDone`, not an ordering patch.
  2. *D29a step 3* (TRY EXCEPT, TRY FINALLY and LOCK frames on `Marker` in
     the MSIR walk), so `Marker` describes the MSIR walk fully.  **Done**:
     `PushTry`, `PushTryElse` and `PushFinally` take the protected body's EH
     scope (`msirScope`); TryStmt pushes TRY or TRY ELSE, and
     `EmitFinallyMSIR` pushes FINALLY for TRY FINALLY and LOCK alike, as C
     does under ex_stack.  The MSIR emitters pass over these frames (the EH
     scopes run the FINALLY and LOCK code).  MSIRObj output unchanged.
  3. *Rehearse D25 step 6.*  Build the base packages in MSIRObj mode with
     the C-only steps skipped (a throwaway switch, not committed) and
     compare the `.msir` and `.m3x` with a normal build.  Each difference is
     a side effect of a C-only step (a used mark, a `Type.Compile`, a link
     note) that the MSIR half must make itself before step 6 can drop the
     C half.  (Timing those steps was dropped: step 6 removes them, so
     guarding them meanwhile would be wasted work.  A compile-time baseline,
     MSIRObj against C, is deferred.)
     *Result (2026-11-23):* the MSIR walk still depends on them.  Skipping
     `DeclareGlobalData`, the module body's C declaration, `EmitBody`'s
     `Scope.InitValues`/`External.InitGlobals`, `Coverage.GenerateTables`,
     `Variable.GenGlobalMap` and `GenLinkerInfo` over the 13 base packages:
     - `.m3x`: only m3linker unchanged.  Every package loses object-layout
       records (`O`/`o`, 313 in libm3: `ObjectType.NoteOffsets`, reached from
       `GenLinkerInfo`'s `Type.GenCells`), a few `C` and gains `w` records;
       m3front also loses 588 imported types and 837 imported version
       stamps.
     - `.msir`: 56 of 775 files change.  Object types the unit only sees
       through a revelation translate to `gc_ref void` instead of their
       struct (e.g. `AtomSetDef.T`): MSIR's type translation relies on the
       C walk having compiled the revealed types.
     So before step 6 drops the C halves, the MSIR half has to compile
     revealed object types and make the object-layout and import notes
     itself.
     *Attribution:* each step skipped alone (set and m3front): only
     `GenLinkerInfo` matters, and within it only `Type.GenCells`, a
     two-payload step already (`InitTypecell` and `InitTypecellMSIR` per
     cell).  Skipping only its C payload, `RefType`/`ObjectType.InitTypecell`
     (with `GenCellPtrs`), reproduces every `.m3x` difference and most of the
     `.msir` ones (7 files in set, 4 in m3front).  `ObjectType.InitTypecell`
     calls `NoteOffsets` (`note_opaque_magic`: the `O` records), and its
     `GenMethodList`/`GenInitProc`/`GenLinkProc` compile method defaults and
     field initializers (m3front's imported types and stamps); MSIR's named
     object types depend on that compilation too.  So step 6 is, here, the
     C content of `InitTypecell` becoming `InitTypecellMSIR`'s: the notes and
     the compilation stay in `InitTypecell`'s MSIR half.  The other C-only
     steps (`DeclareGlobalData`, `EmitBody`'s initialization,
     `Coverage.GenerateTables`, `Variable.GenGlobalMap`) change nothing.
     *Resolved (2026-11-25):* two of `InitTypecell`'s effects were not
     CG work and are now made outside its C payload.  `NoteOffsets` (the
     `O`/`o` records, and through `GetOffsets` the offsets MSIR's object
     types need, hence the `gc_ref void` translations) is a step of
     `Type.GenCells`, between the two payloads, so it is made once in every
     mode; it is not idempotent (each call adds an exported object), so it
     could not simply be repeated in the MSIR payload.  And MSIR's method
     suite (`FillMethodNames`, `GenLinkProcMSIR`) calls
     `Procedure.NoteUsedMSIR` for each default procedure where C's
     (`FillMethods`' `CGName`, `GenLinkProc`'s compile) loads it, as
     `ProcExpr.CompileMSIR` already does: that is what declares and imports
     the procedure, so m3front's imported types and stamps follow.  With
     the C payloads of `RefType`/`ObjectType.InitTypecell` and
     `GenCellPtrs` skipped (a throwaway `@M3skipc_cell`), all 13 base
     packages now build to byte-identical `.msir` and identical `.m3x`
     (`m3xcanon.py`: no record differs); a normal build's output is
     unchanged (set and libm3 byte-identical).  So at step 6 those C
     payloads can go as they are.
- **Dropped: moving MSIR registrations out of the declaration walk.**  A
  few procedures of the declaration walk register things with MSIR as they
  go (`Module.BuildImportLink` → `MSIREmit.RegisterImport`,
  `Revelation.GenList` → `MSIRBuilder.AddRevelation`).  With no mode
  predicate that walk runs in every mode, so these are a step doing both
  walks' work, as `Marker` does, and stay.
- **Not in (A).**  The dual mode (the C backend with `@M3m3front-msir`,
  which the MSIR -> M3CG ladder and the msircg fallback use) runs both
  halves until D25 step 6.

### O18. Closure-call lowering reads the procedure's first word.

**Open.**  `MSIRBuilder.EmitClosureCall` tells a closure from a plain
procedure by loading the first word of the procedure value and comparing
it with `CL_marker` (-1), as the CG path does.

- It does **not** have the #1243/#1244 defect: each arm makes its own
  call (closures go through the shim with the env, plain procedures are
  called directly), so no undefined static link reaches a join.
- It still reads the code bytes of a plain procedure.  The C-output
  analysis in #1243 flagged the same pattern as undefined behaviour in C.
  In LLVM it works in practice, but it would fault on targets where code
  memory is execute-only.
- It skips the CG path's pre-checks (pointer 8-aligned, non-NIL), so
  calling a NIL procedure value is a bare SIGSEGV rather than a Modula-3
  runtime error.

Options: (a) reproduce the CG pre-checks, which keeps the code-byte load
but restores the NIL diagnostic and skips the load for unaligned code
addresses; (b) represent closures with a tag bit in the pointer (code is
at least 4-byte aligned on all three targets), so the test needs no
memory access.  (b) changes the closure ABI shared with C-backend code
and the runtime, so it is a whole-system change.

### O19. Target triple and data layout are hard-coded.

**Open.**  `MSIREmit.BeginUnit` hard-codes the triple and data layout per
target, including minimum OS versions (`arm64-apple-macosx11.0`,
`x86_64-apple-macosx10.15`).  Peter's LLVM backend has the same need; on
master he briefly derived them by compiling `Csupport/hand.c` with
`clang -S -emit-llvm` and reading the header (`04bef6698f`, removed in
`837ada4731`).  Proposal: define triple and data layout once, in `Target`
or the platform config, so both LLVM backends share one source and the
data layout can follow the installed LLVM.

---

## Cross-references

- `MSIR.md` — strategic narrative.
- `m3-sys/m3middle/src/M3CG_Ops.i3` — the legacy IR being bypassed for
  the LLVM path (kept for C/gcc backends).
- `m3-libs/m3core/src/runtime/common/RTCollector.{i3,m3}` — collector
  contract: mostly-copying, software barrier, to-space stack invariant.
- `m3-libs/m3core/src/runtime/common/RTHeapMap.{i3,m3}` — gcmap contract
  MSIR reuses unchanged.
- `m3-sys/llvm/llvm22/src/M3CG_LLVM.m3` — existing experimental LLVM
  backend (consumes M3CG; superseded by MSIR for LLVM path, retained as
  a diff-test oracle during transition).

---

## Implementation Reference

This section records current implementation status, key source files,
internal architecture notes, and known limitations.  It is updated
incrementally as features land.

### Test Harness

Use `m3-sys/msir/test/sweep.py` (run from the repo root with `cm3` on
`PATH`) to run the 288 p0/p1/p2 tests with per-test 60-second timeouts:

```sh
PATH="$HOME/cm3/bin:$PATH" python3 m3-sys/msir/test/sweep.py baseline
python3 m3-sys/msir/test/sweep.py summary          # categorised abandon counts
python3 m3-sys/msir/test/sweep.py grep <pattern>   # find tests matching pattern
python3 m3-sys/msir/test/sweep.py rerun <pattern>  # rebuild matching tests
python3 m3-sys/msir/test/sweep.py check            # re-run all tests with prior abandons
```

The harness is essential: `cm3 -DHTML` in m3tests hangs on tests that
loop at runtime (p161/p267); sweep.py kills those after 60 s.

**Important:** sweep results depend on `M3_BACKEND_MODE` in the installed
config.  In `"C"` mode, MSIR runs alongside the C backend (abandons short-
circuit early, VERIFY warnings don't affect linking).  In `"MSIRObj"` mode,
MSIR IS the backend, so more code paths execute through MSIR.

### Validation Ladder (2026-07-24, ARM64_DARWIN)

> **Superseded (2026-11-19).** A clean rebuild of `b28fd265c3` fails
> p165 and e020, and the fixpoint now covers all MSIR-built modules.
> See *Merge from master (2026-11)*.  The record below is kept for history.

The ladder run for the last three `msir` commits (`f6b3f3c5d2`,
`bbf78535cf`, `b28fd265c3`), the last with incremental + generational
GC live:

1. **Stage 1 (the C-backend driver):** full m3tests: **292 tests, 0
   errors** (`m3-sys/m3tests/m3tests-results.xml`, 2026-07-24 16:27).
2. **Stage 2:** stage 1 builds the compiler packages in MSIRObj mode,
   giving a `cm3` whose own code is entirely MSIR → LLVM.  Smoke test
   passes; p292 stress test 10/10.
3. **Full m3tests under the stage-2 driver:** **292 tests, 0 errors.**
4. **Stage 3:** stage 2 rebuilds the compiler packages; the result
   passes the smoke test.
5. **Fixpoint:** stage 2 and stage 3 emit identical IR.  Note: on
   2026-07-24 this compared `Main.msir` only.  The full check (all 383
   compiler-module `.ll` files bit-identical) was last run on
   2026-07-14; see *Bootstrap Fixpoint Stability*.  Re-run the full
   check before relying on it.

Not covered: AMD64_DARWIN and AMD64_LINUX (no runs yet), and
`do-cm3-base` / `do-cm3-std` package sweeps since 2026-07-14.

### Current Status

**MSIRObj mode** (`M3_BACKEND_MODE = "MSIRObj"`, ARM64_DARWIN):

- Smoke test: **124/124 checks pass, exit 0** against real CM3 runtime.
- **181/181** LLVM link-test checks pass.
- m3tests sweep (**288 tests — all of p0, p1, p2**, forced clean builds):
  **286/288 clean; 0 genuine abandons; 2 TIMEOUTs**.
  - 2 TIMEOUTs: p161 (million-element Sieve, takes > 2 min) and p267
    (intentional `WHILE TRUE DO RTCollector.Collect()` loop) — not codegen
    issues.  sweep.py kills the full process group on timeout
    (`start_new_session=True` + `os.killpg`).
  - Zero `msir-verify` errors.
  - Zero genuine abandons.

**LLVM conformance** (`run-msir-conformance.sh`, ARM64_DARWIN, 2026-07-14):
**259/288 PASS, 2 MISMATCH, 27 SKIP**.

Remaining 2 MISMATCH:

| Test | Root cause |
|------|-----------|
| p251, p259 | UNSAFE stack-height measurement (implementation-specific, unfixable — different stack frame layouts between CG and MSIR) |

27 SKIP — all blocked by the CG reference binary failing to build (the harness
requires a working CG executable to compare against; without one, there is
nothing to compare, regardless of what MSIR produces).  They fall into four
categories:

**Compile-time hangs (3):** p161, p224, p267.  The cm3 front-end loops
indefinitely on these inputs; the harness times out and skips.  Unfixable
without front-end changes unrelated to MSIR.

**M3C assertion failures (8):** the C backend aborts when generating code for
these modules, so no CG binary is ever produced.

| Test | Root cause in M3C.m3 |
|------|---------------------|
| p053, p160 | `SET OF` type > 64 bits; `typedef WORD_T* SET` is gated on set-operation opcodes being present, but these modules only use set-constant initializers, so the typedef is never emitted — clang rejects the generated `.cpp`. |
| p109, p232, p269 | Other M3C assertion failures: integer-type assertion (`m3_eq` macro instantiation), C++ parse error in generated code, and a code-gen abort on a specific expression pattern. |
| p248 | `BITS 4 FOR {enum}` hits `bit_size = 8 OR 16 OR 32` assertion in `declare_enum`; 4-bit packed enums are not supported by M3C. |
| p268, p280 | `TIntLiteral` assertion for out-of-range or negative-UINT64 constant values.  p268 intentionally uses record fields initialised with values outside the field's subrange (to test that a runtime error is raised); M3C asserts rather than emitting the constant. |

**Infrastructure — no standalone binary by design (13):**

| Tests | Reason |
|-------|--------|
| p068 | Calls `<*EXTERNAL "_RTHeap__Print"*>` — a CM3 internal function removed from the runtime long ago.  Linker error. |
| p135, p147 | SAFE-module restrictions: the compiler rejects the program at compile time.  No binary produced. |
| p152, p153, p154 | Use C-level `scanf`-family externals not present in the standard runtime libraries. |
| p201 | Exercises a mutually-recursive type definition that the front-end rejects; no binary. |
| p204 | TCP networking test requiring a live network; cannot be built in an isolated harness invocation. |
| p220–p223 | Parse-only tests: the m3makefile contains no `program()` directive, so no executable is linked. |
| p226 | Prints the compiler version string; the output is build-specific and cannot be golden-file compared across compiler versions. |

**`build_standalone()` MxConfig failures (3):** p240, p270, p277.  These
m3makefiles call `build_standalone()`, which directs the M3 static linker to
resolve all package paths at build time.  In the harness environment
`MxConfig__FindConfig` raises `OSError` (cannot locate the required cm3 config
file).  The same packages build without error under a plain `cm3 -build`
invocation with the correct working directory; something about the harness PATH
or CWD breaks the config search.

The authoritative feature checklist (emission and lowering, item by item)
is in `MSIR-ROADMAP.md §What's Working`.  Summary of coverage: arithmetic,
control flow, records, fixed/open arrays, enums, SETs (all widths), globals,
VAR/READONLY/VALUE params, TRY/EXCEPT/FINALLY, RAISE, GC barriers,
TypeCells, NEW (REF/OBJECT/open-array, all ranks), vtable dispatch,
TYPECASE/NARROW/ISTYPE, nested procedures (lambda-lifted), TEXT,
SUBARRAY, ADR/LOOPHOLE/ADDRESS, float conversions, procedure values,
BITS-N-FOR-T packed fields (ByteArrayFallback), compact subrange arrays,
struct-by-value return, opaque types, SET arithmetic, LOCK.

### LLVM Validation Status (2026-06-08)

m3core: **174/174 .ll files pass `llvm-as`** (zero errors).
libm3: **171/171 .ll files pass `llvm-as`** (zero errors).
m3middle: **26/26 .ll files pass `llvm-as`** (zero errors).

Fixes landed to reach this state:
- RTLinker.ll `declare`+`define` conflict: `MaybeAddExtern` now skips `modName_I3`
  since `EmitModuleInfo` always `define[weak]`s it (LLVM 22 rejects declare+define).
- RTCollector.ll missing `!dbg` on GC barrier calls: `EmitGcReadBarrier`/
  `EmitGcWriteBarrier` now accept `locIdx` and emit `!dbg` when debug info is enabled.
- PickleStubs.ll type mismatch: `LoopholeExpr.Noop` now calls `BuildTrunc`/`BuildZExt`
  when LookupVar-widened value has different width than LOOPHOLE destination type.
- FmtBuf.ll `load void`: `SubscriptExpr.LValueMSIR` falls back to
  `MSIRType.Translate(p.taBase)` when `EltType` of a capture-param pointer is Void.
- Capability.ll dominance violation: `LoopholeExpr.Noop` struct path re-fetches
  `CurrentBlock()` after `CompileMSIR`/`LValueMSIR` (invoke may advance the block).
- m3middle M3CG_Check.ll `[N x i1]` vs `[N x i8]` mismatch: `BuildConstAggArray`
  now uses `MSIR.FixedArrayElt(arrMsir)` instead of re-translating `eltT`, so
  BOOLEAN elements in ByteArrayFallback arrays are widened to `i8` via `ConstInt`
  coercion.

### Bootstrap Fixpoint Stability (2026-07-14)

Two consecutive three-stage fixpoint tests confirm the MSIR code generator is
deterministic and self-consistent.

**Protocol:**
1. Build all compiler packages (m3middle, m3linker, m3front, m3quake, m3objfile,
   m3back, cm3) in MSIRObj mode using the C-backend cm3 → stage-N.
2. Install stage-N cm3; repeat the build → stage-N+1.
3. Diff all `.ll` files from stage-N and stage-N+1.

**Results:**

| Round | Starting binary | .ll files compared | Diffs |
|-------|-----------------|--------------------|-------|
| 1 (stage 1→2) | C-backend cm3 | 383 | 0 |
| 2 (stage 2→3) | MSIRObj cm3   | 383 | 0 |

All 383 `.ll` files are bit-for-bit identical across consecutive stages:
m3middle×26, m3linker×12, m3front×299, m3quake×18, m3objfile×4, cm3×15.
(m3back×0 — m3back is a pure C/M3 package with no MSIR-emitted modules.)

The stage-N cm3 binary SHA256 differs between stages (linker non-determinism is
expected); the `.ll` identity is what matters for fixpoint correctness.

**Since then:** the 2026-07-24 runs compared only stage-2 and stage-3
`Main.msir` (identical).  The full 383-file comparison has not been
re-run since the GC-correctness batch (`f6b3f3c5d2` onward).

**Script:** `m3-sys/msir/test/fixpoint.sh` (in session scratchpad; not tracked).
Requires `PATH=$(brew --prefix llvm)/bin:$HOME/cm3/bin:$PATH` and MSIRObj mode
enabled in `m3-sys/cminstall/src/config/ARM64_DARWIN`.

### Package Coverage (2026-07-14)

MSIRObj mode builds the full CM3 standard library collection without a single
codegen failure.

| Build set | Script | Packages attempted | Packages clean | Failures |
|-----------|--------|--------------------|---------------|---------|
| `base`    | `do-cm3-base.sh buildglobal` | 24 | 24 | 0 |
| `std`     | `do-cm3-std.sh buildglobal`  | 198 | 195 (+ 3 omitted on Darwin) | 1 pre-existing C bug |

The one failure (`caltech-other/voronoi`) is a C89 `void*`→`Triple*` implicit
cast in `mod3_main.c` that modern clang rejects in strict mode. It fails
identically with the C backend; it is not an MSIR issue.

Also fixed: `scripts/sysinfo.sh` arm/arm64 detection — `uname -p` returns `arm`
on Apple Silicon; added `arm | arm64) CM3_TARGET=ARM64_DARWIN` to the Darwin
case so `do-cm3-*.sh` scripts run without the `TARGET` override.

### Merge from master (2026-11)

**Status: merged locally (`593cb56d0c`, 2026-11-19); not yet pushed.**

**Starting point.**  Merge base `3faae855fb` (2026-05-08); master is 64
commits ahead (at `d2108205fd`).  A trial `git merge-tree` reports two
conflicts, both expected-output files:

- [x] `m3-sys/m3tests/src/e0/e019/stdout.build`: resolution: took master's (it matches master's
  expanded test, N0..N8); the merged compiler's output matches it.
- [x] `m3-sys/m3tests/src/e0/e020/stdout.build`: resolution: kept msir's at merge time, then
  updated to the merged compiler's output (`cffab7a395`).  Master's early `RETURN ErrType.T`
  in `Type.Check` now runs before msir's supertype-cycle walk in `ObjectType.Check`, so the
  compiler reports the illegal recursive type plus cascading errors instead of 'illegal
  recursive supertype (2.4.8)'.  Still rejected cleanly.  Revisit with #1254.

**Files changed on both sides that merge without conflicts.**  Check
the MSIR paths in each after merging:

| File | master change | MSIR relevance / result |
|------|---------------|-------------------------|
| `m3front/src/exprs/QualifyExpr.m3` | recursion recovery in `Bounder`, `ResolveTypes` (Rodney) | MSIR read barriers on byref field loads live here |
| `m3front/src/values/Formal.m3` | comment only (BITS FOR excluded from by-reference) | MSIR dirties containers at VAR-argument formation |
| `m3front/src/exprs/NamedExpr.m3` | return `ErrType.T` on illegal recursion | |
| `m3front/src/values/Value.m3` | `TypeOf`/`RepTypeOf` short-circuit on `t.error` | |
| `m3front/src/types/Type.m3` | `Check` returns `ErrType.T` on recursion | `Type.GenCellsMSIR` lives here |
| `m3front/src/misc/CG.m3` | `Store_indirect` stack-type fix (p207) | CG path only |
| `m3middle/src/TFloat.m3` | 17-digit LONGREAL | identical change already on msir |
| `m3core/src/runtime/ex_stack/RTExStack.m3` | `RTEHScan` removed | used only for stack printing, not EH |
| `scripts/pkginfo.txt`, `scripts/sysinfo.sh` | python3, Apple Silicon detection | |

**Other master changes that affect MSIR:**

- `ThreadPThread.DisposeStacks` (`dfa907c4a1`): a WITH alias read after
  `DISPOSE`.  It's a bug in the Modula-3 source, so MSIR-built m3core has
  it too; it shows up on Ubuntu 26.04 only.  Needed for the AMD64_LINUX
  ladder.
- Rodney's 2022 front-end commits (packed actuals passed READONLY by
  value, recovery from illegal recursion) update the expected output for
  p047, p179, p207, p228, p280, e019, e020 and e041.  The earlier "p280:
  all 321 cases pass" was measured against the old expected output.
- `0921ea25f6`: `llvm_backend`/`llvm_opt` in `Unix.common` now take their
  level from `m3back_optimize` (`-O2`), which MSIRObj also uses.  Record
  the effective `opt`/`llc` levels for MSIR builds with and without `-O`.
- `cf2fa27f90`: EXTENDED/LONGREAL alignment in `RTTipe`/`ConvertPacking`
  (pickles).  Runtime-only; MSIR builds pick it up.

**Results** (fresh `ci-ladder.sh` runs on ARM64_DARWIN, 2026-11-19, each
from a copy of the installed cm3 in its own worktree):

| Check | Before merge (`068afbd602` = `b28fd265c3` + CI) | After merge (`dcab4efdde`) |
|-------|------------------------------------------------|----------------------------|
| m3tests, stage-2 driver (MSIRObj runtime + tests) | 292 tests, 2 errors: p165, e020 | 293 tests, 3 errors: p165, e020, p295 |
| Fixpoint, stage 1 = 2 = 3 (all MSIR-built `.mb`/`.ib`) | 1416/1416 identical | 1414/1414 identical (`RTEHScan` removed) |
| Smoke test, stage-3 cm3 | pass | pass |
| msir CI | run `44444889638` cancelled (ladder script bugs, since fixed) | not yet run (merge not pushed) |

Failures:

- **p165** (TYPECODE of `REF INTEGER`): runtime abort "A compile-time
  type is missing".  An msir regression present before the merge.
  Fixed in `7067f7d3be`: `MSIREmit.RegisterImport` skipped the RTHooks
  binders, so RTHooks' typecells were never reached from the program's
  import chain.
- **e020**: see the conflict note above; expected output updated in
  `cffab7a395`.  The output then varied between runs: revelations were
  checked in hash-bucket order, keyed on an M3ID number that depends on
  the imports and the compiler build.  Fixed by checking them in
  declaration order (`39fffcecd5`); ported to master with the recursive
  supertype check (`7f3cebbdc5`), where e020 had been accepted and
  crashed at run time.
- **p295**: new test from master; its `stdout.pgm` lacked the trailing
  newline the program prints.  Fixed in `75a09f8f37`; should also go to
  master.
- **p204** (imports `tcp`) failed in an earlier run only because the
  ladder didn't rebuild `tcp` through MSIR; m3core's ABI link guard
  (`RTAbiCG.c`) correctly refused the mixed link.  Fixed in the ladder.

The 2026-07-24 "292/0" could not be reproduced from a clean build.  The
installed `cm3` reports `bbf78535cf`, while the installed `libm3core.a`
and that results file share a 16:27 timestamp, so which commits that run
used is unclear.

**Ladder lessons:** MSIRObj writes LLVM IR as `<unit>.mb`/`.ib` (not
`.ll`) and deletes it unless `cm3 -keep`; any library a test imports
must also be rebuilt through MSIR because of the ABI link guard.

### Abstraction review and decisions (2026-11-19)

Four independent read-only reviews of `git diff d2108205fd msir`, with the
main findings spot-checked.  Decisions (Tony, 2026-11-19):

- **D22. MSIR behaviour attaches through TypeRep/ExprRep methods.**  Add
  methods such as `initValueMSIR`, `storeMSIR` and `checkUseFailureMSIR`
  beside their CG counterparts, override them per node/type, and delete the
  exported helpers and shape inspection that re-implement them.
- **D23. MSIRBuilder is node-agnostic.**  It only constructs IR.  Node and
  type modules own their lowering; capture results move to CaptureAnalysis
  or `Procedure.T`; the builder's symbol maps take handles from callers
  instead of importing node modules.
- **D24. RT0 layout has one source.**  Use `M3RT` offsets and sizes
  everywhere; keep the LLVM field-type tables, but assert once that their
  padded offsets equal `M3RT`'s `TC_*`/`OTC_*`/`ATC_*`/`MI_*` values.
  - **Amendment (proposed 2026-11-21, with D27): the authority is RT0, in
    Modula-3; `M3RT` is the compiler's copy, checked against it.**
    - *Authority.*  The runtime's data structures are declared once, as
      ordinary Modula-3 in `RT0` (and `RTException`, `RTExFrame` for the
      `EA_`/`EF_` layouts): typecells, module and import info, procedure
      info, revelations, closures, exception descriptors, and the object
      header.  Runtime code reads them as fields.  The open-array dope
      (`OA_`) has no runtime declaration and stays the compiler's.
    - *The object header becomes a packed record again.*  From 2001 to
      2022 `RT0.RefHeader` was a record of `BITS` fields (forwarded,
      typecode, dirty, gray, weak, marka, markb, spare).  #992 replaced it
      with a plain word and out-of-line accessors using literal bit
      positions (`Word.Extract (h.word, 1, 20)`), keeping the packed record
      only as an unused `RefHeaderBits`, so that one C bootstrap would serve
      both byte orders.  The packed record comes back as `RefHeader`;
      `RT0`'s accessors either go or become field reads that the C backend
      inlines.  Reason: it states the layout in Modula-3, runtime code reads
      `h.typecode` as an inline extract rather than a cross-unit call, and
      the declaration can be checked (below).
    - *Why this is safe for portable C.*  Packed-field lowering depends only
      on byte order, word size and `Allow_packed_byte_aligned` (FALSE for
      the C backend).  Checked 2026-11-21: the C the front end generates for
      reads and writes of the packed header is identical for ARM64_DARWIN,
      AMD64_DARWIN, AMD64_LINUX, ARM64_LINUX and AMD64_FREEBSD (narrow loads
      at fixed byte offsets, e.g. `UINT32` at 0 for `typecode`, `UINT8` at 2
      for `gray`).  Only big-endian targets differ, and they are out of
      scope; if they return, the fix belongs in the lowering (a packed
      field as logical bits of its word-aligned container, read with
      whole-container loads), not in the runtime source (D27).
    - *Why `M3RT` stays.*  The compiler needs these layouts for every unit,
      including units that never import `RT0` (every unit has a module info
      and typecells; `RTHooks` does not import `RT0`) and while compiling
      `RT0` itself.  So `M3RT` cannot be derived from `RT0` on demand; it
      remains the compiler's working copy, used by the front end, the
      MSIR lowerings and (through D24's assert) the LLVM field tables.
      m3back and m3linker do not use it.
    - *The check.*  When the front end compiles `RT0`, and whenever it
      loads `RT0` while compiling another unit, it compares the layout it
      computes for `RT0`'s records with `M3RT`: each field's offset and
      size (bits for `RefHeader`), and each record's size.  A difference is
      a compile-time error naming the field.  Building m3core therefore
      fails on any drift, and a compiler whose `M3RT` has changed rejects an
      `RT0` that has not.  Optionally, a layout fingerprint in each module
      info, checked by `RTLinker` at startup, catches a program linked
      against an m3core built by a different compiler.
    - *Considered.*  A compiler-provided interface (`RTLayout`, like the
      built-in `Compiler`) exporting `M3RT`'s values as constants for the
      runtime to use.  It gives one source and real constants, but makes the
      runtime consume compiler constants instead of declaring its own
      structures, needs a bootstrap step (older compilers lack the
      interface), and is unnecessary once the header is a packed record
      again.  Kept in reserve for any value that cannot be stated as an
      `RT0` declaration.
    - *Cost.*  Reverting the header part of #992 in m3core, and the check
      in the front end (one procedure, run when `RT0` is compiled or loaded).
- **D25. MSIR becomes the front end's only lowering; EH stays structured**
  (Tony, 2026-11-20).  The end state is m3front → MSIR only, with M3CG
  kept as the input to the existing backends (C, m3cc, IntLlvm*) and
  produced by an MSIR→M3CG lowering, so every node has one lowering and
  every target keeps working.  EH keeps the envelopes of D8/D20.
  - **Gap found.**  The envelope API exists (`NewTryExcept`,
    `NewTryFinally`, `BuildUnwindTo`, `BuildRetThroughEnvelope`,
    `BuildRaise`) but nothing uses it and `MSIRToLLVM` does not lower it.
    TryStmt, TryFinStmt and LockStmt build `invoke`/`landingpad`/`resume`
    and call `__cxa_get_exception_ptr` themselves, so the IR is committed
    to C++ EH before any backend sees it.  An MSIR→M3CG lowering could
    serve the `ex_stack` targets from that, but not `ex_frame` ones.
  - **Order** (revised 2026-11-20: the lowering comes before decoupling,
    and envelopes before the lowering).
    1. D22–D24.
    2. *Optional spike:* lower only procedures without TRY or LOCK to
       M3CG and push m3core through MSIR → M3CG → C.  Measures how much
       type information the C backend needs that MSIR lacks: `M3C`
       builds its C types from 19 `declare_*` methods (`declare_record`,
       `declare_field`, …) fed by m3front's type modules, which the
       lowering would have to regenerate from MSIR types.
    3. TRY/EXCEPT, TRY/FINALLY and LOCK onto EH scopes (D26), with
       `invoke`/`landingpad`/`resume` produced only in `MSIRToLLVM`; the
       verifier checks D8.  Done before the lowering so the lowering's
       EH is written once, for both `ex_stack` and `ex_frame`.
    4. MSIR→M3CG lowering in `m3-sys/msir`, run as a sibling of MSIRObj
       (CG walk discarded as now, MSIR lowered to M3CG for the C
       backend).  Validated with the ladder and m3tests, first on the
       `ex_stack` targets, then `ex_frame` ones.  Checks are behavioural
       (m3tests, bootstrap fixpoint), not diffs against direct CG
       output.  A failure in both MSIR backends points at MSIR or the
       front end; in one only, at that lowering.
    5. Decouple (O17): an MSIR-only driver walk (A), link information in
       a backend-neutral pass (B), skip the discarded CG work (C).
       Checked against both MSIR backends.
    6. Make MSIR → M3CG the default; delete the per-node CG methods
       (`compile`, `prep*`, `compileLV`, `compileBR`).  With them go the CG-only
       initialization routines (`Variable.LangInit`, `UserInit`,
       `ForceInit`, `Scope.InitValues`), the CG walk's deferral of a default
       value that an explicit initializer will overwrite (`initPending`; the
       MSIR walk tests that condition directly in `LangInitMSIR`), and the
       flag that tells the CG walk's setting of `initDone` from a
       declaration's (`Variable.cgInitDone`, `DoneAtDeclaration`):
       `initDone` then means initialized at declaration only.
    7. Rename the MSIR procedures and methods to the generic names the
       CG path frees: `compileMSIR` → `compile`, `InitTypecellMSIR` →
       `InitTypecell`, `Type.InitValueMSIR` → `Type.InitValue`, and so
       on for every `xxxMSIR`/`xxx_MSIR` name in the front end.  Done
       only after step 6, as one mechanical rename with no behaviour
       change.
- Clutter (session transcripts, a test binary, editor backups) removed in
  `77a724b298`.

- **D26. Exception scopes: a flat CFG with a per-block EH scope**
  (proposed 2026-11-21, for review; D25 step 3).  Replaces the unused
  envelope API (`NewTryExcept`, `NewTryFinally`), whose single-block body
  cannot hold a TRY body, with a representation that keeps D20's flat CFG.
  - **Representation.**  A procedure keeps its flat list of blocks.  It also
    has a tree of *EH scopes*; every block belongs to exactly one scope (the
    procedure's root scope if no TRY encloses it).  Scope kinds:
    - `Except` (TRY/EXCEPT) and `Finally` (TRY/FINALLY, LOCK) are
      *protected*: an exception raised in their blocks is handled by them.
      An `Except` scope lists its handlers, each an exception (an
      `ExcDesc`) or ELSE, and the entry of the handler's code.  A `Finally`
      scope has the entry of its FINALLY code.
    - `Handler` and `FinallyCode` hold a handler's or a FINALLY's code.
      They are not protected by their own TRY: their parent is the TRY
      scope's parent, and they name the TRY scope as their *owner*.  They
      exist so the lowering knows where that code ends (amended
      2026-11-21): under `ex_stack` every way out of handler code must call
      `__cxa_end_catch`, and FINALLY code ends by resuming what entered it.

    A handler's entry block takes the exception's argument as its one
    parameter when the handler uses it (a handler naming several exceptions
    binds the argument only if they share its type).  Nothing is nested in storage, so
    passes that walk blocks are unchanged; the scope tree is consulted only
    where EH matters.
  - **Control (D8).**  An edge whose target is in the same scope, or enters
    a child `Except`/`Finally` scope through its entry, is an ordinary
    `br`/`cond_br`.  `Handler` and `FinallyCode` scopes are entered only by
    the EH mechanism, never by a branch.  Any other edge leaves one or more
    scopes and must be explicit: `unwind_to target(args)` (EXIT, normal
    completion, a handler finishing) or `ret_through_envelope v` (RETURN).
    This holds for normal completion of a TRY/EXCEPT too.  Under `ex_stack`
    leaving an `Except` scope needs no code, but under `ex_frame` it pops
    the scope's setjmp frame, so the exit must be visible to the lowering.
  - **Raising.**  `raise exc, arg` is a terminator; the lowering calls the
    runtime's raise hook (`RTHooks.RaiseEx` today).  Calls in protected
    blocks stay plain `call` / `call_indirect`: the lowering decides which
    may raise.  D10's `raises` attribute covers direct calls only, so in
    practice every call in a protected block is treated as possibly raising
    (indirect calls, methods and runtime hooks such as `ReportFault`, which
    raises runtime errors), as today.  A fault that is not a call (a signal
    converted to an exception) is not covered, by this or by either current
    backend.
  - **FINALLY.**  The front end emits the FINALLY code once, in a
    `FinallyCode` scope.  Each lowering chooses between duplicating it per
    exit path (D9) and dispatching on a selector; MSIRToLLVM keeps today's
    selector scheme.  Semantics the lowering must keep:
    - FINALLY runs on normal completion, on every explicit exit leaving the
      protected scope, and on an exception propagating out of it;
    - the FINALLY code finishes with `end_finally`, which resumes the
      outcome that entered it: the exit continues, the RETURN returns, or
      the exception is re-raised;
    - if the FINALLY code itself leaves by `unwind_to`,
      `ret_through_envelope` or `raise`, that outcome *replaces* the pending
      one.
  - **Language rules stay in the front end** (amended 2026-11-21).  MSIR's
    scopes are mechanism: protection, handlers chosen by exception identity
    or ELSE, FINALLY with resumption, explicit exits.  What Modula-3 means
    is expressed with them by the front end.  In particular EXIT and RETURN
    are exceptions in Modula-3, so EXCEPT ELSE catches them: inside the
    protected blocks of a TRY with an ELSE, the front end compiles EXIT and
    RETURN as `unwind_to` the ELSE code, and the exception path reaches the
    same code through the ELSE handler.  MSIR has no rule for it.
  - **LOCK.**  The front end emits the acquire, then a `Finally` scope whose
    FINALLY code releases the mutex.  No new op.
  - **RAISES checking.**  A procedure whose RAISES clause excludes some
    exceptions must turn any other into a `BlockedException` runtime error.
    That stays a procedure attribute (`ProcSetRaises`, D10), and each
    lowering generates what its scheme needs, as CG's `StartRaises` does
    (`ex_frame`: a `RaisesNone` or `Raises` frame; `ex_stack`: only the
    frame save CG emits today).
  - **Exception arguments.**  `ExcDesc` gains the argument's MSIR type (NIL
    if none), so the verifier can check a handler's block parameter and
    the lowering knows whether the activation carries the argument by value
    or by reference.
  - **The C++ EH logic moves into MSIRToLLVM verbatim.**  The front end's
    current `TryStmt`, `TryFinStmt` and `LockStmt` MSIR code builds
    catch-all landing pads (not cleanups, so phase 1 of the Itanium search
    stops in this frame), only peeks at the activation without
    `__cxa_begin_catch`, and re-raises through `ResumeRaiseEx`.  Each of
    those choices fixed a failing test (p004, p140); the lowering must copy
    them unchanged, checked against the EH m3tests before anything else
    changes.
  - **Verifier.**  Checks that the scope tree is well formed (handlers and
    FINALLY entries in `Handler`/`FinallyCode` scopes owned by their TRY),
    D8 as above, `end_finally` only in `FinallyCode`, and that handler
    parameters match their exceptions' argument types.  The ops only the
    EH pass produces (`invoke`, `landingpad`, `extractvalue`, `resume`) are
    rejected in MSIR that has not been through it.
  - **The EH pass** (the middle course, 2026-11-21).  MSIRToLLVM first runs
    an MSIR-to-MSIR pass that rewrites scopes into today's
    invoke/landing-pad form, carrying the front end's current EH code over
    as builder calls; its output can still be printed and verified.  Once
    the EH tests are stable, folding it into MSIRToLLVM's text emission
    (and deleting those ops from MSIR) can be decided.
  - **Order.**  (1) MSIR API, printer and verifier; (2) MSIRToLLVM lowering
    of scopes, reproducing today's output semantics; (3) TryStmt,
    TryFinStmt, LockStmt, ReturnStmt and ExitStmt emit scopes (ladder,
    m3tests under MSIRObj); (4) MSIRToM3CG lowers scopes, first CG's
    `ex_stack` scheme, then `ex_frame`.
  - **Why ex_frame still matters.**  If every C-backend target moves to
    `ex_stack` (Discussion #1266), `ex_frame` remains for the M3CG consumers
    that have no invoke: m3cc and the integrated x86 backend, which are
    what MSIR→M3CG would feed.
  - **Added while implementing** (2026-11-21):
    - `handler.activation` yields the activation record of the exception
      being handled by the nearest enclosing handler code
      (`Compiler.ThisException`); the verifier allows it only there.
    - `ExcDesc` records whether the raise passes the argument by reference
      (the front end's `Exceptionz.ArgByReference`), so the lowering reads
      the argument word correctly without recomputing the convention.
    - A module records the runtime's re-raise hook (`ResumeRaiseEx`), set by
      `MSIREmit` when any procedure has scopes.
    - Within a protected body of a TRY with an ELSE, the front end routes
      EXIT and RETURN to the ELSE code (`MSIRBuilder.PushElseFrame`); the
      ELSE handler's own code just leaves for the same ELSE code, so the ELSE
      body runs after `__cxa_end_catch` under `ex_stack`.
  - **Considered and rejected:** nested regions (envelopes holding block
    lists), the first draft of this decision.  Tree-shaped storage
    contradicts D20 and would force every block-walking pass to recurse.

- **D27. C is a lowering, not the definition of Modula-3** (Tony,
  2026-11-21).  The language's semantics belong to the report, the front
  end and MSIR.  The C backend, and any portable C distribution, are
  lowerings that must implement those semantics, not shape them.
  - **Why.**  C has been defining Modula-3 by default:
    - `ROUND` took the C library's tie-breaking (halves away from zero)
      against the report's `FloatMode.RoundDefault` (NearestElseEven) in
      both backends built on C-family runtimes: the C backend (`round()`)
      and the gcc backend (`lround`, earlier `TRUNC (x +/- 0.5)`).  The
      integrated x86 backend (x87 round-to-nearest-even before `FISTP`) and
      MSIR's LLVM path (`llvm.roundeven`) followed the report.  Both fixed
      in #1281.
    - `EXTENDED` is whatever C type a platform has: `_Float128` on
      AMD64_LINUX, `double` elsewhere.
    - Exceptions under `ex_stack` are C++ exceptions (`throw _M3Exc`), and
      MSIR's LLVM path uses `__gxx_personality_v0`.  A backend that cannot
      emit C++ `try` (the integrated x86 backend) cannot use `ex_stack`.
    - Runtime source bends to C's limits: object-header bitfields became
      `Word` operations behind out-of-line `RT0` procedures (#992), so every
      header test in the collector's loops is a cross-unit call the C
      backend cannot inline; interfaces avoid data (for `dllimport`);
      platform constants become calls, so they cannot be CASE labels or be
      folded.
    - Proposals for one portable C bootstrap would make the generated C
      the canonical form, so every feature would be judged by whether it
      survives as portable C.
  - **Decision.**
    1. *Semantics.*  The report decides; where it leaves a choice, a
       D-entry here does.  C helpers (`m3_round` and the like) implement
       those semantics.  A disagreement between backends is a bug in the
       one that departs from the report.
    2. *Runtime source is written for Modula-3.*  C interop and platform
       specifics (Mach thread primitives on Darwin, signals on Linux, OS
       structs and constants) live in thin `#ifdef`'d C files behind
       interfaces that are the same on every platform.  A change that costs
       Modula-3 expressiveness or performance only for the sake of C
       portability needs a measured case, and a hot-path abstraction must
       stay inlinable (defined in the unit that uses it, or `static inline`
       in `m3core.h`).
    3. *The bootstrap is an artifact, not a design driver.*  Per-target
       bootstrap tarballs built by CI are an acceptable cost.  If one
       portable artifact is wanted, it is generated from MSIR by a lowering,
       so C's limits stay in that lowering.  A portable bootstrap fixes the
       generated code's target parameters (64-bit, little-endian, one
       `EXTENDED` size, C++ EH); a target's native rebuild then uses its own.
    4. *Layout has one source* (D24, amended).  The runtime's structures,
       the object header included, are declared in Modula-3 in `RT0`; the
       compiler's `M3RT` copy is checked against them, rather than the bit
       positions being duplicated by hand.
  - **Longer term, not scheduled.**  An EH personality owned by Modula-3
    (its own unwinder personality and tables, not C++'s) would separate
    exceptions from the C++ runtime.  A portable IR bootstrap would need
    MSIR with symbolic, target-neutral layout; today the front end resolves
    sizes before building MSIR, so MSIR is per-target like the C it
    replaces.

- **D28. External globals: C variables and interface variables are
  different things** (Tony, 2026-11-22: option B).
  - **Problem.**  MSIR marks two kinds of global "external" alike
    (`MSIR.GlobalIsExternal`), so MSIRToM3CG cannot lower either and falls
    back on any unit that uses one (most of m3core's remaining 30 units):
    - *C variables*, declared `<*EXTERNAL*>` (`Uerror.E2BIG`,
      `Unix.O_RDONLY`, `Usocket`, `Usignal`, `Ustat`, `WinReg.HKEY_*`):
      real linker symbols in every back end.
    - *Interface variables referenced by the module that exports them*
      (`ASCII.Control`, `RTHeapRep.allocatedPages`,
      `RTCollectorSRC.gcRatio`, `IEEESpecial.LongNan`).  CG and the runtime
      place an interface's variables in the interface unit's data segment
      (`I_<Intf>`), reached through the import chain; they are not linker
      symbols.  MSIR→LLVM instead emits a "backing global" symbol, from both
      the interface unit and its exporting module (Variable.m3).
    Units that *import* another interface's variable already use the import
    chain in MSIR (`GlobalMapAddImport`: owner binder `<Intf>_I3`, byte
    offset), as CG does.
  - **Option A (annotate).**  Keep `isExternal` for `<*EXTERNAL*>` C
    variables only; give an interface variable its owner binder and byte
    offset (as imports have).  MSIR→LLVM keeps its backing symbols;
    MSIRToM3CG lowers C variables to `import_global` and interface
    variables through the import chain.  Smallest change; the two lowerings
    keep different models.
  - **Option B (one model).**  The exporting module reaches its own
    interface's variables through the import chain, exactly as importers
    do, in MSIR itself.  The duplicated backing globals go; both lowerings
    follow the layout CG and the runtime use (D24's direction).  Costs one
    load per access to such a variable in the exporting module, as the CG
    path already pays, and touches the LLVM path, so it needs the ladder.
  - **Recommendation: B**, as the representation the runtime actually has
    and the one D27 asks the IR to own; A if the LLVM path should not move
    now.  Either way `<*EXTERNAL*>` C variables stay external symbols.
  - **Decided: B.**  An earlier MSIR attempt at the import chain here was
    dropped for RTLinker, which writes its own `argc`/`argv` in
    `InitRuntime`; but `InitRuntime` runs `FixImports` on itself first ("make
    sure we can at least reference our own interface variables!"), as the
    CG path relies on.  Done (2026-11-22): `Module.DeclareGlobalsMSIR`
    routes an exported non-external interface variable through the import
    chain; MSIRToM3CG declares `<*EXTERNAL*>` variables with
    `import_global`.  m3core: all 291 units lower, none falling back; the
    lowered m3core links and runs the test programs unchanged.

- **D29. The MSIR front end keeps master's architecture** (Tony,
  2026-11-23).  Decided: the principle, D29a, and keeping lambda lifting.
  - **Principle.**  Apart from CG being replaced by MSIR, maintainers should
    see a front end that has not diverged significantly from the one on
    master: the same driver (`Module.Compile`, `Procedure.GenBody`,
    `Scope.InitValues`, `LangInit`/`UserInit`), the same modules and stacks
    (`Marker`), the same node methods (`compile`, `compileLV`, ... with MSIR
    bodies), and backend state kept where master keeps it (on the nodes, as
    `Variable.cg_var` and `Procedure.cg_proc` are).  MSIR-specific machinery
    is justified only where the IR needs something CG did not; such cases
    are listed here and decided explicitly.  Step 7 of D25 (renaming the
    MSIR procedures to the generic names) is the last step of this, not
    the whole of it.
  - **Where the MSIR front end diverges today** (to converge, unless
    marked):
    | Area | Master | MSIR today | Direction |
    |------|--------|------------|-----------|
    | Control frames | `Marker` | exit and cleanup stacks in MSIRBuilder | Converge: D29a below |
    | Variable storage | `cg_var` on `Variable.T` | `varMap` in MSIRBuilder, keyed by `Variable.T` | Converge: an MSIR handle on the node, as `cg_var` |
    | Procedures | `cg_proc` on `Procedure.T` | `procMap` and `shimMap` in MSIRBuilder | Converge: an MSIR handle on the node |
    | Nested procedures: when compiled | after the parent by default; inline in the parent under `Host.inline_nested_procs`, an existing mode | always inline, through its own route (`GenBodyMSIR`, reached from `LangInit`, with its own guard against compiling a body twice) | Converge: MSIR uses master's inline path (the default, `inline_nested_procs` = TRUE) rather than a separate one.  MSIR always inlines; FALSE (`-noinline_nested_procs`, for backends that cannot nest) stays a C-backend option and goes with the C walk at D25 step 6.  Lambda lifting would in principle allow outlining (a lifted body uses only its capture parameters, and pre-registration fixes capture lists before calls), but no target needs it |
| Nested procedures: up-level access | static link to the parent's frame | captures lambda-lifted into parameters, closure shims | **Keep** (Tony, 2026-11-23): lambda lifting is a deliberate choice for the best LLVM code, the one justified divergence here |
    | Initialization | `LangInit`/`UserInit`, `initPending`/`initDone` | `LangInitMSIR`/`UserInitMSIR`, `msirLangDone`/`msirInitDone` | Converge at D25 step 6, when the CG routines go |
    | Unit driver | `CG.Begin_unit`/`End_unit` from `Module.Compile` | `MSIREmit.BeginUnit`/`EndUnit`, plus hooks inside CG emission (review 2d) | Converge with O17 (A) |
    | Globals and typecells | `Module` global segments, `Type.compile` | `DeclareGlobalsMSIR`, `GenCellsMSIR` | Converge with O17 (C) |
    | Node methods | `compile`, `compileLV`, `prep*` | `compileMSIR`, `compileLValueMSIR`, `bitLocMSIR` | Aligned already; renamed at D25 step 7 |
  - **D29a. One Marker stack for both walks** (approved as proposed, Tony,
    2026-11-23: one `Marker`, frames with a payload per walk, both payloads
    on one push procedure; goes with O17 (A)).
  - **Today.**  `Marker` is a stack of control frames (procedure, loop,
    TRY, TRY ELSE, TRY FINALLY, LOCK, RAISES).  It is pushed twice over:
    - by type checking, with no labels, for `ExitOK`, `ReturnOK` and
      `ReturnVar` (e.g. `LoopStmt.Check`, `Procedure.Check`);
    - by the C walk, with CG labels, the setjmp frame variables and the
      result temp, for `EmitExit` and `EmitReturn` (which run the
      intervening FINALLY and LOCK code).

    The MSIR walk pushes neither.  It keeps its own equivalents in
    MSIRBuilder: an exit stack and a cleanup stack (`PushExitBlock`,
    `PushElseFrame`, `CleanupKind`), with `EmitExitMSIR` and
    `EmitReturnMSIR`, and the EH scopes of D26 for TRY/FINALLY and LOCK.
    Where an MSIR statement needs a fact that `Marker` holds, it records it
    some other way: `ReturnStmt` notes its procedure's result type during
    checking (B21), because `Marker.ReturnVar` is valid only in the C walk.
  - **Decision.**  Both walks push the same `Marker` frames, through the
    same procedures, and `Marker` alone answers "where does EXIT or RETURN
    go from here".
    - A frame has a part common to both walks (kind, result type and
      variable, exception set) and a part for each walk: CG labels and
      variables for the C walk, and for MSIR the exit block, the ELSE
      target and the EH scope.
    - The push procedures take both payloads, with defaults, so that call
      sites look the same in both walks:
      `Marker.PushExit (l_stop := CG.No_label;  msirExit: MSIR.Block := NIL)`.
      The C walk passes labels as today; the MSIR walk passes blocks.  Step
      6 of D25 then drops the CG parameters without touching call sites'
      shape.  (Not chosen: parallel names such as `PushExitMSIR`, which
      would keep two vocabularies alive.)
    - `EmitExit` and `EmitReturn` get MSIR counterparts in `Marker`
      (`EmitExitMSIR`, `EmitReturnMSIR`) that walk the same frames and
      call MSIRBuilder for the IR: an ordinary branch within a scope,
      `unwind_to` or `ret_through_envelope` out of one (D26).  MSIRBuilder
      keeps only IR construction (D23): its exit and cleanup stacks move
      into `Marker`.
    - `Marker.ReturnVar` serves both walks; `ReturnStmt.result` goes.
  - **Nested procedures.**  MSIR compiles a nested procedure's body inline,
    in the middle of its parent's.  It pushes a procedure frame first, and
    `ExitOK`, `ReturnOK` and the emitters already stop at a procedure frame,
    so the parent's frames are not visible to the nested body.  The
    builder's per-procedure context (current block, abandon state) is
    unaffected.  Until (A) gives MSIR its own walk, a nested body can also
    be compiled from `LangInit` in the middle of the C walk's parent body;
    the same procedure-frame boundary separates the two, provided pushes
    and pops balance.
  - **Order of work.**  Each step is a refactor, checked by MSIR output being
    byte-identical before and after (libm3, m3quake, m3front), then the
    ladders:
    1. procedure frames: `PushProcedure` in the MSIR walk, `ReturnVar` for
       RETURN.  **Done**: `GenBodyMSIR` and `GenBody`'s MSIR half push
       the frame around the body; libm3's MSIR is byte-identical;
    2. loops and EXIT: `PushExit` with an exit block, `EmitExitMSIR`.
       **Done**, together with TRY ELSE's frame: an EXIT or RETURN in a
       TRY ELSE's body goes to the ELSE code, so the loop and ELSE frames
       have to share one stack, and moving only the loops would have split
       it.  Loops push `Marker.PushExit (msirExit := ...)`, TRY ELSE
       `Marker.PushTryElse (msirElse := ...)`; `Marker.EmitExitMSIR` and
       `EmitReturnMSIR` walk the frames; MSIRBuilder's exit and cleanup
       stacks are gone, leaving it `EmitGoto` and `EmitRet`.  MSIR output
       of libm3, m3quake and m3front is byte-identical;
    3. TRY EXCEPT, TRY FINALLY, LOCK: their frames carry the EH scope.  The
       MSIR emitters already skip them (the EH scopes run FINALLY and LOCK
       code), so this step is for `Marker` to describe the MSIR walk's
       nesting fully, not for routing.  **Done** with O17 (A).

**Progress (2026-11-22, evening): PickleStubs' fallback; a LOOPHOLE bug on master.**
- PickleStubs' fallback (below) was `LOOPHOLE (x: REAL, Swap.Int32)`, with
  `Swap.Int32 = BITS 32 FOR [-2^31 .. 2^31-1]`: MSIR widens the subrange
  to i64 before converting it to f32.  A float/integer convert of
  different widths is defined as in MSIRToLLVM (the integer truncated or
  zero-extended to the float's width, then a bitcast), and MSIRToM3CG now
  lowers it so (`cccd2b94a9`).  A test of the same pattern lowers and
  runs correctly; a full ladder rerun, to confirm no unit of the base
  packages falls back, is still to do.
- Master's C backend gets the same LOOPHOLE wrong: the subrange's stack
  type is Int64, so `LoopholeExpr.Compile` asked for `Loophole (Reel,
  Int64)` and M3C emitted a value conversion (`LOOPHOLE (1.5, Swap.Int32)`
  was 1).  PickleStubs.SwapReal is broken on master as a result.  Fixed
  for CG in PR #1300 (through the memory type Int32/Word32), with m3test
  p308; both MSIR lowerings already get p308 right.
- Master also lacks the ex_stack terminate backstop (see the correction
  below); being ported in its own PR.

**Progress (2026-11-25): master merged; packed-array layout; no fallbacks.**
- Master merged (`0fab3bcad6`, then #1378), with `InitTypecell`'s notes
  made outside its C payload (D25 step 6 rehearsal: full parity, above).
- p315 found three bugs in small packed arrays whose elements may cross a
  byte (element size not a power of two):
  - *Stores of 3, 5, 6 and 7-bit elements* through a run-time subscript
    now go through the array's container (`InsertBitFieldDyn`,
    `536b5e4d57`).
  - *Record layout* (`MSIRType.TranslateRecord`): a record's struct left
    padding to LLVM, which aligns a field it sees as bytes ([N x i8], a
    packed array) to 1 where M3 aligns it to its size.  A field after one
    sat too low and the record was short, so the last field ran past its
    alloca; at -O3 LLVM read it as 0 (p315's "direct local 7").  The
    struct now has explicit padding before each field and at the end
    (`03003c26d7`); record constants fill it with zero, debug info omits it.
  - *Alignment*: MSIR record and array types carry M3's alignment
    (`MSIR.TypeAlign`), which MSIRToLLVM states on an alloca, never below
    LLVM's, and MSIRToM3CG uses for its aggregate temporaries
    (`72ec4b0f82`).
  The C path had the same element bug on master (#1378: a run-time
  subscript into such an array goes through its bytes).  p134 had been
  changed in 2008 to expect the C back ends' wrong 54; it expects 46 again.
- LLVM ladder (run 45596624373, `72ec4b0f82`): m3tests 316/316 on all
  three targets, smoke, IR fixpoint 1420 files.
- MSIR -> M3CG ladder (ARM64_DARWIN, `b73458199d`): **no fallback** in any
  stage (PickleStubs now lowers, closing the bitcast item); generated C
  identical across three stages, 1415 files; smoke; m3tests with every
  test lowered too 316/316.  Built by the C backend directly, 7 tests fail
  (p109 p126 p137 p201 p268 p270 p280), which master's C build also fails
  on ARM64_DARWIN.
- Next: the same `.m3x`/`.msir` parity check over the std packages; the
  MSIR -> M3CG ladder on an `ex_frame` target; then D25 step 6.

**Progress (2026-11-26): std-package parity; two step 6 blockers; eleven
MSIR bugs.**  Each of the 155 std packages that a C build ships (beyond the
13 base packages) was built three ways from `72ec4b0f82` on ARM64_DARWIN:
by the C backend; in MSIRObj mode; and in MSIRObj mode with the C-only
steps skipped (the D25 step 6 rehearsal: `DeclareGlobalData` and the body's
declaration, `EmitBody`'s initialization, `Coverage.GenerateTables`,
`Variable.GenGlobalMap`, and the C payloads of `InitTypecell`/
`GenCellPtrs`; throwaway switches, not committed).  `GenLinkerInfo` is not
skipped: it holds `Type.GenCells`, which makes both payloads.  Dependencies
came from an install with an MSIR-built runtime and C-built libraries, so
programs fail to link (`_m3_abi_cg_v1`); the units still compile and write
their `.msir` and `.m3x`.
- *Rehearsal.*  139 packages compile.  Their `.msir` is byte-identical with
  and without the C-only steps.  Their `.m3x` is identical except in two,
  each losing imports that only the C half notes:
  - **m3-scheme/mscheme** (12 records): with `Scope.InitValues` skipped,
    four modules lose their import of `Atom` (`B`, `Atom.FromText`'s `i`,
    a `t`).  C's compile of a global's initializer makes the import; the
    MSIR half calls `Atom.FromText` without noting it.
  - **m3-www/http** (2 records): with `InitTypecell`'s C payload skipped,
    interface `HTTP` loses `RTHooks.CheckLoadTracedRef` and a `t`.
  Both must be noted by the MSIR half before step 6 drops the C half (as
  `NoteUsedMSIR` was for method defaults).
- *C against MSIR `.m3x`* (packages whose MSIR build compiles): 58
  identical, 54 programs (no `.m3x`), 9 differing only by extra MSIR
  imports, 18 otherwise.  The extra imports include version stamps (`i`),
  e.g. rdwr's `RdClass.CloseDefault`: inherited method defaults in MSIR's
  method suites.  (B)'s rule allows extra `t` only, so this needs a
  decision.  In sx, mst, modula3scheme, mscheme, unittest-numeric,
  sstubgen, http, zeus and vbtkit MSIR is *missing* imports C makes, the
  pattern of B21-B30; still to examine, as are stable's and sstubgen's
  `C`/`D`/`o`/`p` differences.  opengl differs only in the `WINAPI`
  fingerprints (`E`/`T`), as decided.  (`m3xcanon.py` now resolves the name
  in `o`/`p` records; unresolved, any extra name made false differences.)
- *MSIR compile failures*, B33-B43 below: a crash (sgml), three
  unsupported constructs (nested procedure result as an lvalue, in
  simplegrid and arithmetic; a capture missing from the outer map, magic;
  an unsupported statement, deepcopy), three verifier errors (udp, cm3ide,
  webvbt) and four invalid LLVM IR (cit_util, boxes, m3cgcat, m3tk).  The
  remaining failures are the mixed install's (the scheme and anim3D
  packages' `bad version stamps` against installed libraries; anim3D's
  through the `WINAPI` fingerprints).
- Next: the two step 6 blockers; the sgml crash; B34-B43; the missing
  imports.

**Progress (2026-11-22, later): D25 step 4 on the whole compiler; five MSIR bugs.**
- MSIR -> M3CG ladder (ARM64_DARWIN, `ex_stack`), from a C-built cm3 at
  msir HEAD.  Stage 1 builds the 13 base packages (m3core, libm3, set,
  sysutils, tcp and the compiler) with `@M3m3front-msir @M3msircg`:
  1376 units lowered, 1 falling back (libm3's PickleStubs: a float/integer
  bitcast of different widths).  The resulting cm3, whose own code is all
  lowered, does the same (stage 2), and so does its output (stage 3).
  **Fixpoint: the generated C is identical across the three stages, 1414
  files.**  m3tests under the stage-2 cm3 (C backend): 299 tests, 12
  errors (p047 p109 p126 p137 p201 p227 p268 p269 p270 p277 p280), all of
  which master's own C build also fails on ARM64_DARWIN.  Master fails 16:
  r001 r003 r004 r005 as well, because an unhandled exception under
  ex_stack reaches std::terminate ("terminating due to uncaught exception
  of type _M3Exc"); the terminate backstop that reports it (f096d61405)
  is on msir only.  (Corrected 2026-11-22: this entry first said the 12
  were exactly master's failures.)  The smoke
  test passes built lowered; built with CG against the lowered libm3 it
  fails (IO.Error raised by lowered code is not caught by the CG unit), a
  lowered/CG boundary like the closure one below, which per Tony
  (2026-11-21) is not to be bridged.
- Bugs found on the way (each its own commit):
  - *Packed reads wider than four bytes* (`MSIRBuilder.ExtractBitField`):
    a field with bitInByte + bitWidth > 32 was read through an i32, losing
    its high bits (RTHeapRep.Desc.link: 2^31 read back as 0).  Both
    lowerings.
  - *Imported procedures' READONLY scalars* (`LookupOrCreateProc`, closure
    shims) were declared by value, though definitions and calls pass a
    pointer.  MSIR -> LLVM did not notice; MSIR -> M3CG's C prototype
    did (M3File's call of FS.SetModificationTime).
  - *Floating-point `#`* was `ONe`, FALSE for a NaN; it is `UNe`, as
    `NOT (a = b)` and the C backend have it.  Both lowerings.
  - *Enumerations stored at 8 bytes* (`Type.ScalarStorageMSIR` did not
    list the Enum kind): a default like FIRST(M3CG.ZType) = 4 was stored
    as an i64 into a 1-byte field.  M3CG_MultiPass.load_t's initializer
    thus wrote 7 bytes past the object; at a page boundary that cleared
    the next free page's descriptor.  This is why an MSIR -> LLVM-built
    cm3 corrupted its heap as soon as it ran the M3CG code (the lowering,
    or the C backend with `@M3nogc`), which the LLVM ladder never does:
    its m3tests run in MSIRObj mode.  Both lowerings.
  - *Fallbacks removed* (`MSIRToM3CG`): Formal's constant-dope globals
    get data-segment space; REAL <-> 32-bit bitcasts go through Int32.
    Fallback was not only a coverage gap: a lowered unit calls a closure
    with its environment first (MSIR's shims), a CG nested procedure takes
    its static link last, so Utils.WriteFile (lowered) calling
    M3Build.InstallFile.Emit (CG) crashed the stage-2 cm3.
- LLVM ladder with these fixes (`074567661a`): m3tests 298/299 under the
  MSIR-built cm3, smoke, IR fixpoint 1419 files; that cm3 now lowers all
  291 m3core units through MSIR -> M3CG, with and without `@M3nogc`.  The
  one error, p006, is a runtime deadlock, not a code-generation fault:
  the first collection builds RTHeapMap's global map (`NEW` of an
  untraced array, so `calloc`) while the other threads are stopped, and
  the main thread was stopped inside `calloc` (Thread.Join -> InitMutex)
  holding malloc's lock.  Shared by every backend: in 200 runs each, the
  MSIR-built p006 hung 0 times and master's C-backend p006 twice.
  Belongs with #1287.
- Next (D25 step 4, remaining): the same ladder on an `ex_frame` target
  (magpie); PickleStubs' bitcast (done 2026-11-25); then step 5
  (decoupling, O17).

**Progress (2026-11-22): MSIR to M3CG, ex_frame EH and memcpy/memset.**
- On a target without ex_stack, MSIRToM3CG lowers D26 scopes to CG's
  ex_frame scheme (TryStmt.Compile2, TryFinStmt.Compile3).  Each protected
  scope has an EF1 frame and a jmp_buf allocated at the procedure's entry
  (`alloca(m3_jmpbuf_size)`).  Entering it sets the frame's class (Except,
  ExceptElse, Finally), for Except the address of its NIL-terminated uid
  list in the constant segment, links it with RTHooks.PushEFrame and calls
  setjmp, whose nonzero return goes to the scope's pad.  There the
  activation is the frame's info field (the runtime has already unlinked
  the frame); dispatch, the Finally selector and end_finally are as for
  ex_stack.  Leaving protected scopes normally makes one RTHooks.PopEFrame
  call, restoring the handler stack past the outermost frame left.  Calls
  are plain calls.  m3front records the frame hooks in the MSIR module
  (`MSIR.SetModuleFrameHooks`) on such targets.
- RAISES checks (added later the same day): m3front records an enforced
  RAISES clause on the MSIR procedure (D10, amended).  Under ex_frame the
  lowering pushes a RaisesNone frame, or a Raises frame pointing at the
  clause's NIL-terminated uid list, at the procedure's entry and pops it
  before every return; a RETURN out of protected scopes needs only that
  pop.  The runtime reports an exception that reaches the frame and is not
  in the list ("not in RAISES list").  MSIREmit registers the frame hooks
  for a module with checks even if it has no TRY.  Under ex_stack nothing
  is emitted, as for CG.  Note that CG under ex_stack does not enforce
  RAISES clauses at all: Marker.SaveFrame records the scope, but the scope
  table (`Marker.EmitScopeTable`) is no longer emitted, so a blocked
  exception propagates and is reported as unhandled, or is caught by an
  outer handler.  That is a gap on master, not in MSIR.
- memcpy/memset: MSIRBuilder's calls to the C library (aggregate copies;
  zeroing, always fill 0 and a constant size) are lowered to M3CG
  `copy_n` and `zero`, as the CG path does, instead of being declared as
  procedures, whose C prototypes gcc reports as clashing with its
  built-ins (22 m3tests failed on Linux for those warnings alone).
  (M3C deliberately does not implement `zero_n`.)
- Results on AMD64_LINUX (ex_frame, magpie): m3core lowers completely, 291
  of 291 units, and p304, a runtime test and TextConv run unchanged against
  it; m3tests with every unit lowered differ from the baseline only in the
  five doubled-warning tests.  ARM64_DARWIN (ex_stack): unchanged.

**Progress (2026-11-21): D19 set intrinsics for multi-word sets.**
- m3front emits the set intrinsics for sets wider than a word (still typed
  iN): `set_union`, `set_intersect`, `set_difference` and new
  `set_sym_difference` for `+ * - /`; `set_member` for IN (the element's
  i64 bit index, biased by FIRST, guarded when the element's range exceeds
  the set's); new `set_singleton` and `set_range` for constructors, constant
  and runtime; subsets as `set_union`/`set_intersect` then `icmp eq`.
  Single-word sets keep integer operations.  `MSIRBuilder.IsWideSet` and
  `SetIndex` decide and convert from the MSIR types alone.
- MSIRToLLVM expands each intrinsic to the `iN` code it emitted before
  (`set_range` selects 0 when lo > hi, so its shifts may be poison).
- MSIRToM3CG treats a multi-word set as an aggregate of N/64 words, word
  aligned as CG lays sets out, and maps the intrinsics onto M3CG's
  `set_union`/`set_intersection`/`set_difference`/`set_sym_difference`/
  `set_member`/`set_singleton`/`set_range`, `icmp eq/ne` onto
  `set_compare`, and an iN constant (0 or -1) onto a filled temporary.
  m3core: 260 units lower; ASCII and TextConv no longer fall back for
  sets (ASCII's module still does for its exported variables).
  m3tests, lowered: the five warning-duplication differences only.  p136,
  newly lowered, exposed `Word.Rotate` lowered through `rotate_left`,
  whose word-typed count turned a negative constant into a huge one that
  M3C's C rejects; rotations now use `rotate` (signed count), negated
  for right rotations.
- Found on the way, on master's C backend (fixed separately): for
  multi-word sets M3C's `set_compare` passed `<=`/`<`/`>=`/`>` operands
  in reverse, read memcmp the wrong way round for `=`/`#`, and did not
  declare `SET` when a comparison was a unit's only set operation.

**Progress (2026-11-21): MSIR to M3CG, small gaps.**
- TYPECASE: a `RTHooks.TypecaseCell` table per `typecase` in the data
  segment (uids; defn filled lazily by `ScanTypecase`), the hook call, and a
  compare chain to the clause blocks, ELSE last.
- `fpfloor`/`fpceil`/`fpround` (float to float): `cvt_int` with the matching
  rounding then `cvt_float`, as the CG path computes real MOD.
- Large globals (size > `Max_zero_global`, MSIR's standalone backing
  globals): their own zeroed `declare_global`, the module slot initialised
  to its address, as `Variable.Declare` does for CG (`t.bss_var`).
- Integers of odd width below a word (the `i11`/`i24` of packed fields,
  kind `IWide` in MSIR): words on the stack, LLVM's allocation size in a
  struct, never loaded or stored directly.
- Results: m3core 259 of 290 units lower; besides external globals only
  ASCII and TextConv (multi-word sets, next: D19 set intrinsics) fall back.
  m3tests with every unit lowered where possible: the same differences
  as before (p019, p020, p023, p104, p272: warnings printed twice).  These
  come from running the CG and MSIR passes side by side, each reporting
  the unit's warnings; they go with D25's single lowering.  Not worth
  suppressing meanwhile: several goldens (p104, p268, p270, p272, p273,
  p304) legitimately repeat a warning at one position, so deduplicating
  in `Error` would change them.
  *Corrected:* this note first said those five now passed; that run had
  used the installed compiler, not the lowering (`run.sh` resolved its
  wrapper relative to the directory it was started from).
- **Divergence found, not changed here:** the report breaks `ROUND` ties by
  `FloatMode.RoundDefault`, NearestElseEven.  MSIR's `fpround` (roundeven)
  does; M3C's `m3_round` used C's `round` (halves away from zero), and the
  lowering inherits it through `cvt_int(Round)`.  The gcc backend did the
  same with `lround`.  Both fixed separately (#1281).

**Progress (2026-11-21): D26 step 4, EH scopes lowered to M3CG (ex_stack).**
- `MSIRToM3CG` lowers scopes to CG's `ex_stack` scheme, as
  `TryStmt.Compile1` and `TryFinStmt.Compile1` emit it.  A protected scope's
  blocks are placed together between `start_try` and `end_try`, entered
  only by falling in (branches to its entry go to a label before
  `start_try`; C++ forbids a jump into a try block); its calls are
  `invoke`s naming its landing pad, which follows `end_try` with the
  dispatch.  Handler and FINALLY code are placed with the TRY's parent
  scope, so outside the try block.  Leaving a `Finally` scope stores a
  selector and runs the FINALLY code; `end_finally` continues the outcome
  (re-raise through `ResumeRaiseEx`, an exit, or the return, whose scalar
  value waits in one slot per procedure).  The outcomes each `end_finally`
  continues are found before emission, inner FINALLYs first.  `ex_frame`
  targets still fall back for any unit with scopes.
- In combined mode `MSIRToLLVM` ran first and rewrote the module's scopes in
  place (`MSIREH`), so the lowering saw `invoke`/`landingpad`.  When the
  driver lowers (`@M3msircg`) it now calls `MSIREmit.SetDriverLowers`, and
  `EndUnit` writes no `.ll` for a verified module.
- Results: m3core 246 of 290 units lower, none falling back for EH (most of
  the rest for external globals).  m3tests with every unit lowered where
  possible: the same five differences as `@M3m3front-msir` alone; p268 and
  p270 now pass where the baseline fails.  New m3test p304.  A link
  procedure's name with a negative uid (`tc_obj_-1621784506_linkproc`) is
  now a valid C name.
- **Bug found on master's CG path, not changed:** under `ex_stack`, a TRY
  with a named handler and an ELSE crashes when its body does RETURN or
  EXIT; `TryStmt.Compile1` sends the RETURN to the handler tests, which
  read the return marker as an exception.  The MSIR path is correct.

**Progress (2026-11-21): D25 step 2 spike, full lowering with per-module
fallback** (Tony chose full lowering over a hybrid that kept CG's data).
- `MSIRToM3CG` (msir package) lowers a whole unit to M3CG for the C
  backend, using CG's data conventions so lowered and CG-compiled units link:
  one data segment (ModuleInfo, user globals at the front end's offsets,
  ImportInfo records, typecells, type links, revelations, exception
  descriptors) and one constant segment (proc names and ProcInfo, file name,
  GC maps, brands, names, method suites, TEXT literals, constant arrays).
  `Lowerable` falls back to the front end's own stream for anything not yet
  lowered.  Driver: `@M3m3front-msir @M3msircg`; `@M3m3cgtext` dumps the
  stream M3C sees; `@M3msircensus` reports per-procedure blockers.
- m3core: 240 of 291 units lower; a threaded program linked against that
  m3core runs.  m3tests with every test unit lowered where possible: no
  failure caused by the lowering (the six that differ from the baseline
  differ under `@M3m3front-msir` alone: warnings reported twice, verifier
  messages).  Remaining fallbacks: TRY/LOCK (`invoke`/`landingpad`, step 3;
  lowered since, see above), external globals, multi-word sets (`IWide`), TYPECASE,
  FLOOR/CEILING/ROUND, indirect (large) globals.
- **Answer to the spike's question:** M3C needs *no* type declarations from
  the lowering.  Locals take typeid 0; CG passes by-value records as
  addresses (the callee copies) and large results through a hidden
  `_result` address, so `pop_struct`'s typeid is never needed.
- **CG compatibility is not a goal** (Tony, 2026-11-21).  Lowered units
  follow CG's conventions only so per-module fallback can mix them with
  CG-compiled units during the spike; nothing is added to MSIR for it.  No
  hand-written C in the tree reads unit data by CG's layout: RTBuiltin.c
  and RTLinkerC.c use only RT0 records, which every backend must match.
  Consequences: a unit referencing an external global falls back, since
  MSIR does not distinguish a C variable from an exported interface
  variable (an alias symbol in MSIRToLLVM's output, interface data in
  CG's).
- **Front-end completeness fix:** a structured constant declared in a unit
  has a global slot holding its address, which importing units read; the
  MSIR path now emits it (`Constant.DeclareGlobalMSIR`, a global
  initialiser to the constant's MSIR image).  `MSIR.ModuleSetIncomplete`
  marks a module whose data the front end could not fully describe; the
  M3CG lowering then declines it.
- **Struct layout:** MSIR struct types have natural (LLVM) layout;
  `Field.offset` is debug information, set only for structs translated
  from M3 records.  The lowering lays structs out naturally.
- **Gaps found, not changed:** MSIR carries no linker names (both backends
  reconstruct them, `CSymbol` copies `LLSymbol`'s rules); narrow ordinals
  such as `[0..255]` translate to signed `i8` where CG declares `Word8`,
  which matters at separately compiled procedure boundaries.
- M3C bug found and fixed (also on master, with m3test p302):
  `address + offset` was rendered unparenthesised, so a comparison macro's
  cast split it into a sum of two pointers; `p = ADR (z)` for a module global
  did not compile even through CG.

**Progress (2026-11-20).**
- **D24 done** (`db7908eb1d`).  `MSIRToLLVM` checks once per run that its
  Typecell, ObjectTypecell, ArrayTypecell, ModuleInfo and ExceptionDesc
  field tables pad to `M3RT`'s offsets and sizes.  `OTC_defaultMethods`,
  the RefHeader bits, the ModuleInfo size and the open-array dope offsets
  come from `M3RT`; RefType's CG and MSIR typecells share one dope-size
  computation; M3C's ABI guard uses `Target.Has_stack_walker`.
- **D22, default initialization done** (`5f1a89dc08`).  `TypeRep.T` has
  `initValueMSIR (base, bitOff, zeroed)` beside `initValue`, overridden by
  Subrange, Named, Packed, Array, OpenArray and Record and dispatched by
  `Type.InitValueMSIR`; `RecordType.InitFieldsMSIR` serves records and
  objects.  Fixes B3 and B4; new m3test p299 (also on master; numbered p296 until #1258).
- **D22, `storeMSIR` / `checkUseFailureMSIR` done** (`1aa16bfdb5`).
  ExprRep methods replace the four-module sub-byte store probe in
  AssignStmt/INC/DEC and the `EmitUseFailureMSIR` exports; the
  SubscriptExpr, NamedExpr and VarExpr interfaces are back to master's.
- **D22, method calls done** (`24f1f7ca59`).
  `QualifyExpr.CompileMethodCallMSIR` owns receiver, actuals and
  dispatch; the four QualifyExpr accessors are gone.  Fixes B2; new
  m3test p301.
- **D22 done: constant images.**  The constant-array lowering
  (constructor elements, the '..' repeat-last rule, sub-byte packing, the
  open-element workarounds) moved from MSIRBuilder into ArrayExpr:
  `genLiteralMSIR` builds an inline ConstAggArray (`ConstAggMSIR`) and
  `ArrayExpr.ConstArrayMSIR` a named CONST's global, with its per-unit
  cache (now unbounded; it stopped caching after 64).  MSIRBuilder keeps
  only `ConstArrayGlobal(eltT, elts)`, and `EltCount`/`Elt` are gone.
  `RecordExpr.TryConstFieldMSIR` became `Expr.ConstLiteralMSIR` (fold,
  then `genLiteralMSIR`); `TryCompileConstMSIR` is private to RecordExpr.
  MSIRBuilder no longer imports Expr, ArrayExpr, ArrayType,
  OpenArrayType or RecordExpr.
- **D23 not started.**
- **2b, cell ownership done; B1 fixed** (`cd64dbadc6`).  As in CG, a
  module emits typecells only for REF/OBJECT types whose cells no other
  unit owns: `Type.IsForeignCell` (predefined, or an imported unit's full
  cell) sends NEW/RAISE through `RefType.TypecellMSIR` to a TypeLink, and
  foreign supertypes, re-declared types and inherited revelations are no
  longer copied.  The owning cell carries its brand (`Type.OwnsCell`).
  Branding first crashed the stage-2 cm3: an unbranded local copy
  registered first made the owner's branded cell a duplicate
  (DuplicateBrand); then a re-declared inherited revelation reached
  RTLinker before its cell (MissingType).  New m3test p300.
- **2b done.**  `Typecell.T` (`m3front/src/types/Typecell.i3`) describes
  the data parts of a REF or OBJECT typecell: uid, fingerprint, traced,
  kind, data size and alignment, the three byte programs (type map, GC
  map, pickle description), brand, name, the open-array and object
  extensions.  `RefType.Describe` and `ObjectType.Describe` build it; the
  CG `InitTypecell`s and the MSIR `InitTypecellMSIR`s both write it out.
  The code parts (init proc, link proc, default methods) stay per path.
  `TipeMap.Emit`/`TipeDesc.Emit` let CG emit the finished bytes, so the
  CG output does not change (libm3's C is byte-identical).  The REF
  descriptor moved from MSIRBuilder to `RefType.TypeDescMSIR`, taking
  `RefMapBytes`, `RefPickleDescBytes`, the `SetRefInitProcGen` callback
  (2c) and the dead `ObjectTypeCellRef`/`ArrayTypeCellRef` with it;
  `TypecellMSIR` no longer takes sizes from its NEW/RAISE callers.
  `MSIR.TypeDesc` loses `dataOffset` and `methodOffset`, which
  `MSIRToLLVM` never wrote (it emits 0 and RTLinker fills both in).
  Changes on the MSIR side: a REF cell's size and alignment come from
  the REF type as in CG, not from each caller's view of the stripped
  referent (they differed only for `REF BITS n FOR T`); REF cells now
  carry `TC_name`.
- **Found on the way:** MSIR has block parameters (D20) but `MSIRToLLVM`
  never lowers them to phis, and nothing in the front end uses them; loops
  use alloca counters, which the lowering hoists to the entry block.

Scope: `git diff d2108205fd msir` (master merged in) over m3front, m3middle,
m3-sys/msir and the cm3 driver.  Four independent read-only reviews, with the
most serious findings spot-checked against the code.  Nothing built.

#### Summary

- **Holding:** the `*Rep` interfaces are still imported only within their own
  subsystems.  Every expression and statement kind adds `compileMSIR` /
  `capture` methods to ExprRep/StmtRep and dispatches through Expr/Stmt, which
  is the intended pattern.  The `m3-sys/msir` package imports nothing from
  m3front, keeps every IR handle opaque and is lowered only through its own
  API.
- **Not holding:** where MSIR needs per-type or per-node behaviour that CG
  gets from a TypeRep/ExprRep method, it was mostly added as exported
  procedures, cross-module probes, or code in MSIRBuilder that inspects node
  and type shapes.  Several of those copies already disagree with the CG
  path.  The MSIR layer and the front end now import each other, and MSIR
  piggybacks on the CG pass's state and ordering (see O17).

#### 1. Bugs (no judgement call needed)

| # | Where | Problem |
|---|-------|---------|
| B1 | `MSIRToLLVM.m3:3608` | **Fixed** (`cd64dbadc6`). `TC_brand_ptr` always `null`: BRANDED types lose their brand at runtime. |
| B2 | `UserProc.m3` `LValueMSIR` (~582) | **Fixed** (`24f1f7ca59`). Method-call args compiled with `Expr.CompileMSIR`, not `Formal.EmitArgMSIR`: VAR/READONLY/open-array modes ignored. |
| B3 | `ObjectType.m3:805` `GenInitProcMSIR` | **Fixed** (`5f1a89dc08`). Skips fields with no explicit default but `InitCost > 0` (CG calls `Type.InitValue`), and skips packed fields (`:931`). |
| B4 | `RecordType.m3:584` | **Fixed** (`5f1a89dc08`). `GenInitMSIR` silently skips non-byte-aligned fields. |
| B5 | `MSIRBuilder.m3` maps | **Fixed.** Fixed-size tables silently dropped entries when full: a module with more than 256 globals built a program that failed at run time. The module-wide maps (`procMap`, `captureMap`, `globalMap`, `shimMap`) now grow; the per-procedure `varMap` and the exit and cleanup stacks abandon on overflow. `captureMap` was also never reset between modules, so in one compiler run it filled across a package; `BeginModule` now resets it. |
| B6 | `WithStmt.m3` `CompileMSIR` | **Fixed.** Paths that could not bind the alias (no value, no lvalue, untranslatable type, no local slot) returned without `Abandon`, silently omitting the WITH body. Each now abandons the procedure. |
| B7 | `Tracer.m3`, `Variable.m3`, `BlockStmt.m3` | **Fixed.** `<*TRACE*>` was ignored under MSIR: no trace node overrode `msir_apply`, the MSIR variable and formal initializers never scheduled a trace, and module and block traces were never pushed. Variable and block trace nodes now compile their call under MSIR; `LangInitMSIR`/`UserInitMSIR` schedule traces where `LangInit`/`UserInit` do; `InitValuesMSIR`, `GenBodyMSIR`, the module body and `BlockStmt.CompileMSIR` emit, push and pop them at the C path's points. |
| B8 | `QualifyExpr` `LValueMSIR` + `SubByteStoreMSIR` | **Fixed.** Base designator evaluated twice for sub-byte stores (three times under INC): `a[f()].bits := x` called `f` twice (confirmed by test, also for packed array elements and bit-field WITH). Designators with no byte address now have one visitor, `bitLocMSIR` (replacing `storeMSIR` and the exported `QualifyExpr.BitFieldBaseMSIR`), which evaluates the base once and returns an `MSIRBitLoc.T`; assignment, INC/DEC, reads and WITH all go through `MSIRBuilder.ExtractBitLoc`/`InsertBitLoc`. `LValueMSIR` now decides "no byte address" before evaluating anything. Nested packed records (`a[i].f.b`, `f: BITS 13 FOR RECORD`) and WITH over packed elements work as a result. |
| B9 | m3core `runtime/common/m3makefile` | **Fixed.** The ABI guard keyed on `"MSIRObj"` only, so an MSIRAsm-built m3core defined the CG marker while its objects referenced the MSIR one, and could not link. It now keys on both MSIR modes, matching `Target.BackendMSIRSet`. |
| B10 | `Variable.m3` `StoreInitMSIR` / `ForceInitMSIR` | **Fixed.** Initializer stores widened a narrower value with ZExt whatever its sign. Not observed in practice (initializer values arrive at machine width), but wrong for a signed value. Now `CoerceToMSIR` (SExt for signed kinds); the duplicate in `ForceInitMSIR` went with that procedure, which had no callers. |
| B11 | `Variable.AddLocalMSIR` | **Fixed.** MSIR set `t.indirect := FALSE` on a WITH alias it bound by value; the C walk reads that field (`Split`, `LoadLValue`) and compiles nested procedures after the MSIR walk. MSIR no longer writes it: the varMap entry records the by-value binding. Rule (adjudicated): the MSIR walk reads front-end `Value`/`Variable` state but never changes it. |
| B12 | `Procedure.m3` | **Fixed.** Three copies of the capture computation, and GenBody's retry omitted `CaptureOwnScopeInits`. One `NestedCaptures(p)` now serves `PreRegisterNestedCaptures`, `GenBodyMSIR` and the retry. |
| B13 | `Procedure.FilterOwnScope` | **Fixed.** Only p's own scope and its outermost block were filtered from p's captures, so a local of an inner block (or a FOR or WITH variable) used by a procedure declared there became a spurious capture parameter of p, and calls to p were miscompiled. The filter now keeps only variables whose scope encloses p (`Scope.Encloses`). |
| B14 | `Inc.m3`/`Dec.m3` `CompileMSIR`, `AssignStmt` sub-byte path | **Fixed.** No range check on INC/DEC (any designator) or on a sub-byte assignment, where the CG path checks the new value against the designator's bounds. INC and DEC now share `Dec.UpdateMSIR`, which computes at INTEGER/LONGINT width (widening by the designator's sign, since a `[0..255]` field's storage type can be a signed i8) and calls `CheckExpr.EmitBoundsMSIR` exactly where CG emits `Check_lo`/`Check_hi`. |
| B15 | `Procedure.PreRegisterScopeCapturesMSIR` | **Fixed.** Capture pre-registration covered only the procedures declared directly in a scope, not those in its inner blocks, so a procedure P calling a block-local Q missed Q's captures: P got an outer variable that Q writes by value. Pre-registration now covers every procedure declared within the scope at any depth (`Scope.Within`). |
| B16 | `QualifyExpr` / `MethodExpr` | **Fixed.** A method used as a procedure value (`T.m`, a qualify expression, or its folded `MethodExpr`) was unsupported, so the procedure abandoned. `MethodExpr.LoadMSIR` now loads T's binding from its typecell's defaultMethods table, as `Compile` does, including an opaque holder's run-time method offset; `MethodExpr.compileMSIR`, `QualifyExpr.CompileMSIR` and the static-call path share it. |
| B17 | `MSIRBuilder.Abandon` / `MSIRToM3CG.Lowerable` | **Fixed.** An abandoned procedure is emitted as an `unreachable` stub, and `@M3msircg` still lowered the unit, so the program failed at run time where the C code was correct, and the ladder counted the unit as lowered. Abandon now marks the module incomplete, so the unit falls back to C. |
| B18 | `CaseStmt.CompileMSIR` | **Fixed.** MSIR flattened and collapsed `p.tree` in place; in the parallel mode the C walk could then compile a nested procedure's CASE from the rewritten tree. MSIR now works on its own sorted, collapsed copy (`SortedCopy`). |
| B19 | write-barrier container | **Fixed** (with review 2c). The global pending container leaked between designators: `SUBARRAY(pq.heap^, 1, n) := e` (libm3 `PQueue.mg:94`) dirtied `pq` (left over from evaluating `pq.heap`) instead of the array receiving the references, a missed barrier; and a store through `p^` for a traced `p` dirtied nothing (e.g. `SUBARRAY(new_sp^, ...) := ...` in Pickle). `DerefExpr` now reports `p` as the container, as the C path's `PrepLV` does. Across libm3, only `gc.dirty` operands change (11 modules). |
| B20 | `SubscriptExpr` (3 sites) `ClearAbandoned` | **Fixed.** These tried the base as an lvalue and, if that abandoned, cleared the abandon and retried it as a value; since B17 the module was already marked incomplete, and one site returned a silent NIL when the base had no lvalue. Now a NIL lvalue falls back to the value path, an abandon propagates, and `ClearAbandoned` is gone: an abandon is final. |
| B21 | `ReturnStmt.CompileMSIR` | **Fixed.** `RETURN e` did not range-check `e` against a subrange or enumeration result type: `RETURN hi - lo` from a `CARDINAL` procedure returned a negative number where the C path faults (Compile checks via `Marker.EmitReturn` -> `AssignStmt.DoEmit`). Found by comparing link information (O17 (B)). `Check` now records the result type on the statement and `CompileMSIR` checks against it; a stopgap, replaced by D29a step 1: `ReturnStmt.CompileMSIR` now asks `Marker.ReturnVar`. |
| B22 | `NilChkExpr.CompileMSIR` | **Fixed.** MSIR dropped the explicit NIL check that `Compile` emits when a reference is used at an unknown offset or beyond the first readable page (`Target.First_readable_addr`), relying on a hardware fault. On macOS the low 4 GB are unmapped, so a NIL dereference still crashed (as a segmentation fault rather than the runtime error); on targets that map memory above the first page it could read or write memory silently. Found by comparing link information (O17 (B)): the C build imports `ReportFault` where MSIR did not. |
| B23 | `QualifyExpr.LValueMSIR` (object field), `MSIRBuilder` indirect calls | **Fixed.** Two more explicit NIL checks the C path makes and MSIR did not: before loading an object field whose offset is known only at run time (an opaque supertype; `Compile` calls `Check_nil` before `OTC_dataOffset`), and, under `Host.doProcChk`, before an indirect call (`CG.Gen_Call_indirect`), now `MSIRBuilder.CheckCallee` in `EmitCallIndirect`, `EmitMethodCall` and `EmitClosureCall` (which read the closure marker through a NIL pointer first). |
| B24 | `Formal.EmitArgMSIR` (VALUE scalar) | **Fixed.** A VALUE argument was not range-checked against a subrange formal: `Card (-3)` for a `CARDINAL` formal passed -3, where `GenOrdinal` faults (libm3 `LowPerfTool` passes an INTEGER file descriptor to a C `int`). Now checked with `CheckExpr.EmitChecksMSIR`. Found by comparing link information. |
| B25 | `CheckExpr.EmitChecksMSIR` | **Fixed.** It took the bounds of the unfolded expression, so `RETURN -1` from a `[-1..1]` procedure (libm3 `Point.Compare`) got a needless range check; `EmitChecks` folds first (`Expr.ConstValue`). Harmless (extra checks only), found by comparing link information. |
| B26 | `AssignStmt.CompileMSIR`, `MSIRBuilder.LookupVarRaw` | **Fixed.** Barriers MSIR emitted where the C path does not: a write barrier on a store whose value cannot hold a traced reference (`wr.buff := NIL`; `Compile` passes `traced := rhs_info.isTraced`), and a read barrier on a captured local of an enclosing procedure (stack storage; `NamedExpr.Compile` barriers only global or indirect variables). Both rules now follow the C path's. Harmless (extra barriers only), found by comparing link information. |
| B27 | `MSIRBuilder.EmitClosureCall` | **Fixed.** Under `Host.doProcChk` the procedure pointer was NIL-checked twice (once for the closure test, again in the direct path's `EmitCallIndirect`), and the closure path checked its shim pointer, which is never NIL. One check now, before the closure test. |
| B28 | `SubscriptExpr.LValueMSIR`, `BitLocElemMSIR`; `Subarray.LValueMSIR` | **Fixed.** Under `Host.doRangeChk`, C checks an open-array subscript against the dope's size (`Check_index`, SubscriptOutOfRange) and a SUBARRAY's start+len against the source's length (`Check_hi`, ValueOutOfRange). MSIR did neither, so out-of-range stores and loads went through. Found from the `ReportFault` differences in m3front's .m3x parity. `CheckIndexMSIR` now checks every open-array subscript (full, partial and packed, and constant open arrays held as fixed arrays); `CheckRangeMSIR` checks SUBARRAY of a fixed, open or constant open source. |
| B29 | `AssignStmt.EmitRTCheckMSIR` | **Fixed.** C checks a value assigned to a typed location through one dispatcher (`AssignStmt.DoEmit`/`EmitRTCheck`): an ordinal's range, a reference's implicit NARROW, a nested procedure's assignability. MSIR hand-rolled the range check at some sites only, so it never narrowed on assignment, initialization, RETURN or RAISE, and skipped the range check on initializers and RAISE arguments. Found from m3front's missing `CheckIsType` imports (O17 parity). `EmitRTCheckMSIR` mirrors `EmitRTCheck`'s dispatch and is called where C calls it: AssignStmt, Variable initialization, ReturnStmt, RaiseStmt. VALUE arguments were already narrowed (Formal wraps them in a NarrowExpr). |
| B30 | `builtinWord` Extract, Insert, LeftShift/RightShift, LeftRotate/RightRotate | **Fixed.** C range-checks the bit operands of these Word and Long builtins: Extract and Insert need CARDINAL `i` and `n` with `i + n` at most the word size, and the shifts and rotates need `n` in `[0..Size-1]` (ValueOutOfRange). MSIR emitted none of these checks, so an out-of-range operand returned garbage (the shifts masked `n` to its low bits). Found from the `ReportFault` imports of m3core's `Word` and `Long` (O17 parity). `CheckBitsMSIR` (Extract, Insert) and `EmitChecksMSIR` on `n` (shifts, rotates) now make C's checks. |
| B31 | `Formal.GenValueOpenArgMSIR` | **Fixed** (decided by Tony, 2026-11-23: follow C).  A VALUE open-array argument of run-time size was copied with a dynamic `alloca` at the call site; C copies it to the traced heap (`Variable.CopyOpenArray`, `RTHooks.AllocateOpenArray`).  The stack copy could overflow a forked thread's small stack (4096 words by default), and a dynamic `alloca` is freed only when the caller returns, so a call in a loop grew the stack on every iteration.  `Variable.CopyOpenArrayMSIR` now makes C's heap copy; copies of static size stay on the stack, as in C.  It left one .m3x difference in m3front: C also took the heap path for an *empty* array constructor (`Formal.PrepArg` tested `actStaticSize > 0`), e.g. the default `methods := ARRAY OF TEXT{}` of `MSIR.NewTypeDesc`.  Fixed in C instead (#1342, also on msir): an empty actual is static.  m3front's .m3x now matches C's. |
| B32 | `MSIRBuilder.AggByteSize` | **Fixed.** Found by running MSIRObj m3tests at `-O3` (p280: records passed by value from misaligned and short packed fields came back as garbage).  Storing a record or array value into a wider packed field (`BITS 23 FOR RECORD c1, c2: CHAR END`) spills it and reassembles its bytes, clamped to the value's size; but `AggByteSize` added a struct's last field offset, which `MSIR.Field` gives in bits, as bytes, so the clamp never applied and the spill was read past its end.  Unoptimised the extra byte was whatever the stack held; at `-O3` LLVM treats the out-of-bounds read as undefined.  The offset is now converted from bits. |
| B33 | `MSIR.addInsn` (m3-libs/sgml) | **Open.**  The compiler crashes (NIL dereference in `addInsn`) compiling sgml in MSIRObj mode.  Found by the std-package parity run (2026-11-26). |
| B34 | nested procedure result as an lvalue (caltech-other/simplegrid `SimpleGrid.Rectify`; m3-libs/arithmetic `Interpolation.mg` `CubicHermite.InterpolatePiece`) | **Open.**  "lvalue of nested proc result not yet supported in MSIR". |
| B35 | `BuildClosureValue` (caltech-other/magic `MagCell.FlatMapAll.Recurse`) | **Open.**  "cap var not in outer proc varMap": a nested procedure's closure captures a variable its outer procedure's map lacks. |
| B36 | statement in m3-libs/deepcopy `DeepCopy.Copy` | **Open.**  "unsupported statement". |
| B37 | m3-comm/udp `UDPPosix.Len` | **Open.**  Verifier: `icmp` operand types differ (in a WHILE condition). |
| B38 | m3-sys/cm3ide `Default.Init` | **Open.**  Verifier: a load's result type differs from its pointer's element type. |
| B39 | m3-ui/webvbt `HTMLParser` module body | **Open.**  Verifier: a store's value type differs from its pointer's element type. |
| B40 | caltech-parser/cit_util `LRVector` | **Open.**  `opt` rejects the LLVM IR: `icmp` on non-integer operands (the verifier misses it). |
| B41 | caltech-other/boxes `GridPointSetRoutines` | **Open.**  `llc`: use of undefined `@GridPoint__ComparX` (not declared). |
| B42 | m3-sys/m3cgcat `Main` | **Open.**  `llc`: use of undefined `@M3CG_BinRd__Inhale` (not declared). |
| B43 | m3-tools/m3tk `M3CParse` | **Open.**  `llc`: a struct initializer's element 0 does not match the struct's element type. |

#### 2. Abstraction violations by theme

##### 2a. Node internals exported for MSIR (expressions)
- `QualifyExpr.i3`: `LhsExpr`, `MethodSlotBase`, `ObjTypeMethod`, `MethodHolder` hand out
  `lhsExpr`/`holder`/`objType`; `UserProc` rebuilds vtable/default-method dispatch from them.
  Fix: a QualifyExpr operation returning the resolved callee and receiver.  **Done.**
- `ArrayExpr.i3`: `EltCount`/`Elt` let MSIRBuilder walk constructor elements and
  re-implement "repeat last element" (`MSIRBuilder.m3:2320-2650`).  Fix: move into
  `ArrayExpr.genLiteralMSIR`/`compileLValueMSIR`; drop the accessors.  **Done.**
- `TextExpr`: MSIR literals call `SetUID` (CG global-data allocation); `MSIREmit`
  iterates the CG literal table via `LiteralCount`/`LiteralExpr`.  **Done**:
  `TextExpr.CompileMSIR` adds the literal to the MSIR module itself
  (`MSIR.ModuleAddTextLit`, which now deduplicates equal literals), so the CG
  table is the C walk's alone; the transfer and the two accessors are gone.
- Sub-byte stores dispatched by probing four modules in turn (`AssignStmt`, `Inc`,
  `Dec`) instead of an Expr method; `EmitUseFailureMSIR` likewise.  Fix: ExprRep
  methods `storeMSIR`, `checkUseFailureMSIR`.  **Done**; `storeMSIR` has since
  become `bitLocMSIR` (B8), which also serves reads, INC/DEC and WITH.
- `RecordExpr.TryConstFieldMSIR` / `TryCompileConstMSIR` duplicate `genLiteralMSIR`.  **Done.**

##### 2b. Per-type behaviour re-implemented outside TypeRep (types/values)
- Default initialisation: `RecordType.GenInitMSIR`/`GenInitArrayMSIR` (also handling
  arrays and scalar subranges), `Variable.GenScalarInitMSIR`, `ObjectType.GenInitProcMSIR`,
  `RefType.GenInitProcMSIR`.  Divergences B3, B4.  Fix: `initValueMSIR` method on
  `TypeRep.T`, overridden per type constructor, dispatched via `Type.InitValueMSIR`.
- Typecells: built in parallel by ObjectType, RefType and MSIRBuilder; brand lost (B1);
  `ObjectType.m3:1063` invents a `dataOffset` CG leaves to RTLinker.  Fix: one
  backend-neutral typecell description per type constructor, consumed by both.
  **Done** (`Typecell.T`; see Progress).
- `RefMapBytes`/`RefPickleDescBytes` in MSIRBuilder duplicate RefType's.  **Done.**

##### 2c. Two-way layering between front end and MSIR layer
- Type, RefType, ObjectType, RecordType, UserProc, Variable, Formal, Procedure,
  Revelation and Module import MSIRBuilder/MSIREmit, while MSIRBuilder imports
  Variable, Formal, Procedure, RefType, ArrayExpr, RecordExpr and keeps maps keyed
  by `Variable.T`/`Procedure`.
- Capture results live in the builder and are read back by front-end analysis
  (`UserProc.Capture` -> `MSIRBuilder.GetProcCaptures`; `NamedExpr.m3:470`).
  **Done**: a nested procedure's captures live on its `Procedure.T`
  (`Procedure.Captures`); the front end passes them to
  `MSIRBuilder.EmitNestedCall` and `BuildClosureValue`, and the builder's
  `captureMap`, `RegisterCaptures`, `GetProcCaptures` and fixpoint flag are
  gone.  As before, a procedure's list is fixed once its body is compiled.
  libm3 and m3quake MSIR are byte-identical before and after.
- `MSIRBuilder.SetRefInitProcGen` callback guards a cycle that doesn't exist
  (MSIRBuilder already imports RefType).  **Gone** with 2b.
- Global side channel `SetPendingContainer`/`TakePendingContainer` between lvalues
  and Assign/With.  **Done**: `compileLValueMSIR (VAR container)` returns the
  heap object holding the lvalue, `Expr.HeapLValueMSIR` hands it to the stores
  that need it (assignment, VAR arguments, atomics, WITH), a WITH alias's comes
  from `MSIRBuilder.VarContainer`, and the global with its save/restore and
  "discard stale" calls is gone.  Fixed B19 on the way.

##### 2d. MSIR depends on the CG pass (overlaps O17)
- `BlockStmt.CompileMSIR` calls `Scope.InitValues`, re-running `LangInit`/`UserInit`
  and CG `Tracer.EmitPending`; `Variable` patches this with `msirInitDone`.
- Hook points inside CG emission: `Variable.Declare:1007`, `Revelation.GenList:610`,
  `Type.GenCells:878` (+ `GenCellsMSIR` scan), `Module.BuildImportLink:1225`.
- Mode detection by side effect: `p.cg_proc # NIL` (`Procedure.GenBody:913`) and
  `M3CG_DoNothing.New` returning NIL procs as a signal; `M3CG_Check` special-cases
  `M3CG_DoNothing`; Builder bypasses `M3Backend.Open`; the driver imports
  `MSIREmit.SetLLOutPath` instead of using the front-end environment.
- `Revelation.AddInheritedMSIR`/`GenCellsMSIR` re-emit interface content in the
  module unit because "the module .ll overwrites the interface .ll": a file-naming
  problem patched in front-end semantics.

##### 2e. Runtime (RT0) layout written out by hand
- `MSIRToLLVM.m3:3316` `OTC_defaultMethods_BYTES = 136` (64-bit literal next to
  `M3RT.OTC_methodOffset`); `MSIR.i3:509` `MI_nFields = 13` vs front end's
  `M3RT.MI_SIZE`; field tables `:3038-3108`, `:3897-3918`; `RH_*` offsets `:7-11`;
  exception descriptor `{i64, ptr, i64}` (`:4602`); open-array dope in
  `Formal.m3:1557,1772`, `RefType.m3:429`; method dispatch in `UserProc:331`.
- `M3C.m3:3163` third copy of the stack-walker target list (use `Target.Has_stack_walker`).
- **Done** under D24 (`db7908eb1d`): the field tables are checked against `M3RT`
  once per run, the remaining offsets and sizes come from `M3RT`, and M3C uses
  `Target.Has_stack_walker`.

#### 3. Clutter on the public branch

- 7 Claude session transcripts (~1.6 MB) at the repo root, `m3-sys/cm3/`,
  `m3-sys/msir/`, `m3-sys/msir/test/` (checked: no emails, credentials, Defence
  names or mail-tool use; only a generic Claude Code API-key warning).  **Done**
  (no longer tracked).
- `m3-sys/msir/test/smoke/smoke-realrt` (1.2 MB binary).  **Done** (no longer tracked).
- 33 editor backups `*.~<sha>~` across m3front.  **Done** (the last tracked one,
  `m3middle/src/Target.m3.~f10c3866f4~`, removed).
- Dead code: `Variable.BitSize`, `Variable.ForceInitMSIR`, `ObjectType.FillOwnMethodNames`,
  `SetExpr.GetWordBitMask`, `TextExpr.IsTextExpr`, `Variable.m3:1632` (`IF FALSE AND`),
  `Expr.TaCaptureLV`/`TabCaptureLV`; stale comments (MSIREmit env var, ExprRep note).
  **Done**, with `MSIRBuilder.EmitIndirectGlobalInit` (a no-op with no callers).
  `TaCaptureLV`/`TabCaptureLV` stay: they are ExprRep's default `captureLV`
  methods.
- `CG.m3:3597` `Index_bits` change and `TFloat` precision change alter the C backend
  and belong in separate, upstreamable commits.

#### 4. Judged fine

ExprRep/StmtRep additions (dispatch methods only); CallExpr method registration;
CompareExpr/EqualExpr moving to a local `Op` enum (reduces CG coupling); narrow
queries (`EnumType.EltList`, `ObjectType.FieldList`, `RefType.ReduceToRef`,
`M3WString.GetChar`); `CaptureAnalysis` using `Variable.IsUpLevel`; GC/type maps via
`Type.GenMap`/`GenDesc`; `Target.i3` additions; MSIRObj/MSIRAsm fitting Builder's
mode structure; `MSIRToLLVM.Module(forRuntime)`; verifier/printer use only the API;
ABI marker design.

### Known Limitations

All p0/p1/p2 test-suite procedures now compile without abandons.  The
remaining minor gaps are listed below; they do not affect any conformance test
unless otherwise noted.

- **VALUE open-array formals, partial depth coercion** (`actDepth <
  formDepth`): rare; abandons gracefully.
- **NEW(REF record with keyword args)**: abandons when `NUMBER(ce.args^) > 1`.
- **Tracers** (`<*TRACE*>` pragma): CG-only; MSIR silently omits callbacks.
- **Debug symbols:** *(superseded: phases 3 and 4, composite types and objects, are complete, and the debugger strategy is decided; see `MSIR-ROADMAP.md` §3.)*  Phase 2 summary: `DICompileUnit` / `DIFile` /
  `DISubprogram` per proc with `DW_LANG_Modula3`, `DINamespace` per module,
  `name`/`linkageName` split in DISubprogram, nine `DIBasicType` nodes
  (INTEGER…ADDRESS), `DILocalVariable` + `@llvm.dbg.declare` for alloca-backed
  variables of scalar and pointer types.  `lldb frame variable` shows scalar
  locals (INTEGER, BOOLEAN, REAL, CARDINAL, CHAR, pointer-typed refs).
  Untracked: RECORD/ARRAY/SET/proc-value allocas (btIdx = -1; need Phase 3
  composite types), VAR/READONLY aggregate params (no alloca), per-variable
  declaration lines (all use proc line as fallback).
  LLVM 22 constraint: `DILocalVariable` without `type:` crashes the DWARF
  emitter; fixed by the ADDRESS fallback for pointer kinds and skipping
  unrecognised types entirely.  See `MSIR-ROADMAP.md §3` for the phased plan.

### Key Source Files

| File | Role |
|---|---|
| `m3-sys/msir/src/MSIR.i3/.m3` | IR types, values, ops, builders |
| `m3-sys/msir/src/MSIRToLLVM.m3` | Lowers MSIR → LLVM text IR |
| `m3-sys/msir/src/MSIRPrinter.m3` | Prints `.msir` text |
| `m3-sys/msir/src/MSIRVerifier.m3` | Structural checks |
| `m3-sys/m3front/src/msir/MSIRBuilder.m3` | Per-proc builder state; var/proc maps; try-context stack; `ExtractBitField`/`InsertBitField` shared bitfield helpers |
| `m3-sys/m3front/src/msir/MSIREmit.m3` | Module-level gate; writes `.msir` / `.ll` |
| `m3-sys/m3front/src/stmts/TryStmt.m3` | TRY/EXCEPT EH lowering |
| `m3-sys/m3front/src/stmts/TryFinStmt.m3` | TRY/FINALLY EH lowering |
| `m3-sys/m3front/src/stmts/AssignStmt.m3` | Assignment + open→fixed-array copy |
| `m3-sys/m3front/src/stmts/BlockStmt.m3` | Block scope handling |
| `m3-sys/m3front/src/values/Variable.m3` | All MSIR variable registration |
| `m3-sys/m3front/src/values/Procedure.m3` | `GenBodyMSIR`, `BeginProc`/`EndProc` |
| `m3-sys/m3front/src/values/Module.m3` | `DeclareGlobalsMSIR`, module-init MSIR |
| `m3-sys/m3front/src/types/RefType.m3` | `InitTypecellMSIR` |
| `m3-sys/m3front/src/types/ObjectType.m3` | `InitTypecellMSIR`, vtable |
| `m3-sys/m3front/src/types/Type.m3` | `GenCells` — drives both CG and MSIR init |
| `m3-sys/m3front/src/builtinOps/New.m3` | NEW dispatch (Ref/Object/OpenArray) |
| `m3-sys/m3front/src/builtinOps/Subarray.m3` | SUBARRAY lvalue + rvalue |
| `m3-sys/m3front/src/misc/CaptureAnalysis.i3/.m3` | Up-level variable capture walk |
| `m3-sys/m3front/src/exprs/CallExpr.m3` | `MethodList` dispatch for MSIR + Capture |
| `m3-sys/m3front/src/types/UserProc.m3` | Direct / vtable / nested-lambda calls |
| `m3-sys/m3front/src/values/Formal.m3` | `EmitArgMSIR`, open-array arg coercion |
| `m3-sys/m3front/src/exprs/SubscriptExpr.m3` | `LValueMSIR` with try-first rvalue base |
| `m3-sys/msir/test/smoke/Main.m3` | Comprehensive smoke test |
| `m3-sys/msir/test/smoke/llvm_link_test.c` | 181-check C harness |
| `m3-sys/msir/test/smoke/raise_stub.cpp` | C++ runtime stubs: `RTHooks__Raise`, allocation helpers, and import-chain binder stubs (`Main_I3`, `Thread_I3`, `Fmt_I3`, `IO_I3`, `Text_I3`) that satisfy linker without pulling in `libm3core` |
| `m3-sys/msir/test/run-llvm-link-test.sh` | End-to-end driver |

### Declaration Lifecycle

MSIR declarations are co-located with CG declarations, not in separate passes:

| What | When | Where |
|---|---|---|
| Module globals + exception descs | `Module.Compile`, before type compilation | `Module.DeclareGlobalsMSIR` |
| Type cells (Ref + Object) | `Type.GenCells` in `GenLinkerInfo` | `RefType.InitTypecellMSIR` / `ObjectType.InitTypecellMSIR` |
| Proc formals + locals | `MSIRBuilder.BeginProc` | `Variable.BindFormalMSIR` + `Variable.AddLocalMSIR` |
| Variable initializers | CG-path `Scope.InitValues` | `Variable.UserInit` MSIR blocks fire because `BeginProc` is active |
| Nested proc body | `Procedure.LangInit` via `Scope.InitValues` | `Stmt.Capture` pre-scan → `GenBodyMSIR` |

### Lambda-Lifting: Nested Procedures

Each captured up-level variable becomes an explicit `ptr` parameter in the
inner proc's LLVM signature (`%__cap_0`, `%__cap_1`, …).  The outer proc's
captured vars are ordinary allocas whose addresses are passed as capture args.
Multi-level nesting works naturally: inner procs see outer capture params as
their own allocas.

Key routines:
- `Stmt.Capture(body, ca)` — pre-pass recording `(Variable.T, written)` pairs
- `MSIRBuilder.BeginProc(…, captures)` — generates capture params; binds in varMap
- `MSIRBuilder.RegisterProc(p, proc, caps)` — stores capture list
- `MSIRBuilder.EmitNestedCall(…)` — prepends capture addr args at call sites

Read-only scalar captures are passed by value (LLVM alias analysis benefit);
GcRef captures always pass by pointer (conservative GC stack-scan requirement).

### CallExpr MSIR Dispatch

`CallExpr.m3` dispatches MSIR compilation and capture analysis through its
`MethodList` with no per-kind logic inside `CallExpr` itself.

- `CompilerMSIR = PROCEDURE (t: T): MSIR.Value` — MSIR compilation callback
- `CompilerLValueMSIR = PROCEDURE (t: T): MSIR.Value` — lvalue callback (SUBARRAY)
- `Capturer = PROCEDURE (t: T; ca: CaptureAnalysis.T)` — capture-analysis callback
- `CaptureDefault` — set by `NewMethodList`; scans all args as reads
- Per-builtin callbacks wired in each module's `Initialize`

### Conventions

**NIL**: always `ConstNil(TPtr(TVoid()))`.  Call sites coerce to destination
type (`AssignStmt`, `ReturnStmt`, `EqualExpr`).

**VALUE/READONLY scalar formal widening** (`Variable.BindFormalMSIR`): ordinal
formals (BOOLEAN, CARDINAL, INTEGER, subranges, enums) are widened to
`TI(Target.Integer.size)` before spilling into their alloca slots.  Extension
direction is derived from `Type.GetBounds`: non-negative lower bound → `ZExt`;
negative lower bound → `SExt`.  `MSIRBuilder.CoerceToMSIR` cannot be used here
because `MSIRType.Translate(BOOLEAN)` returns `TI1` (signed), which would
`sext i1 1 to i64 = -1`, corrupting `TRUE`.

**Packed integer return widening/narrowing**: when a procedure's declared return
type differs in width from the expression's MSIR type, `ReturnStmt.CompileMSIR`
coerces before the `ret`:
- Narrower expression → wider result: ZExt (if lower bound ≥ 0) or SExt (< 0),
  determined by `Type.GetBounds` on the expression's M3 type.
- Wider expression → narrower result (e.g. `INTEGER` i64 returned into
  `[-1..+1]` i8): Trunc.
Same-width casts are suppressed in `MSIR.BuildZExt/SExt/Trunc`.

**Compact subrange coercion at store sites**: any `MSIR.BuildStore` that writes
a compact subrange value (e.g. i8 from `foo=[0..255]`) into a wider slot (e.g.
i64) must explicitly coerce.  Three sites require this:
- `Variable.UserInit` global init path (lines ~1142–1165): `initVal` from
  `Expr.CompileMSIR(initExpr)` may be narrower than the global slot type; Trunc
  or ZExt to `MSIR.EltType(MSIR.ValueType(addr))` before `BuildStore`.
- `Variable.UserInit` local init path (lines ~1121–1133): same coercion for
  local variables with constant initializers (e.g. `VS4: S4 := 16_FFFFFFFF`
  where S4=[16_FFFFFFFF..16_FFFFFFFF] → i32 slot, literal is i64).
- `ForStmt.CompileMSIR`: the FROM and LIMIT expressions of a FOR loop whose
  bounds are compact subrange variables (e.g. `x, y: [0..255]` stored as i8)
  must be widened (ZExt/SExt) to the loop variable's INTEGER-width (i64) type
  before storing into the loop counter and limit allocas.

**ICmp operand widening in `CompareExpr.CompileMSIR`**: M3 integer comparison
operands must have equal MSIR types.  Compact subrange operands (i8) compared
with INTEGER (i64) — e.g. `i * i <= Big` where `i: [1..100]` — cause
`icmp sle i8 X, i64 100`, which is invalid LLVM IR.  The fix: after compiling
both operands, if `BitWidth(lv) ≠ BitWidth(rv)`, widen the narrower to the
wider's type using `WidenICmpOp` (a module-level helper).  Use ZExt for unsigned
(W8..W64) types, SExt for signed (I1..I64) types.

**MSIRObj / DoNothing guard in `Module.EmitBody`**: the CG path inside
`EmitBody` (specifically `Stmt.Compile(t.block)`) is guarded by
`IF x.cg_proc # NIL`.  When `M3CG_DoNothing` is the active backend,
`Declare_procedure` returns `NIL`; calling CG procedures like `Declare_temp`
on a DoNothing backend also returns NIL, which triggers `<* ASSERT dopeTempVar # NIL *>`
in `Formal.m3`.  The guard avoids the CG path entirely in MSIRObj mode, where
`Stmt.CompileMSIR` handles the body instead.

**ByteArrayFallback / BITS-N-FOR-T packed fields**: any record or array
whose fields or elements are not byte-aligned is represented as `[N x i1]`
(one i1 slot per bit) by `MSIRType.ByteArrayFallback`.  Reads use
`MSIRBuilder.ExtractBitField(base, bitOff, bitWidth, rawFieldType)` — one
or two `i8` loads, shift + mask, then ZExt/SExt to the field's natural M3
type.  Writes use `MSIRBuilder.InsertBitField(base, bitOff, bitWidth, rhs)`
— read-modify-write on one or two bytes.  Both helpers are exported from
`MSIRBuilder` so `QualifyExpr`, `RecordExpr`, and any future subscriber can
call them without circular imports.  The `i1` sentinel distinguishes
ByteArrayFallback from ordinary `[N x i8]` byte arrays used elsewhere
(e.g. `memcpy` destinations).

**GC write barrier (container protocol)**: `QualifyExpr.LValueMSIR` calls
`MSIRBuilder.SetPendingContainer(baseAddr)` before returning the field GEP;
`AssignStmt.CompileMSIR` calls `TakePendingContainer()` and passes it to
`BuildGcStore`.  Globals return NIL container (no barrier needed).

**TEXT literals**: `TextExpr.P` carries `cgOffset`; a single `LiteralTable`
serves both CG and MSIR.  `MSIREmit.EndUnit` bridges data to `MSIR.Module`.

**READONLY TEXT dispatch**: Text literals have no addressable storage.
When a text literal is passed as a READONLY formal (by-reference), `compileLValueMSIR`
in `TextExpr.m3` spills the literal reference into a local alloca and returns that
address.  Previously `compileLValueMSIR` was missing from `TextExpr.P`'s method table,
so the READONLY path fell through to an assertion failure or wrong-type value.

The virtual dispatch path in `UserProc.CompileMSIR` was separately broken: it called
`Expr.CompileMSIR` for all arguments, ignoring READONLY/VAR modes.  Fixed to iterate
formals and call `Formal.EmitArgMSIR(fv, arg)` so that pass-by-reference semantics are
respected.  `TextExpr.i3` now exports `IsTextExpr`, `LiteralCount`, and `LiteralExpr`
to support `MSIREmit.EndUnit` text-literal registration.

**Module globals (var_map/gc_map)**: embedded as trailing fields of
`@Mod_M3_info` (after 104-byte `RT0.ModuleInfo` header).  gc_map TipeMap
encodes `Ref` ops for traced fields; LLVM aliases preserve mangled names.

**Multi-module opaque-supertype types**: When MODULE overwrites INTERFACE's .ll,
two records registered during interface compilation are LOST:

1. **TypeCell** (`type_cells`): the interface's `full_cells` (via `visible_cells` dedup)
   is skipped for the module by `SetGlobals`.  Fix: `Type.GenCellsMSIR()` walks
   `cur.module_types` directly, bypassing `visible_cells`; called from
   `DeclareGlobalsMSIR` for non-interface modules.

2. **Full revelation** (`full_rev`): inherited revelations (l.local=FALSE) are not
   emitted for the module.  Fix: `Revelation.AddInheritedMSIR(s)` iterates `s`
   and calls `MSIRBuilder.AddRevelation` + `ObjectType.InitTypecellMSIR` for each
   inherited full revelation; called from `DeclareGlobalsMSIR`.

When an OBJECT type extends an **opaque supertype** (mBase < 0 — static method base
offset unknown):

3. **UndefinedMethod at runtime**: `GetObjectTypeInfo` returned early, leaving no
   method names in the TypeDesc → no linkProc → vtable slots left as `UndefinedMethod`.
   Fix: collect own methods at LOCAL indices (0..nLocal-1), set `TypeDesc.dynamicMethOff`,
   emit a dynamic linkProc that reads `OTC_methodOffset` at runtime (set by RTLinker
   from parent's methodSize) and stores each own method at `dm[OTC_methodOffset/AP + j]`.

4. **Field init proc wrong offsets**: `GenInitProcMSIR` used a static fallback
   `dataOff = 8` when `fieldOffset < 0`.  Fix: read `OTC_dataOffset` from the TypeCell
   at runtime (gep %tp + 112, load i64) and use it as the base for field stores.

5. **Dynamic vtable dispatch** (method calls via `QualifyExpr.MethodSlotBase < 0`):
   instead of abandoning, load the holder TypeCell, read `OTC_methodOffset` at runtime,
   compute absolute slot, call indirectly via `EmitCallIndirect`.

**Sibling nested-proc calls — transitive captures**: When a nested proc P0_0
calls a sibling proc Dump0, Dump0 may need outer-scope variables (e.g.
P0's `LVisitNo`) that P0_0 does not directly access.  Lambda-lifting requires
these to be threaded through P0_0's capture parameter list.

Three coordinated changes needed (all in `Procedure.m3` / `NamedExpr.m3` /
`UserProc.m3`):

1. **Two-round `PreRegisterNestedCaptures`** — run the capture pre-registration
   pass twice before `Scope.InitValues` (in `GenBody`) and before
   `CompileNestedBodyMSIR` (in `GenBodyMSIR`).  Round 1 builds direct
   captures; round 2 adds transitive sibling captures using round-1 `captureMap`
   data (otherwise Dump0's caps are not yet known when P0_0's round-1 runs).

2. **Skip module globals in `NoteCapture`** (`NamedExpr.NoteCapture` ELSE branch):
   filter out variables with `t.global = TRUE` when propagating a callee's
   captures.  Module globals are always accessible via `LookupVar`/`LookupVarAddr`
   through `globalMap` without lambda-lifting; adding them as value captures
   shadows `globalMap` lookups and breaks `LookupVarAddr` for VAR-param callers.

3. **Also filter globals in `UserProc.Capture`**: the same rule applies when
   explicitly adding sibling captures for nested calls.

Rule: **only non-global (`t.global = FALSE`) stack locals of outer proc scopes**
need to be lambda-lifted through sibling call chains.

**Zero-initialization of solid FixedArray/Struct locals**: `MSIR.ConstZero`
returned NIL for `FixedArray`/`Struct` types (ELSE branch).  When
`Variable.UserInit` tried to zero-initialize `VAR x := ARRAY [N] OF T { 0, .. }`,
it called `ConstZero(TFixedArray(...))` → NIL → store silently skipped.
The alloca started with stack garbage.

Fixes:
- `MSIR.ConstZero`: handle `TypeKind.FixedArray` and `Struct` by returning
  `ConstNil(t)` (the emitter outputs `zeroinitializer` for these types).
- `MSIRToLLVM.LLValStr` (`ConstNil` case): emit `"zeroinitializer"` for
  `FixedArray`/`Struct` types; keep `"null"` for pointer/ref types.
- `Variable.AddLocalMSIR`: also emit an explicit zero-store for solid
  arrays/structs when `InitCost=0` (belt-and-suspenders for block-level vars).

This fixed p140/P26 where `VAR x := ARRAY [0..10] OF BOOLEAN { FALSE, .. }`
left x[3..10] as uninitialized stack garbage.  The root cause was found via
lldb watchpoints: the watchpoint on x[9] fired during C++ exception-runtime
stack unwinding (which wrote registers to the stack), leaving a non-zero
value that survived into the next procedure's stack frame.  The EH control
flow was always correct — the bug was purely in array zero-initialization.

**EXCEPT ELSE + RETURN/EXIT interception**: In CM3 semantics, `EXCEPT ELSE`
catches all exits from the TRY body including RETURN (the C backend stores a
`Return_exception` sentinel in `info.exception`; EXCEPT ELSE detects and swallows
it).  MSIR has no sentinel mechanism: RETURN inside `TRY ... EXCEPT ELSE` would
compile to a direct `ret`, bypassing the handler.

Fix (in `TryStmt.CompileMSIR` when `p.hasElse AND p.handles = NIL`):
1. Allocate `innerSelector` (i32) and `innerRetSlot` (procResultType if non-void).
2. `PushFinallyCleanup(elseDispatch, innerSelector, innerRetSlot)` BEFORE body —
   intercepts RETURN/EXIT exactly as TryFinStmt intercepts them for FINALLY.
3. `PushTryContext(lpad)` for exceptions.
4. The lpad saves the activation ptr, sets `innerSelector = Sel_Exc`, calls
   `__cxa_begin_catch` (owns the exception), then branches to `elseDispatch`.
5. `elseDispatch` compiles the ELSE body, then checks `innerSelector`:
   - `Sel_Exc`: `__cxa_end_catch` (releases owned exception), br merge.
   - Anything else (Sel_Return, Sel_Exit, Sel_Normal): br merge (suppressed).
6. `merge`: execution continues — the ENCLOSING FINALLY's `selector` / `retSlot`
   are untouched, so a pending RETURN 1 from the outer FINALLY still fires.

**LOCK exception re-raise**: The LOCK cleanup must use
`invoke RTHooks__ResumeRaise(actPtr) unwind enclosing` — NOT the LLVM `resume`
instruction — to re-propagate the caught exception to the enclosing handler.

LLVM's `resume` propagates the exception only when executed within an
instruction's exception-table scope (i.e., reachable via an `invoke`'s `unwind`
edge).  Blocks that are reachable only via normal control flow (e.g., a block
following `dispatch.cont.X`) are NOT in any handler's scope: `resume` there
propagates to the function's CALLER instead of the enclosing TRY.

Pattern (mirrors `TryFinStmt.CompileMSIR`):
1. Use `isCleanup := FALSE` (catch-all lpad, not cleanup).
2. Extract actPtr via `__cxa_get_exception_ptr` (do NOT call `__cxa_begin_catch`).
3. In resumeBlk: `invoke RTHooks__ResumeRaise(actPtr) to rcont unwind enclosing`
   where `enclosing = CurrentUnwindBlock()` captured AFTER `PopTryContext()`.
4. If `enclosing = NIL` (no outer try): plain call + unreachable.

**Open←Open bulk copy for multi-depth open arrays**: `AssignStmt.CompileMSIR`
computes `totalElts * EltPack(rhs) / 8` bytes to memcpy.  When the rhs M3 type
has more open dimensions than the MSIR rank (e.g. `ARRAY OF ARRAY OF T`, rank-1
in MSIR but depth-2 in M3), the extra dimensions are absent from the MSIR dope
value.  Load them directly from the raw rhs dope pointer (available via
`Expr.LValueMSIR(rhs)`) at byte offset `apB + k * ipB` for dimension index `k`.
Also use `EltPack(TypeOf(rhs))` (not `lhs`) — the rhs element pack correctly
accounts for any fixed inner dims in the rhs type (e.g. `ARRAY OF ARRAY[0..9] OF T`
has `EltPack = sizeof(T)*10`), while the lhs pack may use a different element structure.

**Aggregate exception arguments**: for exceptions with `ArgByReference = TRUE`
(argument larger than a pointer or a structured type), the argument must be
heap-allocated before calling `RTHooks__Raise`.  A stack alloca becomes a
dangling pointer after C++ exception unwinding destroys the raise frame.
In `RaiseStmt.CompileMSIR`: allocate via `RTHooks__NewTracedRef(typeCell)`,
memcpy the arg, pass the heap pointer.  `typeCell` = `TypeDescValueForRef`
of `REF argType` (= `t.refTipe` in the CG backend's `Exceptionz.DeclareRaiseProc`).

### Cosmetic Issues

- **Unreachable merge blocks**: when all IF branches end with `ret`, the
  `if.merge` block has no predecessors.  Harmless (LLVM DCE removes it).
- **Repeated block label names**: ELSIF chains reuse `if.then`/`if.next`
  hints.  Fix: add counter suffix in `NewBlock`.
