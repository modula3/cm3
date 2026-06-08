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
| EH contract                    | Pinned (`Has_stack_walker`)    |
| Module / init contract         | Pinned                         |
| Intrinsic surface              | Pinned for OBJECT/EH/opens/sets/RTTI; growing |
| Procedure attributes           | Pinned                         |
| Non-local control              | Pinned                         |
| Opacity / visibility           | Pinned (D21); not yet wired    |
| Verifier                       | Sketched (see A9 / D20)        |
| `m3-sys/msir` v0 package       | Built; ships; 181/181 LLVM link test checks; 286/288 p0/p1/p2 tests clean in MSIRObj mode (0 genuine abandons, 2 TIMEOUTs); smoke test 124/124 exit 0 |
| `m3-sys/m3middle` MSIRObj build | **Clean** — 50/50 object files produced, zero crashes, zero llc errors |

Walkthroughs done: OBJECT + METHOD, TRY/EXCEPT/FINALLY, open arrays,
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

**LLVM conformance** (`run-msir-conformance.sh`, ARM64_DARWIN, 2026-06-08):
**255/288 PASS, 4 MISMATCH, 29 SKIP**.

Remaining 4 MISMATCH:

| Test | Root cause |
|------|-----------|
| p140 | 1 residual error in P26: RAISE inside FINALLY caught by inner EXCEPT; outer exception re-raise fails. `catch` lpad without `__cxa_begin_catch` — after inner `__cxa_end_catch` clears the inner exception, the selector check goes wrong (x[9]=TRUE instead of FALSE). |
| p251, p259 | UNSAFE stack-height measurement (implementation-specific, unfixable) |
| p253 | Heap addresses differ between runs (ASLR) |

The authoritative feature checklist (emission and lowering, item by item)
is in `MSIR-ROADMAP.md §What's Working`.  Summary of coverage: arithmetic,
control flow, records, fixed/open arrays, enums, SETs (all widths), globals,
VAR/READONLY/VALUE params, TRY/EXCEPT/FINALLY, RAISE, GC barriers,
TypeCells, NEW (REF/OBJECT/open-array, all ranks), vtable dispatch,
TYPECASE/NARROW/ISTYPE, nested procedures (lambda-lifted), TEXT,
SUBARRAY, ADR/LOOPHOLE/ADDRESS, float conversions, procedure values,
BITS-N-FOR-T packed fields (ByteArrayFallback), compact subrange arrays,
struct-by-value return, opaque types, SET arithmetic, LOCK.

### Known Limitations

All p0/p1/p2 test-suite procedures now compile without abandons.  The
remaining minor gaps are listed below; they do not affect any conformance test
unless otherwise noted.

- **VALUE open-array formals, partial depth coercion** (`actDepth <
  formDepth`): rare; abandons gracefully.
- **NEW(REF record with keyword args)**: abandons when `NUMBER(ce.args^) > 1`.
- **Tracers** (`<*TRACE*>` pragma): CG-only; MSIR silently omits callbacks.
- **Debug symbols (Phase 2 complete):** `DICompileUnit` / `DIFile` /
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
