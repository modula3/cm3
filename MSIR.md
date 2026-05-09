Lifting CM3’s LLVM backend so that it **retains Modula‑3 semantic information**, instead of inheriting the heavy lowering imposed by **M3CG**, is essentially a *compiler re‑architecture project*. It is feasible, but it is not a “refactoring”; it would require changing where LLVM enters the pipeline and how much of Modula‑3’s meaning is preserved into IR.

Below is a clear breakdown of **what would be required**, organized by layers and trade‑offs.

***

## 1. Why the current M3CG → LLVM path loses semantics

CM3 today lowers Modula‑3 into **M3CG**, an extremely low‑level intermediate representation originally designed to target C/assembly‑like backends.

By the time code reaches LLVM:

*   Types are flattened (open arrays, traced refs, objects).
*   Exceptions are rewritten as control‑flow and runtime calls.
*   Garbage‑collection semantics are encoded indirectly via calls and conventions.
*   Module, interface, and type identity information is mostly gone.
*   Objects and methods are emitted as structs + function pointers, with no notion of dynamic type beyond runtime metadata.

LLVM therefore sees *implementation artifacts*, not *language intent*.

So to retain Modula‑3 semantics, LLVM must be introduced **before** this semantic collapse.

***

## 2. Required architectural shift: replace or bypass M3CG

### Core change

You would need a **new IR generation path**:

    Current:
    Modula‑3 front end
      → semantic analysis
        → M3CG (very low-level)
          → LLVM IR (mechanical translation)

    Required:
    Modula‑3 front end
      → semantic analysis
        → Modula‑3–aware mid‑IR
          → LLVM IR (semantic mapping)

This means:

*   Either **replacing M3CG entirely**, or
*   Keeping M3CG for legacy backends (C/GCC) and adding a **parallel LLVM‑native IR path**

In practice, the second option is safer.

***

## 3. Front‑end changes needed

### a) Preserve full semantic metadata

You must extend the front end to keep and expose:

*   Full type identities (not just layout)
*   Interface/module boundaries
*   Object inheritance graphs
*   Method dispatch tables
*   Exception regions and handlers
*   GC trace maps
*   Open array shape and bounds
*   Runtime type information (RTTI)

Many of these exist today but are *consumed* during M3CG emission rather than retained as first‑class data.

***

## 4. Introduce a Modula‑3 semantic IR (new layer)

You will need a **language‑aware intermediate representation**, higher level than LLVM but lower than AST.

This IR should:

*   Represent **objects, methods, exceptions, and modules explicitly**
*   Encode **GC roots and tracing behavior**
*   Preserve **type identity**, not just layout
*   Explicitly model **checked runtime operations** (NARROW, TYPECASE, bounds checks)

Conceptually, this IR is similar to:

*   Swift’s SIL
*   Rust’s HIR/MIR split
*   JVM‑style typed IR

LLVM IR alone is *not* expressive enough without careful encoding.

***

## 5. LLVM IR emission strategy (key design decisions)

### a) Types

You would need a **semantic type mapping**, e.g.:

| Modula‑3 concept | LLVM representation                           |
| ---------------- | --------------------------------------------- |
| REF T            | pointer with tagged address space or metadata |
| OBJECT           | struct + type descriptor global               |
| Open array       | `{ ptr, length, shape }` struct               |
| Interface        | fat pointer or (vtable\*, data\*)             |

Crucially:

*   Attach `!llvm.type` or custom metadata to preserve identity
*   Avoid flattening into anonymous structs

***

### b) Garbage collection integration

This is one of the biggest changes.

Options:

1.  **LLVM GC plugin strategy**
    *   Implement an LLVM GC strategy (`gc "modula3"`)
    *   Emit `gcroot`‑style intrinsics or statepoints
2.  **Shadow stack**
    *   Explicit root stacks per function
3.  **Precise statepoint GC**
    *   Modern LLVM approach, hardest but most robust

Without this, you lose exact tracing semantics.

***

### c) Exceptions

Instead of lowering to runtime calls early:

*   Map Modula‑3 exceptions to **LLVM `invoke` / `landingpad` / `resume`**
*   Preserve exception types as typed metadata
*   Encode `TRY/EXCEPT` structurally

This enables:

*   Better optimization
*   Cleaner debug info
*   Interop with other LLVM‑based languages

***

### d) Method dispatch

Instead of raw function pointers:

*   Emit **explicit vtables** with type metadata
*   Preserve override relationships
*   Attach method identity for debugging and ODR‑like guarantees

***

## 6. Debugging, tooling, and diagnostics

A semantic LLVM backend unlocks:

*   Real source‑level debugging (DWARF mapped to Modula‑3 concepts)
*   Better error reporting post‑optimization
*   Easier interop with LLVM tooling (sanitizers, analyzers)

But this requires:

*   DWARF extensions for Modula‑3 constructs
*   Stable naming and metadata schemes
*   Consistent module/type IDs

***

## 7. Optimization strategy

With semantics preserved, you can:

*   Run **typed optimizations before lowering**
*   Introduce Modula‑3–specific passes (e.g. eliminate redundant checked operations)
*   Delay destructive lowering until very late

Without this, LLVM optimizations often pessimistically inhibit themselves.

***

## 8. Incremental path (realistic roadmap)

A feasible phased approach:

### Phase 1: Parallel backend

*   Keep M3CG intact
*   Add a new “M3‑to‑LLVM‑direct” path
*   Initially target:
    *   No optimization
    *   One platform
    *   Basic language subset

### Phase 2: Semantic coverage

*   Add GC correctness
*   Add exceptions
*   Add objects/interfaces

### Phase 3: Optimization & tooling

*   Custom LLVM passes
*   Full debug info
*   Performance tuning

***

## 9. Cost and effort (realistic assessment)

This would take:

*   **1–2 senior compiler engineers**
*   **12–24 months** for a usable, correct backend
*   Plus ongoing LLVM version maintenance

This is *comparable in scope to a new backend for Swift or Rust*, not a “clean‑up” of existing CM3 LLVM code.

***

## 10. Bottom line

To retain Modula‑3 semantic information in LLVM IR:

✅ You must **bypass M3CG**  
✅ Introduce a **Modula‑3 semantic IR**  
✅ Map language concepts *directly* to structured LLVM IR  
✅ Integrate GC, exceptions, and objects explicitly  
✅ Accept this as a **new backend**, not an evolution of the current one

The payoff is huge—but so is the engineering cost.

If you want, I can:

*   Sketch what a minimal Modula‑3 semantic IR might look like
*   Compare this approach to how Rust/Swift solved similar problems
*   Outline a migration plan that keeps CM3 usable throughout
