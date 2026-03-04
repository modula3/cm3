# MScheme: Modula-3/Scheme Interop and Comparable Systems

## 1. Introduction

MScheme is a Scheme interpreter written in Modula-3, shipped with a stub
generator (`sstubgen`) that processes Modula-3 interface files and
produces bidirectional bindings.  It originated as a line-for-line
translation of Peter Norvig's JScheme (Java) into Modula-3.  The key
design insight is that because Modula-3 is a garbage-collected language
with a universal reference type (`REFANY`), any traced M3 value can
become a Scheme value without wrapping, boxing, or special-casing.

This report describes the MScheme interop architecture in detail, then
compares it to seven other embedding/interop systems that solve similar
problems in different ways.

---

## 2. MScheme Architecture

### 2.1 The Universal Value Type

The foundation of the entire system is one line of Modula-3:

```modula3
(* SchemeObject.i3 *)
TYPE T = REFANY;
```

A Scheme value is `REFANY` -- Modula-3's traced, polymorphic reference
type.  Any heap-allocated M3 value (objects, REFs, TEXTs, arrays)
already *is* a valid Scheme object.  No wrapper structs, no tag bits
on the Scheme side, no special allocation paths.  The M3 runtime's
garbage collector manages all values uniformly.

Scheme-specific types are defined as ordinary M3 reference types:

| Scheme type  | Modula-3 representation                                  |
|--------------|----------------------------------------------------------|
| Number       | `REF LONGREAL` (`SchemeLongReal.T`)                      |
| BigInt       | `BigInt.T` (from `cit_util`)                             |
| Pair         | `REF RECORD first, rest : REFANY END` (`SchemePair.T`)   |
| Symbol       | `Atom.T` (`SchemeSymbol.T`)                              |
| Boolean      | `SchemeBoolean.T`                                        |
| String       | `SchemeString.T`                                         |
| Procedure    | `SchemeProcedure.T` (object with `apply` method)         |
| Any M3 value | Itself -- already `REFANY`                               |

Runtime type discrimination uses Modula-3's `TYPECASE` (a type-safe
downcast), `ISTYPE`, and `TYPECODE` -- the same mechanisms the M3
runtime provides for all M3 code.  No parallel type system is needed.

### 2.2 How M3 Values Enter Scheme

Because `SchemeObject.T = REFANY`, any M3 reference value can be
passed into Scheme simply by assigning it to a Scheme variable.  For
a host program embedding MScheme:

```modula3
(* From the host M3 program: *)
VAR interp := NEW(SchemeM3.T).init(paths^);
interp.bind("my-driver", myM3Object);
```

The `bind` method defines a symbol in the global Scheme environment.
The M3 object `myM3Object` becomes directly accessible in Scheme as
the value of `my-driver`.  No conversion, no copying, no indirection.
Scheme code manipulating this value is manipulating the exact same M3
heap object.

### 2.3 How Scheme Calls M3 Methods

MScheme uses a three-layer dispatch mechanism:

**Layer 1: Type operations registry** (`SchemeProcedureStubs.m3`).
For each M3 type that should have methods callable from Scheme,
operations are registered by typecode:

```modula3
SchemeProcedureStubs.RegisterOp(
    TYPECODE(TextSeq.T),    (* M3 runtime typecode *)
    "call-method",           (* operation name *)
    TextSeqCallMethod);      (* M3 procedure to dispatch *)
```

The dispatch procedure receives the M3 object, a method name (as an
Atom), and the Scheme argument list:

```modula3
PROCEDURE TextSeqCallMethod(interp : Scheme.T;
                            object : SchemeObject.T;
                            args   : SchemeObject.T) : SchemeObject.T
  RAISES { Scheme.E } =
VAR name := Scheme.SymbolCheck(SchemeUtils.First(args));
    rest := SchemeUtils.Rest(args);
BEGIN
  IF name = addhi THEN
    NARROW(object, TextSeq.T).addhi(SchemeUtils.Str(SchemeUtils.First(rest)));
    RETURN SchemeBoolean.True()
  ELSIF name = size THEN
    RETURN SchemeLongReal.FromI(NARROW(object, TextSeq.T).size())
  ELSIF name = get THEN
    RETURN NARROW(object, TextSeq.T).get(SchemeLongReal.Int(SchemeUtils.First(rest)))
  (* ... *)
  END
END TextSeqCallMethod;
```

**Layer 2: Scheme-level wrapper** (`scheme-lib/src/m3.scm`).  The
`obj-method-wrap` function creates a closure that dispatches method
calls through the type operation registry:

```scheme
(define (obj-method-wrap obj type)
  (lambda args
    (if (eq? (car args) '*m3*)
        obj    ; return the raw M3 object
        (modula-type-op type 'call-method obj (car args) (cdr args)))))
```

**Layer 3: Application code**.  The result is natural Scheme syntax:

```scheme
(define seq (obj-method-wrap (new-modula-object 'TextSeq.T) 'TextSeq.T))
(seq 'addhi "hello")
(seq 'size)           ;=> 1
(seq 'get 0)          ;=> "hello"
(seq '*m3*)           ;=> the raw TextSeq.T for passing back to M3
```

The `'*m3*` convention lets Scheme code extract the underlying M3
object to pass it back into M3 methods that expect typed arguments.

### 2.4 How Scheme Values Return to M3

Because Scheme values *are* M3 `REFANY`, return is trivial.  An M3
method receiving a Scheme value simply narrows it:

```modula3
VAR result := interp.evalInGlobalEnv(someExpr);
(* result is REFANY -- narrow to expected type: *)
VAR text := NARROW(result, TEXT);
```

If the Scheme code returned a wrapped M3 object, the narrow succeeds
and the caller gets back the original M3 object with its original type.
If it returned a Scheme-specific type (pair, number), the narrow can
distinguish that.  `ISTYPE` and `TYPECASE` handle all cases safely.

### 2.5 The Stub Generator (sstubgen)

MScheme includes a *stub generator* that processes M3 interface files
at build time and auto-generates bidirectional binding code.  This is
the system's most distinctive feature.  The design goal, as stated
in the sstubgen documentation, is:

> "We want to be able to *write Modula-3 code in Scheme.* That is,
> anything that can be coded in Modula-3 one should be able to code
> in Scheme."

The stub generator operates in three phases:

**Phase 1** parses each M3 interface file using the M3 Toolkit (m3tk)
AST infrastructure.  For each procedure, type, constant, and variable
in the interface, it extracts type signatures and generates an M3
stub module containing:

- *ToModula* converters: functions that take a `SchemeObject.T` and
  produce the appropriate M3 type (with type-checking and bounds
  validation).
- *ToScheme* converters: functions that take an M3 value and produce
  a `SchemeObject.T`.
- *CallStub* procedures: wrappers that unpack Scheme arguments, call
  the real M3 procedure, convert the result, and handle exceptions.
- Registration code that plugs these stubs into the runtime registry.

A generated call stub looks like:

```modula3
PROCEDURE CallStub_Text_Cat(interp : Scheme.T;
                            args   : SchemeObject.T;
                            excH   : SchemeObject.T) : SchemeObject.T
  RAISES { Scheme.E } =
VAR p := SchemePair.Pair(args);
    a1 := ToModula_TEXT(First(p));   (* Scheme -> TEXT *)
    a2 := ToModula_TEXT(Second(p));
BEGIN
  RETURN ToScheme_TEXT(Text.Cat(a1, a2))  (* TEXT -> Scheme *)
END CallStub_Text_Cat;
```

**Phase 2** collects all stub modules within a package into a single
`RegisterStubs()` entry point.

**Phase 3** links all package-level `RegisterStubs` into the final
executable, so that a single call at startup activates all bindings.

The m3makefile for the CSP compiler (cspc) shows this in practice:

```
import ("sstubgen")
SchemeStubs ("CspExpression")
SchemeStubs ("Fmt")
SchemeStubs ("Atom")
SchemeStubs ("Word")
SchemeStubs ("M3Ident")
SchemeStubs ("FileWr")
(* ... 30+ more interfaces ... *)
ExportSchemeStubs ("cspc")
```

Each `SchemeStubs` invocation processes one M3 interface.  The result
is that Scheme code in cspc can call `Fmt.Int`, `Atom.FromText`,
`FileWr.Open`, etc., as naturally as M3 code can.

### 2.6 Runtime Type Introspection from Scheme

MScheme exposes M3's runtime type system to Scheme via primitives:

```scheme
(rttype-typecode obj)          ; get an object's runtime typecode
(rttype-supertype tc)          ; get a typecode's supertype
(rttype-issubtype? tc1 tc2)    ; subtype relationship
(rttype-maxtypecode)           ; highest allocated typecode
(rtbrand-getname tc)           ; get the brand string for a type
(modula-type-class tc)         ; classification (object, ref, etc.)
(list-modula-type-ops tc)      ; list registered operations
(list-modula-types)            ; list all registered type names
(typename->typecode 'Foo.T)    ; look up typecode by name
```

This allows Scheme code to introspect M3 objects, walk the type
hierarchy, and discover available methods -- something that M3 code
itself can do only in limited ways.

### 2.7 Concurrency Model

MScheme is designed for multi-threaded M3 programs.  The key design
rule: a single `Scheme.T` interpreter instance is single-threaded, but
multiple interpreters may share a global environment (which uses
synchronized access).  Environments come in `Safe` (synchronized) and
`Unsafe` (unsynchronized) variants.

### 2.8 Summary of Key Properties

- **Zero-copy value sharing**: M3 objects are Scheme objects, no
  wrapping needed.
- **Unified GC**: Both languages share the M3 traced heap and garbage
  collector.  No reference counting, no prevent-collection APIs, no
  weak reference coordination.
- **Automatic stub generation**: Build-time processing of M3 interfaces
  produces bidirectional bindings with type checking.
- **Runtime type introspection**: Scheme code can query the M3 type
  system, discovering types and operations dynamically.
- **Method dispatch via typecode**: O(n) scan of registered operations
  per typecode, then atom comparison for method name.

---

## 3. Comparable Systems

### 3.1 Lua/C (Lua C API)

**Core representation**: A *stack* of tagged union values (`TValue`
internally).  The C API never exposes `TValue` directly; all value
exchange is through positional stack slots.

**Wrapping C objects**: `lua_newuserdata(L, size)` allocates a GC-managed
memory block.  A *metatable* (a Lua table) is attached to define
behavior.  The `__index` metamethod routes field/method lookups to a
table of C functions.  `__gc` provides a destructor.

**Method dispatch**: When Lua code calls `obj:method(args)`, the VM
performs a metatable lookup for `__index`, finds the method in the
method table, and calls the C function with the Lua state.  The C
function pops arguments from the stack and pushes results.

**Values back to C**: Typed accessors read stack slots: `lua_tonumber`,
`lua_tostring`, `lua_touserdata`.

**Registration**: C functions are registered via arrays of
`luaL_Reg` structs, or individually with `lua_pushcfunction`.

**Key trade-offs**: The stack-based API provides complete ABI isolation
-- Lua's internal value representation can change without breaking
extensions.  But every boundary crossing requires explicit push/pop
operations.  There is no automatic type mapping, no stub generation,
and no introspection of the host's type system.

### 3.2 GNU Guile (Scheme in C)

**Core representation**: `SCM`, a machine-word-sized tagged value.
Fixnums, characters, and booleans are encoded in tag bits.  Heap
objects (pairs, foreign objects, strings) are tagged pointers.

**Wrapping C objects**: *Foreign object types* replace the older SMOB
mechanism.  `scm_make_foreign_object_type` defines a type with named
slots and a finalizer.  Instances store C pointers in slots.

**Method dispatch**: Typically via named procedures (`my-object-get-x`).
Optional GOOPS integration (CLOS-style generic functions with multiple
dispatch) is possible but uncommon for foreign objects.

**Values back to C**: `scm_to_int(val)`, `scm_to_double(val)`, etc.
Every function takes and returns `SCM`.

**Registration**: `scm_c_define_gfunc("name", nreq, nopt, nrest, cfunc)`
registers a C function as a Scheme procedure in the current module.

**Key trade-offs**: The uniform `SCM` type is simpler than Lua's stack
(no positional bookkeeping), but every C function must do its own type
checking.  Guile's conservative GC (BDW-GC) is very friendly to C
embedders -- no need to track roots manually.  But Guile provides no
automatic stub generation from C header files.

### 3.3 Tcl/C

**Core representation**: `Tcl_Obj*`, a dual-ported value with both a
string representation and a cached native ("internal") representation.
Conversions between the two forms are lazy and cached.  Reference
counted (no GC).

**Wrapping C objects**: Tcl uses the "command-as-object" pattern.
Creating an object registers a new Tcl command named after it.
The `ClientData` (a `void*`) stored with the command points to the
C struct.  "Methods" are subcommands: `.button configure -text "OK"`.

**Method dispatch**: String-based.  The first argument to the object's
command is the subcommand name, matched by string comparison or hash
lookup.  Object identity is a string name.

**Registration**: `Tcl_CreateObjCommand(interp, "name", func, clientData,
deleteProc)`.

**Key trade-offs**: "Everything is a string" gives extraordinary
composability but loses type safety.  Reference counting provides
deterministic destruction but cannot handle cycles.  The
command-as-object pattern is elegant and uniform but means object
identity is a string, and there is no direct pointer from Tcl to C
memory without going through command lookup.

### 3.4 CPython Extension API

**Core representation**: `PyObject*`, a heap-allocated struct with a
reference count and a pointer to a `PyTypeObject`.

**Wrapping C objects**: The embedder fills out a `PyTypeObject` struct
with dozens of slots: `tp_methods` (method table), `tp_getset`
(computed properties), `tp_members` (direct struct field access by
offset), `tp_dealloc` (destructor), plus slots for numeric, sequence,
mapping, iterator, and async protocols.

**Method dispatch**: Attribute lookup via the type's method resolution
order (MRO).  If the name is found in `tp_methods`, a bound method
object is created wrapping the C function.

**Values back to C**: `PyLong_AsLong(obj)`, `PyFloat_AsDouble(obj)`,
`PyArg_ParseTuple(args, "is|d", ...)`.

**Registration**: Module initialization fills a `PyModuleDef` struct
and registers types with `PyType_Ready`.

**Key trade-offs**: Maximum control over Python's internals, but the
reference counting discipline is notoriously error-prone.  Every
boundary crossing allocates heap objects.  Defining even a simple
type requires filling many `PyTypeObject` slots.  The API is primarily
designed for *extending* Python, not for *embedding* it.

### 3.5 Kawa Scheme (JVM)

**Core representation**: `java.lang.Object`.  Kawa compiles Scheme to
JVM bytecode; Scheme values are Java objects.

**Wrapping Java objects**: No wrapping needed.  Java objects are
directly usable in Kawa Scheme.  `(define pt (java.awt.Point 10 20))`
creates a Java `Point`.  `pt:x` accesses the field.

**Method dispatch**: When the type is known at compile time, Kawa
generates `invokevirtual` bytecode -- as efficient as Java.  When
unknown, it falls back to reflection.  Type annotations
(`::java.awt.Point`) guide the compiler.

**Values back to Java**: Scheme values are already JVM objects.
Java code invokes Scheme functions by calling `apply` on
`gnu.mapping.Procedure` objects.

**Registration**: None needed -- both languages share the JVM class
loader and object space.

**Key trade-offs**: Zero-cost interop when types are known, but
reflection fallback is slow.  Kawa must work within JVM constraints
(no true tail calls, no first-class continuations without
trampolining).  The shared-runtime model is fundamentally different
from all other systems here.

### 3.6 Common Lisp / CFFI

**Core representation**: Tagged Lisp objects (implementation-dependent
tagging scheme).  Foreign values are `cffi:foreign-pointer` wrappers.

**Wrapping C objects**: The programmer wraps foreign pointers in CLOS
objects and defines generic functions that call through to C.
`trivial-garbage:finalize` attaches destructors.

**Method dispatch**: Generic functions with CLOS multiple dispatch.
Methods dispatch on the classes of *all* arguments, not just the
receiver.

**Values back to C**: `cffi:foreign-slot-value` reads struct fields
by declared offset.  `defcfun` defines C function bindings with
automatic marshaling.

**Registration**: *Pull-based* -- the Lisp side declares C bindings
with `defcfun`, `defcstruct`, `defcenum`.  The C library is
unmodified.

**Key trade-offs**: CFFI is portable across CL implementations but
requires manual struct layout declarations and memory management.
CLOS provides a powerful "glue" layer but is per-project, not
standardized for FFI.

### 3.7 Racket FFI

**Core representation**: Tagged Racket objects.  C types are
first-class Racket values: `_int`, `_double`, `_pointer`, `_fun`.

**Wrapping C objects**: `define-cpointer-type` creates tagged pointer
types.  `define-cstruct` auto-generates constructors, accessors, and
mutators.  Custom type combinators (`make-ctype`) attach conversion
functions to base C types.

**Method dispatch**: Not object-oriented at the FFI layer.  Operations
on foreign values are called as procedures.

**Values back to C**: `_fun` type descriptors handle all marshaling
declaratively.  `(_fun _string _int -> _bool)` specifies argument and
return conversions.

**Registration**: *Pull-based* -- `get-ffi-obj` or `define-ffi-definer`
declares bindings.  The C library is unmodified.

**Key trade-offs**: The most algebraic and composable FFI design.
Types as values enable declarative marshaling.  Tagged pointers add
safety.  But the C library must be described entirely from the Racket
side, and per-call marshaling overhead is significant for tight loops.

---

## 4. Comparative Analysis

### 4.1 The Central Trade-off: Shared Heap vs. Marshaling Boundary

The most fundamental architectural difference across these systems
is whether the host and guest languages *share* their object
representations or maintain *separate* representations connected by
a marshaling boundary.

**Shared heap** (MScheme, Kawa): The host language's objects are
directly usable as scripting language values.  No per-call conversion
cost.  The garbage collector sees everything.  This is only possible
when both languages are GC'd and share a runtime (M3 + MScheme) or
a VM (Java + Kawa).

**Marshaling boundary** (Lua, Guile, Tcl, Python, CFFI, Racket): The
host creates scripting-language values via conversion APIs.  Every
boundary crossing allocates and converts.  The two sides maintain
separate type systems and (often) separate memory management.

MScheme's `SchemeObject.T = REFANY` is the extreme case of shared
heap: it is not that M3 objects are *wrapped* as Scheme values, but
that they literally *are* Scheme values, by type identity.

### 4.2 Type Safety and Dispatch Cost

| System  | Type check at boundary                 | Method dispatch cost               |
|---------|----------------------------------------|------------------------------------|
| MScheme | `TYPECASE` (M3 runtime typecheck)      | Atom comparison per registered op  |
| Lua     | Metatable lookup + manual C-side checks | Hash lookup in metatable          |
| Guile   | Manual `SCM` type predicates           | Symbol lookup in module            |
| Tcl     | String parse on demand                 | String comparison for subcommand   |
| Python  | `PyArg_ParseTuple` format string       | MRO attribute lookup               |
| Kawa    | JVM `checkcast` instruction            | `invokevirtual` or reflection      |
| CFFI    | CLOS `typep` + manual checks           | Generic function dispatch          |
| Racket  | cpointer tag check                     | Direct call (no dispatch)          |

MScheme benefits from M3's runtime type system: `TYPECASE` is a
compiler-supported, type-safe downcast with O(1) performance (the
runtime checks the typecode, which is an integer).  This is
essentially the same mechanism as Java's `instanceof` check, used by
Kawa on the JVM.

### 4.3 Automatic Binding Generation

| System  | Auto-generation?       | Input                       | Mechanism                                       |
|---------|------------------------|-----------------------------|------------------------------------------------ |
| MScheme | Yes (`sstubgen`)       | M3 `.i3` interface files    | M3 Toolkit AST parsing + Scheme code generation |
| Lua     | No                     | --                          | Manual C code                                   |
| Guile   | Partial (g-wrap)       | C headers                   | Third-party tool, not standard                  |
| Tcl     | Partial (critcl, SWIG) | C headers                   | Third-party tools                               |
| Python  | Partial (Cython, etc.) | C/C++ headers + annotations | Third-party tools                               |
| Kawa    | Unnecessary            | --                          | Shared JVM class loader                         |
| CFFI    | No                     | Manual declarations         | Programmer writes `defcfun`                     |
| Racket  | No                     | Manual declarations         | Programmer writes `define-ffi-definer`          |

MScheme's `sstubgen` is notable because it is a *first-party* tool
that ships with the interpreter, processes the host language's
standard interface files, and generates complete bidirectional bindings.
The equivalent in the Python world would be a tool that reads `.pyi`
stub files and generates complete C extension modules automatically.

The closest comparable systems are SWIG (which processes C/C++ headers
to generate bindings for multiple target languages) and Kawa (which
needs no generator at all because Java and Scheme share the JVM).

### 4.4 GC Coordination

| System  | Host GC            | Guest GC                   | Coordination                                      |
|---------|--------------------|----------------------------|---------------------------------------------------|
| MScheme | M3 traced refs     | Same GC                    | None needed -- unified heap                       |
| Lua     | None (C)           | Incremental mark-sweep     | Explicit: stack + registry roots                  |
| Guile   | None (C)           | Conservative (BDW-GC)      | Automatic: scans C stack conservatively           |
| Tcl     | None (C)           | Refcounting                | Manual: `Tcl_IncrRefCount`/`Tcl_DecrRefCount`     |
| Python  | None (C)           | Refcounting + cycle detect | Manual: `Py_INCREF`/`Py_DECREF`                   |
| Kawa    | JVM GC             | Same GC                    | None needed -- unified heap                       |
| CFFI    | CL GC              | Same GC (for CL objects)   | `trivial-garbage:finalize` for foreign ptrs       |
| Racket  | Precise moving GC  | Same GC                    | Special APIs for C extensions holding Racket refs |

The MScheme/M3 and Kawa/JVM systems share a critical advantage: since
both languages use the same garbage collector, there is no coordination
problem.  A Scheme closure that captures an M3 object keeps it alive
automatically.  An M3 data structure containing Scheme pairs is traced
correctly.  No reference counting, no prevent-collection guards, no
weak-reference bridges.

This eliminates an entire class of bugs that plagues C-based embedding
systems.  In CPython, getting reference counts wrong is the single
most common source of C extension bugs.  In Lua, forgetting to anchor
a value on the stack or in the registry causes it to be collected while
C still holds a pointer to it.

### 4.5 Registration Direction: Push vs. Pull

**Push** (the host declares what's available to the script):
MScheme, Lua, Guile, Tcl, Python.

**Pull** (the script declares what it wants from the host):
CFFI, Racket FFI.

**Neither** (shared runtime): Kawa.

MScheme is firmly push-based, but its push is *automated* via sstubgen.
The programmer adds `SchemeStubs("InterfaceName")` to the makefile,
and the build system generates and compiles all binding code.  The
effect feels almost like Kawa's shared runtime: everything visible in
M3 interfaces becomes visible in Scheme.

### 4.6 Expressiveness at the Boundary

MScheme's design allows patterns that are difficult or impossible in
most other systems:

**M3 objects as first-class Scheme data**.  A Scheme list can contain
M3 `TextSeq.T` objects alongside Scheme pairs and numbers.  `map`,
`filter`, and `for-each` work on these mixed lists.  The M3 objects
participate in Scheme's structural equality and serialization (via
`write` and pickling).

**Scheme closures as M3 callbacks**.  The sstubgen documentation
describes how M3 object methods can be overridden with Scheme lambdas,
enabling a pattern where M3 objects have individual methods replaced
at runtime -- something M3 itself cannot do (methods are fixed at
object allocation time).

**Runtime discovery**.  Scheme code can enumerate all registered M3
types, query their supertypes, and list available operations.  This
enables Scheme-level metaprogramming over the M3 type system --
writing generic operations that work on any M3 object based on
runtime type introspection.

---

## 5. Positioning in the Design Space

Plotting the systems on two axes -- *interop cost* (per-call overhead)
vs. *setup cost* (how much binding code must be written) -- reveals
three clusters:

```
                    low setup cost
                         |
            Kawa  *      |
                         |
      MScheme  *         |
                         |
  -------------------------------------
  low interop cost       |       high interop cost
                         |
                         |      * Guile
              * CFFI     |      * Lua
              * Racket   |      * Tcl
                         |      * Python/C
                         |
                    high setup cost
```

Kawa achieves the ideal: zero setup, zero interop cost.  But it
requires both languages to run on the JVM.

MScheme comes closest to Kawa's position among systems where the host
language has its own native runtime.  The shared GC eliminates interop
cost for value passing, and sstubgen eliminates setup cost for binding
generation.  The remaining cost is method dispatch (atom comparison),
which is comparable to Lua's metatable lookup and far cheaper than
Python's MRO traversal.

The C-hosted systems (Lua, Guile, Tcl, Python) all pay for the
impedance mismatch between a non-GC host and a GC'd guest.  CFFI and
Racket minimize setup cost on the host side (the C library is
unmodified) but pay for it on the scripting side (manual declarations)
and at every call boundary (marshaling).

---

## 6. Historical Context

MScheme was written in 2007-2008 and has been used in financial
trading, scientific computing, and hardware design.  Its design
predates many of the modern FFI systems (Racket's FFI was redesigned
around 2010; Python's stable ABI came in 3.2/2011; Guile's foreign
object types replaced SMOBs in 2.0/2011).

The SSTUBGEN.TXT design document explicitly positions the system's
goal as making Scheme a viable "main program" language for large M3
applications: the libraries are in M3, the orchestration is in Scheme.
This is the same pattern later popularized by Lua in game engines and
Python in scientific computing -- but MScheme achieves it with
significantly less impedance mismatch, thanks to the shared GC and
type system.

The closest historical precedent is the Lisp Machine, where Lisp
*was* the systems language and no interop boundary existed at all.
MScheme approximates this by making the interop boundary as thin as
possible: one type alias (`T = REFANY`) and a code generator that
handles the rest.
