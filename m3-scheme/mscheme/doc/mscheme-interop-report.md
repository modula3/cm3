# MScheme: Modula-3/Scheme Interoperability and Comparable Systems

Mika Nyström and Claude (Anthropic)\
March 3, 2026

## 1. Introduction

MScheme is a Scheme interpreter written in Modula-3, shipped with a stub
generator (`sstubgen`) that processes Modula-3 interface files and
produces bidirectional bindings.  It originated as a line-for-line
translation of Peter Norvig's JScheme (Java) into Modula-3.  The key insight
that makes JScheme's Java/Scheme interoperability seamless is that
if the host language has a universal reference type, then every
host-language value already *is* a valid Scheme value -- no wrapping
or boxing is needed.  In Java that type is `java.lang.Object`; in
Modula-3 it is `REFANY`.  MScheme carries this principle into the
Modula-3 world.

This report describes MScheme's interoperability architecture in
detail, then compares it to eight other embedding systems that solve
similar problems in different ways.

### A taste of MScheme

Before diving into the architecture, here is real code from `cspc`,
the CSP compiler built on MScheme.  This is Scheme code running
inside a Modula-3 program:

```scheme
;; Read an environment variable -- Env is an M3 module
(define *m3utils* (Env.Get "M3UTILS"))

;; Create M3 BigInt objects -- BigInt.T <: REFANY, so they are
;; ordinary Scheme values
(define a (BigInt.New 12))
(define b (BigInt.New 42))
(BigInt.Equal a b)          ;=> #f
(BigInt.Format a 10)        ;=> "12"

;; M3 procedures used as first-class Scheme values:
;; BigInt.Equal and BigInt.Compare are passed directly to lambda
(define *boolean-binops*
  (list
    `(== integer ,BigInt.Equal)
    `(<  integer ,(lambda (a b) (< (BigInt.Compare a b) 0)))
    `(>  integer ,(lambda (a b) (> (BigInt.Compare a b) 0)))))

;; Format an integer literal by calling M3's Fmt.Int
(define (format-int-literal x)
  (Fmt.Int (BigInt.ToInteger x) 10))

;; Create an M3 object, call its methods from Scheme
(define the-driver
  (obj-method-wrap
    (new-modula-object 'CspCompilerDriver.T)
    'CspCompilerDriver.T))
(the-driver 'init "hello.procs")
(the-driver 'getProcTypes)  ;=> a TextSeq.T (M3 sequence object)
```

Every value here -- the `BigInt.T` integers, the `TEXT` strings, the
`CspCompilerDriver.T` object -- is a native Modula-3 heap object.
None of them are wrapped, copied, or marshaled.  They are Scheme
values because every M3 reference type already satisfies
`SchemeObject.T = REFANY`.  The procedure names (`BigInt.Equal`,
`Fmt.Int`, etc.) were generated automatically by `sstubgen` from
the corresponding Modula-3 interface files at build time.

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
| Any M3 ref   | Itself -- already `REFANY`                               |

Runtime type discrimination uses Modula-3's `TYPECASE` (a type-safe
downcast), `ISTYPE`, and `TYPECODE` -- the same mechanisms the M3
runtime provides for all M3 code.  No parallel type system is needed.

Modula-3 distinguishes *reference types* (heap-allocated, traced by
the GC) from *value types* (unboxed scalars, records, arrays).
Reference types pass through the Scheme boundary unchanged, but
value types must be boxed into reference types.  The sstubgen-
generated stubs handle this automatically:

| M3 value type   | Scheme representation        | Conversion                         |
|-----------------|------------------------------|------------------------------------|
| INTEGER          | `SchemeLongReal.T`           | `FromI` / `Int` with range check   |
| CARDINAL         | `SchemeLongReal.T`           | `FromI` / `Int` with bounds check  |
| LONGINT          | `SchemeLongReal.T`           | `FromI` / `Int`                    |
| REAL             | `SchemeLongReal.T`           | `FromLR(FLOAT(x, LONGREAL))`      |
| LONGREAL         | `SchemeLongReal.T`           | `FromLR` / `FromO`                 |
| EXTENDED         | `SchemeLongReal.T`           | `FromLR(FLOAT(x, LONGREAL))`      |
| BOOLEAN          | `SchemeBoolean.T`            | `Truth` / `TruthO`                 |
| CHAR             | `SchemeChar.T` (`REF CHAR`)  | Cached; all 256 values preallocated|
| TEXT             | `SchemeString.T`             | `FromText` / `ToText`              |
| Enumeration      | `SchemeLongReal.T`           | As ordinal index (0, 1, 2, ...)    |
| Subrange         | `SchemeLongReal.T`           | As base type, with bounds check    |

All six numeric types collapse to a single Scheme type (`REF
LONGREAL`).  Common values (0.0, 1.0, -1.0, 2.0) are cached to
reduce allocation.  The `SchemeChar` module preallocates all 256
character values at initialization.

Records and arrays require more elaborate treatment because they are
*aggregate* value types.  In Modula-3, a `RECORD` is a stack-allocated
or field-embedded aggregate (like a C struct) and an `ARRAY` is a
contiguous block of elements with fixed bounds.  Neither is a
reference -- neither can be `REFANY`.  Rather than boxing them into
opaque `REF` wrappers (which would make the contents inaccessible to
Scheme), sstubgen *decomposes* them into Scheme data structures:

**Records become association lists.**  Each field becomes a
`(name . value)` pair, with the field value converted recursively.
A record `RECORD x: INTEGER; y: REAL END` with values `x=3, y=1.5`
becomes `((x . 3.0) (y . 1.5))`.  The generated `ToScheme` converter
iterates over fields; the `ToModula` converter walks the alist,
matching field names by symbol identity and assigning back into a
stack-allocated M3 record:

```modula3
(* Generated by sstubgen for RECORD x: INTEGER; y: REAL END *)
PROCEDURE ToModula_MyRecord(x: SchemeObject.T): MyRecord =
  VAR res: MyRecord; p := SchemePair.Pair(x);
  BEGIN
    WHILE p # NIL DO
      IF    sym_x = First(p.first) THEN res.x := Int(Rest(p.first))
      ELSIF sym_y = First(p.first) THEN res.y := Float(Rest(p.first))
      END;
      p := SchemePair.Pair(p.rest)
    END;
    RETURN res
  END;
```

**Fixed arrays become lists of (index, element) pairs.**  An `ARRAY
[0..2] OF INTEGER` with values `{10, 20, 30}` becomes `((0 . 10.0)
(1 . 20.0) (2 . 30.0))`.  The `ToModula` converter also accepts a
simplified form without explicit indices -- a plain list `(10.0 20.0
30.0)` -- assigning elements to successive indices from `FIRST` to
`LAST`.  The generated converter handles both forms:

```modula3
(* Generated by sstubgen for ARRAY [0..2] OF INTEGER *)
PROCEDURE ToModula_A(x: SchemeObject.T): ARRAY [0..2] OF INTEGER =
  VAR res: ARRAY [0..2] OF INTEGER; p := SchemePair.Pair(x);
  BEGIN
    IF p # NIL AND NOT ISTYPE(p.first, SchemePair.T) THEN
      (* Simple form: (10 20 30) *)
      FOR i := FIRST(res) TO LAST(res) DO
        res[i] := Int(p.first); p := SchemePair.Pair(p.rest)
      END
    ELSE
      (* Indexed form: ((0 . 10) (1 . 20) (2 . 30)) *)
      WHILE p # NIL DO
        WITH desc = SchemePair.Pair(p.first) DO
          res[Int(desc.first)] := Int(desc.rest)
        END;
        p := SchemePair.Pair(p.rest)
      END
    END;
    RETURN res
  END;
```

**Multidimensional arrays** are nested.  Modula-3's `ARRAY [0..2],
[0..3] OF REAL` is equivalent to `ARRAY [0..2] OF ARRAY [0..3] OF
REAL` -- an array of arrays.  Sstubgen generates converters
recursively: the outer array's element converter is itself the
converter for the inner array.  In Scheme, this becomes a list of
lists: `((0 . ((0 . 1.0) (1 . 2.0) ...)) (1 . ((0 . 3.0) ...)) ...)`.

**Open arrays** (`ARRAY OF T`, with no fixed bounds) cannot exist as
bare value types -- they exist only as `REF ARRAY OF T` or as formal
parameters.  As `REF` types, they are already `REFANY` and can be
passed through the Scheme boundary directly; sstubgen generates
converters that treat them as Scheme vectors or lists depending on
context.

The key design choice is that sstubgen *decomposes* value types into
Scheme-native structures rather than wrapping them in opaque
references.  This means Scheme code can inspect and construct M3
records and arrays using ordinary Scheme operations (`car`, `cdr`,
`assq`), at the cost of a copy at each boundary crossing.

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
the system's most distinctive feature.

Sstubgen derives directly from the Network Objects stub generator
(`m3-comm/stubgen`) designed by Susan Owicki and Ted Wobber for
Birrell et al.'s distributed object system (SRC Report 115, 1994).
It is not merely modeled on the network objects stubgen -- it shares
code.  Several source files are identical between the two packages
(`FRefRefTbl`, `TypeNames`, `ValueProc`), and others (`AstToType`,
`Type`, `Value`, `StubGenTool`, `StubUtils`) are extended copies.
Both tools are `M3ToolFrame` extensions that hook into the cm3
compiler pipeline: they walk the M3 AST using the M3 Toolkit (`m3tk`)
from Xerox PARC (Mike Jordan, 1992), extract type information via an
`AstToType` module, and generate code from the type signatures.  The
network objects stubgen generates marshalling and unmarshalling
procedures for remote method invocation; sstubgen adds
`TypeTranslator` and `ValueTranslator` modules that generate
`ToScheme`/`ToModula` converters and `CallStub` wrappers for Scheme
interoperability.

M3tk is a comprehensive Modula-3 front-end toolkit providing a
generic AST framework, parser, semantic analyzer, and support for
building compiler extensions.  It was written by Mick Jordan,
originally at Olivetti Research California and later at DEC SRC, and
described in his 1990 paper "An Extensible Programming Environment
for Modula-3" (ACM SDE 4).  It is organized into sub-packages
(`m3tk-ast`, `m3tk-syn`, `m3tk-sem`, `m3tk-fe`, etc.) and is used
by both stubgen and sstubgen, as well as by the language server and
other M3 development tools.

The design goal, as stated in the sstubgen documentation, is:

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

### 2.6 Call Flow for Interface Procedures

When Scheme code evaluates `(Wr.PutText wr "hello")`, the call passes
through four layers before reaching the actual M3 procedure.  Tracing
the full path:

**1. Evaluation.**  `Scheme.Eval` in `Scheme.m3` evaluates the
operator position.  The symbol `Wr.PutText` resolves to a
`SchemePrimitive.T` object that was registered at startup.  The two
argument expressions are evaluated, producing `SchemeObject.T` values.
For two arguments, the evaluator calls the optimized `apply2` method
directly.

**2. Primitive dispatch.**  `SchemePrimitive.Apply2` validates the
argument count and calls the main dispatcher (`Prims`).  Interface
procedure stubs are registered with IDs above the range of the
built-in primitive enum `P`, so the dispatcher recognizes them as
extensions and calls through to the `ExtDefiner`:

```modula3
IF t.id > ORD(LAST(P)) THEN
  RETURN NARROW(t.definer, ExtDefiner).apply(t, interp, args)
```

**3. Stub lookup.**  `ExtDefiner.apply` looks up the procedure ID in
a hash table, finds a `PrimRec` containing the qualified name
(`Wr`, `PutText` as atoms), and calls
`SchemeProcedureStubs.CallStubApply`.  This function walks a linked
list of registered stubs, matching the interface name and item name
by atom identity (pointer comparison), and calls the matching stub
procedure.

**4. Stub execution.**  The stub procedure -- generated by sstubgen
at build time -- unpacks arguments from the Scheme pair list, converts
each from `SchemeObject.T` to the appropriate M3 type using the
`ToModula` functions, calls the real M3 procedure, converts the result
back via `ToScheme`, and handles any M3 exceptions:

```modula3
PROCEDURE Stub_Wr_PutText(interp: Scheme.T;
    args, excH: SchemeObject.T): SchemeObject.T RAISES {Scheme.E} =
  VAR wr  := NARROW(First(args), Wr.T);           (* REFANY -> Wr.T *)
      txt := SchemeString.ToText(Second(args));    (* SchemeString -> TEXT *)
  BEGIN
    TRY Wr.PutText(wr, txt);                      (* actual M3 call *)
        RETURN SchemeBoolean.True()
    EXCEPT Wr.Failure(e) =>
        RAISE Scheme.E("Wr.Failure: " & ...)
    END
  END;
```

For the `wr` argument, which is already a traced reference (`Wr.T` is
an object type), the conversion is just a `NARROW` -- a type-checked
pointer cast, no data copying.  For the `txt` argument, `ToText`
converts the mutable Scheme string (`REF ARRAY OF CHAR`) to an
immutable M3 `TEXT`.  The return value is a Scheme boolean.

The entire path -- from `Eval` to the actual `Wr.PutText` call -- is
native M3 procedure calls.  There is no interpreter loop, no bytecode
dispatch, and no reflection.  The overhead beyond a direct M3 call is:
argument list traversal (walking `SchemePair` cells), type conversion
(one `NARROW` + one `ToText`), and stub lookup (atom comparison in a
linked list, typically short).

### 2.7 Runtime Type Introspection from Scheme

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

### 2.8 Concurrency Model

MScheme is designed for multi-threaded M3 programs.  The key design
rule: a single `Scheme.T` interpreter instance is single-threaded, but
multiple interpreters may share a global environment (which uses
synchronized access).  Environments come in `Safe` (synchronized) and
`Unsafe` (unsynchronized) variants.

### 2.9 C Libraries via M3 as Glue

MScheme can be used even when the goal is simply to call C code from
Scheme.  Modula-3 serves as a type-safe glue layer: a thin M3
interface wraps the C functions, and sstubgen auto-exports that
interface to Scheme.  The programmer never writes Scheme-specific
binding code.

A concrete example is the `Mpz` module in the `m3utils` tree, which
wraps the GNU Multiple Precision Arithmetic Library (GMP) for use
from the CSP compiler's Scheme environment.  The architecture has
three layers:

**Layer 1: C binding.**  `MpzP.i3` declares GMP functions as
`EXTERNAL` procedures with C calling convention:

```modula3
(* MpzP.i3 -- raw C interface *)
<*EXTERNAL "__gmpz_add"*>
PROCEDURE c_add(f0: MpzPtrT; f1: MpzPtrT; f2: MpzPtrT);

<*EXTERNAL "__gmpz_mul"*>
PROCEDURE c_mul(f0: MpzPtrT; f1: MpzPtrT; f2: MpzPtrT);

<*EXTERNAL "mpz_format_decimal"*>
PROCEDURE format_decimal(z: MpzPtrT): Ctypes.const_char_star;
```

A small C file (`MpzC.c`) provides helper functions for formatting
via `gmp_asprintf`.

**Layer 2: Type-safe M3 wrapper.**  `Mpz.i3` defines an opaque
traced type `T <: REFANY` that wraps the raw `mpz_t`.  The
implementation (`MpzOps.m3`) extracts the address and calls through
to layer 1:

```modula3
(* Mpz.i3 -- public interface *)
TYPE T <: REFANY;
PROCEDURE New(): T;
PROCEDURE add(f0: T; f1: T; f2: T);
PROCEDURE Format(t: T; base := FormatBase.Decimal): TEXT;
```

```modula3
(* MpzOps.m3 -- implementation *)
PROCEDURE add(f0: T; f1: T; f2: T) =
  BEGIN P.c_add(ADR(f0.val), ADR(f1.val), ADR(f2.val)) END;
```

Because `T <: REFANY`, `Mpz.T` values are traced by the M3 garbage
collector.  The opaque type prevents Scheme (or M3) code from
accessing the raw `mpz_t` memory directly -- only the operations
declared in `Mpz.i3` are available.

Resource cleanup uses Modula-3's `WeakRef` facility.  When `Mpz.New`
creates a new wrapper object, it registers a weak-reference callback
via `WeakRef.FromRef`:

```modula3
(* Mpz.m3 *)
PROCEDURE New() : T =
  VAR res := NEW(T);
  BEGIN
    P.c_init(LOOPHOLE(ADR(res.val), P.MpzPtrT));
    EVAL WeakRef.FromRef(res, CleanUp);
    set_ui(res, 0);
    RETURN res
  END New;

PROCEDURE CleanUp(<*UNUSED*>READONLY w : WeakRef.T; r : REFANY) =
  BEGIN
    WITH this = NARROW(r, T) DO
      P.c_clear(ADR(this.val))
    END
  END CleanUp;
```

When the GC determines that an `Mpz.T` object is unreachable, it
schedules the `CleanUp` callback, which calls GMP's `mpz_clear` to
release the C-level memory.  No explicit deallocation is needed
from either M3 or Scheme code -- the `Mpz.T` values can be passed
around, stored in lists, and forgotten, just like any other Scheme
value.  This is the same `WeakRef` mechanism that Modula-3 programs
use for any C resource: FFTW plans, file descriptors, database
handles.

**Layer 3: Automatic Scheme export.**  Adding `SchemeStubs("Mpz")` to
the m3makefile causes sstubgen to generate Scheme bindings for every
procedure in `Mpz.i3`.  From Scheme:

```scheme
(define a (Mpz.New))
(define b (Mpz.NewInt 42))
(define c (Mpz.New))
(Mpz.add c a b)
(Mpz.Format c)    ;=> "42"
```

The `Mpz.T` values flowing through Scheme are the same traced M3
heap objects -- no copying, no marshaling.  The GC handles their
lifetime.  The C library's `mpz_t` storage is embedded inside the
M3 object's record fields and is deallocated when the M3 object is
collected.

This pattern -- C library, M3 interface, sstubgen -- is general.
The same approach could wrap SQLite, zlib, OpenSSL, or any C library.
Modula-3's `EXTERNAL` pragma provides the C binding; its type system
provides safety; sstubgen provides the Scheme bridge.  The result is
that MScheme functions as a practical Scheme-with-C-FFI system, even
though its design is not about C interoperability at all.

The `Mpz` module is itself partly auto-generated: a Scheme script
(`make-mpz.scm`) reads GMP's function prototypes and emits `MpzP.i3`,
`Mpz.i3`, and `MpzOps.m3` -- over 200 functions.  This is a second
level of code generation: Scheme generates M3 code, then sstubgen
generates Scheme stubs from that M3 code, closing the loop.

### 2.10 Summary of Key Properties

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

**Key trade-offs**: Zero-cost interoperability when types are known, but
reflection fallback is slow.  Kawa must work within JVM constraints
(no true tail calls, no first-class continuations without
trampolining).  The shared-runtime model is fundamentally different
from all other systems here.

### 3.6 JScheme (Scheme in Java)

**Core representation**: `java.lang.Object`.  JScheme is a tree-walking
Scheme interpreter written in Java.  In Norvig's original (1998), all
numbers are `java.lang.Double`, symbols are interned `java.lang.String`,
strings are `char[]` (for mutability), pairs are a custom `Pair` class
with `Object first, rest` fields, and `null` represents the empty list.
The later Anderson/Hickey version (2003+) switched numbers to `Integer`
or `Double` as appropriate, symbols to a custom `Symbol` class, and
strings to `java.lang.String`.

**Wrapping Java objects**: No wrapping needed.  Any Java object is
already an `Object` and therefore a valid Scheme value.  The evaluator
treats anything that is not a symbol and not a pair as self-evaluating,
so a `java.awt.Frame` or `java.util.HashMap` placed into a Scheme
variable simply evaluates to itself.

**Method dispatch**: Entirely reflection-based.  The later JScheme
provides a dot-notation syntactic sugar parsed at read time:

```scheme
(import "java.awt.Font")
(Font. "Serif" Font.BOLD$ 24)     ; constructor (trailing dot)
(.setFont component font)         ; instance method (leading dot)
(.name$ obj)                      ; field access (trailing $)
(Math.round 123.456)              ; static method (dot in middle)
```

These desugar into calls to `JavaMethod`, `JavaConstructor`, or
`JavaField` objects, all of which use `java.lang.reflect` internally.
Method tables are cached per class in hash tables, and overload
resolution follows Java's "most specific applicable method" rule at
runtime.

**Values back to Java**: Scheme values are already `Object`.  Java
code calls `JScheme.call("proc-name", arg1, arg2)` and receives an
`Object` result, narrowing via cast.

**Scheme as Java interface**: The `SchemeInvocationHandler` class uses
`java.lang.reflect.Proxy` to let a Scheme closure implement any Java
interface, so Scheme callbacks work anywhere Java expects an interface
(listeners, `Runnable`, etc.).

**Registration**: `(import "java.awt.*")` makes classes available;
dot-notation symbols are resolved against the import table.  From the
Java side, `JScheme.setGlobalValue("name", obj)` injects values.

**Key trade-offs**: JScheme achieves the same zero-wrapping interoperability as
Kawa -- Java objects *are* Scheme values -- but pays for it with
reflection overhead on every method call.  Kawa compiles to
`invokevirtual` bytecode; JScheme interprets and reflects.  The
simplicity is striking (Norvig's original is ~1700 lines), but the
performance gap is large.  JScheme is the direct ancestor of MScheme
and establishes the key principle that MScheme inherits: when the host
language has a universal reference type and a GC, the embedding
boundary can be made nearly invisible.

### 3.7 Common Lisp / CFFI

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

### 3.8 Racket FFI

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

**Shared heap** (MScheme, JScheme, Kawa): The host language's objects
are directly usable as scripting language values.  No per-call
conversion cost.  The garbage collector sees everything.  This is only
possible when both languages are GC'd and share a runtime (M3 +
MScheme) or a VM (Java + JScheme, Java + Kawa).

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
| JScheme | `instanceof` (runtime)                 | Reflection + cached method table   |
| CFFI    | CLOS `typep` + manual checks           | Generic function dispatch          |
| Racket  | cpointer tag check                     | Direct call (no dispatch)          |

MScheme benefits from M3's runtime type system: `TYPECASE` is a
compiler-supported, type-safe downcast with O(1) performance (the
runtime checks the typecode, which is an integer).  This is
essentially the same mechanism as Java's `instanceof` check, used by
Kawa and JScheme on the JVM.

### 4.3 Automatic Binding Generation

| System  | Auto-generation?       | Input                       | Mechanism                                       |
|---------|------------------------|-----------------------------|------------------------------------------------ |
| MScheme | Yes (`sstubgen`)       | M3 `.i3` interface files    | M3 Toolkit AST parsing + Scheme code generation |
| Lua     | No                     | --                          | Manual C code                                   |
| Guile   | Partial (g-wrap)       | C headers                   | Third-party tool, not standard                  |
| Tcl     | Partial (critcl, SWIG) | C headers                   | Third-party tools                               |
| Python  | Partial (Cython, etc.) | C/C++ headers + annotations | Third-party tools                               |
| Kawa    | Unnecessary            | --                          | Shared JVM class loader                         |
| JScheme | Unnecessary            | --                          | Reflection on JVM classes at runtime            |
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
| JScheme | JVM GC             | Same GC                    | None needed -- unified heap                       |
| CFFI    | CL GC              | Same GC (for CL objects)   | `trivial-garbage:finalize` for foreign ptrs       |
| Racket  | Precise moving GC  | Same GC                    | Special APIs for C extensions holding Racket refs |

The MScheme/M3, Kawa/JVM, and JScheme/JVM systems share a critical
advantage: since both languages use the same garbage collector, there
is no coordination problem.  A Scheme closure that captures an M3 object keeps it alive
automatically.  An M3 data structure containing Scheme pairs is traced
correctly.  No reference counting, no prevent-collection guards, no
weak-reference bridges.

This eliminates an entire class of bugs that plagues C-based embedding
systems.  In CPython, getting reference counts wrong is the single
most common source of C extension bugs.  In Lua, forgetting to anchor
a value on the stack or in the registry causes it to be collected while
C still holds a pointer to it.

The fundamental problem runs deeper than API discipline: C makes
garbage collection nearly impossible to do correctly.  The language
permits pointers to be hidden in ways that are both legal and common
in practice, defeating any collector that tries to find them.

A "conservative" collector like BDW-GC works by scanning the C stack,
registers, and static data segments for bit patterns that look like
they might be heap addresses.  This is inherently a heuristic, and it
fails in both directions.

**False negatives (hidden pointers -- the dangerous case).**  C
allows a program to store a pointer as an integer (`uintptr_t`), XOR
two pointers together (XOR linked lists), offset a pointer by a
constant and recover it later, split a pointer's bytes across
non-adjacent struct fields, send it through a pipe or socket and read
it back, or store it in a memory-mapped file.  In all of these cases,
the pointer exists but does not appear on the stack or in a register
in a form the collector can recognize.  If the collector runs at that
moment, it may conclude the object is unreachable and free it.  The
program then recovers the hidden pointer and dereferences freed
memory.  This is not a contrived scenario -- XOR-linked lists save
memory in embedded systems, pointer tagging is used in language
runtimes (including CPython's own `ob_refcnt` field packing), and
integer casts are routine in serialization and IPC code.

**False positives (pointer lookalikes -- the slow leak).**  On a
32-bit system, roughly 1 in 1000 random integers looks like a valid
heap address.  On 64-bit systems the odds are better but not zero,
especially for programs that process file offsets, hash values, or
pixel data in the gigabyte range.  Each false positive pins a dead
object (and everything it transitively references) in memory
indefinitely.  Programs that run for a long time with large heaps
can see unbounded memory growth from these phantom roots.

**No solution within C.**  The C standard deliberately leaves pointer
representation implementation-defined and permits casts between
pointers and integers.  A conforming C program can hide a pointer in
a way that no scanning algorithm can find.  This is not a bug in the
language -- it is a consequence of C's design as a portable assembly
language with minimal abstraction over the machine.

Languages like Modula-3 and Java avoid this entirely.  In M3, the
compiler emits stack frame descriptors that tell the garbage collector
exactly which stack slots and object fields contain traced references.
The runtime knows the precise layout of every heap object because
allocations go through `NEW`, which records the type.  The collector
does not guess: it follows exactly the traced references and nothing
else.  No false negatives, no false positives, no heuristics.  Java
achieves the same property through the JVM's typed operand stack and
GC maps.  This is what makes the unified-heap architecture of MScheme,
JScheme, and Kawa possible -- the collector's precision is guaranteed
by the language, not hoped for by convention.

### 4.5 Registration Direction: Push vs. Pull

**Push** (the host declares what's available to the script):
MScheme, Lua, Guile, Tcl, Python.

**Pull** (the script declares what it wants from the host):
CFFI, Racket FFI.

**Neither** (shared runtime): Kawa, JScheme.

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

Plotting the systems on two axes -- *per-call cost* (boundary overhead)
vs. *setup cost* (how much binding code must be written) -- reveals
three clusters:

```
                    low setup cost
                         |
            Kawa  *      |
                         |
      MScheme  *         |
                         |       * JScheme
  -------------------------------------
  low per-call cost      |       high per-call cost
                         |
                         |      * Guile
              * CFFI     |      * Lua
              * Racket   |      * Tcl
                         |      * Python/C
                         |
                    high setup cost
```

Kawa achieves the ideal: zero setup, zero per-call cost.  But it
requires both languages to run on the JVM.

JScheme shares Kawa's low setup cost (both live on the JVM, no
bindings to write) but pays a significant per-call cost: every Java
method call goes through reflection, while Kawa compiles to direct
`invokevirtual` bytecode.

MScheme comes closest to Kawa's position among systems where the host
language has its own native runtime.  The shared GC eliminates per-call
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

## 6. Native Code, Not a Virtual Machine

### 6.1 Why This Distinction Matters

JScheme and Kawa achieve glueless interoperability on the JVM.  But the JVM is
a virtual machine: Java source compiles to bytecode, which is then
interpreted or JIT-compiled by the VM at runtime.  The GC, runtime
type information, and universal reference type (`java.lang.Object`)
are services provided by the VM, not by the compiled program itself.

Modula-3 is different.  The cm3 compiler emits native machine code --
x86, x86-64, ARM64, SPARC, PA-RISC, PowerPC -- directly, with no
intermediate bytecode and no virtual machine.  The generated code
runs as ordinary machine instructions.  Yet Modula-3 still provides
precise garbage collection, runtime type discrimination (`TYPECASE`,
`ISTYPE`, `TYPECODE`), and a universal traced reference type
(`REFANY`).  It achieves this by having the compiler emit metadata
alongside the native code: stack frame descriptors that tell the
runtime exactly which stack slots contain traced references, and type
descriptors that record the layout of every heap-allocated type.

This means MScheme's Scheme interpreter runs as native machine code
calling into native machine code, with no VM layer in between.  When
Scheme code calls an M3 method, it is a direct procedure call -- the
same calling convention as any M3-to-M3 call.  When the garbage
collector runs, it traces M3 objects and Scheme pairs with the same
algorithm, using the same compiler-generated stack maps.  There is no
bytecode interpreter, no JIT warmup, no tiered compilation.

### 6.2 How Rare Is This Architecture?

The combination of properties that makes MScheme possible is unusual:
(1) ahead-of-time compilation to native code, (2) precise tracing GC,
(3) runtime type information with a universal reference type, and
(4) an embedded interpreter for a *different* language that shares the
host's values and GC directly, with no marshaling.

Very few languages in history have provided (1)-(3) simultaneously.
The lineage is essentially Mesa (native code, no GC) to Cedar (added
GC, `REF ANY`, and RTTI at Xerox PARC in the early 1980s) to
Modula-2+ (DEC SRC) to Modula-3.  Cedar had the infrastructure to
support an MScheme-like system -- Paul Rovner's 1985 technical report
documents the addition of `REF ANY`, runtime types, and traced GC to
Mesa -- but no one built a full scripting interpreter on top of it.
The debugger could evaluate expressions in a stopped frame, but that
is not the same as a general-purpose embedded language.

A survey of other natively-compiled GC'd languages shows why MScheme's
architecture is hard to replicate:

**Common Lisp (SBCL, CMUCL, etc.).**  The strongest analogue.  SBCL
compiles to native x86-64 and ARM64 code and has a precise generational
GC.  Its interpreter (when enabled) shares the exact same heap, the
same tagged-pointer value representation, and the same collector as
compiled code.  A Scheme interpreter written in Common Lisp would be
architecturally identical to MScheme.  However, CL's interpreter is
self-hosting -- it interprets Common Lisp, not a different language.
The system is designed for mixed-mode execution of a single language,
not for embedding a scripting language in a systems language.

**Smalltalk (StrongTalk, Cog/Spur).**  JIT-compiles methods to native
code; the interpreter and compiled code share a single object space
with no marshaling boundary.  Again self-hosting: the interpreted
and compiled languages are the same Smalltalk.  Compilation is JIT
(at runtime), not AOT (ahead of time).

**Eiffel (EiffelStudio's Melting Ice).**  Compiles Eiffel to C and
thence to native code ("frozen" code), but also maintains bytecode-
interpreted "melted" code for rapid development.  Frozen and melted
code share the same heap and objects.  Self-hosting: the interpreted
language is Eiffel.

**Julia.**  JIT-compiles to native code via LLVM; `eval` operates in
the same runtime with the same values.  Self-hosting; JIT, not AOT.

**Go (Yaegi).**  Compiles to native code and has precise GC, but
Yaegi (a Go interpreter written in Go) wraps values in `reflect.Value`
at every boundary -- a form of marshaling.  Go also lacks a universal
reference type; `interface{}` is a two-word pair, not a traced
pointer.

**Oberon.**  Compiles to native code and has GC, but lacks a universal
reference type and has no embedded interpreter -- it uses dynamic
loading of compiled modules instead.

**D, Dylan, OCaml.**  All compile to native code and have GC, but
none has an embedded interpreter for a different language that shares
the host heap without marshaling.  OCaml's bytecode toplevel shares
the heap with loaded `.cmo` modules, but the native-code compiler
path has no analogous mechanism for a different language.

The pattern that emerges is that self-hosting interpreters in
natively-compiled languages are not uncommon (CL, Smalltalk, Eiffel,
Julia all have them), but an interpreter for a *different* language
that shares the host's heap with zero marshaling overhead is rare.
The obstacle is not theoretical -- any language with GC, RTTI, and a
universal reference type could support it -- but practical: very few
such languages exist, and in those that do, the idea has apparently
occurred only to the builders of MScheme (and its ancestor JScheme,
which works on the JVM rather than native code).

MScheme appears to be the only system where an ahead-of-time compiled,
statically-typed systems language hosts a dynamically-typed interpreted
scripting language with truly zero marshaling overhead -- no VM, no
bytecode, no reflection layer, no value conversion, just `T = REFANY`
and direct procedure calls.

---

## 7. Historical Context

Peter Norvig published JScheme in 1998 as a compact demonstration
that a useful Scheme interpreter could be written in ~1700 lines of
Java.  The key architectural insight -- `java.lang.Object` as the
universal Scheme value -- was implicit in the choice of Java as the
implementation language.  Ken Anderson (BBN) and Tim Hickey (Brandeis)
later expanded JScheme with dot-notation syntax, overload resolution,
and dynamic proxy support for Java interfaces.

MScheme was written in 2007-2008 as a line-for-line translation of
JScheme into Modula-3.  The translation preserved JScheme's
fundamental principle (`Object` -> `REFANY`) but added two things
JScheme lacks: an automatic stub generator (`sstubgen`) and a runtime
type introspection API.  MScheme has been used in financial trading,
scientific computing, and hardware design.

The MScheme design predates many of the modern FFI systems (Racket's
FFI was redesigned around 2010; Python's stable ABI came in 3.2/2011;
Guile's foreign object types replaced SMOBs in 2.0/2011).

The SSTUBGEN.TXT design document explicitly positions the system's
goal as making Scheme a viable "main program" language for large M3
applications: the libraries are in M3, the orchestration is in Scheme.
This is the same pattern later popularized by Lua in game engines and
Python in scientific computing -- but MScheme achieves it with
significantly less impedance mismatch, thanks to the shared GC and
type system.

The closest historical precedent is the Lisp Machine, where Lisp
*was* the systems language and no interoperability boundary existed
at all.
JScheme approximated this for Java; MScheme approximates it for
Modula-3 -- one type alias (`T = REFANY`) and a code generator that
handles the rest.

---

## Bibliography

### Modula-3

- Cardelli, L., Donahue, J., Glassman, L., Jordan, M., Kalsow, B.,
  and Nelson, G.  "Modula-3 Report (revised)."  DEC Systems
  Research Center, Report 52, November 1989.

- Cardelli, L., Donahue, J., Jordan, M., Kalsow, B., and Nelson, G.
  "The Modula-3 Type System."  In *Conference Record of the Sixteenth
  Annual ACM Symposium on Principles of Programming Languages*,
  pp. 202--212, 1989.

- Cardelli, L.  "Typeful Programming."  DEC Systems Research Center,
  Report 45, May 1989.  Revised version in E.J. Neuhold and
  M. Paul (eds.), *Formal Description of Programming Concepts*,
  Springer-Verlag, 1991.

- Nelson, G. (ed.)  *Systems Programming with Modula-3.*  Prentice
  Hall, 1991.

- Harbison, S.P.  *Modula-3.*  Prentice Hall, 1992.

- Modula3/CM3: Critical Mass Modula-3.
  https://github.com/modula3/cm3

### Network Objects and stub generation

- Birrell, A.D., Nelson, G., Owicki, S., and Wobber, E.  "Network
  Objects."  DEC Systems Research Center, Report 115, February 1994.
  Also in *Proceedings of the 14th ACM Symposium on Operating Systems
  Principles* (SOSP), December 1993; and *Software--Practice and
  Experience*, 25(S4):87--130, December 1995.

- Birrell, A.D., Evers, D., Nelson, G., Owicki, S., and Wobber, E.
  "Distributed Garbage Collection for Network Objects."  DEC Systems
  Research Center, Report 116, December 1993.

### M3 Toolkit (m3tk)

- Jordan, M.  "An Extensible Programming Environment for Modula-3."
  In *Proceedings of the Fourth ACM SIGSOFT Symposium on Software
  Development Environments* (SDE 4), Irvine, California, December
  1990.  *ACM SIGSOFT Software Engineering Notes*, 15(6):66--76.

- Jordan, M. and Robinson, P.  "A Programming Environment for
  Modula-2."  *Software Engineering Journal*, 3(4):119--126, July
  1988.

### MScheme and sstubgen

- MScheme source code: `cm3/m3-scheme/mscheme/`.  In the CM3
  repository.

- SSTUBGEN.TXT: Design document for the Scheme stub generator.
  `cm3/m3-scheme/sstubgen/src/SSTUBGEN.TXT`.

- SchemeM3 README: Embedding guide.
  `cm3/m3-scheme/modula3scheme/src/README`.

### JScheme

- Norvig, P.  "JScheme: Scheme Implemented in Java."
  https://norvig.com/jscheme.html, 1998.

- Norvig, P.  "JScheme: Design Decisions."
  https://norvig.com/jscheme-design.html

- Anderson, K.R., Hickey, T.J., and Norvig, P.  "JScheme."
  https://www.cs.brandeis.edu/~tim/jscheme/main.html.
  Brandeis University / BBN Technologies, 2003.

### Cedar and Modula-3 ancestry

- Rovner, P.  "On Adding Garbage Collection and Runtime Types to
  a Strongly-Typed, Statically-Checked, Concurrent Language."
  Xerox PARC, CSL-84-7, July 1985.

- Lampson, B.W.  "A Description of the Cedar Language."  Xerox
  PARC, CSL-83-15, December 1983.

### Lua

- Ierusalimschy, R., de Figueiredo, L.H., and Celes, W.  *Lua 5.4
  Reference Manual.*  https://www.lua.org/manual/5.4/

- Ierusalimschy, R.  *Programming in Lua*, 4th edition.  Lua.org,
  2016.

- Ierusalimschy, R., de Figueiredo, L.H., and Celes, W.  "The
  Evolution of Lua."  In *Proceedings of the Third ACM SIGPLAN
  Conference on History of Programming Languages* (HOPL III), 2007.

### GNU Guile

- *GNU Guile Reference Manual*, version 3.0.
  https://www.gnu.org/software/guile/manual/

### Tcl/Tk

- Ousterhout, J.K.  *Tcl and the Tk Toolkit.*  Addison-Wesley, 1994.

- *Tcl/Tk Documentation.*  https://www.tcl-lang.org/doc/

### CPython

- *Python/C API Reference Manual.*
  https://docs.python.org/3/c-api/

- *Extending and Embedding the Python Interpreter.*
  https://docs.python.org/3/extending/

### Kawa Scheme

- Bothner, P.  "Kawa -- Compiling Dynamic Languages to the Java VM."
  In *Proceedings of the USENIX Annual Technical Conference*,
  FREENIX Track, 1998.

- *The Kawa Scheme Language.*
  https://www.gnu.org/software/kawa/

### Common Lisp CFFI

- *CFFI: The Common Foreign Function Interface.*
  https://cffi.common-lisp.dev/

- *CFFI User Manual.*
  https://cffi.common-lisp.dev/manual/cffi-manual.html

### Racket FFI

- Barzilay, E. and Orlovsky, D.  "Foreign Interface for PLT Scheme."
  In *Proceedings of the Fifth ACM SIGPLAN Workshop on Scheme and
  Functional Programming*, 2004.

- *The Racket Foreign Interface.*
  https://docs.racket-lang.org/foreign/

### Conservative garbage collection

- Boehm, H.-J. and Weiser, M.  "Garbage Collection in an
  Uncooperative Environment."  *Software--Practice and Experience*,
  18(9):807--820, September 1988.

- Boehm, H.-J., Demers, A.J., and Shenker, S.  "Mostly Parallel
  Garbage Collection."  In *Proceedings of the ACM SIGPLAN '91
  Conference on Programming Language Design and Implementation*,
  pp. 157--164, 1991.

### SBCL

- *SBCL User Manual.*  https://www.sbcl.org/manual/

- Steel Bank Common Lisp source.
  https://github.com/sbcl/sbcl

### Eiffel

- Meyer, B.  *Object-Oriented Software Construction*, 2nd edition.
  Prentice Hall, 1997.

- "Melting Ice Technology."
  https://www.eiffel.org/doc/eiffelstudio/Melting_Ice_Technology

### Smalltalk and StrongTalk

- Bracha, G. and Griswold, D.  "Strongtalk: Typechecking Smalltalk
  in a Production Environment."  In *Proceedings of the ACM
  Conference on Object-Oriented Programming, Systems, Languages and
  Applications* (OOPSLA), pp. 215--230, 1993.

- Ungar, D. and Smith, R.B.  "Self: The Power of Simplicity."  In
  *Proceedings of OOPSLA '87*, pp. 227--241, 1987.

### Oberon

- Wirth, N. and Gutknecht, J.  *Project Oberon: The Design of an
  Operating System and Compiler.*  ACM Press / Addison-Wesley, 1992.
  Revised edition (*The Design of an Operating System, a Compiler,
  and a Computer*), 2013.

### Other systems cited

- Traefik.  "Yaegi: Another Elegant Go Interpreter."
  https://github.com/traefik/yaegi

- "Hint: Runtime Haskell Interpreter."
  https://hackage.haskell.org/package/hint

- Flatt, M., Findler, R.B., and PLT.  *The Racket Guide.*
  https://docs.racket-lang.org/guide/
