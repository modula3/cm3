# Resurrecting ESC/Modula-3

## What Was ESC?

ESC (Extended Static Checker) was a compile-time program checker developed at
DEC Systems Research Center (later Compaq SRC, then absorbed into HP Labs) from
approximately 1991 to 2002.  It caught common runtime errors --- null
dereferences, array bounds violations, race conditions, locking order
violations --- by generating first-order logical formulas from annotated source
code and discharging them with an automatic theorem prover.

There were two incarnations:

- **ESC/Modula-3** (1991--1998): The original.  Checked Modula-3 programs.
- **ESC/Java** (1998--2002): A rewrite for Java, architecturally identical.

The system had four major components:

```
Modula-3 source + annotations
         |
    [1. Front-end]  Parse M3, extract SPEC pragmas
         |
    [2. Translator]  Convert to guarded commands
         |
    [3. VCG]  Weakest-precondition calculus -> first-order formulas
         |
    [4. Simplify]  Automatic theorem prover
         |
    Warnings with counterexample contexts
```

## What Survives

### Complete: Simplify theorem prover

The Simplify prover is fully intact in `ESC/Simplify/`.  75 Modula-3 modules
implementing Nelson-Oppen combination of decision procedures (congruence
closure, linear arithmetic, partial orders) with pattern-driven quantifier
instantiation.

Reference: Detlefs, Nelson, Saxe.  "Simplify: A Theorem Prover for Program
Checking."  *JACM* 52(3), 2005.  109 pages.

### Complete: Axiom files encoding Modula-3 semantics

These are the "knowledge base" that tells the prover about Modula-3.  Two
versions survive:

**`esc-axioms`** (old ESC/Modula-3 format, uses `|@DUMMY|` extra arguments):
- `select`/`store` (McCarthy's array theory) for heap modeling
- `SUBARRAY`, `ADDR`, `NUMBER` for array operations
- `TYPECODE`, `SUBTYPE`, `SUBTYPE1`, `NARROW` for the type system
- `NEW` with allocation, virginity, and type properties
- `Is$REFANY`, `Is$INTEGER`, `Is$MATHINT`, `Is$ORDINAL` for type predicates
- `MUT_LT`/`MUT_LE`/`MUT_GT`/`MUT_GE` (partial order for lock levels)
- `MEMBER`, `INSERT`, `DELETE`, `sup` for lock-level sets
- `REACH` (graph reachability, 8 axioms) for linked-structure verification
- `DIV`, `MOD`, `MAX`, `MIN`, `FLOAT`, `ROUND` for arithmetic
- `R+`, `R*`, `R<`, `R<=`, `R>`, `R>=` for real arithmetic
- `TERM=`, `TERM#`, `TERM<`, etc. for term-level relational operators
- `Is$DCL` (declared/allocated), `Is$NVIRG` (non-virgin), `SUCCEEDS`
- `CONCAT` for Text.Cat
- `mapFill` for array initialization
- `ORD`, `VAL` for enumeration types
- `SUBSET` for set containment

**`esc.ax`** (newer format, shared with ESC/Java, no dummy arguments):
Same logical content, cleaner syntax.  Also includes `CONCVARSYM` for concrete
variable tracking.

### Complete: Annotated Modula-3 library code

Real ESC/Modula-3 annotations survive in the CM3 standard libraries, written
by the original authors (Detlefs, Nelson, Heydon, McJones).  These are the
actual specifications the lost checker was designed to verify.

**`m3-libs/libm3/src/sequence/Sequence.ig`** --- The richest example.
Full formal specification of the generic Sequence type:
```
<*SPEC VAR Valid: MAP T TO BOOLEAN *>
<*SPEC VAR Data: MAP T TO SEQ[Elem.T]*>

<*SPEC T.addhi(t, x)
       MODIFIES Data[t]
       REQUIRES Valid[t]
       ENSURES NUMBER(Data'[t]) = NUMBER(Data[t])+1
           AND (ALL [i: INTEGER] 0 <= i AND i < NUMBER(Data[t]) IMPLIES
                  Data'[t][i] = Data[t][i])
           AND Data'[t][NUMBER(Data[t])] = x *>
```

**`m3-libs/libm3/src/sequence/SequenceRep.ig`** --- Abstraction functions and
representation invariants linking abstract state to concrete representation:
```
<*SPEC ABSTRACT Seq.Valid[t: Seq.T]:
                Seq.Valid[t] IFF
                      t # NIL AND t.elem # NIL
                  AND NUMBER(t.elem^) > 0
                  AND t.st < NUMBER(t.elem^)
                  AND t.sz <= NUMBER(t.elem^) *>
<*SPEC DEPEND Seq.Data[t: Seq.T]: t.st, t.sz, t.elem, RefArray *>

<*SPEC FUNC m(i: INTEGER, n: INTEGER): INTEGER*>
<*SPEC AXIOM (ALL [i: INTEGER, n: INTEGER] i >= n IMPLIES m(i, n) = i - n) *>
<*SPEC AXIOM (ALL [i: INTEGER, n: INTEGER] i < n IMPLIES m(i, n) = i) *>

<*SPEC ABSTRACT Seq.Data[t: Seq.T]: Seq.Data[t] = Abs(t.elem^, t.st, t.sz) *>
```

**`m3-libs/libm3/src/sequence/Sequence.mg`** --- Loop invariants in the
implementation:
```
<*PRAGMA SPEC, LOOPINV*>
<*SPEC INVARIANT (ALL [t1: Seq.T, t2: Seq.T]
                    t1 # t2 IMPLIES t1.elem # t2.elem) *>
```

**`m3-libs/libm3/src/list/List.ig`** --- Postconditions with freshness:
```
<*SPEC Cons(head, tail)
       ENSURES RES # NIL AND FRESH(RES)
           AND RES.head = head AND RES.tail = tail *>
```

**`m3-libs/m3core/src/thread/Common/Thread.i3`** --- Locking discipline:
```
<* SPEC FUNC MaxLL(m: MUTEX): BOOLEAN *>
<* SPEC AXIOM (ALL [m1, m2: MUTEX]
                   (NOT MaxLL(m1) AND MaxLL(m2) AND m1 # NIL AND m2 # NIL)
                   IMPLIES m1 < m2) *>
<* SPEC Acquire(m) MODIFIES LL
                   REQUIRES sup(LL) < m
                   ENSURES LL' = INSERT(LL, m) *>
<* SPEC Release(m) MODIFIES LL
                   REQUIRES MEMBER(m, LL)
                   ENSURES LL' = DELETE(LL, m) *>
```

**`m3-libs/libm3/src/fmtlex/Fmt.i3`** --- Simple non-null postconditions:
```
<*SPEC Bool(b) ENSURES RES # NIL *>
<*SPEC Int(n, base) ENSURES RES # NIL *>
```

**`ESC/Simplify/prover/src/Simplex.m3`** --- The most heavily annotated file.
13-clause representation invariant, abstract variables with dependencies, and
auxiliary predicates.

**Juno-2 (`m3-ui/juno-2/`)** --- Multiple files with locking and tool
synchronization annotations.

### Complete: ESC/Java source code

The Java version survives at [GaloisInc/ESCJava2](https://github.com/GaloisInc/ESCJava2)
(3,286 commits, 86.5% Java).  Architecturally identical to ESC/Modula-3 and
serves as the primary reference for reconstruction.

### Lost: The ESC/Modula-3 front-end and VCG

The verification condition generator (~34,000 lines of Modula-3) was never
released by HP.  The 2011 commit message adding Simplify to the CM3 repo says:

> "It's in the ESC subdirectory in the hope that HP will release the rest of
> ESC soon."

That never happened.

## The ESC/Modula-3 Annotation Language

From the surviving annotated code, we can reconstruct the complete annotation
language.  Annotations appear as Modula-3 pragmas delimited by `<*` and `*>`.

### Pragma declarations

```
<*PRAGMA SPEC*>         (* Enable SPEC pragma in this compilation unit *)
<*PRAGMA SPEC, LOOPINV*>  (* Enable both SPEC and LOOPINV *)
```

### Specification constructs

| Construct | Syntax | Purpose |
|-----------|--------|---------|
| Ghost variable | `SPEC VAR name: type` | Abstract state not in code |
| Mapped variable | `SPEC VAR V: MAP T TO U` | Per-object abstract state |
| Sequence variable | `SPEC VAR D: MAP T TO SEQ[E]` | Abstract sequences |
| Precondition | `SPEC proc(args) REQUIRES P` | Caller obligation |
| Postcondition | `SPEC proc(args) ENSURES P` | Callee guarantee |
| Frame condition | `SPEC proc(args) MODIFIES V[x]` | What may change |
| Abstraction function | `SPEC ABSTRACT M.V[t: T]: expr` | Concrete-to-abstract map |
| Abstraction dependency | `SPEC DEPEND M.V[t: T]: fields` | What abstract var depends on |
| Specification function | `SPEC FUNC f(args): type` | Spec-only function |
| Axiom | `SPEC AXIOM (ALL [x: T] P)` | Logical axiom |
| Object invariant | `SPEC INVARIANT P` | Must hold at stable states |
| Representation invariant | `SPEC REP P` | Rep invariant for REVEAL |
| Auxiliary predicate | `SPEC PRED name(args): expr` | Named predicate |
| Loop invariant | `LOOPINV P` | Inductive loop condition |

### Expression language

- `RES` --- return value of the enclosing function
- `V'[x]` --- post-state value (primed variable)
- `FRESH(x)` --- x is newly allocated
- `ALL [x: T] P` --- universal quantification
- `NUMBER(a)` --- array/sequence length
- `SUBARRAY(a, i, n)` --- subarray
- Standard M3 operators: `AND`, `OR`, `NOT`, `IMPLIES`, `IFF`, `#`, `=`
- Set operations: `INSERT`, `DELETE`, `MEMBER`, `sup`
- Lock level: `LL` (implicit ghost variable for current lock set)

## Publications

### Core ESC papers

1. **SRC-RR-159** (1998).  Detlefs, Leino, Nelson, Saxe.  "Extended Static
   Checking."  The definitive technical report.
   [PDF](https://prolangs.cs.vt.edu/refs/docs/SRC-159.pdf)

2. **CC'98**.  Leino, Nelson.  "An Extended Static Checker for Modula-3."
   The published conference paper.

3. **Leino (2001)**.  "Extended Static Checking: A Ten-Year Perspective."
   Retrospective in *Informatics -- 10 Years Back, 10 Years Ahead*, LNCS 2000.

4. **PLDI'02**.  Flanagan, Leino, Lillibridge, Nelson, Saxe, Stata.
   "Extended Static Checking for Java."  The ESC/Java paper.

### Verification condition generation (critical for reconstruction)

5. **POPL'01**.  Flanagan, Saxe.  "Avoiding Exponential Explosion: Generating
   Compact Verification Conditions."  The key VCG algorithm.
   [PDF](https://users.soe.ucsc.edu/~cormac/papers/popl01.pdf)

6. **SRC-TN-1999-002**.  Leino, Saxe, Stata.  "Checking Java Programs via
   Guarded Commands."  Defines the guarded-command intermediate language.

7. **MSR-TR-2004-34**.  Leino.  "Efficient Weakest Preconditions."  Extends
   the compact VC work.

### Annotation and specification methodology

8. **SRC-TN-2000-002**.  Leino, Nelson, Saxe.  "ESC/Java User's Manual."
   20+ annotation types documented.
   [PDF](https://bitsavers.trailing-edge.com/pdf/dec/tech_reports/SRC-TN-2000-002.pdf)

9. **OOPSLA'98**.  Leino.  "Data Groups: Specifying the Modification of
   Extended State."  Solves the frame problem.

10. **SRC-RR-156** (1998).  Detlefs, Leino, Nelson.  "Wrestling with Rep
    Exposure."

11. **SRC-RR-160 / TOPLAS 2002**.  Leino, Nelson.  "Data Abstraction and
    Information Hiding."  Abstraction dependencies.

12. **SRC-RR-161 / TAPSOFT'97**.  Abadi, Leino.  "A Logic of Object-Oriented
    Programs."

### Theorem prover

13. **JACM 2005 / SRC-RR-178**.  Detlefs, Nelson, Saxe.  "Simplify: A
    Theorem Prover for Program Checking."

14. **TOPLAS 1979**.  Nelson, Oppen.  "Simplification by Cooperating Decision
    Procedures."  Foundation for Simplify.

15. **JACM 1980**.  Nelson, Oppen.  "Fast Decision Procedures Based on
    Congruence Closure."

### Annotation inference

16. **FME 2001 / SRC-TN-2000-003**.  Flanagan, Leino.  "Houdini, an
    Annotation Assistant for ESC/Java."
    [PDF](https://users.soe.ucsc.edu/~cormac/papers/fme01.pdf)

### Successor systems

17. **CASSIS 2004**.  Cok, Kiniry.  "ESC/Java2: Uniting ESC/Java and JML."
    Source at [GaloisInc/ESCJava2](https://github.com/GaloisInc/ESCJava2).

### Online archives

- HP Labs SRC reports: `https://www.hpl.hp.com/techreports/Compaq-DEC/`
- Shiftleft mirror: `https://shiftleft.com/mirrors/www.hpl.hp.com/techreports/Compaq-DEC/`
- Bitsavers: `https://bitsavers.trailing-edge.com/pdf/dec/tech_reports/`
- Flanagan's papers: `https://users.soe.ucsc.edu/~cormac/papers/`

## What Needs to Be Built

The lost component is the front-end: ~34,000 lines that parsed annotated
Modula-3, translated it to guarded commands, and applied weakest-precondition
calculus to produce verification conditions for Simplify.

### Component 1: Annotation parser

Extract `<*SPEC ...*>` and `<*LOOPINV ...*>` pragmas from Modula-3 source.
Parse the specification expression language (quantifiers, primed variables,
ghost variable declarations, abstraction functions).

**Approach**: The m3tk (Modula-3 Toolkit, at `m3-tools/m3tk/`) already parses
Modula-3 and preserves pragmas.  The pragma text needs a secondary parser for
the specification sub-language.

### Component 2: Guarded command translator

Convert Modula-3 procedure bodies + annotations into a guarded command
intermediate language.  This is described precisely in Leino/Saxe/Stata
SRC-TN-1999-002.

The guarded command language has:
- `ASSUME P` --- assume predicate
- `ASSERT P` --- check predicate (generates VC obligation)
- `S1 ; S2` --- sequential composition
- `S1 [] S2` --- nondeterministic choice
- `VAR x IN S` --- local variable introduction
- Assignment as `ASSUME x' = e`

Each Modula-3 construct maps to guarded commands:
- `IF c THEN s1 ELSE s2` becomes `(ASSUME c ; T(s1)) [] (ASSUME NOT c ; T(s2))`
- `WHILE c DO s` becomes loop with `ASSERT loopinv` and `HAVOC` modified vars
- Method call becomes `ASSERT pre ; HAVOC modifies ; ASSUME post`
- `NEW(T)` becomes `HAVOC r ; ASSUME r # NIL AND FRESH(r) AND TYPECODE(r) = T`

### Component 3: Verification condition generator

Apply Dijkstra's weakest-precondition calculus (or the Flanagan-Saxe compact
variant) to transform guarded commands into first-order formulas.

Key insight from Flanagan-Saxe POPL'01: naive wp generates exponentially large
formulas.  Their "passification" technique (converting assignments to
assumptions about fresh variables) produces formulas that are worst-case
quadratic, near-linear in practice.

### Component 4: Prover interface / error reporting

Serialize verification conditions as S-expressions for Simplify (or SMT-LIB
for Z3).  Parse prover output.  Map counterexample contexts back to source
locations and produce human-readable warnings.

## Why mscheme Is the Right Tool

The verification condition generator is fundamentally a symbolic
transformation engine: it walks ASTs, collects annotations, rewrites programs
as logical formulas, and serializes them as S-expressions.  This is exactly
what Scheme excels at.

**mscheme** (at `m3-scheme/mscheme/`) provides:

- **Zero-overhead M3 interop**: Scheme values are literally M3 `REFANY`.  No
  marshaling, no FFI, no copying.  M3 objects pass through Scheme unchanged.
- **Automatic stub generation** (`sstubgen`): Reads M3 interface files via
  m3tk and generates bidirectional call stubs.  This gives Scheme code free
  access to m3tk's parsed ASTs.
- **S-expression native**: The output format for Simplify *is* S-expressions.
  Scheme produces them as naturally as breathing.
- **Pattern matching and rewriting**: Quasiquote/unquote and macros are ideal
  for building and transforming logical formulas.
- **Runtime type introspection**: Can query M3's runtime type system, useful
  for generating type-specific axioms.
- **Production quality**: Used in financial trading, no known bugs.

The architecture would be:

- **Modula-3**: Simplify prover, m3tk parsing, core data structures
- **mscheme**: Annotation parsing, guarded command translation, VCG, formula
  serialization, error reporting orchestration
- **Scheme axiom files**: The M3 semantics axioms (already in S-expression
  form)

This division plays to each language's strengths: M3 for the computation-heavy
prover and parser, Scheme for the symbolic rewriting pipeline.
