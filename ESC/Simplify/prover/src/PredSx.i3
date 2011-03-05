(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 19 21:53:04 PDT 2000 by saxe                     *)
(*      modified on Wed Oct 23 13:44:10 PDT 1996 by detlefs                  *)

(* This interface defines the syntax used to express predicates as
   S-expressions, and declares global atoms used in these S-expressions. *)

INTERFACE PredSx;

IMPORT Atom, Word;
IMPORT AtomSet, AtomRefTbl, RefList;

TYPE T = REFANY (* S-expression *);
CONST Brand = "PredSx";

EXCEPTION Error(TEXT);

(* A "PredSx.T" satisfies the following grammar:

|  formula ::= "(" ( AND | OR )  { formula } ")" |
|              "(" NOT formula ")" |
|              "(" IMPLIES formula formula ")" |
|              "(" EXPLIES formula formula ")" |
|              "(" IFF formula formula ")" |
|              "(" FORALL "(" var* ")" "(" patspec* ")" formula ")" |
|              "(" EXISTS "(" var* ")" "(" patspec* ")" formula ")" |
|              "(" ORDER strictFS nonStrictFS ")" |
|              "(" PROOF formula* ")" |
|              literal 
|
|  literal ::= "(" ( "EQ" | "NEQ" | "<" | "<=" | ">" | ">=" )
|              term term ")" |
|              "(" "DISTINCT" term term+ ")" |
|               "TRUE" | "FALSE" | <propVar>
|
|  term    ::= var | integer | "(" func { term } ")"
|
|  patspec ::= "(" PATS pat* ")" | "(" NOPATS term+ ")"
|  pat     ::= term | "(" MPAT term+ ") | PROMOTE | PLUNGE

"var"'s, "func"'s, and "propVar"'s (propositional variables) are
represented as "Atom.T"'s.

The formula

| (DISTINCT term1 ... termN)

represents a conjunction of distinctions between all pairs of terms in
the list.

The formula

| (ORDER strictFS nonStrictFS)

asserts that the function symbols "strictFS" and "nonStrictFS" are the
strict and non-strict parts, respectively, of a partial order.  That
is, assertions of the form "(EQ (strictFS x y) |@true|)" or "(EQ
(nonStrictFS x y) |@true|)" are satisfiable only if the pair "(x, y)" can
be a member of the corresponding relation.

The formula

| (PROOF form1 ... formN)

is sugar for

| (AND (IMPLIES form1 form2)
|      (IMPLIES (AND form1 form2) form3)
|      ...
|      (IMPLIES (AND form1 ... formN-1) formN))

"func"'s are uninterpreted, except for "+", "-", and "*", which
represent the obvious operations on integers.
*)

PROCEDURE Init();
(* Must be called before using any variables or other procedures of
   this interface.  Is idempotent. *)

PROCEDURE CNF(form: T; neg := FALSE): T RAISES { Error };
(* Return a formula equivalent to "form" (or "(NOT form)", if "neg" is
   "TRUE"), in conjunctive normal form (a conjunction of disjunctions
   of literals.)  The literals in the result are {\it canonicalized.}
   Let "lit" be a literal "[NOT] e R f".  We establish an arbitrary
   ordering on terms; the canonical form of "lit" is an equivalent
   literal of the form "[NOT] s RR t", where "s <= t" under the term
   ordering, and RR is one of "=", ">=", or ">".  Actually, the use of
   the term literal is slightly inaccurate; the disjunctions may
   also have quantified formulae as members.  All such formulas are
   canonicalized to (possibly negated) universal quantifications.
   Removes disjunctions containing both a literal and its negation.
   Raises "Error" if "form" is not a valid "PredSx.T".
*)

PROCEDURE CNFSize(form: T;
                  VAR (*OUT*) conjs: CARDINAL;
                  VAR (*OUT*) maxWidth: CARDINAL);
(* Sets "conjs" to an upper bound on the number of clauses in the CNF
   of "form", and "width" to an upper bound on the maximum width of
   those clauses. *)

PROCEDURE Desugar(form: T): T;
(* The result is equivalent to "form", but contains no "IMPLIES", "EXPLIES",
   or "IFF" symbols. *)

PROCEDURE ProcessLabels(form: T; push: BOOLEAN; vars: RefList.T := NIL): T;
(* The result is equivalent to "(FORALL (vars) form)", but parameterizes
   all "LBL"'s within quantifications by the quantified variables.
   Further, if "push" is "TRUE", ensures that no "LBL"'s enclose boolean
   expressions; all "LBL"'s are moved inwards to enclose literals.
   Requires "form" to be 'desugared', that is, it may not contain
   "IMPLIES", "EXPLIES", or "IFF"; see the "Desugar" procedure above.
*)

PROCEDURE And(f1, f2: T): T;
(* Return a formula equivalent to "(AND f1 f2)"; flattening where
   possible.  For example, "And" of "(AND a b)", "(AND c d)") yields
   "(AND a b c d)".   Requires "f1" and "f2" to be non-nil. *)

PROCEDURE Or(f1, f2: T): T;
(* Return a formula equivalent to "(OR f1 f2)"; flattening where
   possible.  For example, "Or" of ("(OR a b)", "(OR c d)") yields
   "(OR a b c d)".   Requires "f1" and "f2" to be non-nil. *)

PROCEDURE Not(f: T): T;
(* Return a formula equivalent to "(NOT f)", eliminating doubled negatives.
   That is, if "f" is "(NOT g)", returns "g" rather than "(NOT (NOT g))".
   Requires "f" to be non-nil. *)

PROCEDURE NegIf(f: T; sense: BOOLEAN): T;
(* If "sense" is "TRUE", return "f", otherwise return "Not(f)". *)

PROCEDURE MkQuant(q: Atom.T; vars, patSpec: RefList.T; body: T): T;
(* If "patSpec" is non-NIL, returns the list "(q vars patSpec body)";
   if "patSpec" is NIL returns "(q vars body)". *)

PROCEDURE QuantPatSpec(q: RefList.T): RefList.T;
(* Returns the pattern specification of "q", or NIL if there is none. *)

PROCEDURE QuantBody(q: RefList.T): T;
(* Returns the body of the quantified formula "q". *)

PROCEDURE Equal(f1, f2: T): BOOLEAN;
(* Returns "TRUE" iff "f1" and "f2" have the same structure; that is,
   are "equal" in Lisp terms. *)

PROCEDURE Sub(f: T; sub: AtomRefTbl.T): T;
(* Returns the result of substituting "sub[v]" for free occurrences of
   variables "v" in "f", where "v" is in the domain of "sub". *)

PROCEDURE Sub1(f: T; v: Atom.T; val: T): T;
(* Equivalent to "Sub(f, sub)", where "sub" is the map "{ v -> val }". *)

PROCEDURE ComposeSub(sub0, sub1: AtomRefTbl.T): AtomRefTbl.T;
PROCEDURE ComposeSubD(sub0, sub1: AtomRefTbl.T);
PROCEDURE ComposeSub1(sub0: AtomRefTbl.T; v: Atom.T; val: T): AtomRefTbl.T;
PROCEDURE ComposeSub1D(sub0: AtomRefTbl.T; v: Atom.T; val: T);
(* "ComposeSub(sub0, sub1)" returns the result of composing "sub0" and
   "sub1"; that is, a substitution "sub2" such that "sub2(x) =
   sub1(sub0(x))".   "ComposeSubD" is like "ComposeSub" except that it
   destructively modifies its first argument to become the
   composition.  "ComposeSub1" and "ComposeSub1D" are like
   "ComposeSub" and "ComposeSubD" except that their "sub1" arguments
   are singleton substitutions, expressed as a variable/value pair. *)

PROCEDURE VarsOccurFree(f: T; vs: REFANY): BOOLEAN;
(* "vs" may either be a single variable (an "Atom.T") or a list of
   variables. Returns "TRUE" iff the variable "vs", or any of the
   variables in the list "vs", occurs free in "f". *) 

PROCEDURE QuantifiedSubformula(f: T): BOOLEAN;
(* Return "TRUE" iff "f" has a quantified subformula. *)

PROCEDURE OnlyUniversalPos(f: T): BOOLEAN;
(* Return "TRUE" iff all quantified subformulae of "f" are positive
   universal. *)

PROCEDURE FormIsLit(f: T): BOOLEAN;
(* Return "TRUE" if "f" is a (possibly negated) atomic formula (which
   includes quantifications.) *)

PROCEDURE FormIsClause(f: T): BOOLEAN;
(* Returns "TRUE" if "f" is a literal (as defined in "IsLit") or a
   disjunction of literals. *)

PROCEDURE FormIsInCNF(f: T): BOOLEAN;
(* Returns "TRUE" if "f" is a clause (as defined in "IsClause") or a
   conjunction of clauses. *)

PROCEDURE CanonicalizeLit(lit: T; neg := FALSE): T RAISES { Error };
(* Returns a canonical form for "(XOR neg lit)". *)

PROCEDURE CanonicalizeClause(cl: T): T;
(* Assumes "cl" is literal in canonical form, in which case it returns
   the literal, or a disjunction of literals in canonical form, in
   which case it returns a canonical form for the disjunction. *)

TYPE
  RenameStatePublic = OBJECT
   METHODS
    init(): RenameState;
    get(a: Atom.T; VAR i: INTEGER): BOOLEAN;
    inc(a: Atom.T; undo := TRUE): INTEGER;
    dec(a: Atom.T);
    push();
    pop();
  END (* OBJECT *);
  RenameState <: RenameStatePublic;

CONST
  SkolemSepChar = '%';
  SkolemSep = "%";

PROCEDURE SkolemizeOuter(f: T; VAR (*IN/OUT*) rs: RenameState): T
  RAISES { Error };
(* Returns a formula "f2" that is equisatisfiable with "f", but whose
   prenex-normal form begins with a universal quantifier if it has any
   quantifiers at all.  This is accomplished by introducing Skolem
   constants for all outermost existentially quantified variables;
   names for these Skolem constants are chosen based on the original
   names of the quantified variables; they are "unique-ized" by
   appending a "SkolemSep" and an integer.  (Atoms in "f" must not
   contain "SkolemSep".)  A "RenameState" can be thought
   of as a set of names: the union, for each pair "<name, i>" in the
   table, of "name" and "{ name%1 ... name%i" }.  New names are
   names are chosen so as not to conflict with
   any names in "rs" on entry (with "NIL" interpreted as the empty
   set), and "rs" contains the set of names used on exit.  If "rs" is
   non-NIL on entry, then it is assumed to contain all free variables
   of "f".  *)

PROCEDURE IsAStar(f: T; sense := TRUE): BOOLEAN;
(* Returns "TRUE" iff the prenex-normal form "f IFF sense" contains no
   existential quantifiers. *)

PROCEDURE SimplifyOneQuant(q: T; onePoint := TRUE): T RAISES { Error };
(* If "q" is not a quantified formula, returns "q".  If "q" is not a
   valid "PredSx.T", raise "Error".  If "q" is a correct
   quantified formula, returns a {\em simplified} form of "f".  I now
   define what simplified means in this context.  First, if "q" is an
   existential quantification "(EXISTS (vs) p)" let "q'" be the
   universal quantification in the equivalent formula "(NOT (FORALL
   (vs) (NOT p)))"; otherwise, "q" is a universal quantification, so
   let "q'" be identical to "q".  Let "vs" be the set of quantified
   variables in "q", and let "b" be the formula quantified over.  If
   "b" is not in conjunctive normal form (CNF), one simplification is
   rewriting "b" to its CNF form.  Let "cnf" be a formula equivalent
   to "b", in CNF.  "q" can be simplified if any of the
   following conditions hold:

   The first 3 simplifications are different forms of {\it one-point
   rules.}  None of these simplifications are applied if the argument
   "onePoint" is "FALSE".

   For some variable "v" in "vs", "cnf" has a conjunct of the form
| 
| ((NOT (v = T)) OR R)
|
   such that "v" does not occur free in "T".  This is an instance of
   the {\em one-point rule}; the conjunct is rewritten to "R(v:=T)".

   If "cnf" has a conjunct of the form
| 
| ((NOT (FORALL (x) (v[x] = T))) OR R)
|
   where "v[x]" represents a function application whose head symbol is
   "selectSym" and arguments are "v" and "x", and "v" does not occur
   free in "T", this conjunct is rewritten to "R(v[e] := T[x := e])"
   (where this is intended to represent the complete application of a
   higher-order rewrite rule).  This is called the {\it one-point map
   rule}.

   The next rule is somewhat analogous to the rule above, substituting
   logical equivalence for equality.  If for some variable "v" in
   "vs", "cnf" has a conjunct of the form
| 
| ((NOT (FORALL (x) (v[x] = T) IFF P)) OR R)
|
   such that "T" contains no quantified variables and "v" does not
   occur free in "P",  then this conjunct is rewritten to
   "R((v[e] = T) := P(x := e))".

  The simplifications below are applied regardless of the value of "onePoint".

  If "cnf" is of the form
| 
| (p OR R) AND Q
|
   where none of the variables in "vs" occur free in "p", "q'" is
   simplified to "(p OR (FORALL (vs) R)) AND (FORALL (vs) Q)".  If
   "cnf" is of the form
| 
| C1 AND Q
|
   and some variable "v" in "vs" does not occur free in "C1", then
   "q'" is simplifed to "(FORALL (vs - {v}) C1) AND (FORALL (vs) Q)".

   The conjunctive normal form is really only meaningful in
   propositional calculus; constructing the conjunctive normal form of
   "b", quantified sub-formulae are treated as literals.  If "cnf" is
   equivalent to
|
| ((FORALL (vs2) p)) AND Q
|
   "cnf" is rewritten to "(FORALL (vs U vs2) p) AND (FORALL (vs) Q)".

   The value returned is the result of applying these simplifications
   until they can no longer be applied.
*)



PROCEDURE SimplifyQuants(q: T; onePoint := TRUE): T RAISES { Error };
(* Performs the simplifications described by "SimplifyOneQuant"
   bottom-up within the term "q". *)


PROCEDURE FreeVars(f: T): AtomSet.T;
(* Returns the set of variables occuring free in "f". *)

PROCEDURE Hash(f: T): Word.T;
(* Returns a hash value for "f". *)

VAR (*CONST*)
  falseSym, trueSym, eqSym, diffSym, distClassSym,
  andSym, orSym, notSym, impliesSym, expliesSym, iffSym, proofSym,
  plusSym, minusSym, timesSym,
  selectSym, storeSym,
  ltSym, gtSym, leSym, geSym, 
  forallSym, existsSym,
  patsSym, noPatsSym, mpatSym, promoteSym, plungeSym,
  labelSym, lblPosSym, lblNegSym,
  lblNameQuantSym, lblNameAndSym, lblNameOrSym,
  orderSym, defPredSym, defPredMapSym: Atom.T;

  boolOps, relOps, allOps: AtomSet.T;
  (* "boolOps" is the set of boolean operators recognized by "PredSx".
     "relOps" is the set of relational operators.  "allOps" is the set of all
     operators interpreted specially by PredSx; i.e., those that cannot be used
     in terms. *)

END PredSx.
