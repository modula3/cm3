(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* S-expression builder utilities for Simplify formulas.

   Builds RefList.T S-expressions compatible with the Prover library.
   All atoms are Atom.T, consistent with SchemePair.T representation
   (which is just RefList.T under mscheme's zero-copy interop). *)

INTERFACE ESCSx;

IMPORT Atom, RefList;

(* Propositional connectives *)
PROCEDURE MkAnd(a, b: RefList.T): RefList.T;
PROCEDURE MkOr(a, b: RefList.T): RefList.T;
PROCEDURE MkNot(a: RefList.T): RefList.T;
PROCEDURE MkImplies(a, b: RefList.T): RefList.T;
PROCEDURE MkIff(a, b: RefList.T): RefList.T;

(* Conjunction of a list *)
PROCEDURE MkAndList(conjuncts: RefList.T): RefList.T;

(* Quantifiers *)
PROCEDURE MkForall(vars: RefList.T;  (* list of Atom.T *)
                   pats: RefList.T;  (* PATS triggers, or NIL *)
                   body: RefList.T): RefList.T;
PROCEDURE MkExists(vars: RefList.T;
                   body: RefList.T): RefList.T;

(* Equality and comparison *)
PROCEDURE MkEq(a, b: REFANY): RefList.T;
PROCEDURE MkNeq(a, b: REFANY): RefList.T;
PROCEDURE MkLt(a, b: REFANY): RefList.T;
PROCEDURE MkLe(a, b: REFANY): RefList.T;
PROCEDURE MkGt(a, b: REFANY): RefList.T;
PROCEDURE MkGe(a, b: REFANY): RefList.T;

(* Arithmetic *)
PROCEDURE MkPlus(a, b: REFANY): RefList.T;
PROCEDURE MkMinus(a, b: REFANY): RefList.T;
PROCEDURE MkTimes(a, b: REFANY): RefList.T;

(* Array theory *)
PROCEDURE MkSelect(map, index: REFANY): RefList.T;
PROCEDURE MkStore(map, index, val: REFANY): RefList.T;
PROCEDURE MkNumber(arr, dim: REFANY): RefList.T;

(* Labels *)
PROCEDURE MkLabel(name: Atom.T; body: RefList.T): RefList.T;
(* (LBL |name| body) *)

(* Type predicates *)
PROCEDURE MkIsPred(typePred: Atom.T; x: REFANY): RefList.T;
(* (EQ (|Is$T| x) |@true|) *)

PROCEDURE MkSubtype(tc1, tc2: Atom.T): RefList.T;
(* (EQ (SUBTYPE |tc1| |tc2|) |@true|) *)

PROCEDURE MkDistinct(atoms: RefList.T): RefList.T;
(* (DISTINCT a1 a2 ...) *)

(* Helpers *)
PROCEDURE Cons(head: REFANY; tail: RefList.T): RefList.T;
PROCEDURE List1(a: REFANY): RefList.T;
PROCEDURE List2(a, b: REFANY): RefList.T;
PROCEDURE List3(a, b, c: REFANY): RefList.T;

(* Well-known atoms -- lower-case prefix to avoid M3 reserved words *)
VAR (*CONST*)
  atAnd, atOr, atNot, atImplies, atIff: Atom.T;
  atForall, atExists, atPats, atNopats, atMpat: Atom.T;
  atEq, atNeq, atLt, atLe, atGt, atGe: Atom.T;
  atPlus, atMinus, atTimes: Atom.T;
  atSelect, atStore, atNumber, atAddr: Atom.T;
  atLbl, atLblneg, atLblpos: Atom.T;
  atSubtype, atSubtype1, atTypecode: Atom.T;
  atDistinct: Atom.T;
  atTrue, atFalse: Atom.T;
  atDefpred, atDefpredmap: Atom.T;
  atBgPush, atBgPop: Atom.T;
  atOrd: Atom.T;

END ESCSx.
