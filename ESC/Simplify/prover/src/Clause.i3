(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 14 17:42:50 PST 2000 by saxe                     *)
(*      modified on Mon Jul  1 17:54:15 PDT 1996 by detlefs                  *)

(* A "Clause.T" is a disjunction of literals, represented as a list.
   A "Clause.TList" is a conjunction of clauses, again represented as a list.

   This interface provides procedures to convert an arbitrary formula
   to a conjunction of clauses that is satisfiable iff the formula is,
   and to determine the satisfiability of a conjunction of clauses.
*)

INTERFACE Clause;

IMPORT Prover, AF;
IMPORT Sx, Wr, MatchingRule;

TYPE
  T <: ROOT;

CONST Brand = "Clause";

(* A "T" represents a list of clauses; where each clause is a set of
   literals. *)

PROCEDURE CNF(sx: REFANY; 
              READONLY sub: MatchingRule.Substitution := MatchingRule.EmptySub;
              rightMost := FALSE): AF.Lit RAISES { Prover.Error };

(* Return an "AF.Lit" representing the formula "sx"; that is, the
   returned literal is satisfiable iff "sx" is.  Requires "sx" to be a
   "<formula>" as defined in "PredSx", whose prenex-normal form does
   not start with an existential quantifier.  "Enode.T"'s are
   allowed where a term is expected.  If "sx" is syntactically incorrect,
   raises "Error" with a descriptive error message.  "CNF" modifies
   "Context" by interning literals and terms as needed.  All of the
   cells in the returned list will be newly allocated.
*)

PROCEDURE ToSx(c: T; normForm := FALSE) : Sx.T;

PROCEDURE ListLength(l: T): CARDINAL;
(* Returns the length of the clause lit headed by "l". *)

PROCEDURE PrintList(wr: Wr.T; l: T; printRule := FALSE; normForm := FALSE);
(* Prints "l" to "wr".  If "normForm" is "TRUE", prints canonical
   forms of enodes.   If "printRule" is "FALSE", does not print
   literals representing matching rules. *)

END Clause.
