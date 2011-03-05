(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed May 15 22:20:33 PDT 2002 by saxe                     *)
(*      modified on Mon Sep  9 18:21:57 PDT 1996 by detlefs                  *)

INTERFACE Simplex;

<*PRAGMA SPEC*>

IMPORT AF, Enode, RefList, Rat;
IMPORT Wr, Prover;

(* This interface represents a resettable conjunction of linear 
   inequalities.  It also provides utility procedures for
   manipulating linear expressions. *)

(* A {\it term} is an expression of the form

|  (+ i : 1 <= i <= n : q_i * v_i) + q_0

   where the "q"'s are rational numbers and the "v"'s are
   unknown integers.
   
   Unknowns are represented as "Enode.T"'s.  The only 
   property of "Enode.T"'s that is relevant to this
   interface is that it is possible to assert equalities
   between them.

   An {\it inequality constraint} is a constraint of the
   form "L1 >= L2", where the "L"'s are terms.

   An {\it equality constraint} is a constraint of the
   form "L1 = L2", where the "L"'s are terms.

   A {\it constraint} is an inequality constraint or an
   equality constraint.

   A {\it context} is a conjunction of constraints.

   This interface represents a context "C" together
   with a stack "SC" of contexts.  *)

<*SPEC VAR Valid: BOOLEAN *>

TYPE 
  Unknown <: ROOT;
  Equality <: AF.T;
  GEInequality <: AF.T;
  ZeroUnknownAF <: AF.T;

REVEAL
  Enode.SimplexMisc = BRANDED OBJECT
    unknown: Unknown := NIL
  END (* OBJECT *);

PROCEDURE EnodeIsLit(e: Enode.T; q: Rat.T);
(* Records the fact that enode "e" represents the the rational value "q". *)

PROCEDURE IsSum(res, x, y: Enode.T): BOOLEAN;
(* Add to "C" the assertion that "res = x + y", returning "TRUE" iff
   the result is satisfiable.
*)

PROCEDURE IsDiff(res, x, y: Enode.T): BOOLEAN;
(* Add to "C" the assertion that "res = x - y", returning "TRUE" iff
   the result is satisfiable.
*)

PROCEDURE IsProd(res, x, y: Enode.T): BOOLEAN;
(* Add to "C" the assertion that "res = x * y", returning "TRUE" iff
   the result is satisfiable.
*)

<* UNUSED *> PROCEDURE IsQuotient(res, x, y: Enode.T): BOOLEAN;
(* Add to "C" the assertion that "res = x / y", returning "TRUE" iff
   the result is satisfiable.
*)

PROCEDURE NewGT(e1, e2: Enode.T): AF.Lit;
(* Return a literal "l" such that the call "l.af.assert(l)" adds
   the inequality "e1 > e2" to "C". *)

PROCEDURE NewGE(e1, e2: Enode.T): AF.Lit;
(* Return a literal "l" such that the call "l.af.assert(l)" adds
   the inequality "e1 >= e2" to "C". *)

PROCEDURE GEStatus(gei: GEInequality): AF.TruthVal;
(* Returns "TruthVal.TrueAsserted" if "gei" is a comparison between
   equivalent "Enode.T"s or between "Enode.T"'s representing integer
   constants "a" and "b" where "a >= b".  Returns "TruthVal.FalseAsserted"
   if "gei" is a comparison between "Enode.T"'s representing integer
   constants "a" and "b" where "a < b".  Otherwise returns 
   "TruthVal.Unknown". *)

PROCEDURE NewEQ(u1, u2: Unknown): AF.Lit;
(* Requires "u1" and "u2" to be "Unknown"s corresponding to Enodes
   "e1" and "e2", and that an identity between those enodes be
   represented in the Egraph.  Return a literal "l" such that the call
   "l.af.assert(l)" adds the assertion "e1 = e2" to "C". *)

PROCEDURE Init();
(* Set "C := TRUE" and "st := empty". *)

PROCEDURE Push();
(* Push "C" onto "st". *)

PROCEDURE Pop();
(* Replace "C" with the result of popping "st".  A checked
   runtime error if "st" is empty. *)

(* EXP *)
PROCEDURE DeleteAllRedundant();

PROCEDURE TightenBounds(): BOOLEAN RAISES {Prover.Timeout};
(* If a primitive unknown is restricted to a
   non-empty range, it should be possible for it to hold integer
   values in that range; if not, the unsatisfiable value is removed
   from the range and the process is repeated until a satisfiable
   integer value is found, or until the range is made empty.
   In the  last case, the tableau is unsatisfiable and "TightenBounds"
   returns "FALSE", otherwise returns "TRUE".  In addition, if only
   one integer value is satisfiable, propogates an equality with that
   value.
*)

PROCEDURE Top(): RefList.T;
(* Let "E" be the subconjunction of "C" containing only the literals
   asserted by "NewEQ".  Let "P" be the conjunction of all equalities
   between variables that are implied by "C".  Return a minimal list
   "L" of S-expressions such that "L AND P AND E" implies "C".
   Requires that "Enode.ComputeSxSizes" has been called since the last
   change to "Enode.C".  The state of "Simplex.C" is undefined after a
   call to "Top", and must be reset by a call to "Init". *)

PROCEDURE Stats();

PROCEDURE PrintSize(wr: Wr.T);

PROCEDURE UnknownEqual(u1, u2: Unknown): BOOLEAN;
(* Returns "TRUE" iff "u1" and "u2" represent equal unknowns. *)

END Simplex.
