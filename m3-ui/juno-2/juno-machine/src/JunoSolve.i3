(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 12 17:05:54 PDT 1994 by heydon                   *)

INTERFACE JunoSolve;

(* Defines a procedure for solving a conjunctive normal form constraint. This
   procedure performs both unification closure to solve for list structure,
   and Newton-Raphson iteration to solve for numeric values. *)

IMPORT RTVal;

TYPE
  Private <: ROOT;
  Public = Private OBJECT
    known: BOOLEAN;
    val: RTVal.T;
  END;
  Var <: Public;
  Vars = REF ARRAY OF Var;

(* If "x" is a "Var", the "x.known" implies "x.val" is a legal Juno value. If
   "x.known = FALSE" but "x.val # NIL", then "x.val" is a hint for the initial
   value of "x". *)

TYPE
  Constraint <: REFANY;
  Constraints = REF ARRAY OF Constraint;

PROCEDURE New(known := FALSE; val: RTVal.T := NIL): Var;
(* Return a new, valid "Var" with the given field values. It will remain
   valid until the next call to "Dispose". *)

PROCEDURE NewEqual(x, y: Var): Constraint;
(* Return the constraint "x = y". *)

PROCEDURE NewCons(x, y, z: Var): Constraint;
(* Return the constraint "x = (y,z)". *)

PROCEDURE NewPlus(x, y, z: Var): Constraint;
(* Return the constraint "x = y + z". *)

PROCEDURE NewTimes(x, y, z: Var): Constraint;
(* Return the constraint "x = y * z". *)

PROCEDURE NewAtan(x, y, z: Var): Constraint;
(* Return the constraint "x = ATAN(y, z)". *)

PROCEDURE NewSin(x, y: Var): Constraint;
(* Return the constraint "x = SIN(y)". *)

PROCEDURE NewCos(x, y: Var): Constraint;
(* Return the constraint "x = COS(y)". *)

PROCEDURE NewExp(x, y: Var): Constraint;
(* Return constraint "x = EXP(y)". *)

PROCEDURE NewReal(x: Var): Constraint;
(* Return the constraint "REAL(x)". *)

PROCEDURE NewText(x: Var): Constraint;
(* Return constraint "TEXT(x)". *)

PROCEDURE Dispose();
(* Invalidate and reclaim all existing "Var"'s and "Constraint"'s. *)

PROCEDURE P(READONLY c: ARRAY OF Constraint): BOOLEAN;
(* Solve "c" for its unknowns and return "TRUE", setting their "val" fields so
   as to solve the constraints. Return "FALSE" if there is no solution. If "P"
   returns "FALSE", it can have arbitrary effects on the "known" and "val"
   fields of variables referenced in "c".

   Requires that "Init" has been called on every variable referenced in "c"
   since each was last passed in a call to "P". *)

END JunoSolve.
