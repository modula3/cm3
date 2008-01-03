(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Nov 16 14:34:01 PST 1993 by heydon                   *)

INTERFACE LinearSolve;

(* Solves a linear equation of the form A x = b using Gaussian elimination.  *)

FROM JunoValue IMPORT Real;

TYPE
  Vector = ARRAY OF Real;
  Matrix = ARRAY OF Vector;

EXCEPTION BadShapes;

PROCEDURE P(
    m, n: CARDINAL;
    VAR (*INOUT*) a: Matrix;
    VAR (*OUT*) x: Vector):
  BOOLEAN RAISES {BadShapes};
(* The first "n" columns of "a" form an "m"-by-"n" matrix "A" and the (n+1)st
   column forms an "m"-by-1 vector "b". Sets the "n"-by-1 column vector "x" to
   the solution of the matrix equation "Ax = b" with smallest L2 norm, and
   returns "TRUE"; returns "FALSE" if there is no such solution. Raises
   "BadShapes" if "NUMBER(a) < m" or "NUMBER(a[0]) < n+1" or "NUMBER(x) < n".

   On return, the contents of the matrix "a" are unspecified. *)

END LinearSolve.
