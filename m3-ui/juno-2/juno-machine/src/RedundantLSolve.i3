(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May  2 09:36:00 PDT 1996 by heydon                   *)
(*      modified on Fri Dec  2 15:35:09 PST 1994 by gnelson                  *)
(*      modified on Mon Oct 31 18:13:52 PST 1994 by isard                    *)

INTERFACE RedundantLSolve;

(* This interface provides a procedure "P" that approximately solves 
   a set of simultaneous linear equations.  "P" is designed to be 
   used as a subroutine to solve simultaneous nonlinear equations 
   by Newton interation: at each stage of the iteration, the nonlinear 
   equations are approximated by linear ones around the current point, 
   these linear equations are solved, and the current point is moved 
   to the solution found. 

   The reason that "P" finds only an approximate solution instead 
   of an exact one is that the approximate solution works better 
   in the case that the nonlinear equations are redundant but consistent 
   (for example, "x^2=4 AND x^3=8").  In this case the linear approximation 
   often produces inconsistent or very ill-conditioned linear systems. 

   So, instead of solving 

| x : Ax = b

    the procedure "P" solves

|  x : | b - Ax | < |b| / 2 

    That is, it sets "x" so that the norm of "b - Ax" is less than half the
    norm of "b".  This criterion guarantees that progress in made in the
    overall Newton iteration on the non-linear problem. 
    
    The procedure "P" attempts to find a small "x" that satisfies the
    constraint.  It is not guaranteed to find the smallest "x" in any
    precise sense, but if "A" is very ill-conditioned (as in the case
    where the nonlinear problem is redundant but consistent) the
    procedure avoids the directions of motion corresponding to the
    small singular values of "A", and if "A" is very underconstrained,
    so that there are many dimensions worth of "x"s that will solve the 
    constraint, then the Gram-Schmidt algorithm is used to find
    the one that is shortest in the Euclidean norm. *)

FROM JunoValue IMPORT Real;

IMPORT Wr;

TYPE
  Vector = ARRAY OF Real;
  Matrix = ARRAY OF Vector;

PROCEDURE P(
  m, n: CARDINAL;
  VAR (*INOUT*) a: Matrix;
  VAR (*OUT*) x: Vector);
(* Set "x" to the approximate solution of "Ax=b", where matrix "A" is stored
   as "a[0..m-1,0..n-1]" and vector "b" is stored as "a[0..m-1,n]". *)
   
VAR logWr: Wr.T := NIL;

(* If the internal variable "RedundantLSolve.debug" exceeds zero,
   information is written to "logWr" each time "P" is called. *)

PROCEDURE SetGramSchmidt(on: BOOLEAN);
(* By default, calls to "P" use the Gram-Schmidt algorithm to compute a
   minimal solution in the underconstrained case. This procedure can be used
   to disable (or re-enable) that process. *)

END RedundantLSolve.
