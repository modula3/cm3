(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec  8 15:46:18 1994 by gnelson                      *)
(*      modified on Mon Oct 31 18:27:11 PST 1994 by isard                    *)
(*      modified on Wed Sep 14 18:57:07 PDT 1994 by heydon                   *)

INTERFACE RedundantSolve;

(* Solves a set of non-linear equations using Newton-Raphson iteration. *)

FROM JunoValue IMPORT Real;

TYPE
  Args = ARRAY [0..2] OF INTEGER;
  Constraint <: ConstraintPub;
  ConstraintPub = OBJECT arg: Args END;

PROCEDURE NewPlus(): Constraint;
(* Return the constraint "c" for "c.arg[0] = c.arg[1] + c.arg[2]". *)

PROCEDURE NewMinus(): Constraint;
(* Return the constraint "c" for "c.arg[0] = c.arg[1] - c.arg[2]". *)

PROCEDURE NewHalve(): Constraint;
(* Return the constraint "c" for "c.arg[0] = c.arg[1] / 2.0". *)

PROCEDURE NewTimes(): Constraint;
(* Return the constraint "c" for "c.arg[0] = c.arg[1] * c.arg[2]". *)

PROCEDURE NewSin(): Constraint;
(* Return the constraint "c" for "c.arg[0] = SIN(c.arg[1])". *)

PROCEDURE NewCos(): Constraint;
(* Return the constraint "c" for "c.arg[0] = COS(c.arg[1])". *)

PROCEDURE NewAtan(): Constraint;
(* Return the constraint "c" for "c.arg[0] = ATAN(c.arg[1], c.arg[2])". *)

PROCEDURE NewMultTan(): Constraint;
(* Return the constraint "c" for "c.arg[0] = c.arg[1] * TAN(c.arg[2])". *)

PROCEDURE NewExp(): Constraint;
(* Return the constraint "c" for "c.arg[0] = EXP(c.arg[1])". *)

PROCEDURE Dispose();
(* Invalidate and reclaim all existing "Constraint"'s. *)

PROCEDURE P(
    m, n: CARDINAL;
    VAR v: ARRAY OF Real;
    READONLY c: ARRAY OF Constraint): BOOLEAN;
(* The array "v" is the array of known and unknown values. The array "c" is
   the array of constraints, and each constraint's "arg" values are indexes
   into "v". Hence, "(forall con IN c, 0 <= c.arg[0..2] < NUMBER(v))".

   The array "v" is assumed to be divided into two parts: the first "n"
   elements are unknowns, and the remaining "NUMBER(v) - n" elements are
   knowns. The first "m" unknown variables are ``true'' variables; the
   remaining "n - m" unknown variables are ``ghost'' variables. The initial
   values for the ``ghost'' variables are ignored.

   The first "n - m" constraints in "c" are ``ghost'' constraints: they must
   be functional in the ghost variables. Moreover, their order must be such
   that each ghost variable's value can be computed from the value of true
   variables, constants, and ghost variables defined by previous ghost
   constraints. The remaining "NUMBER(c) - (n - m)" constraints are ``true''
   constraints. Here is a picture of the organization of variables and
   constraints in the arrays "v" and "c":

|	    v[]                
|	  ________                   
|	 |        |                  
|	 |  True  |                  
|	 |  Vars  |        c[]          
|	 |________|    _____________ 
|   m -> |        |   |             |
|	 |  Ghost |   |    Ghost    |
|	 |  Vars  |   | Constraints |
|	 |________|   |_____________|
|   n -> |........|   |             | <- n - m
|	 |........|   |    True     |
|	 |________|   | Constraints |
|	 |        |   |_____________|
|	 | Knowns |
|	 |________|                  

   The running time of the solver is "O(n^3)", where "n" is the number of true
   variables. Hence, although the solver functions identically independent of
   how many constraints are ``ghost'' constraints (and hence, how many of the
   variables are ``ghost'' variables), it performs better as the number of
   ``ghost'' variables increases.

   "P" uses Newton-Raphson iteration to solve for the variables. If "P" can
   set the "n" unknown variables in "v" so as to satisfy the constraints "c",
   it returns "TRUE"; if the system cannot be solved (either because there
   is no solution to the constraints, or because this algorithm failed to find
   a solution), "P" returns "FALSE". A constraint is satisfied if its two
   sides are equal to within the tolerance to which it can be computed.

   This procedure is not re-entrant. *)

END RedundantSolve.
