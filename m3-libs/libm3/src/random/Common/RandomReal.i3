(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed Sep  2 12:25:14 PDT 1992 by kalsow     *)
(*      modified on Tue Jan 30 10:57:41 1990 by muller         *)
(*      modified on Thu Jan 25 22:51:07 PST 1990 by stolfi     *)

INTERFACE RandomReal;

IMPORT Random;

(*  
  Machine specific algorithms for generating random floating-point
  values.
*)

PROCEDURE Real     (r: Random.T): REAL;
PROCEDURE Longreal (r: Random.T): LONGREAL;
PROCEDURE Extended (r: Random.T): EXTENDED;

(*
  Each of the procedures above constructs and returns a random
  floating-point value uniformly distributed in [0..1.0) using
  only the "integer" method of the passed random number generator.
*)

END RandomReal.
