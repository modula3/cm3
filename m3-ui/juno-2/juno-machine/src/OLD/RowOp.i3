(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Sep 30 13:27:04 PDT 1994 by heydon                   *)

INTERFACE RowOp;

IMPORT JunoValue;

TYPE Vector = ARRAY OF JunoValue.Real;

PROCEDURE P(
  VAR target: Vector;
  READONLY src: Vector;
  factor: JunoValue.Real)
  : INTEGER;
(* Requires "NUMBER(target) = NUMBER(src)". Subtracts "factor * src" from
   "target", storing the result in "target", and returns the index of the
   maximum absolute value in the result (not counting the last element of the
   result). If this is the maximum over the empty vector, returns -1. *)

END RowOp.
