(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: lazy typechecking & scope interactions *)

(* If P is typechecked before A1(A2) and the chain P.a1 -> A1 -> X
   picks up the X in P, typechecking will fail. *)

MODULE Main;

TYPE
  A1 = ARRAY [0..1] OF X;
CONST
  B1 = Z + 4;

PROCEDURE P () =
  VAR X: INTEGER;
  VAR a1: A1;
  VAR a2: A2;
  VAR Z: INTEGER;
  CONST c1 = B1 + 1;
  CONST c2 = B2 + 1;
  BEGIN
    EVAL X; EVAL a1; EVAL a2;
    EVAL Z; EVAL c1; EVAL c2;
  END P;

TYPE
  A2 = ARRAY [0..1] OF X;
CONST
  B2 = Z + 5;

TYPE
  X = INTEGER;
CONST
  Z = 5;

BEGIN
  P ();
END Main.
