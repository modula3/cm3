(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: recursive types & constant expressions *)

MODULE Main;

TYPE  (* A1 == A2,  B1 == B2,  C1 == C2 *)
  A1 = RECORD b1: B1;  b2: B2 END;
  B1 = REF C1;
  C1 = ARRAY [0 .. BITSIZE (A2)] OF INTEGER;
  A2 = RECORD b1: B1;  b2: B2 END;
  B2 = REF C2;
  C2 = ARRAY [0 .. BITSIZE (A1)] OF INTEGER;

VAR
  a1: A1;  a2: A2;
  c1: C1;  c2: C2;

BEGIN
  a1 := a2;
  EVAL c1;
  EVAL c2;
END Main.
