(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: BRANDED REF types *)

MODULE Main;

TYPE
  t2 = REF INTEGER;
  t1 = BRANDED REF INTEGER;
  t3 = BRANDED "mybrand" REF INTEGER;

VAR
  v1: t1;
  v2: t2;
  v3: t3;

BEGIN
  EVAL v1;
  EVAL v2;
  EVAL v3;
END Main.
