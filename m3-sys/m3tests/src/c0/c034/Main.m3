(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: NEW of REF types that require initialization *)

MODULE Main;

TYPE
  t1 = REF [3..8];
  t2 = REF RECORD a, b: [2..4] END;
  t3 = REF ARRAY [0..9] OF [1..9];

VAR
  v1: t1;
  v2: t2;
  v3: t3;

BEGIN
  v1 := NEW (t1);
  v2 := NEW (t2);
  v3 := NEW (t3);
END Main.
