(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: assignment of open arrays *)

MODULE Main;

TYPE
  R = RECORD
    x: INTEGER;
    y: [3..4];
  END;

  t1 = REF ARRAY OF ARRAY OF ARRAY OF R;
  t3 = REF ARRAY OF ARRAY OF ARRAY [1..5] OF R;
VAR
  v1: t1;
  v2, v4: ARRAY [1..3] OF ARRAY [1..4] OF ARRAY [1..5] OF R;
  v3: t3;
  r1, r2: R;

BEGIN
  v1 := NEW (t1, 3, 4, 5);
  v3 := NEW (t3, 3, 4);
  v1^[2][3][4].x := 7;
  v1^ := v2;
  v2  := v1^;
  v4  := v2;
  EVAL r1;
  EVAL r2;
END Main.

