(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: NEW of open arrays *)

MODULE Main;

TYPE
  R = RECORD
    x: INTEGER;
    y: [3..4];
  END;

  t1 = REF ARRAY OF R;
  t2 = REF ARRAY OF ARRAY OF [7..8];
  t3 = REF ARRAY OF ARRAY OF ARRAY OF [10..12];
  t4 = REF ARRAY OF CHAR;
  t5 = REF ARRAY OF ARRAY OF CHAR;

VAR
  v1: t1;
  v2: t2;
  v3: t3;
  v4: t4;
  v5: t5;
  i, j, k: INTEGER;

BEGIN
  v1 := NEW (t1, i);
  v2 := NEW (t2, i, j);
  v3 := NEW (t3, i, j, k);
  v4 := NEW (t4, i);
  v5 := NEW (t5, i, j);
END Main.
