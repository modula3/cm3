(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT print;

TYPE 
  T = BRANDED "T" REF ARRAY [0..10] OF INTEGER;
  U = BRANDED "U" REF ARRAY [0..10] OF CHAR;
  V = BRANDED "V" REF ARRAY [0..10] OF ['a'..'z'];
  To = BRANDED "To" REF ARRAY OF INTEGER;
  Uo = BRANDED "Uo" REF ARRAY OF CHAR;
  Vo = BRANDED "Vo" REF ARRAY OF ['a'..'z'];

  TA = BRANDED "Ta" REF ARRAY OF TEXT;

VAR
  t := NEW (T);
  u := NEW (U);
  v := NEW (V);

  to := NEW (To, 11);
  uo := NEW (Uo, 11);
  vo := NEW (Vo, 11);

  te := "Hello Eric";

  ta := NEW (TA, 2);

BEGIN
  ta[0] := "hello";
  ta[1] := "eric";

  FOR i := 0 TO 10 DO 
    t[i] := 10 - i;
    u[i] := VAL (ORD ('A') + i, CHAR);
    v[i] := VAL (ORD ('a') + i, CHAR);
    to[i] := 10 - i;
    uo[i] := VAL (ORD ('A') + i, CHAR);
    vo[i] := VAL (ORD ('a') + i, CHAR); END;

  print.ref (t);
  print.ref (u);
  print.ref (v);
  print.ref (to);
  print.ref (uo);
  print.ref (vo);
  print.ref (te);
  print.ref (ta);
END Main.

