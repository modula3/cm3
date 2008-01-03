(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

TYPE X = OBJECT
           a : ARRAY [FIRST (A) .. LAST (A)] OF INTEGER;
           b : ARRAY [FIRST (B) .. LAST (B)] OF INTEGER;
         END;

CONST A = ARRAY [0..3] OF INTEGER { 1, 2, 3, BITSIZE (X) };
CONST B = ARRAY OF INTEGER { 1, 2, BITSIZE (X) };

VAR x := NEW (X);
BEGIN
  checkI (NUMBER (x.a), 4);
  checkI (NUMBER (A),   4);
  checkI (NUMBER (x.b), 3);
  checkI (NUMBER (B),   3);
  done ();
END Main.
