(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT A, B;
FROM Test IMPORT checkI, done;

REVEAL
  B.T = BRANDED REF RECORD  x: INTEGER END;

VAR a := NEW (A.T);
VAR b := NEW (B.T);
BEGIN
  checkI (BITSIZE (b.x), BITSIZE (INTEGER));
  checkI (BITSIZE (a.x), BITSIZE (INTEGER));
  checkI (TYPECODE (a), TYPECODE (b));
  done ();
END Main.
