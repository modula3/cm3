(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT B, C;

VAR
  c := NEW (C.T);
  b := NEW (B.V);
  bb := NEW (B.U);
  cc := NEW (C.U);

BEGIN 
  bb := cc;
  cc := bb;
  c := b;
  b := c;
END Main.
