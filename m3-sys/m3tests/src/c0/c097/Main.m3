(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE
  A = REF C;
  D = REF B;

  B = RECORD c: C; END;
  C = RECORD d: D; END;

VAR
  a: A;

BEGIN
  a := NEW (A);
END Main.
