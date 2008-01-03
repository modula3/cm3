(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
UNSAFE MODULE Main;

VAR
   v : REFANY;

BEGIN
   EVAL LOOPHOLE (v, REF INTEGER)^ MOD 3;
   EVAL (LOOPHOLE (v, REF INTEGER)^ * 3) MOD 3;
   EVAL NARROW(v, REF INTEGER)^  MOD 3; 
   EVAL (NARROW(v, REF INTEGER)^ * 3) MOD 3;
END Main.

