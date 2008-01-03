(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
UNSAFE MODULE Main;
VAR x: INTEGER;
BEGIN
  EVAL x + LOOPHOLE (ADR (x), UNTRACED REF INTEGER)^;
END Main.
