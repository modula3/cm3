(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: common subexpressions are not always common *)

MODULE Main;

VAR
  x : REF RECORD
       y : REF INTEGER; END;
  i : INTEGER;

BEGIN

IF x = NIL THEN
  x.y := NIL
ELSIF x.y = NIL THEN
  i := 1; END;

END Main.
