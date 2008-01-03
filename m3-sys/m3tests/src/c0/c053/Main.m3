(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: MAX *)

MODULE Main;

VAR 
  o : OBJECT x, y: INTEGER; END;
  a, b: INTEGER;

BEGIN

o.x := MAX (o.x, o.y);
a := MAX (a, b);

END Main.
