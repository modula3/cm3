(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE 
  V <: ROOT;
  W <: ROOT;
REVEAL
  V = W BRANDED "foo" OBJECT a: INTEGER; END;
  W = V BRANDED "bar" OBJECT b: INTEGER; END;

VAR v := NEW (V);
BEGIN
   EVAL v.a;
   EVAL v.b;
END Main.
