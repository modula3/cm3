(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE Supertype = OBJECT x: INTEGER; END;

VAR i: INTEGER;

BEGIN
  i := TYPECODE (Supertype OBJECT a: INTEGER END OBJECT X: CHAR END);
END Main.
