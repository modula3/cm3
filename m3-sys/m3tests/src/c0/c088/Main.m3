(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

PROCEDURE Main () =
  VAR foo: REAL;
  PROCEDURE local (aa: REAL) : REAL =
    BEGIN RETURN aa + foo; END local;
  BEGIN 
    foo := local (foo);
  END Main;

BEGIN
Main ();
END Main.
