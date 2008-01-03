(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE blockinfor
  EXPORTS Main;

PROCEDURE foo () =
  BEGIN
    (* VAR x := 1; BEGIN *)
    FOR x := 1 TO 10 DO
      (*WITH x = 1 DO *)
      PROCEDURE aaa() = BEGIN EVAL x; END aaa;
      BEGIN
        aaa ();
      END	
    END
  END foo;

BEGIN
  foo ();
END blockinfor.
