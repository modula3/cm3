(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

PROCEDURE Foo (r: REFANY) =
  BEGIN
    TYPECASE r OF
    | REF REAL, REF INTEGER =>
    ELSE
    END;
  END Foo;

BEGIN
  Foo (NIL);
END Main.
