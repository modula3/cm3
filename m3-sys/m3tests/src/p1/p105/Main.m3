(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;
FROM Foo IMPORT C;

VAR c := NEW (C);
BEGIN
  Test.checkI (c.a, 1);
  Test.done ();
END Main.
