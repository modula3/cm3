(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

PROCEDURE Foo () =
  BEGIN
  END Foo;

VAR a: INTEGER;
VAR r: REFANY;
BEGIN
  TYPECASE r OF
  | TEXT =>
        IF (a = 0) THEN END;  a := 1;
  | MUTEX =>
        IF (a = 0) THEN END;  a := 2;
  ELSE
        IF (a = 0) THEN END;  a := 3;
        Foo ();
  END;
  Test.checkI (a, 1); (* cause r = NIL *)

  CASE a OF
  | 1 =>
        IF (a = 0) THEN END;  a := 10;
  | 50 =>
        IF (a = 0) THEN END;  a := 20;
  ELSE
        IF (a = 0) THEN END;  a := 30;
        Foo ();
  END;
  Test.checkI (a, 10); (* cause a = 1 on entry *)
  Test.done ();
END Main.
