(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

(* each procedure should only get called once *)

TYPE
  Obj = OBJECT METHODS apply () := DoIt; END;

VAR a: INTEGER := 0;

PROCEDURE DoIt (<*UNUSED*> o: Obj) = 
  BEGIN
    Test.checkI (a, 2);
    INC (a);
  END DoIt;

PROCEDURE G (): INTEGER =
  BEGIN
    Test.checkI (a, 0);
    INC (a);
    RETURN 7;
  END G;

PROCEDURE F (i: INTEGER): Obj =
  BEGIN
    Test.checkI (i, 7);
    Test.checkI (a, 1);
    INC (a);
    RETURN NEW (Obj);
  END F;

BEGIN
  F (G ()).apply ();
  Test.checkI (a, 3);
  Test.done ();
END Main.
