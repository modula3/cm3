(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

TYPE Obj = OBJECT METHODS
             a (x: INTEGER): INTEGER := A;
             b (x: INTEGER): INTEGER := A;
           END;

PROCEDURE A (<*UNUSED*> o: Obj;  <*UNUSED*>x: INTEGER): INTEGER =
  BEGIN INC (z);  RETURN 3; END A;

PROCEDURE F (): Obj =
  BEGIN INC (z);  RETURN NEW (Obj); END F;

PROCEDURE G (): INTEGER =
  BEGIN INC (z);  RETURN 0; END G;

VAR o := NEW (Obj);
VAR z := 0;
BEGIN
  EVAL o.a (3);                       Test.checkI (z, 1);
  EVAL o.a (G());                     Test.checkI (z, 3);
  EVAL o.a (o.b (3));                 Test.checkI (z, 5);
  EVAL o.a (o.a (3));                 Test.checkI (z, 7);

  EVAL F().a (3);                     Test.checkI (z, 9);
  EVAL F().a (G());                   Test.checkI (z, 12);
  EVAL F().a (F().b (3));             Test.checkI (z, 16);
  EVAL F().a (F().a (3));             Test.checkI (z, 20);

  EVAL NEW(Obj).a (3);                Test.checkI (z, 21);
  EVAL NEW(Obj).a (G());              Test.checkI (z, 23);
  EVAL NEW(Obj).a (NEW(Obj).b (3));   Test.checkI (z, 25);
  EVAL NEW(Obj).a (NEW(Obj).a (3));   Test.checkI (z, 27);

  Test.done ();
END Main.
