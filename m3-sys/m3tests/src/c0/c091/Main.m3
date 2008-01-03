(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Test;

PROCEDURE Foo (           z: INTEGER;
               <*UNUSED*> y: INTEGER;
               <*UNUSED*> a: INTEGER;
                          b: INTEGER) =
  BEGIN
    INC (z);
    INC (a);
  END Foo;

<*OBSOLETE*> PROCEDURE BarO () = BEGIN END BarO;
<*OBSOLETE*> PROCEDURE BarP () = BEGIN END BarP;
<*UNUSED*>   PROCEDURE BarU () = BEGIN END BarU;
<*UNUSED*>   PROCEDURE BarV () = BEGIN END BarV;
             PROCEDURE BarN () = BEGIN END BarN;
             PROCEDURE BarM () = BEGIN END BarM;

VAR a: INTEGER;

<*OBSOLETE*> VAR vO, vP: INTEGER;
<*UNUSED*>   VAR vU, vV: INTEGER;
             VAR vN, vM: INTEGER;
             
BEGIN
  Foo (a, a, a, a);
  BarO ();
  BarU ();
  BarN ();
  vO := 0;
  vU := 0;
  vN := 0;
  Test.Foo ();
END Main.
