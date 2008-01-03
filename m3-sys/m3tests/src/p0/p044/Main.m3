(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

TYPE T = RECORD a, b: INTEGER; END;

PROCEDURE foo (a, b: INTEGER) : T =
  BEGIN
    WITH t = T { MIN (a, b), MAX (a, b) } DO
      RETURN t; END;
  END foo;

PROCEDURE bar (a, b: INTEGER): INTEGER =
  BEGIN
    WITH t = MAX (a, b) DO 
      RETURN t; END;
  END bar;

BEGIN
  WITH t = foo (3, 4) DO
    checkI (t.a, 3);
    checkI (t.b, 4); END;

  WITH i = bar (3, 4) DO
    checkI (i, 4); END;

  done ();

END Main.
