(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

VAR i : INTEGER;

PROCEDURE foo () =
  BEGIN
    FOR x := 1 TO 10 DO
      i := x;

      PROCEDURE aaa() = BEGIN checkI (i, x); END aaa;
      BEGIN	
        aaa ();
      END	

    END
  END foo;

BEGIN

foo ();
done ();

END Main.
