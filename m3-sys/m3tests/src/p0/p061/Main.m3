(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

TYPE A = OBJECT a: INTEGER; END;

TYPE B = A OBJECT METHODS m (x:CHAR); END;

TYPE C = B OBJECT f: REAL; END;

VAR b: B; i: INTEGER;

PROCEDURE F (x: A): INTEGER =
  VAR b: B;
  BEGIN
    b := x;
    RETURN x.a;
  END F;

BEGIN

b := NEW (C, a := 12, f := 28.8);
i := F (b);

checkI (i, 12);

done ();

END Main.
