(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
UNSAFE MODULE Main;

FROM Test IMPORT checkR, done, msg;

PROCEDURE SIGN(x: REAL): [-1 .. +1] =
  BEGIN
    IF x > 0.0 THEN 
      RETURN +1
    ELSIF x < 0.0 THEN
      RETURN -1
    ELSE
      RETURN 0
    END
  END SIGN;

PROCEDURE TestSIGN() =

  PROCEDURE CheckSIGN(arg, ref: REAL) =
    BEGIN
      checkR (FLOAT(SIGN(arg)), ref)
    END CheckSIGN;
    
  BEGIN
    msg ("Testing: f(arg) = SIGN(arg)");
    
    CheckSIGN(+1.0, +1.0);
    CheckSIGN(-1.0, -1.0);
    
    CheckSIGN(+0.5, +1.0);
    CheckSIGN(-0.5, -1.0); (***)
    
    CheckSIGN(+2.0, +1.0);
    CheckSIGN(-2.0, -1.0); (***)
    
    CheckSIGN(+0.0, +0.0);
  END TestSIGN;

BEGIN
  TestSIGN();
  done ();
END Main.

