(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
 
FROM Test IMPORT msgI, check, done;

CONST
        maxPoints = 300;
VAR
        pointarr1: ARRAY [0 .. maxPoints - 1] OF INTEGER;

PROCEDURE FooVar(VAR pointarr: ARRAY OF INTEGER) =
BEGIN
        FOR i := 0 TO 10 DO pointarr[i] := 2*i; END;
END FooVar;

PROCEDURE Foo(pointarr: ARRAY OF INTEGER) =
BEGIN
  FOR i := 0 TO 10 DO pointarr[i] := 2*i; END;
END Foo;

BEGIN
  FOR i := 0 TO 10 DO pointarr1[i] := i; END;
  FooVar(pointarr1);
  msgI (pointarr1[1]);   check (pointarr1[1] = 2);

  FOR i := 0 TO 10 DO pointarr1[i] := i; END;
  Foo(pointarr1);
  msgI (pointarr1[1]);   check (pointarr1[1] = 1);

  done ();
END Main. 
