(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

VAR x, y: ARRAY [0..9] OF INTEGER; i, j: INTEGER;

BEGIN
  FOR a := 0 TO 9 DO x[a] := a; END;
  FOR a := 0 TO 9 DO y[a] := a+100; END;
  i := 10; j := 10;
  SUBARRAY(x, 0, i) := SUBARRAY(y, 0, j);

  FOR a := 0 TO 9 DO checkI (y[a], x[a]); END;

  done ();
END Main.
