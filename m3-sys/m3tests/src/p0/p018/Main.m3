(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: IF statements *)

MODULE Main;

FROM Test IMPORT checkI, done;

VAR
  a, b: INTEGER;
  c: BOOLEAN;
  j: INTEGER;

BEGIN
  a := 23;
  IF (a > 3) THEN
    j := 1;
  ELSIF (b < 3) THEN
    j := 2;
  ELSIF c THEN
    j := 3;
  ELSE
    j := 4;
  END;
  checkI (j, 1);

  a := 3;
  b := 2;
  IF (a > 3) THEN
    j := 1;
  ELSIF (b < 3) THEN
    j := 2;
  ELSIF c THEN
    j := 3;
  ELSE
    j := 4;
  END;
  checkI (j, 2);

  a := 3;
  b := 23;
  c := TRUE;
  IF (a > 3) THEN
    j := 1;
  ELSIF (b < 3) THEN
    j := 2;
  ELSIF c THEN
    j := 3;
  ELSE
    j := 4;
  END;
  checkI (j, 3);

  a := 2;
  b := 17;
  c := FALSE;
  IF (a > 3) THEN
    j := 1;
  ELSIF (b < 3) THEN
    j := 2;
  ELSIF c THEN
    j := 3;
  ELSE
    j := 4;
  END;
  checkI (j, 4);

  done ();
END Main.
