(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: REPEAT & EXIT statements *)

MODULE Main;

VAR
  i: INTEGER;
  b: BOOLEAN;

BEGIN
  REPEAT
    i := 1;
  UNTIL (i < 3);

  REPEAT
    i := 2;
    IF b THEN EXIT END;
    i := 3;
  UNTIL (i < 3);

  REPEAT
    i := 4;
    REPEAT
      i := 5;
      IF b THEN EXIT END;
      i := 6;
    UNTIL (i < 3);
    IF b THEN EXIT END;
    i := 7;
  UNTIL (i < 3);

END Main.
