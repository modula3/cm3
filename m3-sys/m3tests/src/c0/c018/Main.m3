(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: WHILE & EXIT statements *)

MODULE Main;

VAR
  i: INTEGER;
  b: BOOLEAN;

BEGIN

  WHILE (i < i+2) DO
    i := 1;
  END;

  WHILE (i < i+2) DO
    i := 2;
    IF b THEN EXIT END;
    i := 3;
  END;

  WHILE (i < i+2) DO
    i := 4;
    WHILE (i < i+2) DO
      i := 5;
      IF b THEN EXIT END;
      i := 6;
    END;
    IF b THEN EXIT END;
    i := 7;
  END;

END Main.
