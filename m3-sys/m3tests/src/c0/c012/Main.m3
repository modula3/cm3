(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: LOOP & EXIT statement *)

MODULE Main;

VAR
  i: INTEGER;
  b: BOOLEAN;

BEGIN

  LOOP
    i := 2;
    IF b THEN EXIT END;
    i := 3;
  END;

  LOOP
    i := 4;
    LOOP
      i := 5;
      IF b THEN EXIT END;
      i := 6;
    END;
    IF b THEN EXIT END;
    i := 7;
  END;

  LOOP
    i := 1;
  END;

  (* this infinite loop does not return, so the code below may not be
     generated *)

  i := 100;

END Main.
