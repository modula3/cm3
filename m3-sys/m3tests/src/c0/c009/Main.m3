(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: FOR & EXIT statements *)

MODULE Main;

VAR
  j: INTEGER;
  b: BOOLEAN;

BEGIN
  FOR i := 0 TO 9 DO
  END;

  FOR i := 9 TO 0 DO
  END;

  FOR i := 0 TO 9 BY 3 DO
  END;

  FOR i := 9 TO 0 BY -3 DO
  END;

  FOR i := 0 TO 9 BY 3 DO
     j := i;
     IF b THEN EXIT END;
     j := i;
  END;

  FOR i := 0 TO 9 BY j DO
     j := i;
     IF b THEN EXIT END;
     j := i;
  END;

  FOR i := j+0 TO j+9 BY 2*j DO
     j := i;
     IF b THEN EXIT END;
     j := i;
  END;

  FOR i := 0 TO 9 BY 3 DO
     j := i;

     FOR i := 0 TO 9 BY 3 DO
        j := i;
        IF b THEN EXIT END;
        j := i;
     END;

     IF b THEN EXIT END;
     j := i;
  END;

END Main.
