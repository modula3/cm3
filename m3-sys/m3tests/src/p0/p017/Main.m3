(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: FOR & EXIT statements *)

MODULE Main;

IMPORT Fmt, Wr, Stdio;
<*FATAL ANY*>

VAR
  j: INTEGER;

PROCEDURE z(i: INTEGER) =
  BEGIN
    Wr.PutText (Stdio.stdout, Fmt.Int (i) & " ");
  END z;

PROCEDURE done(t: TEXT) =
  BEGIN
    Wr.PutText (Stdio.stdout, "[" & t & "]\n");
  END done;

BEGIN
  FOR i := 0 TO 9 DO z(i);
  END;
  done ("0..9");

  FOR i := 9 TO 0 DO z(i);
  END;
  done ("");

  FOR i := 0 TO 9 BY 3 DO z(i);
  END;
  done ("0 3 6 9");

  FOR i := 9 TO 0 BY -3 DO z(i);
  END;
  done ("9 6 3 0");

  FOR i := 0 TO 9 BY 3 DO z(i);
     j := i;
     EXIT;
  END;
  done ("0");

  FOR i := 0 TO 9 BY j DO z(i);
     j := i;
     EXIT;
  END;
  done ("0");

  j := 1;
  FOR i := j+0 TO j+9 BY 2*j DO z(i);
     j := i;
     j := i;
  END;
  done ("1 3 5 7 9");

  FOR i := 0 TO 9 BY 3 DO z(i);
     j := i;

     FOR i := 0 TO 9 BY 3 DO z(i);
        j := i;
        EXIT;
     END;

     EXIT;
  END;
  done ("0 0");

  Wr.Close (Stdio.stdout);
END Main.
