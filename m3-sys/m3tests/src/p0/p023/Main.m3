(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: WHILE & EXIT statements *)

MODULE Main;

IMPORT Fmt, Wr, Stdio;
<*FATAL ANY*>

PROCEDURE msg (is, shouldBe: INTEGER) =
  BEGIN
    Wr.PutText (Stdio.stdout, "i = " & Fmt.Int (is) &
                              " [" & Fmt.Int(shouldBe) & "]\n");
  END msg;

PROCEDURE z(i: INTEGER) =
  BEGIN
    Wr.PutText (Stdio.stdout, Fmt.Int (i) & " ");
  END z;

PROCEDURE done(t: TEXT) =
  BEGIN
    Wr.PutText (Stdio.stdout, "[" & t & "]\n");
  END done;

VAR
  i: INTEGER;

BEGIN

  i := 3;
  WHILE (i < 10) DO
    z (i); INC (i);
  END;
  done ("3 .. 9");

  i := 13;
  WHILE (i < 10) DO
    z (i);
  END;
  done ("");

  i := 6;
  WHILE (i < 10) DO
    z (i);
    i := 2;
    EXIT;
    i := 3;
  END;
  done ("6");
  msg (i, 2);

  i := 45;
  WHILE (i < 50) DO z(i);
    i := 4;
    WHILE (i < 100) DO z(i);
      i := 5;
      EXIT;
      i := 6;
    END;
    EXIT;
    i := 7;
  END;
  done ("45 4");
  msg (i, 5);

  Wr.Close (Stdio.stdout);

END Main.
