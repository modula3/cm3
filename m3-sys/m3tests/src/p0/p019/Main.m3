(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: REPEAT & EXIT statements *)

MODULE Main;

IMPORT Fmt, Wr, Stdio;
<*FATAL ANY*>

VAR
  i: INTEGER;

PROCEDURE z(i: INTEGER) =
  BEGIN
    Wr.PutText (Stdio.stdout, Fmt.Int (i) & " ");
  END z;

PROCEDURE done(t: TEXT) =
  BEGIN
    Wr.PutText (Stdio.stdout, "[" & t & "]\n");
  END done;

BEGIN
  i := 12; 
  REPEAT
    z (i);
    DEC (i);
  UNTIL (i < 3);
  done ("12 .. 3");

  i := 2;
  REPEAT
    z (i);
    EXIT;
  UNTIL (i > 3);
  done ("2");

  REPEAT
    i := 4;
    z (i);
    REPEAT
      i := 5;
      z (i);
      EXIT;
      i := 6;
    UNTIL (i < 3);
    EXIT;
    i := 7;
  UNTIL (i < 3);
  done ("4 5");

  Wr.Close (Stdio.stdout);
END Main.
