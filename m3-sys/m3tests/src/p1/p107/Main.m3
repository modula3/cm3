(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main EXPORTS Foof, Main;

IMPORT Wr, Stdio, Fmt, Foof;
<*FATAL ANY*>


PROCEDURE P (y := 3;  x := 4) =
  BEGIN
    Wr.PutText (Stdio.stdout, "x := ");
    Wr.PutText (Stdio.stdout, Fmt.Int (x));
    Wr.PutText (Stdio.stdout, "  y := ");
    Wr.PutText (Stdio.stdout, Fmt.Int (y));
    Wr.PutText (Stdio.stdout, "\n");
  END P;

BEGIN
  P (5, 6);
  P (x := 5);
  P (y := 5);
  Foof.P (5, 6);
  Foof.P (x := 5);
  Foof.P (y := 5);
END Main.
