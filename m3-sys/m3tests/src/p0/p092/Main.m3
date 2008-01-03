(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Wr, Stdio, Fmt;
IMPORT foo;
<*FATAL ANY*>

VAR points : ARRAY [0..2] OF foo.TwoVec;
VAR x, y : LONGREAL;
BEGIN
  x := 1.0d0;
  y := 2.0d0;
  Wr.PutText(Stdio.stdout, "Main:\n");
  FOR i := 0 TO 2 DO
    points[i][0] := x;
    points[i][1] := y;

    Wr.PutText(Stdio.stdout, " " & Fmt.LongReal(x));
    Wr.PutText(Stdio.stdout, " " & Fmt.LongReal(y) & "\n");

    x := x * 3.0d0;
    y := y * 2.0d0;
  END;

  foo.proc1 (points);
  foo.proc2 (points);

END Main.
