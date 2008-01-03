(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE foo;
IMPORT Wr, Stdio, Fmt;
<*FATAL ANY*>

PROCEDURE proc1 (READONLY p : ARRAY [0..2] OF TwoVec) =
BEGIN
  Wr.PutText(Stdio.stdout,"\nproc1\n");
  FOR i := 0 TO NUMBER(p)-1 DO
    Wr.PutText(Stdio.stdout," " & Fmt.LongReal(p[i][0]));
    Wr.PutText(Stdio.stdout," " & Fmt.LongReal(p[i][1]) & "\n");
  END;
END proc1;

PROCEDURE proc2 (READONLY p : ARRAY OF TwoVec) =
BEGIN
  Wr.PutText(Stdio.stdout,"\nproc2\n");
  FOR i := 0 TO NUMBER(p)-1 DO
    Wr.PutText(Stdio.stdout," " & Fmt.LongReal(p[i][0]));
    Wr.PutText(Stdio.stdout," " & Fmt.LongReal(p[i][1]) & "\n");
  END;
END proc2;

BEGIN
END foo.
