(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Test, Wr, Stdio, Fmt;
<*FATAL ANY*>

VAR
  x, y, z, z1, tmp: LONGREAL;
BEGIN
  x := 23.4d+0;
  y := 8388608.0d+0;

  z := (x + y) - y;

  tmp := (x + y);
  z1 := tmp - y;

  Wr.PutText(Stdio.stdout, Fmt.F("%s %s\n", Fmt.LongReal(z), Fmt.LongReal(z1)));
  Test.checkL (z, z1);
  Test.done ();
END Main.
