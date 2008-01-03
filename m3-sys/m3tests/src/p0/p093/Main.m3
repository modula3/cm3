(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Text, TextRd, Rd;
FROM Test IMPORT checkI, done;
<*FATAL ANY*>

VAR t := "";
VAR u : TEXT;
BEGIN
  FOR i := 1 TO 200 DO t := t & "X" END;
  t := t & "\n";
  WITH rd = TextRd.New(t) DO u := Rd.GetLine (rd) END;
  checkI (Text.Length (u) + 1, Text.Length (t));
  done ();
END Main.
