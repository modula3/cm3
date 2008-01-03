(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Readers *)

MODULE Main;

FROM Stdio IMPORT stdin, stdout;
IMPORT Rd, Wr;

BEGIN
  WHILE NOT Rd.EOF (stdin) DO
    Wr.PutChar (stdout, Rd.GetChar (stdin)); END;

  Wr.Close (stdout);
END Main.
