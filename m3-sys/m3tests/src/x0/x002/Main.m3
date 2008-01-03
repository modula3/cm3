(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Readers *)

MODULE Main;

IMPORT Rd, Wr, FileStream;

VAR
  r: Rd.T; w: Wr.T;

BEGIN
  r := FileStream.OpenRead ("/etc/passwd");
  w := FileStream.OpenWrite ("aaa");

  TRY
    LOOP
      Wr.PutChar (w, Rd.GetChar (r)); END;
  EXCEPT
  | Rd.EndOfFile => END;

  Rd.Close (r);
  Wr.Close (w);

END Main.
