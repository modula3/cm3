(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Rd, Stdio;
FROM Test IMPORT done, msg;

VAR i: TEXT;

BEGIN
  TRY
    LOOP
      msg ("Enter something");
      i := Rd.GetLine (Stdio.stdin);
      msg ("thanks for " & i & "$$$"); END;
  EXCEPT 
  | Rd.EndOfFile => msg ("you don\'t want to speak, eh ?"); END;

  done ();
END Main.
