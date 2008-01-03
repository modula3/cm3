(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Thu Jun 17 14:28:58 PDT 1993 by kalsow    *)
(*      modified on Thu Apr 19 03:31:54 1990 by muller        *)

MODULE Main;

FROM Test IMPORT msg, done;
IMPORT Fmt, Thread;

BEGIN
  FOR i := 1 TO 10 DO
    msg (Fmt.Int (i));
    Thread.Pause (1.0d0);
  END;

  done ();
END Main.

