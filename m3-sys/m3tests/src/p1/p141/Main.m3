(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test, RTProcess;

BEGIN
  Test.check (TRUE);
  RTProcess.Exit (1);  (* HALT(1); *)
  Test.check (FALSE);
  Test.done ();
END Main.
