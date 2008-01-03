(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Hello world *)

MODULE Main;
IMPORT Wr, Stdio;
<*FATAL ANY*>
BEGIN
  Wr.PutText (Stdio.stdout, "Hello world!\n");
END Main.
