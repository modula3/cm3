(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Hello world *)

MODULE Main;
IMPORT Wr, Stdio;
IMPORT Test;
<*FATAL ANY*>

VAR t:Test.T;
BEGIN
  Test.CheckFloatsAndTypes(t);
  Wr.PutText (Stdio.stdout, "Hello world!\n");
END Main.
