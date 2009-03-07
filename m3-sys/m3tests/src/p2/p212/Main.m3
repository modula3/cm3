(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)

MODULE Main;
IMPORT Test;

VAR t:Test.T;
BEGIN
  Test.CheckFloatsAndTypes(t);
END Main.
