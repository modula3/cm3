(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

VAR
  i: INTEGER := 35;
BEGIN
  Test.checkI (35 DIV 2, 17); (* Succeeds *)
  Test.checkI (i DIV 2, 17);  (* Succeeds *)
  Test.done ();
END Main.
