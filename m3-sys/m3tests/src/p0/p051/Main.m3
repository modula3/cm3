(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Test, A, B;

BEGIN
  A.m3_test_var := 0;
  Test.checkI (A.m3_test_var, B.m3_test_var);
  Test.done ();
END Main.
