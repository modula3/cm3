(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT N, Z, Test;
BEGIN
  Test.checkI (Z.a, 2);
  Test.checkI (Z.b, 3);
  N.P ();
  Test.done ();
END Main.
