(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Test;

TYPE W = Test.T;
VAR x : Test.T;
VAR w : W;

BEGIN
  EVAL x;
  EVAL w;
END Main.
