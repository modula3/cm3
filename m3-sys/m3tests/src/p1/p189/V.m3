(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE V;
IMPORT N, S, Z, Test;
PROCEDURE P () =
  BEGIN
    EVAL N.P;
    EVAL S.P;
  END P;
BEGIN
  Test.checkI (Z.a, 2);
  INC (Z.b);
END V.
