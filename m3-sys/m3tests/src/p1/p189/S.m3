(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE S;
IMPORT V, N, Z, Test;
PROCEDURE P () =
  BEGIN
    EVAL N.P;
    EVAL V.P;
  END P;
BEGIN
  Test.checkI (Z.a, 2);
  INC (Z.b);
END S.
