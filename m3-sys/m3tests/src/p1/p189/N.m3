(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE N;
IMPORT S, U, Z, Test;
PROCEDURE P () =
  BEGIN
    EVAL S.P;
    EVAL U.P;
  END P;
BEGIN
  Test.checkI (Z.a, 2);
  INC (Z.b);
END N.
