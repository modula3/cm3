(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE U;
IMPORT Z, Test;
PROCEDURE P () = BEGIN END P;
BEGIN
  Test.checkI (Z.a, 1);
  Z.a := 2;
END U.
