(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE A;

IMPORT RefList;

PROCEDURE P(proc := RefList.Length) =
  BEGIN
    EVAL proc(NIL);
  END P;

BEGIN 
END A.
