(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main EXPORTS Main, A;

IMPORT Text;

PROCEDURE P (t: T): T =
  BEGIN
    RETURN t;
  END P;

BEGIN
  EVAL BITSIZE (Text.T);
END Main.
