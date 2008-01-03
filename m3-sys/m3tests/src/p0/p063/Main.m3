(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT A, B;

BEGIN
  EVAL BITSIZE (A.T); (* to get rid of the unused symbol warning *)
  EVAL BITSIZE (B.U); (* to get rid of the unused symbol warning *)
END Main.
