(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = REF RECORD a: BITS 32 FOR RECORD aa: BOOLEAN; END; END;

BEGIN
  EVAL BITSIZE (T);
END Main.
