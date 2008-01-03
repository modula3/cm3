(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE
    (* ANSI C does not allow empty structs *)
    R = RECORD END;
BEGIN
  EVAL BYTESIZE (R);
END Main.
