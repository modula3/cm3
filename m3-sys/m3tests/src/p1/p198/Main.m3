(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = { tEOF, tDIV };
(* once upon a time these two identifiers hashed to the
   same value in the compiler string table and then it choked. *)

BEGIN
  EVAL BITSIZE (T);
END Main.
