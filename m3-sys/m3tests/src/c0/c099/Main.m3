(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Test, Word;

TYPE T = Test.T;
     V = REF Word.T;

VAR t: T; v: V;

BEGIN
t := 1; v := NEW (V);
END Main.
