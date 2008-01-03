(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Text.PutStr (passing a fixed array to an open array formal) *)

MODULE Main;

IMPORT Text;

VAR s : ARRAY [1..10] OF CHAR;
    t : Text.T;

BEGIN
 Text.SetChars (s, t);
END Main.
