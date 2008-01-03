(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

(* try a nasty recursion through the procedure value *)

TYPE T = PROCEDURE (t: T);

PROCEDURE P (t: T := P) =
  BEGIN
    EVAL t;
  END P;

BEGIN
  EVAL P;  
END Main.
