(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Text;

PROCEDURE Main () = 
  VAR t: TEXT;
  BEGIN
    t := Text.Cat ("foo", "bar", "baz", "quux");
  END Main;

BEGIN
  Main ();
END Main.
