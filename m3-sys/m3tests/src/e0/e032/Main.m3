(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
(* importer cannot see "FROM IMPORTS" *)

IMPORT A;

BEGIN
  RAISE A.Alerted;
END Main.
