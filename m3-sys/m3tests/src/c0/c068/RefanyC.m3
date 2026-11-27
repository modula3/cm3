(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE RefanyC;

PROCEDURE Compare (a, b: T): INTEGER =
(*  VAR t : T; *)
  BEGIN
    EVAL a;
    EVAL b;
(*    NEW (t); *)
    RETURN 0;
  END Compare;

BEGIN
END RefanyC.
