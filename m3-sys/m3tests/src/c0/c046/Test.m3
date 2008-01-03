(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: using a revealed opaque type *)

MODULE Test;

REVEAL
  T = BRANDED "T" REF CHAR;

VAR
  t : T;

BEGIN
  t := NEW (T);
  t^ := 'a';
END Test.
