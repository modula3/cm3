(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE foo;

TYPE 
  TwoVec   = ARRAY [0..1] OF LONGREAL;

PROCEDURE proc1 (READONLY p : ARRAY [0..2] OF TwoVec); 
PROCEDURE proc2 (READONLY p : ARRAY OF TwoVec); 

END foo.
