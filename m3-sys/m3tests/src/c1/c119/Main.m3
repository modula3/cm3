(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE
    T1 = ARRAY OF INTEGER;
VAR
    v : REF T1 := NEW( REF T1, 10);

PROCEDURE Proc( VAR x : ARRAY[1..10] OF INTEGER) =
  BEGIN
    EVAL x;
  END Proc;

BEGIN
(*
    For a VAR parameter, the actual must be a writable designator whose type is
    the same as that of the formal, or, in case of a VAR array parameter, 
     assignable to that of the formal.
    T1 is assignable to ARRAY[1..10] OF INTEGER if:
       ARRAY[1..10] OF INTEGER <: T1
    These conditions are met, so why is this an error?
*)
   Proc( v^);
END Main.
