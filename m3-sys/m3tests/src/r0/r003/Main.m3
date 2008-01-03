(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T1 = ARRAY OF INTEGER;

VAR v : REF T1;

PROCEDURE foo (<*UNUSED*>x: T1) = BEGIN END foo;

PROCEDURE Proc(<*UNUSED*>x : ARRAY[1..10] OF INTEGER) = BEGIN END Proc;

BEGIN
    foo (ARRAY [1..2] OF INTEGER { 1, 2 });
    v := NEW( REF T1, 11);
    (* Doesn't appear to generate a runtime check *)
    Proc( v^);
END Main.

