(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: NIL is a ROOT *)

MODULE Main;

TYPE
  o <: ROOT;

REVEAL
  o = BRANDED "bar" OBJECT
       integer:  INTEGER;
       next: o;
     END;

TYPE 
  oo = BRANDED "barbar" OBJECT
       integer:  INTEGER;
       next: o;
     END;

VAR
  x: o;
  y: oo;

BEGIN
x := NIL;
x.integer := 3;
x.next := NIL;
x.next.next := NIL;

y := NIL;
y.integer := 3;
y.next := NIL;
y.next.next := NIL;

END Main.
