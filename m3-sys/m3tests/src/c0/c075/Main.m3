(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE
  IntStar = ARRAY OF INTEGER;

CONST
  funny = IntStar {1, 2, 3};

VAR
  x : INTEGER;

BEGIN

x := funny [1];

END Main.
