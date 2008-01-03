(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE
  IntStar = ARRAY OF INTEGER;

VAR
  anotherChose := ARRAY [1..3] OF INTEGER {10,20,30};

PROCEDURE P (x: IntStar) =
  BEGIN EVAL x; END P;

BEGIN
  EVAL anotherChose;
  EVAL P;
END Main.
