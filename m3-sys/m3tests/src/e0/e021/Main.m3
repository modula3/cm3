(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

VAR
  v := P ();

PROCEDURE P (): ARRAY [1..NUMBER(v)] OF INTEGER = 
  BEGIN
    RETURN ARRAY [1..NUMBER(v)] OF INTEGER {0,..};
  END P;

BEGIN
END Main.
