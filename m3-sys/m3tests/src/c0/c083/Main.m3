(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = RECORD west, east, north, south: INTEGER; END;
CONST Empty = T {0,0,0,0};

PROCEDURE P () : T RAISES ANY = 
  PROCEDURE Q (): T RAISES ANY =
    BEGIN RETURN Empty; END Q;
  BEGIN
    EVAL Q;
    RETURN Empty;
  END P;

BEGIN
  EVAL P;
END Main.
