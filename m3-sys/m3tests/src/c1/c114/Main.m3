(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

VAR
  x: UNTRACED REF INTEGER;
  y: REF INTEGER;

PROCEDURE P () =
  VAR
    x: UNTRACED REF INTEGER;
    y: REF INTEGER;
  BEGIN
    x^ := 7;
    y^ := 8;
  END P;

BEGIN
  P();
  x^ := 7;
  y^ := 8;
END Main.

  
