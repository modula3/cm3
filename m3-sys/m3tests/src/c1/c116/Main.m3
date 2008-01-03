(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE
    base = {green, blue, red, yellow};
    index = [base.blue..base.red];
    array = ARRAY index OF CHAR;

VAR
    a   : array;

BEGIN
  a[base.blue] := 'B';
  a[base.red]  := 'C';
END Main.
