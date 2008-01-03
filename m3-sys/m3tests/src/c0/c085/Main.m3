(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
UNSAFE MODULE Main;

VAR
  string: ARRAY [0..10] OF CHAR;
  step : INTEGER;
  adr: UNTRACED REF CHAR;

BEGIN
  adr := ADR (string[0]);
  INC (adr);
  step := 1;
  INC (adr, 1);
END Main.
