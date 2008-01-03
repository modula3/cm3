(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

TYPE R1 = RECORD a: INTEGER := 1 END;
TYPE R2 = RECORD a: INTEGER := 2 END;

VAR y1: R1;  y2: R2;

BEGIN
  y1 := y2;
  checkI (y1.a, y2.a);
  done ();
END Main.
