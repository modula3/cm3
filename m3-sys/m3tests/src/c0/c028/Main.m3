(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: type minimization of REF/RECORD *)

MODULE Main;

TYPE
  r1 = REF r1;
  r2 = REF REF r2;
  r3 = REF REF REF r3;
  t  = INTEGER;
  x1 = RECORD i: INTEGER;  r: r1 END;
  x2 = RECORD i: t;        r: r3 END;
  x3 = RECORD i: INTEGER;  r: REF r2 END;

PROCEDURE P (<*UNUSED*> VAR x: x2) =
  BEGIN
  END P;

VAR a: x1;
VAR b: x3;
BEGIN
  P (a);
  P (b);
END Main.
