(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

TYPE
  F2 = ARRAY [0..9] OF ARRAY [0..9] OF INTEGER;
  O1F1 = ARRAY OF ARRAY [0..9] OF INTEGER;
  O2 = ARRAY OF ARRAY OF INTEGER;

PROCEDURE f2 (<*NOWARN*> x: F2) =
  VAR
    vf2 := NEW (REF F2);
    vo1f1 := NEW (REF O1F1, 10);
    vo2 := NEW (REF O2, 10, 10);
  BEGIN
    FOR i := 0 TO 9 DO
      FOR j := 0 TO 9 DO 
        checkI (x[i][j], 2*i + j); END; END;
    vf2^ := x;
    vo1f1^ := x;
    vo2^ := x;
    Check (vf2^);
    Check (vo1f1^);
    Check (vo2^);
  END f2;

PROCEDURE o1f1 (<*NOWARN*> x: O1F1) = 
  VAR
    vf2 := NEW (REF F2);
    vo1f1 := NEW (REF O1F1, 10);
    vo2 := NEW (REF O2, 10, 10);
  BEGIN
    FOR i := 0 TO 9 DO
      FOR j := 0 TO 9 DO 
        checkI (x[i][j], 2*i + j); END; END;
    vf2^ := x;
    vo1f1^ := x;
    vo2^ := x;
    Check (vf2^);
    Check (vo1f1^);
    Check (vo2^);
  END o1f1;

PROCEDURE o2 (<*NOWARN*> x: O2) =
  VAR
    vf2 := NEW (REF F2);
    vo1f1 := NEW (REF O1F1, 10);
    vo2 := NEW (REF O2, 10, 10);
  BEGIN
    FOR i := 0 TO 9 DO
      FOR j := 0 TO 9 DO 
        checkI (x[i][j], 2*i + j); END; END;
    vf2^ := x;
    vo1f1^ := x;
    vo2^ := x;
    Check (vf2^);
    Check (vo1f1^);
    Check (vo2^);
  END o2;

PROCEDURE Check (<*NOWARN*> x: O2) =
  BEGIN
    FOR i := 0 TO 9 DO
      FOR j := 0 TO 9 DO 
        checkI (x[i][j], 2*i + j); END; END;
  END Check;    

VAR 
  vf2: REF F2;
  vo1f1: REF O1F1;
  vo2: REF O2;

BEGIN

vf2 := NEW (REF F2);
vo1f1 := NEW (REF O1F1, 10);
vo2 := NEW (REF O2, 10, 10);

FOR i := 0 TO 9 DO
  FOR j := 0 TO 9 DO 
    vf2[i][j] := 2 * i + j;
    vo1f1[i][j] := 2 * i + j;
    vo2[i][j] := 2 * i + j; END; END;

f2 (vf2^);
f2 (vo1f1^);
f2 (vo2^);

o1f1 (vf2^);
o1f1 (vo1f1^);
o1f1 (vo2^);

o2 (vf2^);
o2 (vo1f1^);
o2 (vo2^);

done ();

END Main.

