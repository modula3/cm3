(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Type operations: TYPECODE, NARROW *)

MODULE Main;

FROM Test IMPORT done, checkI, checkB;

TYPE
  T1 = REF INTEGER;
  T2 = REF CHAR;
  T3 = REF BOOLEAN;

  T10 = OBJECT a: INTEGER; END;
  T11 = T10 BRANDED       OBJECT b: INTEGER; END;
  T12 = T10 BRANDED "T12" OBJECT b: INTEGER; END;
  T13 = T10 BRANDED "T13" OBJECT b: INTEGER; END;
  T14 = T10 BRANDED       OBJECT b: INTEGER; END;
  T16 = T10 		  OBJECT b: INTEGER; END;
  T17 = T10 		  OBJECT b: INTEGER; END;

VAR
  v1: T1;
  v2: T2;
  v3: REF INTEGER;
  v4: REFANY;

  v10: T10; v11: T11; v12: T12; v13: T13; v14: T14;
  v16: T16; v17: T17;
  vo: ROOT;

  v: INTEGER;

  ttt: ARRAY [1..9] OF INTEGER;

  te := "cde";

BEGIN

v := TYPECODE (ROOT);
checkI (TYPECODE (NULL), 0);
checkI (TYPECODE (NIL), 0);

checkI (TYPECODE ("abc"), 1);
checkI (TYPECODE (te), 1);

ttt[1] := TYPECODE (T1);
ttt[2] := TYPECODE (T2);
ttt[3] := TYPECODE (T3);

v1 := NEW (T1); v2 := NEW (T2); v3 := NEW (REF INTEGER);

checkI (TYPECODE (v1), TYPECODE (T1));
checkI (TYPECODE (v2), TYPECODE (T2));
checkI (TYPECODE (v3), TYPECODE (T1));

ttt[4] := TYPECODE (T10);
ttt[5] := TYPECODE (T11);
ttt[6] := TYPECODE (T12);
ttt[7] := TYPECODE (T13);
ttt[8] := TYPECODE (T14);
ttt[9] := TYPECODE (T16);
checkI (TYPECODE (T17), TYPECODE(T16));

FOR i := 1 TO 9 DO
  FOR j := i+1 TO 9 DO
    checkB (ttt[i] = ttt[j], FALSE); END; END;

v10 := NEW (T10); v11 := NEW (T11); v12 := NEW (T12);
v13 := NEW (T13); v14 := NEW (T14); 
v16 := NEW (T16); v17 := NEW (T17);

checkI (TYPECODE (v10), TYPECODE(T10));
checkI (TYPECODE (v11), TYPECODE(T11));
checkI (TYPECODE (v12), TYPECODE(T12));
checkI (TYPECODE (v13), TYPECODE(T13));
checkI (TYPECODE (v14), TYPECODE(T14));
checkI (TYPECODE (v16), TYPECODE(T16));
checkI (TYPECODE (v17), TYPECODE(T17));

v4 := v1;
v3 := NARROW (v4, T1);

vo := v10;
v10 := NARROW (vo, T10);

vo := v11;
v10 := NARROW (vo, T10);

vo := v16;
v17 := NARROW (vo, T17);

v17 := NARROW (vo, T16);


done();

END Main.
