(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkB, done;

TYPE T = SET OF [5..45];
     A = [0..7];

CONST t = TRUE; f = FALSE;

VAR s1 := T {5, 7,    11,     44};
    s2 := T {   7, 9,     40, 44};
    s3 := T {   7,            44};

    v := ARRAY A OF INTEGER  { 1, 5, 6, 7, 40, 43, 44, 100 };

    v1 := ARRAY A OF BOOLEAN { f, t, f, t, f, f, t, f};
    v2 := ARRAY A OF BOOLEAN { f, f, f, t, t, f, t, f};
    v3 := ARRAY A OF BOOLEAN { f, f, f, t, f, f, t, f};

    v1plus2 := ARRAY A OF BOOLEAN  { f, t, f, t, t, f, t, f};
    v1inter2 := ARRAY A OF BOOLEAN { f, f, f, t, f, f, t, f};
    v1diff2 := ARRAY A OF BOOLEAN  { f, t, f, f, f, f, f, f};
    v1div2 := ARRAY A OF BOOLEAN   { f, t, f, f, t, f, f, f};

PROCEDURE TestIn (s: T; READONLY c: ARRAY OF BOOLEAN) =
BEGIN
  FOR i := 0 TO LAST (v) DO checkB (v[i] IN s, c[i]); END;
END TestIn;

BEGIN


TestIn (s1, v1);
TestIn (s2, v2);
TestIn (s3, v3);

TestIn (s1 + s2, v1plus2);
TestIn (s1 * s2, v1inter2);
TestIn (s1 - s2, v1diff2);
TestIn (s1 / s2, v1div2);

checkB (s1 <  s1, FALSE);
checkB (s1 <= s1, TRUE);
checkB (s1 =  s1, TRUE);
checkB (s1 #  s1, FALSE);
checkB (s1 >= s1, TRUE);
checkB (s1 >  s1, FALSE);

checkB (s1 <  s2, FALSE);
checkB (s1 <= s2, FALSE);
checkB (s1 =  s2, FALSE);
checkB (s1 #  s2, TRUE);
checkB (s1 >= s2, FALSE);
checkB (s1 >  s2, FALSE);

checkB (s1 <  s3, FALSE);
checkB (s1 <= s3, FALSE);
checkB (s1 =  s3, FALSE);
checkB (s1 #  s3, TRUE);
checkB (s1 >= s3, TRUE);
checkB (s1 >  s3, TRUE);

done ();

END Main.
