(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkB, done;

TYPE T1 = SET OF [5..35]; (* < 32 bits *)
     T2 = SET OF [5..45]; (* < 64 bits *)
     T3 = SET OF [5..75]; (* < 128 bits *)
     T4 = SET OF [5..195]; (* > 128 bits *)
     A = [0..7];

CONST t = TRUE; f = FALSE;

VAR r1 := T1 {5, 7,    11,     34};
    r2 := T1 {   7, 9,     30, 34};
    r3 := T1 {   7,            34};
    s1 := T2 {5, 7,    11,     44};
    s2 := T2 {   7, 9,     40, 44};
    s3 := T2 {   7,            44};
    t1 := T3 {5, 7,    11,     74};
    t2 := T3 {   7, 9,     70, 74};
    t3 := T3 {   7,            74};
    u1 := T4 {5, 7,    11,     94};
    u2 := T4 {   7, 9,     90, 94};
    u3 := T4 {   7,            94};

    v := ARRAY A OF INTEGER  { 1, 5, 6, 7, 40, 43, 44, 100 };

    v1 := ARRAY A OF BOOLEAN { f, t, f, t, f, f, t, f};
    v2 := ARRAY A OF BOOLEAN { f, f, f, t, t, f, t, f};
    v3 := ARRAY A OF BOOLEAN { f, f, f, t, f, f, t, f};

    v1plus2 := ARRAY A OF BOOLEAN  { f, t, f, t, t, f, t, f};
    v1inter2 := ARRAY A OF BOOLEAN { f, f, f, t, f, f, t, f};
    v1diff2 := ARRAY A OF BOOLEAN  { f, t, f, f, f, f, f, f};
    v1div2 := ARRAY A OF BOOLEAN   { f, t, f, f, t, f, f, f};

PROCEDURE TestIn (s: T2; READONLY c: ARRAY OF BOOLEAN) =
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

checkB (r1 <  r1, FALSE);
checkB (s1 <  s1, FALSE);
checkB (t1 <  t1, FALSE);
checkB (u1 <  u1, FALSE);
checkB (r1 <= r1, TRUE);
checkB (s1 <= s1, TRUE);
checkB (t1 <= t1, TRUE);
checkB (u1 <= u1, TRUE);
checkB (r1 =  r1, TRUE);
checkB (s1 =  s1, TRUE);
checkB (t1 =  t1, TRUE);
checkB (u1 =  u1, TRUE);
checkB (r1 #  r1, FALSE);
checkB (s1 #  s1, FALSE);
checkB (t1 #  t1, FALSE);
checkB (u1 #  u1, FALSE);
checkB (r1 >= r1, TRUE);
checkB (s1 >= s1, TRUE);
checkB (t1 >= t1, TRUE);
checkB (u1 >= u1, TRUE);
checkB (r1 >  r1, FALSE);
checkB (s1 >  s1, FALSE);
checkB (t1 >  t1, FALSE);
checkB (u1 >  u1, FALSE);

checkB (r1 <  r2, FALSE);
checkB (s1 <  s2, FALSE);
checkB (t1 <  t2, FALSE);
checkB (u1 <  u2, FALSE);
checkB (r1 <= r2, FALSE);
checkB (s1 <= s2, FALSE);
checkB (t1 <= t2, FALSE);
checkB (u1 <= u2, FALSE);
checkB (r1 =  r2, FALSE);
checkB (s1 =  s2, FALSE);
checkB (t1 =  t2, FALSE);
checkB (u1 =  u2, FALSE);
checkB (r1 #  r2, TRUE);
checkB (s1 #  s2, TRUE);
checkB (t1 #  t2, TRUE);
checkB (u1 #  u2, TRUE);
checkB (r1 >= r2, FALSE);
checkB (s1 >= s2, FALSE);
checkB (t1 >= t2, FALSE);
checkB (u1 >= u2, FALSE);
checkB (r1 >  r2, FALSE);
checkB (s1 >  s2, FALSE);
checkB (t1 >  t2, FALSE);
checkB (u1 >  u2, FALSE);

checkB (r1 <  r3, FALSE);
checkB (s1 <  s3, FALSE);
checkB (t1 <  t3, FALSE);
checkB (u1 <  u3, FALSE);
checkB (r1 <= r3, FALSE);
checkB (s1 <= s3, FALSE);
checkB (t1 <= t3, FALSE);
checkB (u1 <= u3, FALSE);
checkB (r1 =  r3, FALSE);
checkB (s1 =  s3, FALSE);
checkB (t1 =  t3, FALSE);
checkB (u1 =  u3, FALSE);
checkB (r1 #  r3, TRUE);
checkB (s1 #  s3, TRUE);
checkB (t1 #  t3, TRUE);
checkB (u1 #  u3, TRUE);
checkB (r1 >= r3, TRUE);
checkB (s1 >= s3, TRUE);
checkB (t1 >= t3, TRUE);
checkB (u1 >= u3, TRUE);
checkB (r1 >  r3, TRUE);
checkB (s1 >  s3, TRUE);
checkB (t1 >  t3, TRUE);
checkB (u1 >  u3, TRUE);

done ();

END Main.
