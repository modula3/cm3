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
    i := -5;

PROCEDURE TestIn (s: T2; READONLY c: ARRAY OF BOOLEAN) =
BEGIN
  FOR i := 0 TO LAST (v) DO checkB (v[i] IN s, c[i]); END;
END TestIn;

BEGIN

checkB(-4 IN s1, FALSE); checkB(-3 IN s1, FALSE); checkB(-2 IN s1, FALSE); checkB(-1 IN s1, FALSE); checkB(-0 IN s1, FALSE);
checkB( 0 IN s1, FALSE); checkB( 1 IN s1, FALSE); checkB( 2 IN s1, FALSE); checkB( 3 IN s1, FALSE); checkB( 4 IN s1, FALSE);
checkB( 5 IN s1, TRUE ); checkB( 6 IN s1, FALSE); checkB( 7 IN s1, TRUE ); checkB( 8 IN s1, FALSE); checkB( 9 IN s1, FALSE);
checkB(10 IN s1, FALSE); checkB(11 IN s1, TRUE ); checkB(12 IN s1, FALSE); checkB(13 IN s1, FALSE); checkB(14 IN s1, FALSE);
checkB(15 IN s1, FALSE); checkB(16 IN s1, FALSE); checkB(17 IN s1, FALSE); checkB(18 IN s1, FALSE); checkB(19 IN s1, FALSE);
checkB(20 IN s1, FALSE); checkB(21 IN s1, FALSE); checkB(22 IN s1, FALSE); checkB(23 IN s1, FALSE); checkB(24 IN s1, FALSE);
checkB(25 IN s1, FALSE); checkB(26 IN s1, FALSE); checkB(27 IN s1, FALSE); checkB(28 IN s1, FALSE); checkB(29 IN s1, FALSE);
checkB(30 IN s1, FALSE); checkB(31 IN s1, FALSE); checkB(32 IN s1, FALSE); checkB(33 IN s1, FALSE); checkB(34 IN s1, FALSE);
checkB(35 IN s1, FALSE); checkB(36 IN s1, FALSE); checkB(37 IN s1, FALSE); checkB(38 IN s1, FALSE); checkB(39 IN s1, FALSE);
checkB(40 IN s1, FALSE); checkB(41 IN s1, FALSE); checkB(42 IN s1, FALSE); checkB(43 IN s1, FALSE); checkB(44 IN s1, TRUE );
checkB(45 IN s1, FALSE); checkB(46 IN s1, FALSE); checkB(47 IN s1, FALSE); checkB(48 IN s1, FALSE); checkB(49 IN s1, FALSE);

INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);
INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);
INC(i); checkB(i IN s1, TRUE ); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, TRUE ); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);
INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, TRUE ); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);
INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);
INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);
INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);
INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);
INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);
INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, TRUE );
INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE); INC(i); checkB(i IN s1, FALSE);

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
