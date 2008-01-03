(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue May 11 17:18:19 PDT 1993 by swart          *)
(*      modified on Mon May 10 09:31:20 PDT 1993 by muller         *)
(*      modified on Mon Aug 26 22:53:51 1991 by kalsow         *)
(*      modified on Fri Jul 22 18:59:24 PDT 1988 by stewart    *)
(*      modified on Fri Jul 22 13:44:56 PDT 1988 by luca       *)

UNSAFE MODULE Transform;

IMPORT Point, Math, Word;


PROCEDURE Apply (tr: T; p: Point.T): Point.T =
  VAR xp: Point.T; temp: REAL;
  BEGIN
    WITH z = tr DO
      temp := 5.5000001e-01 + FLOAT (p.h) * z.a11 + FLOAT (p.v) * z.a21
        + z.a31;
      IF temp >= 0.0000000e+00 THEN
        xp.h := TRUNC (temp);
      ELSE
        xp.h := TRUNC (temp - 1.0000000e+00);
      END;
      temp := 5.5000001e-01 + FLOAT (p.h) * z.a12 + FLOAT (p.v) * z.a22
        + z.a32;
      IF temp >= 0.0000000e+00 THEN
        xp.v := TRUNC (temp);
      ELSE
        xp.v := TRUNC (temp - 1.0000000e+00);
      END;
    END;
    RETURN xp
  END Apply;

PROCEDURE Identity (): T =
  VAR tr: T;
  BEGIN
    WITH z = tr DO
      z.a11 := 1.0000000e+00;
      z.a12 := 0.0000000e+00;
      z.a21 := 0.0000000e+00;
      z.a22 := 1.0000000e+00;
      z.a31 := 0.0000000e+00;
      z.a32 := 0.0000000e+00
    END;
    RETURN tr
  END Identity;
(* translate is equivalent to the following composition:
|
|    [a b 1][ a11 a12 0 ][ 1 0 0 ]
|           [ a21 a22 0 ][ 0 1 0 ]
|           [ a31 a32 1 ][ h v 1 ]
|
|   =[a b 1][  a11   a12  0 ]
|           [  a21   a22  0 ]
|           [ a31+h a32+v 1 ]
*)

PROCEDURE Translate (h, v: REAL; READONLY tr: T): T =
  VAR xtr: T;
  BEGIN
    xtr := tr;
    xtr.a31 := xtr.a31 + h;
    xtr.a32 := xtr.a32 + v;
    RETURN xtr
  END Translate;
(* rotate is equivalent to the following composition:
|
|    [a b 1][    a11        a12     0 ][  c  s  0 ]
|           [    a21        a22     0 ][ -s  c  0 ]
|           [    a31        a32     1 ][  0  0  1 ]
|   =[a b 1][ a11c-a12s  a11s+a12c  0]
|           [ a21c-a22s  a21s-a22c  0]
|           [ a31c-a32s  a31s+a32c  1]
*)

PROCEDURE Rotate (theta: REAL; READONLY tr: T): T =
  VAR xtr: T; c, s: REAL;
  BEGIN
    c := FLOAT (Math.cos (FLOAT (theta, LONGREAL)));
    s := FLOAT (Math.sin (FLOAT (theta, LONGREAL)));
    xtr.a11 := tr.a11 * c - tr.a12 * s;
    xtr.a12 := tr.a11 * s + tr.a12 * c;
    xtr.a21 := tr.a21 * c - tr.a22 * s;
    xtr.a22 := tr.a21 * s + tr.a22 * c;
    xtr.a31 := tr.a31 * c - tr.a32 * s;
    xtr.a32 := tr.a31 * s + tr.a32 * c;
    RETURN xtr
  END Rotate;
(* Scale is equivalent to the following composition:
|
|    [a b 1][ a11 a12 0 ][ fh  0 0 ]
|           [ a21 a22 0 ][  0 fv 0 ]
|           [ a31 a32 1 ][  0  0 1 ]
|
|   =[a b 1][ a11*fh a12*fv 0 ]
|           [ a21*fh a22*fv 0 ]
|           [ a31*fh a32*fv 1 ]
*)

PROCEDURE Scale (fh, fv: REAL; READONLY tr: T): T =
  VAR xtr: T;
  BEGIN
    xtr.a11 := tr.a11 * fh;
    xtr.a21 := tr.a21 * fh;
    xtr.a31 := tr.a31 * fh;
    xtr.a12 := tr.a12 * fv;
    xtr.a22 := tr.a22 * fv;
    xtr.a32 := tr.a32 * fv;
    RETURN xtr
  END Scale;

PROCEDURE FromPoint (READONLY p: Point.T): T =
  VAR tr: T;
  BEGIN
    WITH z = tr DO
      z.a11 := 1.0000000e+00;
      z.a12 := 0.0000000e+00;
      z.a21 := 0.0000000e+00;
      z.a22 := 1.0000000e+00;
      z.a31 := FLOAT (p.h);
      z.a32 := FLOAT (p.v)
    END;
    RETURN tr
  END FromPoint;

PROCEDURE Compose (READONLY t1, t2: T): T =
  VAR tr: T;
  BEGIN
    WITH z = tr DO
      z.a11 := t1.a11 * t2.a11 + t1.a12 * t2.a21;
      z.a12 := t1.a11 * t2.a12 + t1.a12 * t2.a22;
      z.a21 := t1.a21 * t2.a11 + t1.a22 * t2.a21;
      z.a22 := t1.a21 * t2.a12 + t1.a22 * t2.a22;
      z.a31 := t1.a31 * t2.a11 + t1.a32 * t2.a21 + t2.a31;
      z.a32 := t1.a31 * t2.a12 + t1.a32 * t2.a22 + t2.a32
    END;
    RETURN tr
  END Compose;
(* rotate about is equivalent to the composition of three transforms:
   translation to the origin, rotation, and translation back to the point:
|
|   [a b 1][  1  0  0 ][  c s 0 ][ 1 0 0 ]
|          [  0  1  0 ][ -s c 0 ][ 0 1 0 ]
|          [ -h -v  1 ][  0 0 1 ][ h v 1 ]
|
|  =[a b 1][    c     s    0 ][ 1 0 0 ]
|          [   -s     c    0 ][ 0 1 0 ]
|          [ -hc+vs -vc-hs 1 ][ h v 1 ]
|
|  =[a b 1][    c       s    0 ]
|          [   -s       c    0 ]
|          [ h-hc+vs v-vc-hs 1 ]
*)

PROCEDURE RotateAbout (READONLY p: Point.T; theta: REAL): T =
  VAR tr: T; c, s: REAL;
  BEGIN
    c := FLOAT (Math.cos (FLOAT (theta, LONGREAL)));
    s := FLOAT (Math.sin (FLOAT (theta, LONGREAL)));
    WITH z = tr DO
      z.a11 := c;
      z.a12 := s;
      z.a21 :=  -s;
      z.a22 := c;
      z.a31 := FLOAT (p.h) * (1.0000000e+00 - c) + FLOAT (p.v) * s;
      z.a32 := FLOAT (p.v) * (1.0000000e+00 - c) - FLOAT (p.h) * s;
    END;
    RETURN tr
  END RotateAbout;

PROCEDURE IsoScale (f: REAL): T =
  VAR tr: T;
  BEGIN
    tr.a11 := f;
    tr.a12 := 0.0000000e+00;
    tr.a21 := 0.0000000e+00;
    tr.a22 := f;
    tr.a31 := 0.0000000e+00;
    tr.a32 := 0.0000000e+00;
    RETURN tr
  END IsoScale;

PROCEDURE AnIsoScale (fh, fv: REAL): T =
  VAR tr: T;
  BEGIN
    tr.a11 := fh;
    tr.a12 := 0.0000000e+00;
    tr.a21 := 0.0000000e+00;
    tr.a22 := fv;
    tr.a31 := 0.0000000e+00;
    tr.a32 := 0.0000000e+00;
    RETURN tr
  END AnIsoScale;

PROCEDURE Compare (READONLY a, b: T): [-1 .. 1] =
  BEGIN
    IF (a.a11 < b.a11) THEN RETURN  -1 END;
    IF (a.a11 > b.a11) THEN RETURN  +1 END;
    IF (a.a12 < b.a12) THEN RETURN  -1 END;
    IF (a.a12 > b.a12) THEN RETURN  +1 END;
    IF (a.a21 < b.a21) THEN RETURN  -1 END;
    IF (a.a21 > b.a21) THEN RETURN  +1 END;
    IF (a.a22 < b.a22) THEN RETURN  -1 END;
    IF (a.a22 > b.a22) THEN RETURN  +1 END;
    IF (a.a31 < b.a31) THEN RETURN  -1 END;
    IF (a.a31 > b.a31) THEN RETURN  +1 END;
    IF (a.a32 < b.a32) THEN RETURN  -1 END;
    IF (a.a32 > b.a32) THEN RETURN  +1 END;
    RETURN 0;
  END Compare;

PROCEDURE Equal (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN (a.a11 = b.a11) AND (a.a12 = b.a12) AND (a.a21 = b.a21)
    AND (a.a22 = b.a22) AND (a.a31 = b.a31) AND (a.a32 = b.a32);
  END Equal;

PROCEDURE Hash (READONLY a: T): INTEGER =
  TYPE  Int = BITS BITSIZE(REAL) FOR [-16_7fffffff-1 .. 16_7fffffff];
  BEGIN
    RETURN
        Word.Xor (
          Word.Xor (LOOPHOLE (a.a11, Int), LOOPHOLE (a.a12, Int)),
            Word.Xor (
              Word.Xor (LOOPHOLE (a.a21, Int), LOOPHOLE (a.a22, Int)),
              Word.Xor (LOOPHOLE (a.a31, Int), LOOPHOLE (a.a32, Int))));
  END Hash;

BEGIN
END Transform.
