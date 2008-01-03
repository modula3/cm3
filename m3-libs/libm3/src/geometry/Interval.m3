(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed May 12 12:14:58 PDT 1993 by swart      *)
(*      modified on Tue Feb 11 16:20:58 PST 1992 by muller     *)
(*      modified on Tue Nov 19 17:47:52 PST 1991 by gnelson    *)
(*      modified on Thu Sep 19 18:25:13 1991 by kalsow         *)
(*      modified on Tue Jul 25 14:12:51 PDT 1989 by luca       *)
(*      modified on Mon Jul 17 01:18:21 1989 by stolfi         *)
(*      modified on Thu Mar 5 17:12:35 1987 by msm             *)

MODULE Interval;

IMPORT Word, Text;

PROCEDURE FromBounds (lo, hi: INTEGER): T RAISES {} =
  VAR a: T;
  BEGIN
    IF lo < hi THEN a.lo := lo; a.hi := hi;  ELSE a := Empty;  END;
    RETURN a;
  END FromBounds;

PROCEDURE FromAbsBounds (lo, hi: INTEGER): T RAISES {} =
  VAR a: T;
  BEGIN
    IF lo < hi THEN
      a.lo := lo;
      a.hi := hi;
    ELSIF hi < lo THEN
      a.lo := hi;
      a.hi := lo;
    ELSE
      a := Empty
    END;
    RETURN a;
  END FromAbsBounds;

PROCEDURE FromBound (lo: INTEGER; s: CARDINAL): T RAISES {} =
  VAR a: T;
  BEGIN
    IF s = 0 THEN RETURN Empty;  END;
    a.lo := lo;
    a.hi := lo + s;
    RETURN a;
  END FromBound;

PROCEDURE FromSize (s: CARDINAL): T RAISES {} =
  VAR a: T;
  BEGIN
    a.lo := 0; a.hi := s; RETURN a;
  END FromSize;

PROCEDURE Center (READONLY a: T; b: INTEGER): T RAISES {} =
  VAR res: T; d: INTEGER;
  BEGIN
    IF a.lo = a.hi THEN RETURN a END;
    d := b - (a.lo + a.hi) DIV 2;
    res.lo := a.lo + d;
    res.hi := a.hi + d;
    RETURN res;
  END Center;

PROCEDURE Size (READONLY a: T): CARDINAL RAISES {} =
  BEGIN
    RETURN a.hi - a.lo;
  END Size;

PROCEDURE PickBound (READONLY a: T; n: INTEGER): Bound RAISES {} =
  BEGIN
    IF n <= Middle (a) THEN RETURN Bound.Lo ELSE RETURN Bound.Hi END;
  END PickBound;

PROCEDURE Project (READONLY a: T; n: INTEGER): INTEGER RAISES {} =
  BEGIN
    IF (a.hi <= a.lo) THEN FAIL("Interval.Project: empty interval") END;
    RETURN MAX (a.lo, MIN (a.hi - 1, n))
  END Project;

PROCEDURE Middle (READONLY a: T): INTEGER RAISES {} =
  BEGIN RETURN (a.lo + a.hi) DIV 2 END Middle;

PROCEDURE Move (READONLY a: T; n: INTEGER): T RAISES {} =
  VAR b: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN Empty END;
    b.lo := a.lo + n;
    b.hi := a.hi + n;
    RETURN b;
  END Move;

PROCEDURE Inset (READONLY a: T; n: INTEGER): T RAISES {} =
  VAR b: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN Empty END;
    b.lo := a.lo + n;
    b.hi := a.hi - n;
    IF b.lo >= b.hi THEN RETURN Empty END;
    RETURN b
  END Inset;

PROCEDURE Change (READONLY a: T; dlo, dhi: INTEGER): T RAISES {} =
  VAR b: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN Empty;  END;
    b.lo := a.lo + dlo;
    b.hi := a.hi + dhi;
    IF b.lo >= b.hi THEN RETURN Empty;  END;
    RETURN b;
  END Change;

PROCEDURE MoveBound (x: Bound; READONLY a: T; dn: INTEGER): T RAISES {} =
  VAR b: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN Empty;  END;
    b := a;
    IF (x = Bound.Lo) THEN b.lo := b.lo + dn;  ELSE b.hi := b.hi + dn;  END;
    IF b.lo >= b.hi THEN RETURN Empty;  END;
    RETURN b;
  END MoveBound;

PROCEDURE Join (READONLY a, b: T): T RAISES {} =
  VAR c: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN b;  END;
    IF b.lo >= b.hi THEN RETURN a;  END;
    c.lo := MIN (a.lo, b.lo);
    c.hi := MAX (a.hi, b.hi);
    RETURN c;
  END Join;

PROCEDURE Meet (READONLY a, b: T): T RAISES {} =
  VAR c: T;
  BEGIN
    c.lo := MAX (a.lo, b.lo);
    c.hi := MIN (a.hi, b.hi);
    IF c.lo >= c.hi THEN RETURN Empty;  END;
    RETURN c;
  END Meet;

PROCEDURE Chop (READONLY a: T; n: INTEGER; VAR (* out *) b, c: T) RAISES {} =
  BEGIN
    IF n <= a.lo THEN
      b := Empty;
      c := a
    ELSIF n >= a.hi THEN
      b := a;
      c := Empty
    ELSE
      b.lo := a.lo;
      b.hi := n;
      c.lo := n;
      c.hi := a.hi
    END
  END Chop;

PROCEDURE Factor (READONLY a, by: T;  VAR (*out*) f: Partition; dn: INTEGER)
  RAISES {} =
  VAR index: [0..2]; temp: T;
  BEGIN
    IF dn > 0 THEN index := 2;  ELSE index := 0;  END;
    Chop (a, by.lo, f[index], temp);
    Chop (temp, by.hi, f[1], f[2 - index]);
  END Factor;

PROCEDURE Mod (n: INTEGER; READONLY a: T): INTEGER RAISES {} =
  VAR res: INTEGER;
  BEGIN
    IF (a.lo >= a.hi) THEN FAIL("Interval.Mod: a is empty!") END;
    res := (n - a.lo) MOD (a.hi - a.lo);
    RETURN res + a.lo
  END Mod;

PROCEDURE Equal (READONLY a, b: T): BOOLEAN RAISES {} =
  BEGIN RETURN a = b END Equal;

PROCEDURE IsEmpty (READONLY a: T): BOOLEAN RAISES {} =
  BEGIN RETURN a.lo >= a.hi; END IsEmpty;

PROCEDURE Member (n: INTEGER; READONLY a: T): BOOLEAN RAISES {} =
  BEGIN RETURN (a.lo <= n) AND (n < a.hi) END Member;

PROCEDURE Overlap (READONLY a, b: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN MAX (a.lo, b.lo) < MIN (a.hi, b.hi);
  END Overlap;

PROCEDURE Subset (READONLY a, b: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (a.lo >= a.hi) OR ((a.lo >= b.lo) AND (a.hi <= b.hi));
  END Subset;

PROCEDURE Compare (READONLY a, b: T): [-1..1] =
  BEGIN
    IF (a.lo < b.lo) THEN
      RETURN  -1;
    ELSIF (a.lo > b.lo) THEN
      RETURN  +1;
    ELSIF (a.hi = b.hi) THEN
      RETURN 0;
    ELSIF (a.hi < b.hi) THEN
      RETURN  -1;
    ELSE
      RETURN  +1;
    END;
  END Compare;

PROCEDURE Hash (READONLY a: T): Word.T =
  BEGIN
    RETURN Word.Xor (a.lo, a.hi);
  END Hash;

EXCEPTION ASSERT_FAILED (Text.T);
<*INLINE*> PROCEDURE FAIL (msg: Text.T := NIL) =
<*FATAL ASSERT_FAILED*>
  BEGIN
    RAISE ASSERT_FAILED (msg);
  END FAIL;

BEGIN
END Interval.
