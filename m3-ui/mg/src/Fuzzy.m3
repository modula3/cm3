(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Created by stolfi on Thu Jan 26 13:55:01 1989               *)
(* Last modified on Tue Jul 21 16:13:27 PDT 1992 by harrison   *)
(*      modified on Thu Oct 18 20:46:08 PDT 1990 by stolfi     *)

MODULE Fuzzy;

IMPORT Math, Fmt;

PROCEDURE IsEmpty (READONLY a: T): BOOLEAN =
  BEGIN
    RETURN a.lo > a.hi
  END IsEmpty;

PROCEDURE Member (n: REAL; READONLY a: T): BOOLEAN =
  BEGIN
    RETURN n >= a.lo AND n <= a.hi
  END Member;

PROCEDURE Overlap (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN a.hi >= b.lo AND a.lo <= b.hi
  END Overlap;

PROCEDURE Subset (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN a.lo >= b.lo AND a.hi <= b.hi
  END Subset;

PROCEDURE Scale (s: REAL; READONLY a: T): T =
  BEGIN
    IF a.lo > a.hi THEN RETURN Empty END;
    IF s < 0.0 THEN
      RETURN T{a.hi * s, a.lo * s}
    ELSIF s > 0.0 THEN
      RETURN T{a.lo * s, a.hi * s}
    ELSE (* s = 0.0 *)
      RETURN T{0.0, 0.0}
    END;
  END Scale;

PROCEDURE Shift (s: REAL; READONLY a: T): T =
  BEGIN
    IF a.lo > a.hi THEN RETURN Empty ELSE RETURN T{a.lo + s, a.hi + s} END
  END Shift;

PROCEDURE Expand (s: REAL; READONLY a: T): T =
  BEGIN
    IF a.lo > a.hi THEN
      RETURN T{s, s}
    ELSE
      RETURN T{MIN(a.lo, s), MAX(a.hi, s)}
    END;
  END Expand;

PROCEDURE Add (READONLY a, b: T): T =
  BEGIN
    IF a.lo > a.hi OR b.lo > b.hi THEN
      RETURN Empty
    ELSE
      RETURN T{a.lo + b.lo, a.hi + b.hi}
    END
  END Add;

PROCEDURE Sub (READONLY a, b: T): T =
  BEGIN
    IF a.lo > a.hi OR b.lo > b.hi THEN
      RETURN Empty
    ELSE
      RETURN T{a.lo - b.hi, a.hi - b.lo}
    END;
  END Sub;

PROCEDURE Mul (READONLY a, b: T): T =
  BEGIN
    IF a.lo > a.hi OR b.lo > b.hi THEN RETURN Empty END;
    IF a.lo >= 0.0 THEN
      IF b.lo >= 0.0 THEN
        RETURN T{a.lo * b.lo, a.hi * b.hi}
      ELSIF b.hi <= 0.0 THEN
        RETURN T{a.hi * b.lo, a.lo * b.hi}
      ELSE
        RETURN T{a.hi * b.lo, a.hi * b.hi}
      END
    ELSIF a.hi <= 0.0 THEN
      IF b.lo >= 0.0 THEN
        RETURN T{a.lo * b.hi, a.hi * b.lo}
      ELSIF b.hi <= 0.0 THEN
        RETURN T{a.hi * b.hi, a.lo * b.lo}
      ELSE
        RETURN T{a.lo * b.hi, a.lo * b.lo}
      END
    ELSE
      IF b.lo >= 0.0 THEN
        RETURN T{a.lo * b.hi, a.hi * b.hi}
      ELSIF b.hi <= 0.0 THEN
        RETURN T{a.hi * b.lo, a.lo * b.lo}
      ELSE
        RETURN T{MIN(a.lo * b.hi, a.hi * b.lo),
                 MAX(a.lo * b.lo, a.hi * b.hi)}
      END
    END;
  END Mul;

PROCEDURE Sqr (READONLY a: T): T =
  BEGIN
    IF a.lo > a.hi THEN RETURN Empty END;
    IF a.lo >= 0.0 THEN
      RETURN T{a.lo * a.lo, a.hi * a.hi}
    ELSIF a.hi <= 0.0 THEN
      RETURN T{a.hi * a.hi, a.lo * a.lo}
    ELSE
      RETURN T{0.0, MAX(-a.lo, a.hi) * MAX(-a.lo, a.hi)}
    END;
  END Sqr;

PROCEDURE Minus (READONLY a: T): T =
  BEGIN
    IF a.lo > a.hi THEN RETURN Empty ELSE RETURN T{-a.hi, -a.lo} END;
  END Minus;

PROCEDURE Sqrt (READONLY a: T): T =
  BEGIN
    IF a.lo > a.hi THEN
      RETURN Empty
    ELSE
      RETURN T{FLOAT(Math.sqrt(FLOAT(a.lo, LONGREAL))),
               FLOAT(Math.sqrt(FLOAT(a.hi, LONGREAL)))}
    END;
  END Sqrt;

PROCEDURE ClipHi (READONLY a: T; hi: REAL): T =
  BEGIN
    IF hi >= a.hi THEN
      RETURN a
    ELSIF a.lo > hi THEN
      RETURN Empty
    ELSE
      RETURN T{a.lo, hi}
    END
  END ClipHi;

PROCEDURE ClipLo (READONLY a: T; lo: REAL): T =
  BEGIN
    IF lo <= a.lo THEN
      RETURN a
    ELSIF lo > a.hi THEN
      RETURN Empty
    ELSE
      RETURN T{lo, a.hi}
    END
  END ClipLo;

PROCEDURE Inset(READONLY a: T; amount: REAL): T =
  VAR b: T; 
  BEGIN
    IF a.lo > a.hi THEN RETURN Empty END;
    b.lo := a.lo + amount;
    b.hi := a.hi - amount;
    IF b.lo > b.hi THEN RETURN Empty END;
    RETURN b
  END Inset;
  
PROCEDURE InsetPair(READONLY a: Pair; amount: REAL): Pair =
  BEGIN
    RETURN Pair{Inset(a[0], amount), Inset(a[1], amount)}
  END InsetPair;

PROCEDURE Fudge (READONLY a: T; f: REAL := 0.0001): T =
  VAR d: REAL; 
      b: T;
  BEGIN
    IF a.lo > a.hi THEN RETURN Empty END;
    d := 0.0;
    IF ABS(a.lo) # LAST(REAL) THEN d := MAX(d, f * ABS (a.lo)) END;
    IF ABS(a.hi) # LAST(REAL) THEN d := MAX(d, f * ABS (a.hi)) END;
    IF d = 0.0 THEN RETURN a END;
    IF a.lo > FIRST(REAL) + d THEN b.lo := a.lo - d ELSE b.lo := a.lo END;
    IF a.hi < LAST(REAL) - d THEN b.hi := a.hi + d ELSE b.hi := a.hi END;
    RETURN b
  END Fudge;

PROCEDURE Join (READONLY a, b: T): T =
  BEGIN
    IF a.lo > a.hi THEN 
      RETURN b
    ELSIF b.lo > b.hi THEN
      RETURN a
    ELSE
      RETURN T{MIN(a.lo, b.lo), MAX(a.hi, b.hi)}
    END
  END Join;

PROCEDURE Meet (READONLY a, b: T): T =
  VAR t: T;
  BEGIN
    t.lo := MAX(a.lo, b.lo);
    t.hi := MIN(a.hi, b.hi);
    IF t.lo > t.hi THEN 
      RETURN Empty
    ELSE
      RETURN t
    END
  END Meet;

PROCEDURE ToText(READONLY a: T): TEXT =
BEGIN
  RETURN "[" & Fmt.Real(a.lo) & "_" & Fmt.Real(a.hi) & "]"
END ToText;

BEGIN
END Fuzzy.

