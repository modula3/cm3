(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Contributed by Michel Dagenais (dagenais@vlsi.polymtl.ca), 1994. *)

MODULE RealInterval;

IMPORT Word, Text;

PROCEDURE FromBounds (lo, hi: REAL): T RAISES {} =
  VAR a: T;
  BEGIN
    IF lo < hi THEN a.lo := lo; a.hi := hi;  ELSE a := Empty;  END;
    RETURN a;
  END FromBounds;

PROCEDURE FromAbsBounds (lo, hi: REAL): T RAISES {} =
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

PROCEDURE FromBound (lo: REAL; s: REAL): T RAISES {} =
  VAR a: T;
  BEGIN
    IF s <= 0.0 THEN RETURN Empty;  END;
    a.lo := lo;
    a.hi := lo + s;
    RETURN a;
  END FromBound;

PROCEDURE FromSize (s: REAL): T RAISES {} =
  VAR a: T;
  BEGIN
    IF s <= 0.0 THEN RETURN Empty;  END;
    a.lo := 0.0; a.hi := s; RETURN a;
  END FromSize;

PROCEDURE Center (READONLY a: T; b: REAL): T RAISES {} =
  VAR res: T; d: REAL;
  BEGIN
    IF a.lo = a.hi THEN RETURN a END;
    d := b - (a.lo + a.hi) / 2.0;
    res.lo := a.lo + d;
    res.hi := a.hi + d;
    RETURN res;
  END Center;

PROCEDURE Size (READONLY a: T): REAL RAISES {} =
  BEGIN
    RETURN a.hi - a.lo;
  END Size;

PROCEDURE PickBound (READONLY a: T; n: REAL): Bound RAISES {} =
  BEGIN
    IF n <= Middle (a) THEN RETURN Bound.Lo ELSE RETURN Bound.Hi END;
  END PickBound;

PROCEDURE Project (READONLY a: T; n: REAL): REAL RAISES {} =
  BEGIN
    IF (a.hi <= a.lo) THEN FAIL("Interval.Project: empty interval") END;
    RETURN MAX (a.lo, MIN (a.hi, n))
  END Project;

PROCEDURE Middle (READONLY a: T): REAL RAISES {} =
  BEGIN RETURN (a.lo + a.hi) / 2.0 END Middle;

PROCEDURE Move (READONLY a: T; n: REAL): T RAISES {} =
  VAR b: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN Empty END;
    b.lo := a.lo + n;
    b.hi := a.hi + n;
    RETURN b;
  END Move;

PROCEDURE Inset (READONLY a: T; n: REAL): T RAISES {} =
  VAR b: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN Empty END;
    b.lo := a.lo + n;
    b.hi := a.hi - n;
    IF b.lo >= b.hi THEN RETURN Empty END;
    RETURN b
  END Inset;

PROCEDURE Change (READONLY a: T; dlo, dhi: REAL): T RAISES {} =
  VAR b: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN Empty;  END;
    b.lo := a.lo + dlo;
    b.hi := a.hi + dhi;
    IF b.lo >= b.hi THEN RETURN Empty;  END;
    RETURN b;
  END Change;

PROCEDURE MoveBound (x: Bound; READONLY a: T; dn: REAL): T RAISES {} =
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

PROCEDURE Chop (READONLY a: T; n: REAL; VAR (* out *) b, c: T) RAISES {} =
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

PROCEDURE Factor (READONLY a, by: T;  VAR (*out*) f: Partition; dn: REAL)
  RAISES {} =
  VAR index: [0..2]; temp: T;
  BEGIN
    IF dn > 0.0 THEN index := 2;  ELSE index := 0;  END;
    Chop (a, by.lo, f[index], temp);
    Chop (temp, by.hi, f[1], f[2 - index]);
  END Factor;

PROCEDURE Mod (n: REAL; READONLY a: T): REAL RAISES {} =
  VAR res: REAL;
  BEGIN
    IF (a.lo >= a.hi) THEN FAIL("Interval.Mod: a is empty!") END;
    res := (n - a.lo) MOD (a.hi - a.lo);
    RETURN res + a.lo
  END Mod;

PROCEDURE Equal (READONLY a, b: T): BOOLEAN RAISES {} =
  BEGIN RETURN a = b END Equal;

PROCEDURE IsEmpty (READONLY a: T): BOOLEAN RAISES {} =
  BEGIN RETURN a.lo >= a.hi; END IsEmpty;

PROCEDURE Member (n: REAL; READONLY a: T): BOOLEAN RAISES {} =
  BEGIN RETURN (a.lo <= n) AND (n < a.hi) END Member;

PROCEDURE Overlap (READONLY a, b: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN MAX (a.lo, b.lo) < MIN (a.hi, b.hi);
  END Overlap;

PROCEDURE Subset (READONLY a, b: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (a.lo >= a.hi) OR ((a.lo >= b.lo) AND (a.hi <= b.hi));
  END Subset;

PROCEDURE New (READONLY value: T): REF T =
  VAR r: REF T;
  BEGIN
    r := NEW (REF T);
    r^ := value;
    RETURN r;
  END New;

PROCEDURE NewArray (size: CARDINAL;  READONLY value := Empty): REF ARRAY OF T =
  VAR arr: REF ARRAY OF T;
  BEGIN
    arr := NEW (REF ARRAY OF T, size);
    (* Assumes the allocator initializes to Empty automatically: *)
    IF value # Empty THEN
      FOR i := 0 TO size - 1 DO arr[i] := value END;
    END;
    RETURN arr
  END NewArray;

PROCEDURE UntracedNew (READONLY value: T): UNTRACED REF T =
  VAR r: UNTRACED REF T;
  BEGIN
    r := NEW (UNTRACED REF T);
    r^ := value;
    RETURN r;
  END UntracedNew;

PROCEDURE UntracedNewArray (size: CARDINAL;  READONLY value := Empty):
                                                      UNTRACED REF ARRAY OF T =
  VAR arr: UNTRACED REF ARRAY OF T;
  BEGIN
    arr := NEW (UNTRACED REF ARRAY OF T, size);
    (* Assumes the allocator initializes to Empty automatically: *)
    IF value # Empty THEN
      FOR i := 0 TO size - 1 DO arr[i] := value END;
    END;
    RETURN arr
  END UntracedNewArray;

PROCEDURE Compare (READONLY a, b: T): INTEGER =
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

PROCEDURE Lt (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN (a.lo < b.lo) OR ((a.lo = b.lo) AND (a.hi < b.hi));
  END Lt;

PROCEDURE Eq (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN Equal (a, b);
  END Eq;

PROCEDURE Hash (READONLY a: T): INTEGER =
  BEGIN
    RETURN Word.Xor (TRUNC(a.lo), TRUNC(a.hi));
  END Hash;

EXCEPTION ASSERT_FAILED (Text.T);
<*INLINE*> PROCEDURE FAIL (msg: Text.T := NIL) =
<*FATAL ASSERT_FAILED*>
  BEGIN
    RAISE ASSERT_FAILED (msg);
  END FAIL;

BEGIN
END RealInterval.
