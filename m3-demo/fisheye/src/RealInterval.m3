(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 17 21:11:27 PDT 1994 by mhb                      *)
(*      modified on Tue Jun 16 16:46:26 PDT 1992 by muller                   *)

MODULE RealInterval;
 
IMPORT Interval;

PROCEDURE FromBounds (lo, hi: REAL): T RAISES {} =
  VAR a: T;
  BEGIN
    IF lo <= hi THEN a.lo := lo; a.hi := hi;  ELSE a := Empty;  END;
    RETURN a;
  END FromBounds;

PROCEDURE FromAbsBounds (lo, hi: REAL): T RAISES {} =
  VAR a: T;
  BEGIN
    IF lo <= hi THEN
      a.lo := lo;
      a.hi := hi;
    ELSE
      a.lo := hi;
      a.hi := lo;
    END;
    RETURN a;
  END FromAbsBounds;

PROCEDURE FromBound (lo: REAL; s: REAL): T RAISES {} =
  VAR a: T;
  BEGIN
    IF s = 0.0 THEN RETURN Empty;  END;
    a.lo := lo;
    a.hi := lo + s;
    RETURN a;
  END FromBound;

PROCEDURE FromSize (s: REAL): T RAISES {} =
  VAR a: T;
  BEGIN
    IF 0.0 <= s THEN a.lo := 0.0; a.hi := s; ELSE a.lo := s; a.hi := 0.0; END;
    RETURN a;
  END FromSize;

PROCEDURE Center (READONLY a: T; b: REAL): T RAISES {} =
  VAR res: T; d: REAL;
  BEGIN
    d := b - (a.lo + a.hi) / 2.0;
    res.lo := a.lo + d;
    res.hi := a.hi + d;
    RETURN res;
  END Center;

PROCEDURE Floor(a: T): Interval.T =
  VAR b: Interval.T;
  BEGIN
    IF a.lo > a.hi THEN RETURN Interval.Empty END;
    b.lo := TRUNC(a.lo); IF FLOAT(b.lo) > a.lo THEN DEC(b.lo) END;
    b.hi := TRUNC(a.hi); IF FLOAT(b.hi) > a.hi THEN DEC(b.hi) END;
    b.hi := b.hi + 1;
    RETURN b
  END Floor;

PROCEDURE Round(a: T): Interval.T =
  VAR b: Interval.T;
  BEGIN
    IF a.lo > a.hi THEN RETURN Interval.Empty END;
    IF a.lo>0.0 THEN b.lo := TRUNC(a.lo+0.5) ELSE b.lo := TRUNC(a.lo-0.5) END;
    IF a.hi>0.0 THEN b.hi := TRUNC(a.hi+0.5) ELSE b.hi := TRUNC(a.hi-0.5) END;
    b.hi := b.hi + 1;
    RETURN b
  END Round;

PROCEDURE Size (READONLY a: T): REAL RAISES {} =
  BEGIN
    RETURN a.hi - a.lo;
  END Size;

PROCEDURE PickBound (READONLY a: T; n: REAL): Bound RAISES {} =
  BEGIN
    IF n <= Middle (a) THEN RETURN Bound.Lo ELSE RETURN Bound.Hi END;
  END PickBound;

PROCEDURE Project (READONLY a: T; n: REAL): REAL RAISES {} =
  <* FATAL Error *>
  BEGIN
    IF a.lo > a.hi THEN
      RAISE Error
    ELSIF n > a.hi THEN
      RETURN a.hi
    ELSIF n < a.lo THEN
      RETURN a.lo
    ELSE
      RETURN n
    END
  END Project;

PROCEDURE Middle (READONLY a: T): REAL RAISES {} =
  VAR m: REAL;
  BEGIN
    IF a.lo >= a.hi THEN
      RETURN 0.0
    ELSE
      m := (a.lo + a.hi) / 2.0;
      RETURN m
    END;
  END Middle;

PROCEDURE Move (READONLY a: T; n: REAL): T RAISES {} =
  VAR b: T;
  BEGIN
    b.lo := a.lo + n;
    b.hi := a.hi + n;
    RETURN b;
  END Move;

PROCEDURE Inset (READONLY a: T; n: REAL): T RAISES {} =
  VAR b: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN Empty;  END;
    b.lo := a.lo + n;
    b.hi := a.hi - n;
    IF b.lo >= b.hi THEN RETURN Empty;  END;
    RETURN b;
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
    IF c.lo > c.hi THEN RETURN Empty;  END;
    RETURN c;
  END Meet;

PROCEDURE Chop (READONLY a: T; n: REAL; VAR (* out *) b, c: T) RAISES {} =
  BEGIN
    b.lo := a.lo;
    b.hi := MAX (a.lo, MIN (a.hi, n));
    c.lo := MIN (a.hi, MAX (a.lo, n));
    c.hi := a.hi;
  END Chop;

PROCEDURE Factor (READONLY a, by: T;  VAR (*out*) f: Partition; dn: REAL)
  RAISES {} =
  VAR index: [0..2]; temp: T;
  BEGIN
    IF dn > 0.0 THEN index := 2;  ELSE index := 0;  END;
    Chop (a, by.lo, f[index], temp);
    Chop (temp, by.hi, f[1], f[2 - index]);
  END Factor;

PROCEDURE Mod (n: REAL; READONLY a: T): REAL RAISES {Error} =
  VAR quo: INTEGER; size, res: REAL;
  BEGIN
    IF a.lo >= a.hi THEN RAISE Error END;
    size := a.hi - a.lo;
    quo := TRUNC((n - a.lo)/size);
    res := n - FLOAT(quo)*size;
    WHILE res < a.lo DO res := res + size END;
    WHILE res >= a.hi DO res := res - size END;
    RETURN res
  END Mod;

PROCEDURE Equal (READONLY a, b: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN ((a.lo = b.lo) AND (a.hi = b.hi))
        OR ((a.lo >= a.hi) AND (b.lo >= b.hi));
  END Equal;

PROCEDURE IsEmpty (READONLY a: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN a.lo >= a.hi;
  END IsEmpty;

PROCEDURE Member (n: REAL; READONLY a: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (a.lo <= n) AND (n < a.hi);
  END Member;

PROCEDURE Overlap (READONLY a, b: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (MAX (a.lo, b.lo) < MIN (a.hi, b.hi));
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


PROCEDURE Float(a: Interval.T): T =
  VAR b: T;
  BEGIN
    IF a.lo >= a.hi THEN RETURN Empty END;
    b.lo := FLOAT(a.lo);
    b.hi := FLOAT(a.hi); (* RealExtra.PRED(FLOAT(a.hi)); *)
    RETURN b
  END Float;

PROCEDURE Hash (READONLY a: T): INTEGER =
  BEGIN
    RETURN ROUND(a.lo * a.hi)
  END Hash;

BEGIN
END RealInterval.


