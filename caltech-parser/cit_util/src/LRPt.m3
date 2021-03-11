MODULE LRPt;
IMPORT Math;

PROCEDURE Normalize(p: T; newLength: LONGREAL): T =
  VAR
    oldLength := Math.sqrt(p.h*p.h + p.v*p.v);
    s := newLength / oldLength;
  BEGIN
    RETURN T{p.h*s, p.v*s};
  END Normalize;

PROCEDURE Add(a,b: T): T =
  BEGIN
    RETURN T{a.h+b.h, a.v+b.v};
  END Add;

PROCEDURE Sub(a,b: T): T =
  BEGIN
    RETURN T{a.h-b.h, a.v-b.v};
  END Sub;

BEGIN
END LRPt.
