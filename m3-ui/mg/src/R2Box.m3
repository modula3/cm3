(* Copyright 1989 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created by stolfi on Sat Mar 3 0:08:44 PST 1990 *)
(* Last modified on Tue Jul 21 16:10:38 PDT 1992 by harrison *)
(* modified on Wed May 9 3:27:47 PDT 1990 by stolfi *)

MODULE R2Box;

IMPORT R2, Fuzzy;

PROCEDURE FromEdges (h1, h2, v1, v2: REAL): T =
  BEGIN
    IF h1 >= h2 OR v1 >= v2 THEN RETURN Empty END;

    RETURN T{Fuzzy.T{h1, h2}, Fuzzy.T{v1, v2}};
  END FromEdges;

PROCEDURE FromAbsEdges (h1, h2, v1, v2: REAL): T =
  BEGIN
    RETURN FromEdges(MIN(h1, h2), MAX(h1, h2), MIN(v1, v2), MAX(v1, v2))
  END FromAbsEdges;

PROCEDURE FromCorners (READONLY p, q: R2.T): T =
  BEGIN
    RETURN FromAbsEdges(p[0], q[0], p[1], q[1]);
  END FromCorners;

PROCEDURE IsEmpty (READONLY a: T): BOOLEAN =
  BEGIN
    RETURN a[0].lo > a[0].hi OR a[1].lo > a[1].hi;
  END IsEmpty;

PROCEDURE Meet (READONLY a, b: T): T =
  BEGIN
    IF a[0].lo > a[0].hi OR a[1].lo > a[1].hi OR b[0].lo > b[0].hi
         OR b[1].lo > b[1].hi THEN
      RETURN Empty
    ELSE
      RETURN T{Fuzzy.T{MAX(a[0].lo, b[0].lo), MIN(a[0].hi, b[0].hi)},
               Fuzzy.T{MAX(a[1].lo, b[1].lo), MIN(a[1].hi, b[1].hi)}};
    END
  END Meet;

PROCEDURE Join (READONLY a, b: T): T =
  BEGIN
    IF a[0].lo > a[0].hi OR a[1].lo > a[1].hi THEN RETURN b END;
    IF b[0].lo > b[0].hi OR b[1].lo > b[1].hi THEN RETURN a END;
    RETURN T{Fuzzy.T{MIN(a[0].lo, b[0].lo), MAX(a[0].hi, b[0].hi)},
             Fuzzy.T{MIN(a[1].lo, b[1].lo), MAX(a[1].hi, b[1].hi)}};
  END Join;

PROCEDURE NorthWest (READONLY a: T): R2.T =
  BEGIN
    RETURN R2.T{a[0].lo, a[1].hi};
  END NorthWest;

PROCEDURE NorthEast (READONLY a: T): R2.T =
  BEGIN
    RETURN R2.T{a[0].hi, a[1].hi};
  END NorthEast;

PROCEDURE SouthWest (READONLY a: T): R2.T =
  BEGIN
    RETURN R2.T{a[0].lo, a[1].lo};
  END SouthWest;

PROCEDURE SouthEast (READONLY a: T): R2.T =
  BEGIN
    RETURN R2.T{a[0].hi, a[1].lo};
  END SouthEast;

PROCEDURE Middle (READONLY a: T): R2.T =
  BEGIN
    IF a[0].lo > a[0].hi OR a[1].lo > a[1].hi THEN
      RETURN R2.Origin;
    ELSE
      RETURN R2.T{0.5 * a[0].lo + 0.5 * a[0].hi,
                  0.5 * a[1].lo + 0.5 * a[1].hi};
    END;
  END Middle;

PROCEDURE Size (READONLY a: T): R2.T =
  BEGIN
    IF a[0].lo > a[0].hi OR a[1].lo > a[1].hi THEN
      RETURN R2.Origin;
    ELSE
      RETURN R2.T{a[0].hi - a[0].lo, a[1].hi - a[1].lo};
    END;
  END Size;

PROCEDURE HalfSize (READONLY a: T): R2.T =
  BEGIN
    IF a[0].lo > a[0].hi OR a[1].lo > a[1].hi THEN
      RETURN R2.Origin;
    ELSE
      RETURN R2.T{0.5 * a[0].hi - 0.5 * a[0].lo,
                  0.5 * a[1].hi - 0.5 * a[1].lo};
    END;
  END HalfSize;

PROCEDURE Inset (READONLY a: T; by: REAL): T =
  BEGIN
    RETURN T{Fuzzy.Inset(a[0], by), Fuzzy.Inset(a[1], by)};
  END Inset;

PROCEDURE Extend (READONLY a: T; p: R2.T): T =
  BEGIN
    RETURN T{Fuzzy.Expand(p[0], a[0]), Fuzzy.Expand(p[1], a[1])};
  END Extend;

PROCEDURE Translate (READONLY a: T; p: R2.T): T =
  BEGIN
    RETURN T{Fuzzy.Shift(p[0], a[0]), Fuzzy.Shift(p[1], a[1])};
  END Translate;

PROCEDURE ToText (READONLY a: T): TEXT =
  BEGIN
    IF a[0].lo > a[0].hi OR a[1].lo > a[1].hi THEN
      RETURN "(empty)";
    ELSE
      RETURN "(" & Fuzzy.ToText(a[0]) & ", " & Fuzzy.ToText(a[1]) & ")";
    END;
  END ToText;

BEGIN
END R2Box.

