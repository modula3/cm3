(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* Copied from Rect.m3                                         *)

MODULE RectR;

IMPORT PointR;


PROCEDURE Join (READONLY r, s: T): T RAISES {} =
  VAR t: T;
  BEGIN
    IF (r.west >= r.east)(* OR (r.north>=r.south) *)  THEN RETURN s;  END;
    IF (s.west >= s.east)(* OR (s.north>=s.south) *)  THEN RETURN r;  END;
    t.west := MIN (r.west, s.west);
    t.east := MAX (r.east, s.east);
    t.north := MIN (r.north, s.north);
    t.south := MAX (r.south, s.south);
    RETURN t;
  END Join;

PROCEDURE Overlap (READONLY r, s: T): BOOLEAN =
  BEGIN
    RETURN (MAX (r.west, s.west) <= MIN (r.east, s.east))
    AND (MAX (r.north, s.north) <= MIN (r.south, s.south));

  (* Note the use of "<=" instead of "<". This allows an empty "RectR.T" to 
     overlap with other rectamgles. *)

  END Overlap;

PROCEDURE Subset (READONLY r, s: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (r.west >= r.east)
    OR ((r.west >= s.west) AND (r.east <= s.east) AND (r.north >= s.north)
        AND (r.south <= s.south));
  END Subset;

PROCEDURE Member (READONLY p: PointR.T; READONLY r: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (r.west <= p.h) AND (p.h < r.east) AND (r.north <= p.v)
    AND (p.v < r.south);
  END Member;

PROCEDURE Add (READONLY r: T; READONLY p: PointR.T): T RAISES {} =
  VAR s: T;
  BEGIN
    s.west := r.west + p.h;
    s.east := r.east + p.h;
    s.north := r.north + p.v;
    s.south := r.south + p.v;
    RETURN s;
  END Add;

BEGIN
END RectR.
