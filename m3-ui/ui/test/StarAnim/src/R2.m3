(* Copyright (C)-ip992, Digital Equipment Corporation                        *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 15 13:06:52 PST 1996 by heydon                   *)

MODULE R2;

IMPORT Point, RealFloat, FloatMode;

PROCEDURE Plus(READONLY a, b: T): T =
  BEGIN RETURN T{a.x + b.x, a.y + b.y} END Plus;

PROCEDURE Minus(READONLY a, b: T): T =
  BEGIN RETURN T{a.x - b.x, a.y - b.y} END Minus;

PROCEDURE Scale(READONLY p: T; k: REAL): T =
  BEGIN RETURN T{k * p.x, k * p.y} END Scale;

PROCEDURE Length(READONLY p: T): REAL =
  <* FATAL FloatMode.Trap *> BEGIN
    RETURN RealFloat.Sqrt(p.x * p.x + p.y * p.y)
  END Length;

PROCEDURE Normalize(READONLY p: T): T =
  VAR len := Length(p); BEGIN
    RETURN T{p.x / len, p.y / len}
  END Normalize;

PROCEDURE Rel(READONLY p, a, b: T): T =
  VAR xVec := Minus(b, a); yVec := T{-xVec.y, xVec.x}; BEGIN
    RETURN Plus(a, Plus(Scale(xVec, p.x), Scale(yVec, p.y)))
  END Rel;

PROCEDURE FromPoint(READONLY p, origin: Point.T): T =
  VAR q := Point.Sub(p, origin); BEGIN
    RETURN T{FLOAT(q.h), -FLOAT(q.v)}
  END FromPoint;

PROCEDURE ToPoint(READONLY p: T; READONLY origin: Point.T): Point.T =
  VAR res := Point.T{ROUND(p.x), -ROUND(p.y)}; BEGIN
    RETURN Point.Add(origin, res)
  END ToPoint;

BEGIN
END R2.
