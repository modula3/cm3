(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 26 14:00:52 PST 1997 by heydon                   *)

MODULE JunoPt;

IMPORT JunoAST, JunoASTUtils, JunoValue, RTVal, Point;

<* INLINE *>
PROCEDURE ToHV(READONLY xyPt: T; READONLY xform: Transform): Point.T =
  VAR
    x :=  ROUND(xform.xScale * xyPt.x);
    y := -ROUND(xform.yScale * xyPt.y);
  BEGIN
    RETURN Point.Add(Point.T{x, y}, xform.origin);
  END ToHV;

<* INLINE *>
PROCEDURE FromHV(READONLY hvPt: Point.T; READONLY xform: Transform): T =
  VAR xyPt := Point.Sub(hvPt, xform.origin); BEGIN
    RETURN T{
      FLOAT( xyPt.h, JunoValue.Real) / xform.xScale,
      FLOAT(-xyPt.v, JunoValue.Real) / xform.yScale}
  END FromHV;

<* INLINE *>
PROCEDURE ToASTPair(READONLY xyPt: T): JunoAST.Pair =
  BEGIN
    RETURN JunoASTUtils.NewPoint(xyPt.x, xyPt.y)
  END ToASTPair;

<* INLINE *>
PROCEDURE ToValuePair(READONLY xyPt: T): RTVal.Pair =
  BEGIN
    RETURN RTVal.FromPair(RTVal.FromReal(xyPt.x), RTVal.FromReal(xyPt.y))
  END ToValuePair;

<* INLINE *>
PROCEDURE FromValuePair(pr: RTVal.Pair): T RAISES {BadPt} =
  BEGIN
    TYPECASE pr.car OF
      NULL => (* SKIP *)
    | RTVal.Number (x) =>
        TYPECASE pr.cdr OF
          NULL => (* SKIP *)
        | RTVal.Number (y) =>
            RETURN T{x.val, y.val}
        ELSE (* SKIP *)
        END
    ELSE (* SKIP *)
    END;
    RAISE BadPt
  END FromValuePair;

PROCEDURE RelVal(
    cx, cy, ax, ay, bx, by: JunoValue.Real;
    VAR (*OUT*) x, y: JunoValue.Real): BOOLEAN =
  VAR
    v1 := T{cx - ax, cy - ay};
    v2 := T{bx - ax, by - ay};
    denom := (v2.x * v2.x) + (v2.y * v2.y);
  BEGIN
    IF denom = 0.0 THEN RETURN FALSE END;
    (* x = R2.Dot(v1, v2) / denom *)
    (* y = R2.Dot(v1, T{-v2.y, v2.x}) / denom *)
    x := (v1.x * v2.x + v1.y * v2.y) / denom;
    y := (v1.y * v2.x - v1.x * v2.y) / denom;
    RETURN TRUE
  END RelVal;

BEGIN
END JunoPt.
