(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 24 10:49:31 PDT 1994 by heydon                   *)

INTERFACE JunoPt;

(* A "JunoPt.T" represents a point in Juno coordinates. The procedures in this
   module perform conversions to/from Juno points. *)

IMPORT JunoAST, JunoValue, RTVal, Point;

EXCEPTION BadPt;

TYPE
  T = RECORD
    x, y: JunoValue.Real
  END;

(* A "JunoPt.T" is a point in Juno coordinates. As opposed to Trestle's
   "Point.T", Juno points are real-valued, their "y" coordinate increases
   to the north, and they are measured in units of screen-independent points
   instead of screen-dependent pixels. *)

TYPE
  Transform = RECORD
    origin := Point.T{0, 0};
    xScale, yScale, widthScale: JunoValue.Real := 1.0; (* in pixels / point *)
  END;

(* A "JunoPt.Transform" specifies a transformation for converting between Juno
   coordinates and Trestle coordinates. The field "origin" is the location (in
   Trestle coordinates) of the origin of the Juno coordinate system; this is
   usually the southwest corner of a drawing's domain. The fields
   "xScale" and "yScale" are the screen-dependent conversion factors for the
   orthogonal axes in pixels-per-point: multiply by these factors to convert
   from points to pixels, and divide by them to convert from pixels to
   points. The field "widthScale" is the scale factor to use for line widths;
   it is a function of the scale factors in the x and y directions. *)

PROCEDURE ToHV(READONLY xyPt: T; READONLY xform: Transform): Point.T;
(* Return the point in Trestle coordinates with the point "origin" as origin
   of the location "xyPt" in Juno coordinates. *)

PROCEDURE FromHV(READONLY hvPt: Point.T; READONLY xform: Transform): T;
(* Return the location in Juno coordinates of the point "hv" in Trestle
   coordinates with the point "origin" as origin. *)

PROCEDURE ToASTPair(READONLY xyPt: T): JunoAST.Pair;
(* Return the "JunoAST.Pair" representing the point equivalent to the Juno
   point "xyPt". *)

PROCEDURE ToValuePair(READONLY xyPt: T): RTVal.Pair;
(* Return the "RTVal.Pair" representing the point equivalent to the Juno
   point "xyPt". *)

PROCEDURE FromValuePair(pr: RTVal.Pair): T RAISES {BadPt};
(* Return the Juno point equivalent to the pair "pr". Raises "BadPoint" if
   "pr" is not a pair of "RTVal.Number"'s (this includes the case where
   "pr = NIL". *)

PROCEDURE RelVal(
    cx,cy, ax,ay, bx,by: JunoValue.Real;
    VAR (*OUT*) x,y: JunoValue.Real):
  BOOLEAN;
(* Solve "(cx, cy) = (x, y) REL ((ax, ay), (bx, by))" for "x" and "y" and
   return TRUE, or return FALSE if there is no solution (namely, if "ax = bx"
   and "ay = by"). *)

END JunoPt.
