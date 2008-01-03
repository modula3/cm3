(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Dec 18 09:03:10 PST 1995 by heydon                   *)

INTERFACE JunoRect;

(* A "JunoRect.T" represents a rectangle in Juno coordinates. As opposed to
   Trestle's "Rect.T", Juno rectangles are real-valued, and their
   y-coordinates increase to the north. The point "(x, y)" is in the rectangle
   "r" if and only if:

|    r.west <= x < r.east
|    r.south <= y < r.north

   Although "JunoRect.T"'s use half-open intervals just like Trestle's
   "Rect.T"'s, the fact that they are over the real numbers makes the
   distinction less important. *)

IMPORT JunoValue, RTVal, JunoPt;

TYPE T = RECORD west, east, north, south: JunoValue.Real END;

CONST Empty = T{0.0, 0.0, 0.0, 0.0};

PROCEDURE Scale(READONLY r: T; s: JunoValue.Real): T;
(* Return the rectangle produced by scaling "r" about the origin by the scale
   factor "s". *)

PROCEDURE Rotate90(READONLY r: T): T;
(* Return the rectangle produced by rotating "r" 90 degrees about the
   origin. *)

PROCEDURE Add(READONLY r: T; READONLY p: JunoPt.T): T;
(* Return the rectangle produced by translating the rectangle "r" by the
   vector "p". *)

PROCEDURE Join(READONLY r1, r2: T): T;
(* Return the smallest rectangle enclosing both "r1" and "r2". *)

PROCEDURE ToRTVal(READONLY r: T): RTVal.T;
(* Return the Juno-valued representation of "r", that is a pair of points
   "(sw, ne)" where "sw" is the pair "(r.west, r.south)" and "ne" is the pair
   "(r.east, r.north)". *)

END JunoRect.
