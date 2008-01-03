(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Aug 29 15:55:21 PDT 1994 by mhb                      *)
(*      modified on Fri Jun 11 23:05:00 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 12:56:44 PDT 1992 by muller                   *)
(*      modified on Fri Apr 17 18:14:06 1992 by steveg                       *)

(* The "Pts" interface contains utilities to convert between points
   and pixels.  VBTkit uses 72 points per inch and 25.4 millimeters
   per inch.

   The locking level is arbitrary for all procedures in this interface. *)

INTERFACE Pts;

IMPORT Axis, VBT;

PROCEDURE ToScreenPixels (v: VBT.T; pts: REAL; ax: Axis.T):
  INTEGER;
(* Return the number of screen pixels that correspond to "pts"
   points on "v"'s screentype in the axis "ax"; or return "0" if
   "v"'s screentype is "NIL".  Equivalent to "ROUND (ToPixels (v, pts, ax))"
  *)

PROCEDURE ToPixels (v: VBT.T; pts: REAL; ax: Axis.T): REAL;
(* Return the number of pixels that correspond to "pts" points on
   "v"'s screentype in the axis "ax"; or return "0" if "v"'s
   screentype is "NIL". *)

PROCEDURE FromPixels (v: VBT.T; pixels: REAL; ax: Axis.T): REAL;
(* Return the number of points that correspond to "pixels"
   pixels on "v"'s screentype in the axis "ax"; or return "0" if
   "v"'s screentype is "NIL". *)

CONST
  PtsPerInch = 72.0;
  MMPerInch  = 25.4;

PROCEDURE FromMM (mm: REAL): REAL;
(* Convert from millimeters to points. *)

PROCEDURE ToMM (pts: REAL): REAL;
(* Convert from points to millimeters. *)

END Pts.




