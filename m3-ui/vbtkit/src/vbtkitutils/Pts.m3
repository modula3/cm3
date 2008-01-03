(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Aug 29 16:10:10 PDT 1994 by mhb                      *)
(*      modified on Tue Jun 16 12:56:55 PDT 1992 by muller                   *)
(*      modified on Fri Apr 17 18:14:28 1992 by steveg   *)
(*      modified on Fri Nov 29 18:17:59 PST 1991 by meehan   *)

MODULE Pts;

IMPORT Axis, VBT;

PROCEDURE ToScreenPixels (v: VBT.T; pts: REAL; ax: Axis.T): INTEGER =
  VAR st := VBT.ScreenTypeOf(v);
  BEGIN
    IF st = NIL THEN
      RETURN 0
    ELSE
      RETURN ROUND(pts * MMPerInch * st.res[ax] / PtsPerInch)
    END
  END ToScreenPixels;

PROCEDURE ToPixels (v: VBT.T; pts: REAL; ax: Axis.T): REAL  =
  VAR st := VBT.ScreenTypeOf(v);
  BEGIN
    IF st = NIL THEN
      RETURN 0.0
    ELSE
      RETURN pts * MMPerInch * st.res[ax] / PtsPerInch
    END
  END ToPixels;

PROCEDURE FromPixels (v: VBT.T; pixels: REAL; ax: Axis.T): REAL  =
  VAR st := VBT.ScreenTypeOf(v);
  BEGIN
    IF st = NIL THEN
      RETURN 0.0
    ELSE
      RETURN pixels * PtsPerInch / (MMPerInch * st.res[ax])
    END
  END FromPixels;

PROCEDURE FromMM (mm: REAL): REAL =
  BEGIN
    RETURN mm * PtsPerInch / MMPerInch;
  END FromMM;

PROCEDURE ToMM (pts: REAL): REAL =
  BEGIN
    RETURN pts * MMPerInch / PtsPerInch
  END ToMM;

BEGIN
END Pts.
