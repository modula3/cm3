(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Fri Mar 11 17:00:27 PST 1994 by gnelson *)
(*      modified on Fri Oct 30 17:30:13 PST 1992 by msm *)
(* modified on Mon Feb 24 13:59:46 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XPaint;

IMPORT XClientF, TrestleOnX, XScreenType, X, PaintPrivate;

REVEAL TrestleOnX.Display = XClientF.T_Abs BRANDED OBJECT END;

TYPE
  T <: TrestleOnX.Display;
  (* T supplies the painting and capturing methods *)

PROCEDURE ForceCapturePM (st : XScreenType.T;
                          dpy: X.DisplayStar;
                          pm : PaintPrivate.Pixmap)
  RAISES {X.Error};

END XPaint.

