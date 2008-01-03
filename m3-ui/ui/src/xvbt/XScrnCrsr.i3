(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Fri Nov  6 19:55:50 PST 1992 by msm    *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XScrnCrsr;

IMPORT ScrnCursor, TrestleComm, X, XScreenType, XScrnPxmp;

REVEAL XScreenType.T <: T;

TYPE
  T_Pub = XScrnPxmp.T OBJECT END;
  T <: T_Pub;

PROCEDURE NullCursor(dpy: X.DisplayStar; w: X.Drawable): X.Cursor
  RAISES {TrestleComm.Failure};

PROCEDURE NewOracle (scrn: XScreenType.T): ScrnCursor.Oracle
  RAISES {TrestleComm.Failure};

END XScrnCrsr.

