(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Fri Nov  6 14:30:30 PST 1992 by msm    *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XScrnFont;

IMPORT ScrnFont, XScreenType, TrestleComm;

PROCEDURE NewOracle (scrn: XScreenType.T; depthOne := FALSE): ScrnFont.Oracle
  RAISES {TrestleComm.Failure};
(* create a ScrnFont.Oracle suitable for use with an XScreenType.T. if
   "depthOne" is TRUE then the oracle will be suitable for a 1-bit font for
   painting on screens of type XScreenType.T. See the description of "bits"
   in ScreenType.i3 *)

END XScrnFont.
