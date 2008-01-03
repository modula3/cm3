(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Fri Nov  6 18:02:54 PST 1992 by msm    *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XScrnCmap;

IMPORT ScrnColorMap, TrestleComm, X, XScreenType;

PROCEDURE NewOracle (scrn: XScreenType.T; READONLY vinfo: X.XVisualInfo):
  ScrnColorMap.Oracle RAISES {TrestleComm.Failure};
(* create a ScrnColorMap.Oracle suitable for use with an XScreenType.T *)

PROCEDURE ColorMapID(cm: ScrnColorMap.T): X.Colormap;
(* Return the XID for cm, or X.None if the screentype of cm
   is not of type T.  LL arbitrary. *)

END XScrnCmap.

