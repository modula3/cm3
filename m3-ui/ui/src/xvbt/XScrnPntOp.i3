(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Fri Nov  6 19:55:57 PST 1992 by msm    *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XScrnPntOp;

IMPORT ScrnPaintOp, XScreenType, XScrnCrsr;

REVEAL XScreenType.T <: T;

TYPE
  T_Pub = XScrnCrsr.T OBJECT END;
  T <: T_Pub;

PROCEDURE NewOracle(st: XScreenType.T): ScrnPaintOp.Oracle;

END XScrnPntOp.
