(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Nov 10 13:34:27 PST 1992 by msm     *)
<*PRAGMA LL*>

INTERFACE JoinPaintOp;

IMPORT ScrnPaintOp, JoinScreen, Palette, PaintOp;

TYPE
  Oracle <: ScrnPaintOp.Oracle;

PROCEDURE New(st: JoinScreen.T): Oracle;

PROCEDURE Apply (st: JoinScreen.T; cl: Palette.OpClosure; op: PaintOp.T):
  ScrnPaintOp.T;

END JoinPaintOp.
