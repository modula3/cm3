(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Wed Jun  3 00:06:20 1992 by steveg   *)

INTERFACE MGPaintOp;

(* Allows the user to animate the color of a PaintOp.T *)

IMPORT
  PaintOp, VBT;

TYPE
  RGB = RECORD r, g, b: REAL END;

PROCEDURE New(rgb: RGB): PaintOp.T;
(* produce a new paint op whose color is initially "rgb" *)

PROCEDURE Set(st: VBT.ScreenType; op: PaintOp.T; rgb: RGB);
(* change the color of op (created by New) to "rgb".  If "op" was
   not created by New, then "Set" is a noop.  *)

PROCEDURE Get(op: PaintOp.T): RGB;
(* return the current rgb value of "op".  If "op" was not created by
   "New" then the result of "Get" is undefined. *)

END MGPaintOp.

