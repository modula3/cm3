(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jan 14 08:49:06 PST 1993 by mhb                      *)

MODULE ColorTools;

IMPORT Color, PaintOp;

PROCEDURE FromHSV (h, s, v: REAL;
                   mode            := PaintOp.Mode.Normal;
                   gray            := -1.0;
                   bw              := PaintOp.BW.UseIntensity):
  PaintOp.T =
  BEGIN
    WITH hsv = Color.HSV{h, s, v},
         rgb = Color.FromHSV(hsv)  DO
      RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b, mode, gray, bw)
    END
  END FromHSV;

BEGIN
END ColorTools.

