(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jan 14 08:43:26 PST 1993 by mhb                      *)

INTERFACE ColorTools;

IMPORT PaintOp;

PROCEDURE FromHSV(
   h, s, v: REAL; 
   mode := PaintOp.Mode.Normal;
   gray := -1.0;
   bw := PaintOp.BW.UseIntensity): PaintOp.T;

END ColorTools.

