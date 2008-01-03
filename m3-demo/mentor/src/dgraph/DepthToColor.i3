(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE DepthToColor;

IMPORT PaintOp;

(* Consistent map of "depth" values in events to colors *)

PROCEDURE Map(depth: INTEGER): PaintOp.T RAISES {};
(* map "depth" into a color. *)

END DepthToColor.
