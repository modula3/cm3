(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE LineStyle;
IMPORT PaintOp;
TYPE
  T = RECORD
    color: PaintOp.T;
    thick: CARDINAL;
  END;
CONST
  Default = T{PaintOp.Fg, 1};
END LineStyle.
