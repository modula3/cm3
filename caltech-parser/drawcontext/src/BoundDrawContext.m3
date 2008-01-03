(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: BoundDrawContext.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE BoundDrawContext;
IMPORT DrawContextClass;
IMPORT Line;
IMPORT LinoText;
REVEAL
  T = Public BRANDED "BoundDrawContext" OBJECT
  OVERRIDES
    gLine := GLine;
    gText := GText;
  END;

PROCEDURE GLine(self: T; l: Line.T) =
  BEGIN self.gBox(Line.GetBoundRect(l)); END GLine;

PROCEDURE GText(self: T; t: LinoText.T) =
  BEGIN self.gBox(self.textBounder.bound(t)); END GText;

BEGIN
END BoundDrawContext.
