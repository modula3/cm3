(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Stroker.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE Stroker;
IMPORT DrawContext;
IMPORT Point;
IMPORT Rect;
IMPORT LineStyle;
TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(dc: DrawContext.T; ls := LineStyle.Default): T;
    moveTo(p: Point.T);
    lineTo(p: Point.T);
    frameRect(r: Rect.T);
  END;
END Stroker.
