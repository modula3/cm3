(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: LinoText.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE LinoText;
IMPORT Point;
IMPORT Rect;
IMPORT PaintOp;
CONST
  Brand = "LinoText";
TYPE
  T = RECORD
    a: Point.T; (* attach point *)
    t: TEXT;
    size: CARDINAL;
    attach := Attach.West;
    color := PaintOp.Fg;
  END;
  Attach = {North, West, South, East, Center, CenterBase};
  
PROCEDURE AttachPoint(r: Rect.T; baseV: INTEGER; a: Attach): Point.T;
(* given a text bounding rect and vertical pen position,
   return a point attached to r, as specified by a. *)

PROCEDURE Equal(a, b: T): BOOLEAN;
PROCEDURE Format(a: T): TEXT;

CONST
  FormatAttach = ARRAY Attach OF TEXT
  {"North", "West", "South", "East", "Center", "CenterBase"};
END LinoText.
