(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DrawContextClass.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE DrawContextClass;
IMPORT DrawContext;
IMPORT TransformOther;
IMPORT Region;
IMPORT Line;
IMPORT LinoText;
IMPORT Point;
IMPORT Rect;
IMPORT TextBounder;
REVEAL
  DrawContext.T <: Public;
TYPE
  T = DrawContext.T;
  Public = DrawContext.Public OBJECT
    transform := TransformOther.Identity;
    (* local -> global. Set using setTransform. *)

    clip := Region.Full;
    (* clip region in global coordinates. Always set using setClip. *)

    textBounder: TextBounder.T := NIL;
    (* must be initialized if local text operations are supported. *)

    resDPI: [1..LAST(INTEGER)] := 72;
    (* useful for writing PostScript and copying via CacheDrawContext. *)
  METHODS
    gLine(l: Line.T);
    (* draw line in global coordinates. *)

    gText(t: LinoText.T);
    (* draw text in global coordinates.
       Can assume t.attach = LinoText.Attach.West. *)

    getClipRects(): REF ARRAY OF Rect.T;
    (* return disjoint array of rectangles comprising self.clip *)

    grText(t: LinoText.T);
    (* draw text in global coordinates. *)

    grBoundText(t: LinoText.T; VAR penOffset: Point.T): Rect.T;
    (* bound text in global coordinates.
       Also return "penOffset": how to move pen (relative to "t.a") to
       achieve desired attachment. *)
  END;
END DrawContextClass.

  
