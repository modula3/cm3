(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DrawContext.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE DrawContext;
IMPORT Transform;
IMPORT Region;
IMPORT Rect;
IMPORT Line;
IMPORT LinoText;
IMPORT TextBounder;
TYPE
  T <: Public;
  Public = OBJECT METHODS
    (* the following methods refer to local coordinates *)

    line(l: Line.T);
    text(t: LinoText.T);
    boundText(t: LinoText.T): Rect.T;
    regionVisible(r: Region.T): BOOLEAN;


    (* the following methods refer to global coordinates *)

    setClip(r: Region.T);
    (* limit subsequent drawing to r *)

    setTransform(t: Transform.T);
    (* define local coordinates *)

    preTransform(t: Transform.T): Transform.T;
    (* precede existing transform by t, returning old transform. E.g.
       restore := ctx.preTransform(myCellTransform);
       ctx.setTransform(restore); *)

    setTextBounder(tb: TextBounder.T);
    (* textBounder must be initialized if text is to be transformed *)
  END;

END DrawContext.
