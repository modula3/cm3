(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE BoundDrawContext;
IMPORT DrawContext;
IMPORT Rect;
TYPE
  T <: Public;
  Public = DrawContext.T OBJECT METHODS
    gBox(r: Rect.T);
    (* called with r = bounding box, when text or line is plotted. *)
  END;
END BoundDrawContext.
