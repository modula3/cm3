(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE DCPaneVBT;
IMPORT PaneVBT;
IMPORT DrawContext;
IMPORT Transform;

TYPE
  T <: Public;
  Public = PaneVBT.T OBJECT
    (* overrides PaneVBT.T 'key' method to scroll,
       and 'write' method to save PS *)
  METHODS
    paint(dc: DrawContext.T);
    getTransformFrom(other: T);
    getTransform(): Transform.T;
  END;

END DCPaneVBT.
