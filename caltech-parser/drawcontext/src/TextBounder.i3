(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE TextBounder;
IMPORT LinoText;
IMPORT Rect;
TYPE
  T = OBJECT METHODS
    bound(t: LinoText.T): Rect.T;
    (* Can assume t.attach = LinoText.Attach.West. *)
  END;
END TextBounder.
