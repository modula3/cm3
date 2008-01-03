(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: VBTTextBounder.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE VBTTextBounder;
IMPORT LinoText;
IMPORT Rect;
IMPORT VBT;
IMPORT OneFont;
REVEAL
  T = Public BRANDED "VBTTextBounder" OBJECT
    v: VBT.T;
  OVERRIDES
    init := Init;
    bound := Bound;
  END;

PROCEDURE Init(self: T; v: VBT.T): T =
  BEGIN
    self.v := v;
    RETURN self;
  END Init;

PROCEDURE Bound(self: T; t: LinoText.T): Rect.T =
  BEGIN
    <* ASSERT t.attach = LinoText.Attach.West *>
    RETURN Rect.Move(VBT.BoundingBox(self.v,t.t,OneFont.FromSize(t.size)),t.a);
  END Bound;

BEGIN
END VBTTextBounder.
