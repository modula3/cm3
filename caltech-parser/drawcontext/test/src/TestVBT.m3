(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE TestVBT;
IMPORT DCPaneVBT;
IMPORT PaneVBT;
IMPORT Rd;
IMPORT Line;
IMPORT LinoText;
IMPORT Point;
IMPORT DrawContext;
IMPORT TransformOther;
IMPORT Fmt;
IMPORT Rect;
IMPORT Stroker;
IMPORT VBT;
IMPORT PaneManOp;

REVEAL
  T = DCPaneVBT.T BRANDED "TestVBT" OBJECT
    pm: PaneManOp.T;
  OVERRIDES
    init := Init;
    clone := Clone;
    key := Key;
    paint := Paint;
  END;

PROCEDURE Init(self: T; rd: Rd.T; pm: PaneManOp.T): PaneVBT.T =
  BEGIN
    self.pm := pm;
    RETURN DCPaneVBT.T.init(self, rd, pm);
  END Init;

PROCEDURE Clone(self: T): PaneVBT.T =
  VAR
    result: T := NEW(T).init(NIL, self.pm);
  BEGIN
    result.getTransformFrom(self);
    RETURN result;
  END Clone;

PROCEDURE Key(self: T; READONLY key: VBT.KeyRec) =
  BEGIN
    IF key.wentDown THEN
      CASE key.whatChanged OF
      | ORD('h') => self.pm.print("hello world");
      ELSE
      END;
    END;
    DCPaneVBT.T.key(self, key);
  END Key;

PROCEDURE Paint(<*UNUSED*>self: T; dc: DrawContext.T) =
  VAR
    stroker := NEW(Stroker.T).init(dc);
    lt: LinoText.T;
  BEGIN
    stroker.frameRect(Rect.FromEdges(-10,10,-10,10));
    FOR i := 0 TO 3 DO
      EVAL dc.preTransform(TransformOther.RotLeft);
      dc.line(Line.T{Point.T{10,0}, Point.T{100,0}});
      lt := LinoText.T{Point.T{110,0}, "vertex #" & Fmt.Int(i), 20};
      dc.text(lt);
      stroker.frameRect(Rect.Inset(dc.boundText(lt), -10));
    END;
  END Paint;

BEGIN
END TestVBT.
