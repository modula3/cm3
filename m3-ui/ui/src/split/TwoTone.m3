(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Sep 25 16:09:24 PDT 1992 by msm  *)
<*PRAGMA LL*>

MODULE TwoTone;
IMPORT PaintOp, Pixmap, Point, Palette, Rect, 
       ScreenType, ScrnPaintOp, ScrnPixmap, VBT;

TYPE
  PMClosure = Palette.PixmapClosure OBJECT
    pm: Pixmap.T
  METHODS OVERRIDES 
    apply := PMApply
  END;

PROCEDURE PMApply(cl: PMClosure; st: ScreenType.T): ScrnPixmap.T =
  BEGIN
    IF st.color OR st.depth > 1 THEN
      RETURN Palette.ResolvePixmap(st, Pixmap.Solid)
    ELSE
      RETURN Palette.ResolvePixmap(st, cl.pm)
    END
  END PMApply;

TYPE
  OpClosure = Palette.OpClosure OBJECT
    op: PaintOp.T;
  METHODS OVERRIDES
    apply := OpApply;
  END;

PROCEDURE OpApply(cl: OpClosure; st: ScreenType.T): ScrnPaintOp.T =
  BEGIN
    IF st.color OR st.depth > 1 THEN
      RETURN Palette.ResolveOp(st, cl.op)
    ELSE
      RETURN Palette.ResolveOp(st, PaintOp.BgFg)
    END
  END OpApply;

PROCEDURE New(colorop: PaintOp.T; monotxt: Pixmap.T):T =
  BEGIN
    RETURN T { Palette.FromOpClosure(NEW(OpClosure, op := colorop)),
               Palette.FromPixmapClosure(NEW(PMClosure, pm := monotxt)) }
  END New;

PROCEDURE Paint (         v    : VBT.Leaf;
                 READONLY clip : Rect.T;
                 READONLY tone : T;
                 READONLY delta             := Point.Origin) = <* LL.sup < v *>
  BEGIN VBT.PaintTexture(v, clip, tone.op, tone.txt, delta); END Paint;

BEGIN END TwoTone.
