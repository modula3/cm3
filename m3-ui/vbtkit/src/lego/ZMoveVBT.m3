(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 19 15:14:44 PST 1996 by mhb    *)
(*      modified on Mon Mar  1 14:57:01 PST 1993 by meehan *)
(*      modified on Tue Jun 16 13:07:56 PDT 1992 by muller *)
(*      modified on Fri Mar 27 02:59:03 1992 by steveg*)

MODULE ZMoveVBT;

IMPORT Axis, BtnVBTClass, HighlightVBT, PaintOp, Pixmap, Point,
       Rect, SourceVBT, Split, VBT, VBTColors, ZChildVBT, 
       ZSplit, ZSplitUtils;

REVEAL
  T = SourceVBT.T BRANDED OBJECT
        pt     : Point.T;
        op     : PaintOp.T;
        hl     : HighlightVBT.T;
        zChild : VBT.T;
        shifted: BOOLEAN;
        rect   : Rect.T;
      OVERRIDES
        pre    := Pre;
        during := During;
        post   := Post;
        cancel := Cancel;
        mouse  := Mouse;
      END;

PROCEDURE Pre (v: T) =
  <* FATAL Split.NotAChild *>
  BEGIN
    v.zChild := ZSplitUtils.FindZChild(v);
    IF v.zChild = NIL THEN v.ready := FALSE; RETURN END;
    SourceVBT.T.pre(v);
    v.hl := HighlightVBT.Find(VBT.Parent(v.zChild));
    v.rect := VBT.Domain(v.zChild);
    WITH bg = Split.Pred(VBT.Parent(v.zChild), NIL) DO
      v.op := VBTColors.Get(bg).transparentSwap
    END;
    IF NOT v.shifted THEN ZSplit.Lift(v.zChild, ZSplit.Altitude.Top) END;
  END Pre;

PROCEDURE Post (v: T) =
  BEGIN
    HighlightVBT.SetRect(v.hl, Rect.Empty);
    ZChildVBT.Grew(
      v.zChild, Rect.HorSize(v.rect), Rect.VerSize(v.rect));
    ZSplit.Move(v.zChild, v.rect);
    ZChildVBT.Moved(v.zChild);
    SourceVBT.T.post(v);
  END Post;

PROCEDURE Cancel (v: T) =
  BEGIN
    HighlightVBT.SetRect(v.hl, Rect.Empty);
    SourceVBT.T.cancel(v);
  END Cancel;

PROCEDURE During (v: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    IF NOT Rect.Member(
             cd.cp.pt, VBT.Domain(VBT.Parent(v.zChild))) THEN
      RETURN
    END;
    WITH newRect = Rect.Move(v.rect, Point.Sub(cd.cp.pt, v.pt)) DO
      IF IsVisible(v, newRect) THEN
        v.rect := newRect;
        v.pt := cd.cp.pt
      END;
      MoveAndHighlight(v, v.rect)
    END
  END During;

PROCEDURE MoveAndHighlight (v: T; READONLY rect: Rect.T) =
  BEGIN
    v.rect := rect;
    HighlightVBT.SetTexture(v.hl, Pixmap.Gray, Point.Origin, v.op);
    HighlightVBT.SetRect(v.hl, v.rect, OutlineThickness(v));
  END MoveAndHighlight;

PROCEDURE Mouse (v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      v.pt := cd.cp.pt;
      v.shifted := VBT.Modifier.Shift IN cd.modifiers
    END;
    SourceVBT.T.mouse(v, cd);
  END Mouse;

PROCEDURE IsVisible (v: T; newFrameDom: Rect.T): BOOLEAN =
  CONST
    MinSize = 4.0;
  VAR
    offset := Point.Sub(Rect.NorthWest(VBT.Domain(v.zChild)),
                        Rect.NorthWest(newFrameDom));
    newDraggerDom := Rect.Sub(VBT.Domain(v), offset);
    draggerShowing := Rect.Meet(
                        newDraggerDom, VBT.Domain(VBT.Parent(v.zChild)));
  BEGIN
    RETURN (FLOAT(Rect.HorSize(draggerShowing))
              >= VBT.MMToPixels(v, MinSize, Axis.T.Hor))
             AND (FLOAT(Rect.VerSize(draggerShowing))
                    >= VBT.MMToPixels(v, MinSize, Axis.T.Ver));
  END IsVisible;

PROCEDURE OutlineThickness (v: T): INTEGER =
  CONST Thickness = 0.75;
  BEGIN
    RETURN ROUND(MAX(VBT.MMToPixels(v, Thickness, Axis.T.Hor),
                     VBT.MMToPixels(v, Thickness, Axis.T.Ver)))
  END OutlineThickness;

BEGIN
END ZMoveVBT.
