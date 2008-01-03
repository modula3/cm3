(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Sep 23 11:17:54 PDT 1993 by mhb    *)

MODULE IntArrayView;

IMPORT Axis, Fmt, Font, MG, MGV, PaintOp, Pts, R2, VBT;

REVEAL
  V = PublicV BRANDED OBJECT
  OVERRIDES
    reshape := MGV.ReshapeLeaveOrigin;
    shape := Shape;
    init := Init;
  END; (* object *)

PROCEDURE Shape (v: V; ax: Axis.T; <* UNUSED *> n: CARDINAL):
  VBT.SizeRange =
  BEGIN
    WITH p = MAX(10, Pts.ToScreenPixels(v, v.prefDimPts[ax], ax)) DO
      RETURN VBT.SizeRange{0, p, MAX(VBT.DefaultShape.hi, p + 1)}
    END;
  END Shape;

PROCEDURE Init (v                    : V;
                size                 : CARDINAL;
                widthPts, heightPts  : REAL;
                font                 : Font.T     := Font.BuiltIn;
                prefWidth, prefHeight             := 0.0           ): V =
  VAR elems := NEW(Elems, size);
  BEGIN
    EVAL MG.V.init(v);
    FOR i := 0 TO size - 1 DO
      elems[i] :=
        NEW(Elem, id := i, i := i, font := font, weight := 0.0).init(
          corner1 := R2.T{FLOAT(i) * widthPts, 0.0},
          corner2 := R2.T{FLOAT(i + 1) * widthPts, heightPts}, v := v);
    END;
    LOCK v.mu DO
      v.setNW(R2.T{0.0, heightPts});
      IF prefWidth = 0.0 THEN prefWidth := FLOAT(size) * widthPts END;
      IF prefHeight = 0.0 THEN prefHeight := heightPts END;
      v.prefDimPts[Axis.T.Hor] := prefWidth;
      v.prefDimPts[Axis.T.Ver] := prefHeight;
      v.elems := elems;
    END;
    RETURN v;
  END Init;

PROCEDURE SetColor(v: V; elem: CARDINAL; color: PaintOp.ColorScheme) =
  BEGIN
    LOCK v.mu DO
      v.elems[elem].setColor(v, color);
    END;
  END SetColor;

PROCEDURE SetFont(v: V; elem: CARDINAL; font: Font.T) =
  BEGIN
    LOCK v.mu DO
      v.elems[elem].setFont(v, font);
    END;
  END SetFont;

PROCEDURE SetValue (v: V; elem: CARDINAL; i: INTEGER) =
  BEGIN
    LOCK v.mu DO
      v.elems[elem].i := i;
      v.elems[elem].setLabel(v, Fmt.Int(i));
    END;
  END SetValue;

PROCEDURE ClearValue(v: V; elem: CARDINAL) =
  BEGIN
    LOCK v.mu DO
      v.elems[elem].setColor(v, PaintOp.bgFg);
      v.elems[elem].setLabel(v, "");
    END;
  END ClearValue;

   
BEGIN
END IntArrayView.

