(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Feb  6 18:05:17 PST 1993 by meehan     *)
(*      modified on Sat Jan 30 00:09:39 PST 1993 by mhb        *)
(*      modified on Tue Jun 16 13:08:15 PDT 1992 by muller     *)
(*      modified on Wed Mar  4 00:22:33 1992 by steveg     *)


MODULE ShadowedVBT;

IMPORT Axis, BdrVBTClass, BorderedVBT, Filter, MouseSplit,
       Rect, Region, Shadow, ShadowPaint, VBT, VBTClass;

REVEAL
  Private = BorderedVBT.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
        shadow: Shadow.T;
        style : Shadow.Style;
      OVERRIDES
        init      := Init;
        rescreen  := Rescreen;
        repaintBorder := RepaintBorder;
      END;

PROCEDURE Init (v     : T;
                ch    : VBT.T;
                shadow: Shadow.T     := NIL;
                style : Shadow.Style := Shadow.Style.Flat): T =
  BEGIN
    IF shadow = NIL THEN shadow := Shadow.None END;
    v.shadow := shadow;
    v.style := style;
    ComputeBSize(v);
    EVAL Filter.T.init(v, ch);
    RETURN v
  END Init;

PROCEDURE Set (v: T; shadow: Shadow.T) =
  BEGIN
    IF v.shadow.size # shadow.size THEN VBT.NewShape(v) END;
    v.shadow := shadow;
    VBT.Mark(v)
  END Set;

PROCEDURE SetStyle (v: T; style: Shadow.Style) =
  BEGIN
    IF v.style # style THEN v.style := style; VBT.Mark(v) END
  END SetStyle;

PROCEDURE Get (v: T): Shadow.T =
  BEGIN
    RETURN v.shadow
  END Get;

PROCEDURE GetStyle (v: T): Shadow.Style =
  BEGIN
    RETURN v.style
  END GetStyle;

PROCEDURE ComputeBSize (v: T) =
  VAR size: REAL;
  BEGIN
    size := ABS(v.shadow.size);
    IF NOT Shadow.Supported(v.shadow, v) THEN
      size := size / 2.0
    END;
    FOR ax := FIRST(Axis.T) TO LAST(Axis.T) DO
      v.bSize[ax] := ROUND(VBT.MMToPixels(v, size, ax));
    END;
  END ComputeBSize;

PROCEDURE ChDom (v: T): Rect.T =
  (* Compute child domain from v's domain and border sizes *)
  BEGIN
    WITH dh = v.bSize [Axis.T.Hor], dv = v.bSize [Axis.T.Ver] DO
      RETURN Rect.Change (VBT.Domain (v), dh, -dh, dv, -dv)
    END
  END ChDom;

PROCEDURE Rescreen (v: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    ComputeBSize(v);
    VBT.NewShape(v);
    Filter.T.rescreen(v, cd)
  END Rescreen;

PROCEDURE RepaintBorder (v: T; READONLY clip: Rect.T) =
  (* repaint the part of v's border that lies within clip.  LL =
     VBT.mu. *)
  BEGIN
    ShadowPaint.Border(v, Region.FromRect(clip), v.shadow,
                       v.style, ChDom(v), VBT.Domain(v));
  END RepaintBorder;

BEGIN
END ShadowedVBT.
