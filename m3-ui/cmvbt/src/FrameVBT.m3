(* Copyright 1996-2000, Critical Mass, Inc. All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE FrameVBT;

IMPORT Axis, BdrVBTClass, BorderedVBT, FilterClass, Font, MouseSplit;
IMPORT Point, Rect, Region, Shadow, ShadowPaint, Text, VBT, VBTClass;

CONST
  ChiselWidth = 2;  (* pixels *)

REVEAL
  T = Public BRANDED OBJECT
        title: Text.T;
        font: Font.T;
        shadow: Shadow.T;
      OVERRIDES
        init := Init;
        rescreen := Rescreen;
        reshape := Reshape;
        repaintBorder := RepaintBorder;
      END;

PROCEDURE New (ch: VBT.T;  title: TEXT;  fnt: Font.T := Font.BuiltIn;
                shadow: Shadow.T := NIL): T =
  BEGIN
    RETURN NEW (T).init (ch, title, fnt, shadow);
  END New;

PROCEDURE Init (v     : T;
                ch    : VBT.T;
                title : TEXT;
                fnt   : Font.T;
                shadow: Shadow.T := NIL): T = 
  BEGIN
    IF (shadow = NIL) THEN shadow := Shadow.None; END;
    v.title  := title;
    v.font   := fnt;
    v.shadow := shadow;
    EVAL BorderedVBT.T.init (v, ch, BorderSize (v), shadow.bg);
    RETURN v
  END Init;

PROCEDURE RepaintBorder (v: T; READONLY clip: Rect.T) =
  CONST Z = MAX (1, ChiselWidth DIV 2);
  VAR
    dom     := VBT.Domain(v);
    dh      := v.bSize[Axis.T.Hor] DIV 2;
    dv      := v.bSize[Axis.T.Ver] DIV 2;
    bbox    := VBT.BoundingBox (v, v.title, v.font);
    pt      := Point.Add (Rect.NorthWest (dom), 
                          Point.T {h := 3 * dh, v := dv + dv - bbox.south});
    bkgnd   := Rect.Change (Rect.Move (bbox, pt), -2, +2, 0, 0);
    chisout := Rect.Change (dom, dw := dh+Z, de := -dh-Z, ds := -dv-Z, dn := dv+Z);
    chisin  := Rect.Change (chisout, ChiselWidth, -ChiselWidth,
                                    +ChiselWidth, -ChiselWidth);
  BEGIN
    BorderedVBT.T.repaintBorder (v, clip);
    ShadowPaint.Border (v, Region.FromRect(clip), v.shadow,
                        Shadow.Style.Chiseled, chisin, chisout);
    VBT.PaintTint (v, Rect.Meet (clip, bkgnd), op := v.shadow.bg);
    VBT.PaintText (v, clip, pt, v.font, v.title, op := v.shadow.bgFg);
  END RepaintBorder;

PROCEDURE Rescreen (v: T; READONLY cd: VBT.RescreenRec) RAISES {} =
  BEGIN
    BorderedVBT.SetSize (v, BorderSize (v));
    BorderedVBT.T.rescreen (v, cd);
  END Rescreen;

PROCEDURE Reshape(v: T;  READONLY cd: VBT.ReshapeRec) =
  (* LL = VBT.mu *)
  BEGIN
    BorderedVBT.SetSize (v, BorderSize (v));
    BorderedVBT.T.reshape (v, cd);
    RepaintBorder (v, Rect.Full);
  END Reshape;

PROCEDURE BorderSize (v: T): REAL =
  VAR
    txt_height := Rect.VerSize (VBT.BoundingBox(v, v.title, v.font));
    border := MAX (ChiselWidth + 2, txt_height + MAX (txt_height DIV 2, 2));
  BEGIN
    RETURN PixelsToMM (v, border);
  END BorderSize;

PROCEDURE PixelsToMM (v: T;  pixels: INTEGER): REAL =
  VAR
    pv := VBT.MMToPixels (v, 10.0, Axis.T.Ver);
    ph := VBT.MMToPixels (v, 10.0, Axis.T.Hor);
    xx := MIN (pv, ph);
  BEGIN
    IF xx = 0.0 THEN RETURN 10.0; END;
    RETURN FLOAT (pixels) * 10.0 / xx;
  END PixelsToMM;

BEGIN
END FrameVBT.
