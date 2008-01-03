(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: TextVBT.m3, coded by cgn Sun Jun 28 16:13:38 1987 *)
(* Last modified on Thu Sep 28 20:34:28 PDT 1995 by mhb     *)
(*      modified on Thu Mar  9 18:01:01 PST 1995 by msm     *)
(*      modified on Tue Mar 10 19:08:01 1992 by steveg  *)
(*      modified on Mon Feb 24 13:54:51 PST 1992 by muller  *)
(*      modified on Tue Nov 19 19:11:50 PST 1991 by gnelson *)
<*PRAGMA LL*>

MODULE PixmapVBT;

IMPORT Axis, PaintOp, Pixmap, Point, Rect, Region, VBT;

REVEAL
  T = Public BRANDED OBJECT
        pm      : Pixmap.T;
        op, bg  : PaintOp.T;
        margin  : ARRAY Axis.T OF REAL;
        align   : ARRAY Axis.T OF REAL;
      OVERRIDES
        init      := Init;
        repaint   := Repaint;
        shape     := Shape;
      END;

PROCEDURE Init (v               : T;
                pm              : Pixmap.T;
                halign, valign  : REAL       := 0.5;
                hmargin, vmargin: REAL       := 0.0;
                op              : PaintOp.T  := PaintOp.BgFg;
                bg              : PaintOp.T  := PaintOp.Bg    ): T =
  BEGIN
    v.pm := pm;
    v.op := op;
    v.bg := bg;
    v.margin[Axis.T.Hor] := hmargin;
    v.margin[Axis.T.Ver] := vmargin;
    v.align[Axis.T.Hor] := halign;
    v.align[Axis.T.Ver] := valign;
    RETURN v
  END Init;

PROCEDURE Put (v: T; pm: Pixmap.T) =
  VAR oldRect := VBT.PixmapDomain(v, v.pm);
  BEGIN
    v.pm := pm;
    IF NOT Rect.Equal(oldRect, VBT.PixmapDomain(v, pm)) THEN
      VBT.NewShape(v);
    END;
    VBT.Mark(v);
  END Put;

PROCEDURE SetColors (v: T; op: PaintOp.T; bg: PaintOp.T := PaintOp.Bg) = 
  BEGIN
    v.op := op;
    v.bg := bg;
    VBT.Mark(v);
  END SetColors;

PROCEDURE Repaint (v: T; READONLY rgn: Region.T) =
  VAR
    dom     := VBT.Domain(v);
    bounds  := VBT.PixmapDomain(v, v.pm);
    hmargin := ROUND(VBT.MMToPixels(v, v.margin[Axis.T.Hor], Axis.T.Hor));
    vmargin := ROUND(VBT.MMToPixels(v, v.margin[Axis.T.Ver], Axis.T.Ver));
  VAR
    delta: Point.T;
    a    : Rect.Partition;
  BEGIN
    delta.h := (dom.west + hmargin - bounds.west)
                 + TRUNC(v.align[Axis.T.Hor] * FLOAT(
                           (Rect.HorSize(dom) - 2 * hmargin
                              - Rect.HorSize(bounds))));
    delta.v := (dom.north + vmargin - bounds.north)
                 + TRUNC(v.align[Axis.T.Ver] * FLOAT(
                           (Rect.VerSize(dom) - 2 * vmargin
                              - Rect.VerSize(bounds))));
    Rect.Factor(Rect.Meet(dom, rgn.r), Rect.Move(bounds, delta), a, 0, 0);
    VBT.PaintPixmap(v, a[2], v.op, v.pm, delta);
    a[2] := Rect.Empty;
    VBT.PolyTexture(v, a, v.bg, Pixmap.Solid)
  END Repaint;

PROCEDURE Shape (v: T; ax: Axis.T; <* UNUSED *> n: CARDINAL):
  VBT.SizeRange =
  VAR sr: VBT.SizeRange;
  BEGIN
    sr.lo := Rect.Size(ax, VBT.PixmapDomain(v, v.pm))
               + 2 * ROUND(VBT.MMToPixels(v, v.margin[ax], ax));
    sr.pref := sr.lo;
    sr.hi := MAX(sr.pref + 1, VBT.DefaultShape.hi);
    RETURN sr;
  END Shape;

BEGIN END PixmapVBT.
