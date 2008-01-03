(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: BorderedVBT.mod, by cgn, Tue Apr 21 22:00:25 1987 *)
<*PRAGMA LL*>

(* Last modified on Fri Sep 25 16:31:07 PDT 1992 by msm     *)
(*      modified on Tue Mar 10 01:31:52 1992 by steveg  *)
(*      modified on Mon Feb 24 13:52:49 PST 1992 by muller  *)
(*      modified on Sun Nov 10 19:41:00 PST 1991 by gnelson *)
(*      modified on Tue May 15 12:05:23 PDT 1990 by mcjones *)


MODULE BorderedVBT EXPORTS BorderedVBT, BdrVBTClass;

IMPORT
  VBT, Rect, Axis, VBTClass, Filter, Point,
  PaintOp, Pixmap, Region, FilterClass;

REVEAL
  T = Private BRANDED OBJECT
        borderSize   : REAL;
        borderOp     : PaintOp.T;
        borderTexture: Pixmap.T;
      OVERRIDES
        repaintBorder := RepaintBorder;
        reshape       := Reshape;
        rescreen      := Rescreen;
        repaint       := Repaint;
        shape         := Shape;
        locate        := Locate;
        init          := Be
      END;

PROCEDURE Locate(v: T; READONLY pt: Point.T; VAR r: Rect.T): VBT.T =
  BEGIN
    RETURN VBT.Split.locate(v, pt, r)
  END Locate;

PROCEDURE Be(
    v: T;
    ch: VBT.T;
    borderSize: REAL := Default;
    borderOp: PaintOp.T := PaintOp.Fg;
    borderTexture: Pixmap.T := Pixmap.Solid): T =
  BEGIN
    v.borderSize := borderSize;
    v.borderOp := borderOp;
    v.borderTexture := borderTexture;
    FOR ax := FIRST(Axis.T) TO LAST(Axis.T) DO
      v.bSize[ax] := ROUND(VBT.MMToPixels(v, borderSize, ax))
    END;
    EVAL Filter.T.init(v, ch);
    RETURN v
  END Be;

PROCEDURE New(
    ch: VBT.T;
    borderSize: REAL := Default;
    borderOp: PaintOp.T := PaintOp.Fg;
    borderTexture: Pixmap.T := Pixmap.Solid)
    : T =
  BEGIN
    RETURN Be(NEW(T), ch, borderSize, borderOp, borderTexture)
  END New;

PROCEDURE Shape(v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange 
  RAISES {} =
  VAR temp: VBT.SizeRange; bw := 2 * v.bSize[ax]; BEGIN
    IF v.ch = NIL THEN 
      temp := VBT.DefaultShape 
    ELSE
      IF n > bw THEN
        n := n - bw
      ELSE
        n := 0
      END;
      temp := VBTClass.GetShape(v.ch, ax, n)
    END;
    RETURN VBT.SizeRange{lo := temp.lo + bw, hi := temp.hi + bw,
                         pref := temp.pref + bw}
  END Shape;

PROCEDURE SetSize(v: T; newSize: REAL) =
  BEGIN
    IF newSize # v.borderSize THEN
      v.borderSize := newSize;
      FOR ax := FIRST(Axis.T) TO LAST(Axis.T) DO
        v.bSize[ax] := ROUND(VBT.MMToPixels(v, newSize, ax))
      END;
      VBT.NewShape(v);
      VBT.Mark(v)
    END
  END SetSize;

PROCEDURE SetColor(
  v: T; 
  op: PaintOp.T; 
  texture: Pixmap.T := Pixmap.Solid) =
  BEGIN
    v.borderOp := op;
    v.borderTexture := texture;
    VBT.Mark(v)
  END SetColor;

PROCEDURE Get(
  v: T; 
  VAR size: REAL; 
  VAR op: PaintOp.T; 
  VAR texture: Pixmap.T) =
  BEGIN
    size := v.borderSize;
    op := v.borderOp;
    texture := v.borderTexture
  END Get;

PROCEDURE RepaintBorder(v: T; READONLY clip: Rect.T) RAISES {} =
  (* repaint the part of v's border that lies within clip. LL = VBT.mu. *)
  VAR a: Rect.Partition;
  BEGIN
    Rect.Factor(
      Rect.Meet(v.domain, clip),
      ChDom(v), a, 0, 0);
    a[2] := a[4];
    VBT.PolyTexture(v, SUBARRAY(a, 0, 4), 
      v.borderOp, v.borderTexture)
  END RepaintBorder;

PROCEDURE ChDom(v: T): Rect.T =
(* Compute child domain from v's domain and border sizes *)
  BEGIN
    WITH dh = v.bSize[Axis.T.Hor], dv = v.bSize[Axis.T.Ver] DO
      RETURN Rect.Change(v.domain, dh, -dh, dv, -dv)
    END
  END ChDom;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  VAR chDom: Rect.T; 
  BEGIN
    VBTClass.LocateChanged(v);
    IF v.ch # NIL THEN
      chDom := ChDom(v);
      VBTClass.Reshape(v.ch, new := chDom, saved := cd.saved)
    END;
    IF NOT Rect.IsEmpty(cd.new) THEN
      v.repaintBorder(cd.new)
    END
  END Reshape;

PROCEDURE Rescreen(v: T; READONLY cd: VBT.RescreenRec) RAISES {} =
  BEGIN
    FOR ax := FIRST(Axis.T) TO LAST(Axis.T) DO
      v.bSize[ax] := ROUND(VBT.MMToPixels(v, v.borderSize, ax))
    END;
    VBT.NewShape(v);
    Filter.T.rescreen(v, cd)
  END Rescreen;

PROCEDURE Repaint(v: T; READONLY badR: Region.T) RAISES {} =
  BEGIN
    Filter.T.repaint(v, badR);
    v.repaintBorder(badR.r)
  END Repaint;

BEGIN
END BorderedVBT.
