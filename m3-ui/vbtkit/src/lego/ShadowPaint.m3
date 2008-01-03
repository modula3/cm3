(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Tue Jan 25 14:10:36 PST 1994 by mhb      *)
(*      modified on Tue Jan 25 12:22:06 1994 by harrison *)
(* modified on Tue Jun 16 13:08:18 PDT 1992 by muller *)

MODULE ShadowPaint;

IMPORT Axis, Interval, PaintOp, Pixmap, Point, Rect, Region, Shadow,
       Trapezoid, VBT, VBTClass, Palette, ScreenType, ScrnPixmap,
       TrestleComm;

CONST SmallBevelSize = 16;
VAR
  BevelPixmap := Palette.FromPixmapClosure(NEW(Closure,
                                               size := SmallBevelSize));

TYPE
  Closure = Palette.PixmapClosure OBJECT
              size: CARDINAL;
            OVERRIDES
              apply := Apply;
            END;

PROCEDURE Apply (self: Closure; st: ScreenType.T): ScrnPixmap.T =
  VAR raw := ScrnPixmap.NewRaw(1, Rect.FromSize(self.size, self.size));
  BEGIN
    FOR y := 0 TO self.size - 1 DO
      FOR x := 0 TO self.size - 1 DO
        raw.set(Point.T{x, y}, ORD(x + y >= self.size));
      END;
    END;

    TRY
      RETURN st.pixmap.load(raw);
    EXCEPT
    | TrestleComm.Failure => RETURN NIL;
    END
  END Apply;

PROCEDURE Bar (         v     : VBT.T;
               READONLY clip  : Region.T;
                        shadow: Shadow.T;
                        style : Shadow.Style;
                        axis  : Axis.T;
               READONLY target: Rect.T        ) =
  VAR
    topOp, bottomOp, both: PaintOp.T;
    topR, bottomR: Rect.T;
    mid: INTEGER;
  BEGIN
    CASE style OF
    | Shadow.Style.Flat =>
        VBT.PaintTexture(v, Rect.Meet(target, clip.r), shadow.bg,
                         Pixmap.Solid, Point.Origin);
    ELSE
      GetOps(shadow, style, topOp, bottomOp, both);
      IF axis = Axis.T.Hor THEN
        mid := Interval.Middle(Interval.T{target.west, target.east})
      ELSE
        mid := Interval.Middle(Interval.T{target.north, target.south})
      END;
      Rect.Chop(axis, clip.r, mid, topR, bottomR);
      VBT.PaintTexture(v, topR, topOp, Pixmap.Solid, Point.Origin);
      VBT.PaintTexture(v, bottomR, bottomOp, Pixmap.Solid, Point.Origin);
    END;
  END Bar;


PROCEDURE Diamond (         v           : VBT.T;
                   READONLY clip        : Region.T;
                            shadow      : Shadow.T;
                            style       : Shadow.Style;
                   READONLY in, out     : Rect.T;
                            insideOp    : PaintOp.T;
                            insidePixmap: Pixmap.T      ) =

  PROCEDURE FillTriangle (         op     : PaintOp.T;
                          READONLY a, b, c: Point.T;
                                   pm     : Pixmap.T   ) =
    VAR t: Trapezoid.T;
    BEGIN
      t := Trapezoid.FromTriangle(a, b, c);
      IF (t.vlo >= t.vhi) OR (t.m1.n = 0) OR (t.m2.n = 0) THEN RETURN END;
      VBT.PaintTrapezoid(v, clip.r, t, op, pm, Point.Origin);
    END FillTriangle;

  VAR
    top, bottom, both: PaintOp.T;
    ptW, ptE, ptN, ptS: Point.T;
  BEGIN
    CASE style OF
    | Shadow.Style.Flat =>
        VBT.PaintTexture(v, Rect.Meet(out, clip.r), shadow.bg,
                         Pixmap.Solid, Point.Origin);
    ELSE
      GetOps(shadow, style, top, bottom, both);
      Midpoints(out, ptW, ptE, ptN, ptS);
      FillTriangle(shadow.bg, ptN, ptW, Rect.NorthWest(out), Pixmap.Solid);
      FillTriangle(shadow.bg, ptN, ptE, Rect.NorthEast(out), Pixmap.Solid);
      FillTriangle(shadow.bg, ptS, ptW, Rect.SouthWest(out), Pixmap.Solid);
      FillTriangle(shadow.bg, ptS, ptE, Rect.SouthEast(out), Pixmap.Solid);
      FillTriangle(top, ptW, ptE, ptN, Pixmap.Solid);
      FillTriangle(bottom, ptW, ptE, ptS, Pixmap.Solid);
      Midpoints(in, ptW, ptE, ptN, ptS);
      FillTriangle(insideOp, ptW, ptE, ptN, insidePixmap);
      FillTriangle(insideOp, ptW, ptE, ptS, insidePixmap);
    END;
  END Diamond;


PROCEDURE Border (         v      : VBT.T;
                  READONLY clip   : Region.T;
                           shadow : Shadow.T;
                           style  : Shadow.Style;
                  READONLY in, out: Rect.T        ) =
  VAR
    top, bottom, both: PaintOp.T;
    mid: Rect.T;
  BEGIN
    GetOps(shadow, style, top, bottom, both);
    CASE style OF
    | Shadow.Style.Flat => MonoColoredBorder(v, clip, in, out, shadow.bg);
    | Shadow.Style.Raised, Shadow.Style.Lowered =>
        BiColoredBorder(v, clip, in, out, top, bottom, both);
    | Shadow.Style.Chiseled, Shadow.Style.Ridged =>
        mid := Midline(in, out);
        BiColoredBorder(v, clip, mid, out, top, bottom, both);
        BiColoredBorder(v, clip, in, mid, bottom, top, both);
    END;
  END Border;


PROCEDURE MonoColoredBorder (         v      : VBT.T;
                             READONLY clip   : Region.T;
                             READONLY in, out: Rect.T;
                                      op     : PaintOp.T ) =
  VAR a: Rect.Partition;
  BEGIN
    Rect.Factor(Rect.Meet(out, clip.r), in, a, 0, 0);
    VBT.PaintTint(v, a[0], op);
    VBT.PaintTint(v, a[1], op);
    VBT.PaintTint(v, a[3], op);
    VBT.PaintTint(v, a[4], op);
  END MonoColoredBorder;


PROCEDURE BiColoredBorder (         v                : VBT.T;
                           READONLY clip             : Region.T;
                           READONLY in, out          : Rect.T;
                                    top, bottom, both: PaintOp.T ) =

  PROCEDURE FillRect (op: PaintOp.T; READONLY r: Rect.T) =
    BEGIN
      IF NOT Rect.IsEmpty(r) THEN
        VBT.PaintTint(v, Rect.Meet(r, clip.r), op);
      END;
    END FillRect;

  PROCEDURE FillTrapezoid (op: PaintOp.T; READONLY t: Trapezoid.T) =
    BEGIN
      IF t.vlo < t.vhi AND t.m1.n # 0 AND t.m2.n # 0 THEN
        VBT.PaintTrapezoid(v, clip.r, t, op);
      END;
    END FillTrapezoid;

  PROCEDURE IsSquareAndSmall (READONLY rect: Rect.T): BOOLEAN =
    BEGIN
      RETURN NOT Rect.IsEmpty(rect)
               AND Rect.HorSize(rect) = Rect.VerSize(rect)
               AND Rect.HorSize(rect) <= SmallBevelSize;
    END IsSquareAndSmall;

  VAR
    topBevelRect := Rect.FromCorners(Rect.SouthWest(in),
                                     Rect.SouthWest(out));
    bottomBevelRect := Rect.FromCorners(Rect.NorthEast(in),
                                        Rect.NorthEast(out));
  BEGIN
    (* If the corners containing the diagonal boundary between the top and
       bottom shadows are small and square, we can use a fast drawing
       method for the shadow.  This means drawing four rectangles and two
       square bevel pixmaps.  If we've got some strange bevel size or
       shape, we take the slow road. *)
    IF IsSquareAndSmall(topBevelRect) AND IsSquareAndSmall(bottomBevelRect) THEN
      (* north *)
      VBT.PaintTint(v,
                    Rect.Meet(Rect.FromEdges(out.west, in.east, out.north,
                                             in.north), clip.r), top);
      (* west *)
      VBT.PaintTint(v,
                    Rect.Meet(Rect.FromEdges(out.west, in.west, in.north,
                                             in.south), clip.r), top);
      (* east *)
      VBT.PaintTint(v,
                    Rect.Meet(Rect.FromEdges(in.east, out.east, in.north,
                                             in.south), clip.r), bottom);
      (* south *)
      VBT.PaintTint(v,
                    Rect.Meet(Rect.FromEdges(in.west, out.east, in.south,
                                             out.south), clip.r), bottom);
      (* north east bevel *)
      VBT.PaintTexture(
        v, Rect.Meet(topBevelRect, clip.r), both, BevelPixmap,
        Point.MoveV(Rect.SouthWest(topBevelRect), -SmallBevelSize));
      (* south west bevel *)
      VBT.PaintTexture(
        v, Rect.Meet(bottomBevelRect, clip.r), both, BevelPixmap,
        Point.MoveV(Rect.SouthWest(bottomBevelRect), -SmallBevelSize));

      RETURN;
    END;

    (* Slower, but more general method *)
    FillRect(top, Rect.FromEdges(out.west, in.west, out.north, out.south));
    FillRect(top, Rect.FromEdges(in.west, out.east, out.north, in.north));
    FillTrapezoid(bottom, Trapezoid.FromTriangle(
                            Rect.NorthEast(in), Rect.NorthEast(out),
                            Point.T{out.east, in.north}));
    FillRect(
      bottom, Rect.FromEdges(in.east, out.east, in.north, out.south));
    FillTrapezoid(
      bottom, Trapezoid.FromEdges(in.south, in.west, in.east, out.south,
                                  out.west, in.east));
  END BiColoredBorder;

PROCEDURE GetOps (    shadow           : Shadow.T;
                      style            : Shadow.Style;
                  VAR top, bottom, both: PaintOp.T     ) =
  BEGIN
    CASE style OF
    | Shadow.Style.Raised, Shadow.Style.Ridged =>
        top := shadow.light;
        bottom := shadow.dark;
        both := shadow.both;
    | Shadow.Style.Lowered, Shadow.Style.Chiseled =>
        top := shadow.dark;
        bottom := shadow.light;
        both := shadow.reversed;
    | Shadow.Style.Flat =>
        top := shadow.bg;
        bottom := shadow.bg;
        both := shadow.both;
    END;
  END GetOps;

PROCEDURE Midline (READONLY in, out: Rect.T): Rect.T RAISES {} =
  VAR
    de := (in.east - out.east) DIV 2;
    dw := (in.west - out.west) DIV 2;
    ds := (in.south - out.south) DIV 2;
    dn := (in.north - out.north) DIV 2;
  BEGIN
    RETURN Rect.Change(out, dw, de, dn, ds);
  END Midline;

PROCEDURE Midpoints (READONLY r                     : Rect.T;
                     VAR      midW, midE, midN, midS: Point.T ) =
  VAR
    midH := (r.west + r.east) DIV 2;
    midV := (r.north + r.south) DIV 2;
  BEGIN
    midN := Point.FromCoords(midH, r.north);
    midS := Point.FromCoords(midH, r.south);
    midW := Point.FromCoords(r.west, midV);
    midE := Point.FromCoords(r.east, midV);
  END Midpoints;

BEGIN
END ShadowPaint.
