(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Fri Jul 17 19:14:48 PDT 1992 by harrison *)
(*      modified on Sat Jul 11 02:07:17 1992 by steveg   *)

MODULE Appearances;

<* PRAGMA LL *>

IMPORT Axis, MG, MGPublic, Point, Rect, Pts, R2, R2Box, Region;

TYPE
  T = MG.T;
  V = MG.V;

REVEAL
  DropShadow = DSPublic BRANDED OBJECT
         OVERRIDES
                 paint       := DSPaint;
                 boundingBox := DSBB;
                 region      := DSRegion;
               END;

PROCEDURE DSPaint (app: DropShadow; t: T; v: V; VAR clip: Rect.T) =
  VAR
    color := t.color;
    pos   := MGPublic.Pos(t, v);
  BEGIN
    MG.SetPosLocked(t, R2.Add(pos, app.delta), v);
    t.color := app.color;
    t.render(v, clip);
    MG.SetPosLocked(t, pos, v);
    t.color := color;
    Appearance.paint(app, t, v, clip);
  END DSPaint;

PROCEDURE DSBB(app: DropShadow; t: T; v: V): R2Box.T =
  VAR
    bounds := Appearance.boundingBox(app, t, v);
  BEGIN
    IF app.delta[0] > 0.0 THEN
      bounds[0].hi := bounds[0].hi + app.delta[0];
    ELSE
      bounds[0].lo := bounds[0].lo + app.delta[0];
    END;
    IF app.delta[1] > 0.0 THEN
      bounds[1].hi := bounds[1].hi + app.delta[1];
    ELSE
      bounds[1].lo := bounds[1].lo + app.delta[1];
    END;

    RETURN bounds;
  END DSBB;

PROCEDURE DSRegion(app: DropShadow; t: T; v: V): Region.T =
  VAR 
    rgn := Appearance.region(app, t, v);
    delta := MG.MGCToScreenPoint(v, app.delta);
  BEGIN
    RETURN Region.Join(rgn, Region.AddHV(rgn, delta.h, delta.v))
  END DSRegion;

REVEAL
  DropShadowHighlight = DSHPublic BRANDED OBJECT
               OVERRIDES
                 paint       := DSHPaint;
                 boundingBox := DSHBB;
                 region      := DSHRegion;
               END;

PROCEDURE DSHPaint (app: DropShadowHighlight; t: T; v: V; VAR clip: Rect.T) =
  VAR
    color := t.color;
    pos   := MGPublic.Pos(t, v);
  BEGIN
    IF t.highlight > 0.0 THEN
      MG.SetPosLocked(t, R2.Add(pos, R2.Scale(t.highlight, app.delta)), v);
      t.color := app.color;
      t.render(v, clip);
      MG.SetPosLocked(t, pos, v);
      t.color := color;
    END;
    Appearance.paint(app, t, v, clip);
  END DSHPaint;

PROCEDURE DSHBB (              app       : DropShadowHighlight;
                               t         : T;
                               v         : V): R2Box.T =
  VAR
    bounds := Appearance.boundingBox(app, t, v);
  BEGIN
    IF t.highlight > 0.0 THEN
      IF app.delta[0] > 0.0 THEN
        bounds[0].hi := bounds[0].hi + t.highlight * app.delta[0];
      ELSE
        bounds[0].lo := bounds[0].lo + t.highlight * app.delta[0];
      END;
      IF app.delta[1] > 0.0 THEN
        bounds[1].hi := bounds[1].hi + t.highlight * app.delta[1];
      ELSE
        bounds[1].lo := bounds[1].lo + t.highlight * app.delta[1];
      END;
    END;

    RETURN bounds;
  END DSHBB;

PROCEDURE DSHRegion (app: DropShadowHighlight; t: T; v: V): Region.T =
  VAR
    rgn := Appearance.region(app, t, v);
    delta: Point.T;
  BEGIN
    IF t.highlight > 0.0 THEN
      delta := MG.MGCToScreenPoint(v, R2.Scale(t.highlight, app.delta));
      RETURN Region.Join(rgn, Region.AddHV(rgn, delta.h, delta.v))
    ELSE
      RETURN rgn
    END;
  END DSHRegion;

REVEAL
  WeightUnderlay = WUPublic BRANDED OBJECT
                   OVERRIDES
                     paint       := WUPaint;
                     boundingBox := WUBB;
                     region      := WURegion;
                   END;

PROCEDURE WUPaint (app: WeightUnderlay; t: T; v: V; VAR clip: Rect.T) =
  VAR
    color  := t.color;
    weight := t.weight;
  BEGIN
    t.weight := app.weight;
    t.color := app.color;
    t.render(v, clip);
    t.weight := weight;
    t.color := color;
    Appearance.paint(app, t, v, clip);
  END WUPaint;

PROCEDURE WUBB (              app       : WeightUnderlay;
                              t         : T;
                              v         : V): R2Box.T =
  VAR dw := app.weight - t.weight;
      bounds := Appearance.boundingBox(app, t, v);
  BEGIN
    IF dw > 0.0 THEN
      bounds := R2Box.Inset(bounds, -dw);
    END;

    RETURN bounds;
  END WUBB;

PROCEDURE WURegion (app: WeightUnderlay; t: T; v: V): Region.T =
  VAR
    rgn := Appearance.region(app, t, v);
    dw  := app.weight - t.weight;
  BEGIN
    IF dw <= 0.0 THEN
      RETURN rgn
    ELSE
      RETURN Region.Inset(rgn, MAX(Pts.ToScreenPixels(v, dw, Axis.T.Hor),
                                   Pts.ToScreenPixels(v, dw, Axis.T.Ver)))
    END;
  END WURegion;


REVEAL
  WeightUnderlayHighlight = WUHPublic BRANDED OBJECT
               OVERRIDES
                 paint       := WUHPaint;
                 boundingBox := WUHBB;
                 region      := WUHRegion;
               END;

PROCEDURE WUHPaint (    app : WeightUnderlayHighlight;
                        t   : T;
                        v   : V;
                    VAR clip: Rect.T                   ) =
  VAR
    color  := t.color;
    weight := t.weight;
  BEGIN
    IF t.highlight > 0.0 THEN
      t.weight := t.weight + t.highlight * (app.weight - t.weight);
      t.color := app.color;
      t.render(v, clip);
      t.weight := weight;
      t.color := color;
    END;
    Appearance.paint(app, t, v, clip);
  END WUHPaint;

PROCEDURE WUHBB (              app       : WeightUnderlayHighlight;
                               t         : T;
                               v         : V): R2Box.T =
  VAR dw := t.highlight * (app.weight - t.weight);
    bounds := Appearance.boundingBox(app, t, v);
  BEGIN
    IF dw > 0.0 THEN
      bounds := R2Box.Inset(bounds, -dw);
    END;

    RETURN bounds;
  END WUHBB;

PROCEDURE WUHRegion (app: WeightUnderlayHighlight; t: T; v: V): Region.T =
  VAR
    rgn := Appearance.region(app, t, v);
    dw  := t.highlight * (app.weight - t.weight);
  BEGIN
    IF dw <= 0.0 THEN
      RETURN rgn
    ELSE
      RETURN Region.Inset(rgn, MAX(Pts.ToScreenPixels(v, dw, Axis.T.Hor),
                                   Pts.ToScreenPixels(v, dw, Axis.T.Ver)))
    END;
  END WUHRegion;

REVEAL
  ColorLineHighlight =
    CLHPublic BRANDED OBJECT OVERRIDES paint := CLHPaint; END;

PROCEDURE CLHPaint (app: ColorLineHighlight; t: T; v: V; VAR clip: Rect.T) =
  VAR
    pos                       := MGPublic.Pos(t, v);
    delta                     := R2.Sub(MGPublic.LineTo(t, v), pos);
    scale           : REAL;
    clipTo, clipFrom: Rect.T;
    split           : Point.T;
    color                     := t.color;
  BEGIN
    IF t.highlight > 0.0 THEN
      IF app.fromFrom THEN
        scale := t.highlight
      ELSE
        scale := 1.0 - t.highlight
      END;
      split := MG.MGCToScreenPoint(v, R2.Add(pos, R2.Scale(scale, delta)));

      IF delta[0] > delta[1] THEN
        IF (app.fromFrom AND delta[0] < 0.0)
             OR (NOT app.fromFrom AND delta[0] > 0.0) THEN
          Rect.Chop(Axis.T.Hor, clip, split.h, clipFrom, clipTo)
        ELSE
          Rect.Chop(Axis.T.Hor, clip, split.h, clipTo, clipFrom);
        END;
      ELSE
        IF (app.fromFrom AND delta[1] < 0.0)
             OR (NOT app.fromFrom AND delta[1] > 0.0) THEN
          Rect.Chop(Axis.T.Ver, clip, split.v, clipTo, clipFrom);
        ELSE
          Rect.Chop(Axis.T.Ver, clip, split.v, clipFrom, clipTo)
        END;
      END;
      t.color := app.highlightColor;
      Appearance.paint(app, t, v, clipFrom);
      t.color := color;
    END;
    Appearance.paint(app, t, v, clip);
  END CLHPaint;

REVEAL
  Clipped =
    ClippedPublic BRANDED OBJECT OVERRIDES paint := ClippedPaint; END;

CONST
  LeftRight = Sides{Side.Left,Side.Right};
  TopBottom = Sides{Side.Top,Side.Bottom};

PROCEDURE ClippedPaint (    self: Clipped;
                            t   : MG.T;
                            v   : MG.V;
                        VAR clip: Rect.T             ) =
  VAR
    rect           := t.rgn(v).r;
    wd             := FLOAT(Rect.HorSize(rect));
    ht             := FLOAT(Rect.VerSize(rect));
    hscale, vscale := 1.0;
  BEGIN
    IF LeftRight = LeftRight * self.sides THEN hscale := 0.5 END;
    IF TopBottom = TopBottom * self.sides THEN vscale := 0.5 END;
    IF Side.Top IN self.sides THEN
      rect.north :=
        rect.north + ROUND(vscale * t.highlight * ht);
    END;
    IF Side.Bottom IN self.sides THEN
      rect.south :=
        rect.south - ROUND(vscale * t.highlight * ht);
    END;
    IF Side.Left IN self.sides THEN
      rect.west := rect.west + ROUND(hscale * t.highlight * wd);
    END;
    IF Side.Right IN self.sides THEN
      rect.east := rect.east - ROUND(hscale * t.highlight * wd);
    END;
    rect := Rect.Meet(clip, rect);
    Appearance.paint(self, t, v, rect);
  END ClippedPaint;

BEGIN
END Appearances.
