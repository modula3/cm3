(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Thu Jan  5 20:51:37 PST 1995 by najork *)
(*      modified on Fri Apr 23 15:48:34 PDT 1993 by mhb    *)
(*      modified on Wed Jan  6 10:30:50 PST 1993 by steveg *)
(*      modified on Wed Jul 29 23:17:19 PDT 1992 by johnh  *)
<* PRAGMA LL *>

MODULE EuclidView EXPORTS Euclid;

IMPORT Animate, Axis, EuclidViewClass, Filter, Font, Math, Matrix2D, MG,
       MGPublic, MGV, PaintOp, Pts, R2, R2Path, Region, ScaleFilter, Thread,
       VBT, View, ZeusPanel;

TYPE
  T = EuclidViewClass.T BRANDED OBJECT
        v    : V;
        a    : REAL                               := 3.0;
        b    : REAL                               := 4.0;
        pts  : ARRAY [A .. X] OF MG.T;
        lines: ARRAY [A .. X], [A .. X] OF MG.T;
        stash: ARRAY [0 .. 20] OF
                 RECORD
                   p1, p2, p3, p4: Id;
                   mg            : MG.T
                 END;
      OVERRIDES
        startrun := StartRun;

        <* LL=0 *>
        oeSetupTriangle  := SetupTriangle;
        oeSetupSquare    := SetupSquare;
        oeLineAL         := LineAL;
        oeTriangle       := Triangle;
        oeQuad           := Quad;
        oeShear          := Shear;
        oeRotateTriangle := RotateTriangle;
        oeRotateAngle    := RotateAngle;
        oeRotateLine     := RotateLine;
        oeHighlightLine  := HighlightLine;
        oeHighlightAngle := HighlightAngle;
        oeRemove         := Remove;
      END;

  V = MGV.V OBJECT OVERRIDES shape := ShapeV; END;

VAR
  lineColor := ARRAY [0 .. 2] OF
                 PaintOp.ColorScheme{
                 PaintOp.bgFg, MGPublic.ColorFromText("Green", "Green"),
                 MGPublic.ColorFromText("Red", "Red")};

PROCEDURE ShapeV (v: V; ax: Axis.T; <* UNUSED *> n: CARDINAL):
  VBT.SizeRange =
  VAR
    t   : T    := v.view;
    size: REAL;
  BEGIN
    IF t # NIL THEN
      IF ax = Axis.T.Hor THEN
        size := t.a + t.b + t.b
      ELSE
        size := t.a + t.a + t.b
      END;
    ELSE
      IF ax = Axis.T.Hor THEN
        size := 11.0
      ELSE
        size := 10.0
      END;
    END;

    WITH pref = Pts.ToScreenPixels(v, EScale * 1.4 * size, ax) DO
      RETURN VBT.SizeRange{pref, pref, MAX(pref + 1, VBT.DefaultShape.hi)}
    END;
  END ShapeV;

VAR
  font := Font.FromName(
            ARRAY OF
              TEXT{"-*-helvetica-bold-r-*-*-*-100-*-*-*-*-iso8859-1"});

CONST EScale = 20.0;

CONST
  Label = ARRAY OF TEXT{"A", "B", "C", "D", "E", "F", "G",
      "H", "K", "L", ""};

PROCEDURE InitPt (t: T; id: Id; setLabel := TRUE) =
  VAR
    a := t.a;
    b := t.b;
    r := (a * a) / (a * a + b * b);
  BEGIN
    IF t.pts[id] = NIL THEN
      CASE id OF
      | A => Pt(t, A, b, a, MG.Alignment.SW);
      | B => Pt(t, B, b + a, a, MG.Alignment.SE);
      | C => Pt(t, C, b, a + b, MG.Alignment.NW);
      | D => Pt(t, D, a + b + b, a + a, MG.Alignment.E);
      | E => Pt(t, E, b + b, a + a + b, MG.Alignment.NE);
      | F => Pt(t, F, a + b, 0.0, MG.Alignment.SE);
      | G => Pt(t, G, b, 0.0, MG.Alignment.SW);
      | H => Pt(t, H, 0.0, a, MG.Alignment.SW);
      | K => Pt(t, K, 0.0, a + b, MG.Alignment.NW);
      | L => Pt(t, L, a + b + b - r * a, a + a + r * b, MG.Alignment.NE);
      | X => Pt(t, X, a + b - r * a, a + r * b, MG.Alignment.NE);
      ELSE                      <* ASSERT FALSE *>
      END;
    END;
    IF setLabel THEN t.pts[id].setLabel(t.v, Label[id]); END;
  END InitPt;

PROCEDURE Pt (t: T; id: Id; h, v: REAL; dir: MG.Alignment) =
  BEGIN
    t.pts[id] := NEW(MG.Label, font := font,
                     alignment := dir,
                     m := Matrix2D.Translate(
                            EScale * (2.0 + h), EScale * (2.0 + v))).init(
                   v := t.v)
  END Pt;

CONST
  Weight = 2.0;

PROCEDURE NewLine (t     : T;
                   p1, p2: Id;
                   weight: REAL;
                   color : PaintOp.ColorScheme): MG.Line =
  BEGIN
    RETURN NEW(MG.Line, weight := weight, color := color).init(
             from := MGPublic.Pos(t.pts[p1], t.v),
             to := MGPublic.Pos(t.pts[p2], t.v), v := t.v);
  END NewLine;

PROCEDURE Line (t: T; p1, p2: Id) =
  BEGIN
    IF t.lines[p1, p2] = NIL THEN
      t.lines[p1, p2] := NewLine(t, p1, p2, Weight, PaintOp.bgFg);
      t.lines[p2, p1] := t.lines[p1, p2];
    END;
  END Line;

PROCEDURE LineAL (t: T; show: BOOLEAN) =
  BEGIN
    InitPt(t, L);
    Line(t, A, L);
    IF show THEN t.v.mgRedisplay(Region.Full) END;
  END LineAL;

VAR
  objColor := ARRAY [1..2] OF PaintOp.ColorScheme{
     MGPublic.ColorFromText("VeryLightBlue", "Black"),
     MGPublic.ColorFromText("VeryLightRed", "Black")
     };

PROCEDURE MakeTriangle (t: T; p1, p2, p3: Id; color: INTEGER): MG.Shape =
  VAR
    path            := NEW(R2Path.T);
    shape: MG.Shape;
  BEGIN
    path.init();
    path.moveTo(MGPublic.Pos(t.pts[p1], t.v));
    path.lineTo(MGPublic.Pos(t.pts[p2], t.v));
    path.lineTo(MGPublic.Pos(t.pts[p3], t.v));
    path.close();
    shape :=
      NEW(MG.Shape, color := objColor[color]).init(R2.Origin, path, v := t.v);
    AddToStash(t, shape, p1, p2, p3);
    RETURN shape;
  END MakeTriangle;

PROCEDURE Remove (t: T; p1, p2, p3, p4: Id; show: BOOLEAN) =
  BEGIN
    RemoveFromStash(t, p1, p2, p3, p4);
    IF show THEN t.v.mgRedisplay(Region.Full) END;
  END Remove;

PROCEDURE Triangle (t: T; p1, p2, p3: Id; color: INTEGER) =
  BEGIN
    EVAL MakeTriangle(t, p1, p2, p3, color);
    t.v.mgRedisplay(Region.Full);
  END Triangle;

PROCEDURE Quad (t: T; p1, p2, p3, p4: Id; color: INTEGER) =
  VAR
    path : R2Path.T;
    shape: MG.Shape;
  BEGIN
    InitPt(t, p1, FALSE);
    InitPt(t, p2, FALSE);
    InitPt(t, p3, FALSE);
    InitPt(t, p4, FALSE);
    path := NEW(R2Path.T);
    path.init();
    path.moveTo(MGPublic.Pos(t.pts[p1], t.v));
    path.lineTo(MGPublic.Pos(t.pts[p2], t.v));
    path.lineTo(MGPublic.Pos(t.pts[p3], t.v));
    path.lineTo(MGPublic.Pos(t.pts[p4], t.v));
    path.close();
    shape :=
      NEW(MG.Shape, color := objColor[color]).init(R2.Origin, path, v := t.v);
    AddToStash(t, shape, p1, p2, p3, p4);
    t.v.mgRedisplay(Region.Full);
  END Quad;

TYPE
  ShearAnimation = Animate.T OBJECT
    p1, p2, from, to: R2.T;
  OVERRIDES
    doStep := ShearStep;
  END;

PROCEDURE ShearStep (             t       : ShearAnimation;
                                  time    : REAL;
                     <* UNUSED *> timePrev: REAL;
                                  v       : MG.V;
                                  mg      : MG.T            ) =
  VAR path := NEW(R2Path.T);
  BEGIN
    path.init();
    path.moveTo(t.p1);
    path.lineTo(t.p2);
    path.lineTo(R2.Add(t.from, R2.Scale(time, R2.Sub(t.to, t.from))));
    path.close();
    NARROW(mg, MG.Shape).reshape(v, R2.Origin, path);
  END ShearStep;

PROCEDURE Shear (t: T; p1, p2, from, to: Id) RAISES {Thread.Alerted} =
  VAR
    ptFrom       := MGPublic.Pos(t.pts[from], t.v);
    ptTo         := MGPublic.Pos(t.pts[to], t.v);
    triangle     := Stash(t, from, p1, p2);
  BEGIN
    LOCK t.v.mu DO t.v.displayList.top(t.v, t.lines[p1, p2]); END;
    MGV.AddAnimation(
      t.v, NEW(ShearAnimation, from := ptFrom, to := ptTo,
               p1 := MGPublic.Pos(t.pts[p1], t.v),
               p2 := MGPublic.Pos(t.pts[p2], t.v)).init(), triangle);
    MGV.Animation(t.v);
  END Shear;

CONST
  TwoPi = 2.0 * Math.Pi;

PROCEDURE RotationAngle (t: T; pivot, pFrom: Id; pTo: Id): REAL =
  VAR
    ptFrom := MGPublic.Pos(t.pts[pFrom], t.v);
    ptTo   := MGPublic.Pos(t.pts[pTo], t.v);
    ptPiv  := MGPublic.Pos(t.pts[pivot], t.v);
    angle1 := Angle(R2.Sub(ptFrom, ptPiv));
    angle2 := Angle(R2.Sub(ptTo, ptPiv));
    ang    := angle2 - angle1;
  BEGIN
(*
    IF ang < 0.0 THEN
      ang := ang + TwoPi;
    ELSIF ang > TwoPi THEN
      ang := ang - TwoPi;
    END;
    IF ang > Math.Pi THEN
      ang := TwoPi - ang;
    END;
*)
    RETURN ang;
  END RotationAngle;

PROCEDURE RotateLine (t: T; pivot, pFrom: Id; pTo: Id) 
    RAISES {Thread.Alerted} =
  VAR
    line   := Stash(t, pivot, pFrom);
    ang := RotationAngle(t, pivot, pFrom, pTo);
  BEGIN
    MGV.AddAnimation(
      t.v, NEW(Animate.Rotate, origin := MGPublic.Pos(t.pts[pivot], t.v),
               angle := (180.0 / Math.Pi) * ang).init(), line);
    MGV.Animation(t.v);
  END RotateLine;

PROCEDURE RotateTriangle (             t            : T;
                                       pivot, p1From: Id;
                                       p1To         : Id;
                                       p2From       : Id;
                          <* UNUSED *> p2To         : Id  ) 
    RAISES {Thread.Alerted} =
  VAR
    triangle := Stash(t, pivot, p1From, p2From);
    ang := RotationAngle(t, pivot, p1From, p1To);
  BEGIN
    MGV.AddAnimation(
      t.v, NEW(Animate.Rotate, origin := MGPublic.Pos(t.pts[pivot], t.v),
               angle := (180.0 / Math.Pi) * ang).init(), triangle);
    MGV.Animation(t.v);
  END RotateTriangle;

PROCEDURE RotateAngle (             t            : T;
                                    pivot, p1From: Id;
                                    p1To         : Id;
                                    p2From       : Id;
                       <* UNUSED *> p2To         : Id  ) 
    RAISES {Thread.Alerted} =
  VAR
    angle  := Stash(t, p1From, pivot, p2From);
    ang := RotationAngle(t, pivot, p1From, p1To);
  BEGIN
    MGV.AddAnimation(
      t.v, NEW(Animate.Rotate, origin := MGPublic.Pos(t.pts[pivot], t.v),
               angle := (180.0 / Math.Pi) * ang).init(), angle);
    MGV.Animation(t.v);
  END RotateAngle;

PROCEDURE HighlightLine (t: T; p1, p2: Id; color: INTEGER; show: BOOLEAN) 
    RAISES {Thread.Alerted} =
  VAR 
    line := NewLine(t, p1, p2, 2.0 * Weight, lineColor[color]);
  BEGIN
    AddToStash(t, line, p1, p2);
    IF show THEN MGV.Animation(t.v) END;
  END HighlightLine;

PROCEDURE AddToStash (t: T; mg: MG.T; p1, p2, p3, p4: Id := -1) =
  BEGIN
    FOR i := 0 TO LAST(t.stash) DO
      WITH e = t.stash[i] DO
        IF e.mg = NIL THEN
          e.p1 := p1;
          e.p2 := p2;
          e.p3 := p3;
          e.p4 := p4;
          e.mg := mg;
          RETURN
        END;
      END;
    END;
  END AddToStash;

PROCEDURE RemoveFromStash (t: T; p1, p2, p3, p4: Id := -1) =
  BEGIN
    FOR i := 0 TO LAST(t.stash) DO
      WITH e = t.stash[i] DO
        IF e.p1 = p1 AND e.p2 = p2 AND e.p3 = p3 AND e.p4 = p4 THEN
          t.v.displayList.remove(t.v, e.mg);
          e.mg := NIL;
          RETURN
        END;
      END;
    END;
  END RemoveFromStash;

PROCEDURE Stash (t: T; p1, p2, p3, p4: Id := -1): MG.T =
  BEGIN
    FOR i := 0 TO LAST(t.stash) DO
      WITH e = t.stash[i] DO
        IF e.p1 = p1 AND e.p2 = p2 AND e.p3 = p3 AND e.p4 = p4 THEN
          RETURN e.mg
        END;
      END;
    END;
    <* ASSERT FALSE *>
  END Stash;

PROCEDURE Angle (pt: R2.T): REAL =
  VAR
    l     := R2.Length(pt);
    angle := FLOAT(Math.acos(FLOAT(pt[0] / l, LONGREAL)));
  BEGIN
    IF pt[1] < 0.0 THEN angle := TwoPi - angle; END;
    RETURN angle;
  END Angle;

PROCEDURE HighlightAngle (t             : T;
                          p1, vertex, p2: Id;
                          value         : INTEGER;
                          show          : BOOLEAN  ) 
    RAISES {Thread.Alerted} =
  VAR
    pt1              := MGPublic.Pos(t.pts[p1], t.v);
    ptV              := MGPublic.Pos(t.pts[vertex], t.v);
    pt2              := MGPublic.Pos(t.pts[p2], t.v);
    path: R2Path.T;
    r   : REAL;
    v1               := R2.Sub(pt1, ptV);
  BEGIN
    path := NEW(R2Path.T);
    path.init();
    r := EScale;
    path.moveTo(R2.Add(ptV, R2.Scale(r / R2.Length(v1), v1)));
    path.arcTo(ptV, r, Angle(v1), Angle(R2.Sub(pt2, ptV)));
    AddToStash(t, NEW(MG.Shape, weight := 2.0 * Weight,
                      color := lineColor[value]).init(
                    R2.Origin, path, FALSE, v := t.v), p1, vertex, p2);
    IF show THEN MGV.Animation(t.v) END;
  END HighlightAngle;

PROCEDURE SetupTriangle (t: T; a, b: REAL) =
  VAR v := NEW(V, border := ARRAY Axis.T OF REAL{0.0, 0.0}).init();
  BEGIN
    LOCK VBT.mu DO EVAL Filter.Replace(t, NEW(ScaleFilter.T).init(v)) END;
    t.v := v;
    v.view := t;
    FOR i := 0 TO LAST(t.pts) DO
      t.pts[i] := NIL;
      FOR j := 0 TO LAST(t.lines[i]) DO t.lines[i, j] := NIL; END;
    END;
    FOR i := 0 TO LAST(t.stash) DO t.stash[i].mg := NIL END;

    t.a := a;
    t.b := b;

    InitPt(t, A);
    InitPt(t, B);
    InitPt(t, C);

    Line(t, A, B);
    Line(t, C, B);
    Line(t, A, C);
    t.v.mgRedisplay(Region.Full);
  END SetupTriangle;

PROCEDURE SetupSquare (t: T; p1, p2, p3, p4: Id) =
  BEGIN
    InitPt(t, p1);
    InitPt(t, p2);
    InitPt(t, p3);
    InitPt(t, p4);

    Line(t, p1, p2);
    Line(t, p2, p3);
    Line(t, p3, p4);
    Line(t, p4, p1);
    t.v.mgRedisplay(Region.Full);
  END SetupSquare;

PROCEDURE StartRun(view: T) =
  BEGIN
    EVAL Filter.Replace(view, NEW(V).init())
  END StartRun;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(V).init())
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Euclid View", "Euclid");
END EuclidView.
