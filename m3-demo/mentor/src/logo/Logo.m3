(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

MODULE Logo;

IMPORT Algorithm, Animate, Axis, Color, Filter, FormsVBT, GraphVBT,
       GraphVBTExtras, logoAlgClass, logoIE, logoViewClass, MG,
       MGPaintOp, MGV, PaintOp, R2, Random, Text, Thread, VBT, View,
       ZeusPanel;

TYPE
  T = logoViewClass.T OBJECT
        v         : GraphVBT.T;
        colors    : REF ARRAY OF PaintOp.T;
        colorPhase                                := 0;
      OVERRIDES
        startrun       := StartRunV;
        oeInit         := InitV;
        oeRandomStep   := RandomStepV;
        oeShift        := ShiftV;
        oeMarquee      := MarqueeV;
        oeMarqueeSpots := MarqueeSpotsV;
      END;

  T2 = T OBJECT
       OVERRIDES
         oeRandomStep := RandomStep2;
       END;

VAR
  rand := NEW (Random.Default).init ();

PROCEDURE RandomStep2 (view: T2; delta: REAL) RAISES {Thread.Alerted} =
  BEGIN
    T.oeRandomStep(view, delta);
(*    LOCK VBT.mu DO ZeusPanel.PhotographViews(NIL) END; *)
  END RandomStep2;

TYPE
  MyVertex = GraphVBT.Vertex OBJECT pref: R2.T;  END;
  Spots = GraphVBT.Vertex OBJECT
            sap: SpotAP;
            p  : REAL
          END;

  MyGraphVBT = GraphVBT.T OBJECT
                 marquee           : ARRAY [0 .. 3] OF GraphVBT.Edge;
                 mlengths          : ARRAY Axis.T OF REAL;
                 mcorner0, mcorner1: R2.T;
                 mspots            : ARRAY [0 .. 30] OF Spots;
                 mspotSize         : REAL;
                 spotShifts         := 0.0;
                 spotDelta : REAL;
                 text: REF ARRAY OF Glyph;
               OVERRIDES
                 reshape := ReshapeGraphVBT;
               END;

PROCEDURE ReshapeGraphVBT (v: MyGraphVBT; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    GraphVBT.T.reshape(v, cd);
    v.redisplay();
  END ReshapeGraphVBT;

CONST
  YFloor = 0.4;
  Scale  = 3.0 / 4.0;
  Width  = 0.02;

(* num is the position number of the letter, x, y are the coordinates
   assuming a 1, 1 glyph box *)
PROCEDURE RefPt (v: MyGraphVBT; n: INTEGER; p: R2.T): R2.T =
  VAR
    len := FLOAT(MAX(1, NUMBER(v.text^)));
    xs  := 1.0 / (len + 2.0);
    ys  := 1.5 * xs;
  BEGIN
    RETURN R2.T{(1.0 + FLOAT(n) + p[0] * Scale) * xs,
                1.0 - (YFloor + p[1] * Scale * ys)};
  END RefPt;

(* num is the position number of the letter, x, y are the coordinates
   assuming a 1, 1 box *)
PROCEDURE NewVertex (v: GraphVBT.T; num: INTEGER; p: R2.T): GraphVBT.Vertex =
  BEGIN
    RETURN
      NEW(MyVertex, graph := v, shape := GraphVBT.VertexShape.Rectangle,
          pos := R2.T{rand.real(), rand.real()},
          pref := RefPt(v, num, p), color := PaintOp.Bg).init();
  END NewVertex;

PROCEDURE Edge (v: MyGraphVBT; num: INTEGER; v0, v1, c0, c1: R2.T):
  GraphVBT.Edge =
  BEGIN
    RETURN
      NEW(
        GraphVBT.Edge, vertex0 := NewVertex(v, num, v0),
        vertex1 := NewVertex(v, num, v1),
        control0 := NewVertex(v, num, c0),
        control1 := NewVertex(v, num, c1), width := Width,
        color := NARROW(GraphVBTExtras.GetMG(v).view, T).colors[num]).init();
  END Edge;

PROCEDURE RandomRange (current, pref, range: REAL): REAL =
  BEGIN
    IF ABS(current - pref) > 2.0 * range THEN
      RETURN pref
    ELSE
      VAR
        min := MIN(current, pref);
        max := MAX(current, pref);
        lo  := MAX(0.0, max - range);
        hi  := MIN(1.0, min + range);
      BEGIN
        RETURN lo + rand.real() * (hi - lo);
      END;
    END;
  END RandomRange;

PROCEDURE RandomPos (v: MyVertex; delta: REAL): R2.T =
  BEGIN
    RETURN R2.T{RandomRange(v.pos[0], v.pref[0], delta),
                RandomRange(v.pos[1], v.pref[1], delta)}
  END RandomPos;

PROCEDURE RandomMoveEdge (<* UNUSED *> v    : GraphVBT.T;
                                 e    : GraphVBT.Edge;
                                 delta: REAL           ) =
  VAR v0, v1, c0, c1: MyVertex;
  BEGIN
    v0 := e.vertex0;
    v1 := e.vertex1;
    c0 := e.control0;
    c1 := e.control1;
    v0.move(RandomPos(v0, delta), TRUE);
    v1.move(RandomPos(v1, delta), TRUE);
    IF delta = 0.0 AND R2.Equal(v0.pref, c0.pref) THEN
      e.move(v0, v1, NIL, NIL, TRUE)
    ELSE
      c0.move(RandomPos(c0, delta), TRUE);
      c1.move(RandomPos(c1, delta), TRUE);
    END;
  END RandomMoveEdge;

PROCEDURE RandomMoveEdges (         v    : MyGraphVBT;
                           READONLY a    : ARRAY OF GraphVBT.Edge;
                                    delta: REAL                    ) =
  BEGIN
    FOR i := 0 TO LAST(a) DO RandomMoveEdge(v, a[i], delta) END;
  END RandomMoveEdges;

PROCEDURE RandomMove (v: MyGraphVBT; delta: REAL) =
  BEGIN
    FOR i := 0 TO LAST(v.text^) DO
      IF v.text[i] # NIL THEN RandomMoveEdges(v, v.text[i]^, delta); END;
    END;
  END RandomMove;

TYPE
  ColorAnimation = Animate.T OBJECT
    op: PaintOp.T;
    hsvStart, hsvEnd: Color.HSV;
  OVERRIDES
    length := CALength;
    doStep := CADoStep;
  END;

PROCEDURE CALength (<* UNUSED *> ca: ColorAnimation;
                    <* UNUSED *> v : MG.V;
                    <* UNUSED *> mg: MG.T            ): INTEGER =
  BEGIN
    RETURN 30
  END CALength;

PROCEDURE ScaleHSV (start, end: Color.HSV; time: REAL): Color.HSV =
  PROCEDURE Scale (s, e, t: REAL): REAL =
    BEGIN
      RETURN s + t * (e - s)
    END Scale;
  BEGIN
    RETURN Color.HSV{Scale(start.h, end.h, time), Scale(start.s, end.s, time),
                 Scale(start.v, end.v, time)}
  END ScaleHSV;

PROCEDURE CADoStep (             ca      : ColorAnimation;
                                 time    : REAL;
                    <* UNUSED *> timePrev: REAL;
                                 v       : MG.V;
                    <* UNUSED *> mg      : MG.T            ) =
  VAR rgb := Color.FromHSV(ScaleHSV(ca.hsvStart, ca.hsvEnd, time));
  BEGIN
    MGPaintOp.Set(
      VBT.ScreenTypeOf(v), ca.op, MGPaintOp.RGB{rgb.r, rgb.g, rgb.b});
  END CADoStep;

PROCEDURE NewRandomColor (view: T; op: PaintOp.T; i: CARDINAL; delta: REAL) =
  VAR
    rgb := MGPaintOp.Get(op);
    hue := FLOAT(i) / 6.0;
  BEGIN
    MGV.AddAnimation(
      GraphVBTExtras.GetMG(view.v), NEW(ColorAnimation, op := op,
                  hsvStart := Color.ToHSV(rgb),
                  hsvEnd := Color.HSV{RandomRange(hue, hue, delta),
                                  RandomRange(1.0, 1.0, delta),
                                  RandomRange(0.5, 0.5, delta)}).init(),
      NIL);
  END NewRandomColor;

PROCEDURE RandomColor(v: MyGraphVBT; delta: REAL) =
  VAR view: T := GraphVBTExtras.GetMG(v).view;
  BEGIN
    FOR i := 0 TO LAST(v.text^) DO
      NewRandomColor(view, view.colors[i], i, delta)
    END;
  END RandomColor;

TYPE
  SpotAP = GraphVBT.AnimationPath OBJECT
    spot: Spots;
  OVERRIDES
    pos := SpotAPos;
  END;

PROCEDURE SpotAPos (sap: SpotAP; t: REAL): R2.T =
  BEGIN
    RETURN SpotPos(
             sap.spot.graph,
             sap.spot.p + t * NARROW(sap.spot.graph, MyGraphVBT).spotDelta)
  END SpotAPos;

PROCEDURE ShiftV (view: T) RAISES {Thread.Alerted} =
  VAR v: MyGraphVBT := view.v;
  BEGIN
    FOR i := 0 TO LAST(v.text^) DO
      VAR
        iStart := (i + view.colorPhase) MOD 6;
        iEnd   := (iStart + 1);
      BEGIN
        MGV.AddAnimation(
          GraphVBTExtras.GetMG(v), NEW(ColorAnimation, op := view.colors[i],
                 hsvStart := Color.HSV{FLOAT(iStart) / 6.0, 1.0, 0.5},
                 hsvEnd := Color.HSV{FLOAT(iEnd) / 6.0, 1.0, 0.5}).init(), NIL);
      END;
    END;
    INC(view.colorPhase);

    FOR i := 0 TO LAST(v.mspots) DO
      WITH spot = v.mspots[i] DO
        spot.move (SpotPos(v, spot.p + v.spotDelta), TRUE, path := spot.sap);
      END;
    END;
    view.v.animate(0.0, 1.0);
    v.spotShifts := v.spotShifts + v.spotDelta;
  END ShiftV;

(* We want spots to just cover the line.  Setting the box's center 1/2 spot
   width out from the line, puts the inner edge of the box right over the
   middle of the line.  So, have to get the line's width in world
   coordinates (from MM) and then adjust the box accordingly *)
PROCEDURE SpotPos (v: MyGraphVBT; p: REAL): R2.T =
  VAR
    c0     := v.mcorner0;
    c1     := v.mcorner1;
    hor    := v.mlengths[Axis.T.Hor];
    ver    := v.mlengths[Axis.T.Ver];
    circum := 2.0 * (hor + ver);
  BEGIN
    p := (p + v.spotShifts) MOD circum;

    IF p <= hor THEN
      RETURN R2.Add(c0, R2.T{p, 0.0})
    ELSIF p <= hor + ver THEN
      RETURN R2.Add(c1, R2.T{0.0, (hor + ver) - p})
    ELSIF p <= hor + hor + ver THEN
      RETURN R2.Add(c1, R2.T{(hor + ver - p), 0.0})
    ELSE
      RETURN R2.Add(c0, R2.T{0.0, p - circum})
    END;
  END SpotPos;

PROCEDURE MarqueeV (view: T; side: INTEGER) RAISES {Thread.Alerted} =
  VAR
    v         : MyGraphVBT := view.v;
    start, end: R2.T; 
    MDelta := MIN(0.1, 1.0 / FLOAT(LAST(v.text^) + 4));
 BEGIN
    CASE side OF
    | 0 =>
        start := R2.Add(RefPt(v, 0, P0), R2.T{-MDelta, +MDelta});
        end := R2.Add(RefPt(v, LAST(v.text^), P6), R2.T{MDelta, +MDelta});
    | 1 =>
        start :=
          R2.Add(RefPt(v, LAST(v.text^), P6), R2.T{MDelta, +MDelta});
        end := R2.Add(RefPt(v, LAST(v.text^), P4), R2.T{MDelta, -MDelta});
    | 2 =>
        start :=
          R2.Add(RefPt(v, LAST(v.text^), P4), R2.T{MDelta, -MDelta});
        end := R2.Add(RefPt(v, 0, P2), R2.T{-MDelta, -MDelta});
    | 3 =>
        start := R2.Add(RefPt(v, 0, P2), R2.T{-MDelta, -MDelta});
        end := R2.Add(RefPt(v, 0, P0), R2.T{-MDelta, +MDelta});
    ELSE
    END;
    v.marquee[side] :=
      NEW(GraphVBT.Edge, width := Width,
          vertex0 :=
            NEW(MyVertex, graph := v,
                shape := GraphVBT.VertexShape.Rectangle, pos := start,
                pref := start, size := R2.Origin).init(),
          vertex1 :=
            NEW(MyVertex, graph := v,
                shape := GraphVBT.VertexShape.Rectangle, pos := start,
                pref := end, size := R2.Origin).init()).init();
    LOCK v.mu DO v.marquee[side].vertex1.move(end, TRUE) END;
    v.animate(0.0, 1.0);
  END MarqueeV;

PROCEDURE MarqueeSpotsV (view: T) =
  VAR
    v               : MyGraphVBT := view.v;
    length, delta, p: REAL;
    MDelta := MIN(0.1, 1.0 / FLOAT(LAST(v.text^) + 4));
    c0 := R2.Add(RefPt(v, 0, P0), R2.T{-MDelta, +MDelta});
    c1 := R2.Add(RefPt(v, LAST(v.text^), P4), R2.T{+MDelta, -MDelta});
  BEGIN
    FOR i := 0 TO 3 DO v.marquee[i].setColor(PaintOp.Bg); END;
    v.mcorner0 := c0;
    v.mcorner1 := c1;
    v.mlengths[Axis.T.Hor] := ABS(c1[0] - c0[0]);
    v.mlengths[Axis.T.Ver] := ABS(c1[1] - c0[1]);
    length := 2.0 * (v.mlengths[Axis.T.Hor] + v.mlengths[Axis.T.Ver]);
    delta := length / FLOAT(NUMBER(v.mspots));
    v.mspotSize := 1.0 * delta / 2.0;
    v.spotDelta := delta;
    p := 0.0;
    FOR i := 0 TO LAST(v.mspots) DO
      v.mspots[i] := NEW(Spots, graph := v, p := p,
                         shape := GraphVBT.VertexShape.Rectangle,
                         size := R2.T{v.mspotSize, v.mspotSize},
                         pos := SpotPos(v, p), color := PaintOp.Fg).init();
      v.mspots[i].sap := NEW(SpotAP, spot := v.mspots[i]);
      p := p + delta;
    END;
  END MarqueeSpotsV;

CONST
  P0 = R2.T{0.0, 0.0};
  P1 = R2.T{0.0, 0.5};
  P2 = R2.T{0.0, 1.0};
  P3 = R2.T{0.5, 1.0};
  P4 = R2.T{1.0, 1.0};
  P5 = R2.T{1.0, 0.5};
  P6 = R2.T{1.0, 0.0};
  P7 = R2.T{0.5, 0.0};
  P8 = R2.T{0.5, 0.5};
  P9 = R2.T{1.0, 0.75};
  P10 = R2.T{1.0, 0.25};
  P11 = R2.T{0.25, 0.5};
  P12 = R2.T{0.75, 0.5};
  P13 = R2.T{0.75, 0.25};
  P14 = R2.T{0.25, 0.0};
  (*P15 = R2.T{0.75, 1.0};*)
  P16 = R2.T{0.75, 0.0};
  P17 = R2.T{0.0, 0.75};
  P18 = R2.T{0.0, 0.25};

TYPE
  Glyph = REF ARRAY OF GraphVBT.Edge;
  OEdge = RECORD v0, v1, c0, c1: R2.T END;
  Outline = REF ARRAY OF OEdge;

VAR
  font: ARRAY CHAR OF Outline;

PROCEDURE InitFont () =
  VAR NullOutline := NEW(Outline, 0);
  BEGIN
    FOR i := FIRST(font) TO LAST(font) DO
      font[i] := NullOutline;
    END;
    font['A'] := NEW(Outline, 3);
    font['A'][0] := OEdge{P0, P3, P0, P3};
    font['A'][1] := OEdge{P6, P3, P6, P3};
    font['A'][2] := OEdge{P11, P12, P11, P12};
    font['B'] := NEW(Outline, 5);
    font['B'][0] := OEdge{P0, P2, P0, P2};
    font['B'][1] := OEdge{P2, P9, P4, P4};
    font['B'][2] := OEdge{P9, P1, P5, P5};
    font['B'][3] := OEdge{P1, P10, P5, P5};
    font['B'][4] := OEdge{P10, P0, P6, P6};
    font['C'] := NEW(Outline, 4);
    font['C'][0] := OEdge{P1, P3, P2, P2};
    font['C'][1] := OEdge{P3, P9, P4, P4};
    font['C'][2] := OEdge{P10, P7, P6, P6};
    font['C'][3] := OEdge{P7, P1, P0, P0};
    font['D'] := NEW(Outline, 3);
    font['D'][0] := OEdge{P0, P2, P0, P2};
    font['D'][1] := OEdge{P2, P5, P4, P4};
    font['D'][2] := OEdge{P5, P0, P6, P6};
    font['E'] := NEW(Outline, 4);
    font['E'][0] := OEdge{P6, P0, P6, P0};
    font['E'][1] := OEdge{P0, P2, P0, P2};
    font['E'][2] := OEdge{P2, P4, P2, P4};
    font['E'][3] := OEdge{P1, P5, P1, P5};
    font['F'] := NEW(Outline, 3);
    font['F'][0] := OEdge{P4, P2, P4, P2};
    font['F'][1] := OEdge{P0, P2, P0, P2};
    font['F'][2] := OEdge{P1, P5, P1, P5};
    font['G'] := NEW(Outline, 5);
    font['G'][0] := OEdge{P1, P3, P2, P2};
    font['G'][1] := OEdge{P3, P9, P4, P4};
    font['G'][2] := OEdge{P5, P7, P6, P6};
    font['G'][3] := OEdge{P7, P1, P0, P0};
    font['G'][4] := OEdge{P5, P12, P5, P12};
    font['H'] := NEW(Outline, 3);
    font['H'][0] := OEdge{P0, P2, P0, P2};
    font['H'][1] := OEdge{P4, P6, P4, P6};
    font['H'][2] := OEdge{P1, P5, P1, P5};
    font['I'] := NEW(Outline, 1);
    font['I'][0] := OEdge{P7, P3, P7, P3};
    font['J'] := NEW(Outline, 3);
    font['J'][0] := OEdge{P4, P5, P4, P5};
    font['J'][1] := OEdge{P5, P7, P6, P6};
    font['J'][2] := OEdge{P7, P1, P0, P0};
    font['K'] := NEW(Outline, 3);
    font['K'][0] := OEdge{P0, P2, P0, P2};
    font['K'][1] := OEdge{P1, P4, P1, P4};
    font['K'][2] := OEdge{P1, P6, P1, P6};
    font['L'] := NEW(Outline, 2);
    font['L'][0] := OEdge{P0, P2, P0, P2};
    font['L'][1] := OEdge{P0, P6, P0, P6};
    font['M'] := NEW(Outline, 4);
    font['M'][0] := OEdge{P0, P2, P0, P2};
    font['M'][1] := OEdge{P2, P8, P2, P8};
    font['M'][2] := OEdge{P8, P4, P8, P4};
    font['M'][3] := OEdge{P4, P6, P4, P6};
    font['N'] := NEW(Outline, 3);
    font['N'][0] := OEdge{P0, P2, P0, P2};
    font['N'][1] := OEdge{P2, P6, P2, P6};
    font['N'][2] := OEdge{P6, P4, P6, P4};
    font['O'] := NEW(Outline, 4);
    font['O'][0] := OEdge{P1, P3, P2, P2};
    font['O'][1] := OEdge{P3, P5, P4, P4};
    font['O'][2] := OEdge{P5, P7, P6, P6};
    font['O'][3] := OEdge{P7, P1, P0, P0};
    font['P'] := NEW(Outline, 3);
    font['P'][0] := OEdge{P0, P2, P0, P2};
    font['P'][1] := OEdge{P9, P1, P5, P5};
    font['P'][2] := OEdge{P2, P9, P4, P4};
    font['Q'] := NEW(Outline, 5);
    font['Q'][0] := OEdge{P1, P3, P2, P2};
    font['Q'][1] := OEdge{P3, P5, P4, P4};
    font['Q'][2] := OEdge{P5, P7, P6, P6};
    font['Q'][3] := OEdge{P7, P1, P0, P0};
    font['Q'][4] := OEdge{P13, P6, P13, P6};
    font['R'] := NEW(Outline, 4);
    font['R'][0] := OEdge{P0, P2, P0, P2};
    font['R'][1] := OEdge{P1, P6, P1, P6};
    font['R'][2] := OEdge{P2, P9, P4, P4};
    font['R'][3] := OEdge{P9, P1, P5, P5};
    font['S'] := NEW(Outline, 6);
    font['S'][0] := OEdge{P9, P3, P4, P4};
    font['S'][1] := OEdge{P3, P17, P2, P2};
    font['S'][2] := OEdge{P17, P8, P1, P1};
    font['S'][3] := OEdge{P8, P10, P5, P5};
    font['S'][4] := OEdge{P10, P7, P6, P6};
    font['S'][5] := OEdge{P7, P18, P0, P0};
    font['T'] := NEW(Outline, 2);
    font['T'][0] := OEdge{P7, P3, P7, P3};
    font['T'][1] := OEdge{P2, P4, P2, P4};
    font['U'] := NEW(Outline, 4);
    font['U'][0] := OEdge{P2, P1, P2, P1};
    font['U'][1] := OEdge{P1, P7, P0, P0};
    font['U'][2] := OEdge{P7, P5, P6, P6};
    font['U'][3] := OEdge{P5, P4, P5, P4};
    font['V'] := NEW(Outline, 2);
    font['V'][0] := OEdge{P7, P2, P7, P2};
    font['V'][1] := OEdge{P7, P4, P7, P4};
    font['W'] := NEW(Outline, 4);
    font['W'][0] := OEdge{P2, P14, P2, P14};
    font['W'][1] := OEdge{P14, P8, P14, P8};
    font['W'][2] := OEdge{P16, P8, P16, P8};
    font['W'][3] := OEdge{P16, P4, P16, P4};
    font['X'] := NEW(Outline, 2);
    font['X'][0] := OEdge{P6, P2, P6, P2};
    font['X'][1] := OEdge{P0, P4, P0, P4};
    font['Y'] := NEW(Outline, 3);
    font['Y'][0] := OEdge{P7, P8, P7, P8};
    font['Y'][1] := OEdge{P8, P2, P8, P2};
    font['Y'][2] := OEdge{P8, P4, P8, P4};
    font['Z'] := NEW(Outline, 3);
    font['Z'][0] := OEdge{P2, P4, P2, P4};
    font['Z'][1] := OEdge{P4, P0, P4, P0};
    font['Z'][2] := OEdge{P0, P6, P0, P6};
    FOR i := 'a' TO 'z' DO
      font[i] := font[VAL(ORD(i) - ORD('a') + ORD('A'), CHAR)]
    END;
  END InitFont;

PROCEDURE New (): View.T =
  VAR view := NEW(T);
  BEGIN
    view.v := NEW(MyGraphVBT).init();
    RETURN view.init(view.v);
  END New;

PROCEDURE NewWAlbum (): View.T =
  VAR view := NEW(T2);
  BEGIN
    view.v := NEW(MyGraphVBT).init();
    RETURN view.init(view.v);
  END NewWAlbum;

PROCEDURE StartRunV (view: T) =
  BEGIN
    view.v := NEW(MyGraphVBT).init();
    GraphVBTExtras.GetMG(view.v).view := view;
    EVAL Filter.Replace(view, view.v);
  END StartRunV;

PROCEDURE AdjustGlyph (v: MyGraphVBT; outline: Outline; num: INTEGER):
  Glyph =
  VAR
    glyph := NEW(Glyph, NUMBER(outline^));
  BEGIN
    FOR i := 0 TO LAST(outline^) DO
      glyph[i] := Edge(v, num, outline[i].v0, outline[i].v1, outline[i].c0,
                       outline[i].c1);
    END;
    RETURN glyph;
  END AdjustGlyph;

PROCEDURE InitText(v: MyGraphVBT; text: TEXT) =
  BEGIN
    v.text := NEW(REF ARRAY OF Glyph, Text.Length(text));
    FOR i := 0 TO LAST(v.text^) DO
      v.text[i] := AdjustGlyph(v, font[Text.GetChar(text, i)], i);
    END;
  END InitText;

PROCEDURE InitV (view: T; text: TEXT) RAISES {Thread.Alerted} =
  VAR v: MyGraphVBT := view.v;
  BEGIN
    LOCK v.mu DO v.clear() END;
    InitFont();
    view.colors := NEW(REF ARRAY OF PaintOp.T, Text.Length(text));
    FOR i := 0 TO LAST(view.colors^) DO
      view.colors[i] :=
        MGPaintOp.New(
          MGPaintOp.RGB{rand.real(), rand.real(), rand.real()});
    END;
    InitText(view.v, text);
    view.oeRandomStep(0.5);
  END InitV;

PROCEDURE RandomStepV (view: T; delta: REAL) RAISES {Thread.Alerted} =
  BEGIN
    LOCK view.v.mu DO RandomMove(view.v, delta); END;
    RandomColor(view.v, delta);
    view.v.animate(0.0, 1.0);
  END RandomStepV;

TYPE
  Alg = logoAlgClass.T OBJECT 
  OVERRIDES
    run := AlgRun
  END;

PROCEDURE NewA (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("logoinput.fv");
  BEGIN
    RETURN NEW(Alg, data := fv).init()
  END NewA;

PROCEDURE AlgRun (alg: Alg) RAISES {Thread.Alerted} =
  VAR
    delta: REAL;
    text: TEXT;                 <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    LOCK VBT.mu DO text := FormsVBT.GetText(alg.data, "text"); END;
    LOOP
      logoIE.Init(alg, text);
      delta := 0.25;
      WHILE delta > 0.01 DO
        logoIE.RandomStep(alg, delta);
        delta := delta - 0.02;
      END;
      logoIE.RandomStep(alg, 0.0);
      FOR i := 0 TO 3 DO logoIE.Marquee(alg, i); END;
      logoIE.MarqueeSpots(alg);
      FOR i := 1 TO 15 DO logoIE.Shift(alg) END
    END;
  END AlgRun;

BEGIN
  ZeusPanel.RegisterAlg(NewA, "Mentor Demo", "logo");
  ZeusPanel.RegisterView(New, "Mentor Demo", "logo");
  ZeusPanel.RegisterView(NewWAlbum, "Mentor Demo w/ album", "logo");
END Logo.
