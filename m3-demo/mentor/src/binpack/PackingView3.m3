(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Thu Jan  5 15:36:05 PST 1995 by najork *)
(*      modified on Wed Jan  6 11:25:12 PST 1993 by steveg *)
(*      modified on Sat Aug  8 00:17:54 PDT 1992 by mhb    *)
(*      modified on Wed Jul 29 23:17:19 PDT 1992 by johnh  *)
<* PRAGMA LL *>

MODULE PackingView3;

IMPORT AlgFFSimple, AnimationPath, ColorName, Filter, GraphVBT, PackingView2,
       PaintOp, R2, Thread, View, ZeusClass, ZeusPanel;

REVEAL
  T = PackingView2.T BRANDED OBJECT
      OVERRIDES
        <* LL=0 *>
        oeSetup      := Setup;
        oePack       := Pack;
        oeProbe      := Probe;
        createGraph  := CreateGraph;
        createWeight := CreateWeight;
        isCompat     := IsCompat;
      END;

TYPE
  Weight = PackingView2.Weight;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(GraphVBT.T).init())
  END New;

PROCEDURE Setup (view: T; nBins, nWts: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    PackingView2.T.oeSetup(view, nBins, nWts);
    VAR
      mg: GraphVBT.T := Filter.Child(view);
      v0 := NEW(
              GraphVBT.Vertex, graph := mg,
              pos := R2.T{-100.0, 1.0}, size := R2.T{0.0, 0.0}).init();
      v1 := NEW(GraphVBT.Vertex, graph := mg,
                pos := R2.T{2.0 * FLOAT(nBins), 1.0},
                size := R2.T{0.0, 0.0}).init();
    BEGIN
      EVAL
        NEW(GraphVBT.Edge, vertex0 := v0, vertex1 := v1).init();
      mg.redisplay();
    END
  END Setup;

PROCEDURE Probe (view: T; bin: INTEGER; total: REAL) RAISES {Thread.Alerted} =
  VAR mg: GraphVBT.T := Filter.Child(view);
  BEGIN
    LOCK mg.mu DO
      WITH start = view.curr.pos,
           end = R2.T{
                   0.5 + FLOAT(bin), total + view.curr.amt / 2.0},
           p = NEW(AnimationPath.BezierPath).init(
                 p0 := start,
                 p1 := R2.T{start[0] + (end[0] - start[0]) / 3.0,
                            start[1] + 0.25},
                 p2 := R2.T{end[0] - (end[0] - start[0]) / 3.0,
                            end[1] + 0.25}, p3 := end) DO
        view.curr.move(end, path := p, animated := TRUE)
      END
    END;
    mg.animate(0.0, 1.0)
  END Probe;

PROCEDURE Pack (             view : T;
                <* UNUSED *> bin  : INTEGER;
                <* UNUSED *> total: REAL     ) =
  VAR mg: GraphVBT.T := Filter.Child(view);
  BEGIN
    LOCK mg.mu DO
      WITH w = view.curr DO
        w.setColor(w.borderColor);
        w.setBorderColor(PaintOp.Fg);
      END
    END;
    mg.redisplay()
  END Pack;

PROCEDURE CreateGraph (<* UNUSED *> view : T;
                                    nBins: INTEGER;
                       <* UNUSED *> nWts : INTEGER  ):
  GraphVBT.T =
  BEGIN
    RETURN NEW(GraphVBT.T, world := GraphVBT.WorldRectangle{
                                      w := -2.0, s := 0.0, e :=
                                      FLOAT(nBins), n := 2.0},
               pixelSizeDivisor :=
                 ARRAY [0 .. 1] OF CARDINAL{nBins + 2, 1}).init()
  END CreateGraph;

PROCEDURE CreateWeight (view: T; id: INTEGER; amt: REAL): Weight =
  VAR w := PackingView2.T.createWeight(view, id, amt);
  <* FATAL ColorName.NotFound *>
  BEGIN
    LOCK w.graph.mu DO
      w.setBorderColor(w.color);
      w.move(R2.T{-1.0, 0.5}, animated := FALSE);
      WITH rgb = ColorName.ToRGB("LightGray") DO
        w.setColor(PaintOp.FromRGB(rgb.r, rgb.g, rgb.b));
      END;
    END;
    RETURN w
  END CreateWeight;


PROCEDURE IsCompat (<* UNUSED *> self : T; 
                                 alg  : ZeusClass.T): BOOLEAN =
  BEGIN
    RETURN NOT ISTYPE (alg, AlgFFSimple.T);
  END IsCompat;


BEGIN
  ZeusPanel.RegisterView (New, "Packing with Probes", "Binpack");
END PackingView3.


