(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Thu Jan  5 15:34:11 PST 1995 by najork *)
(*      modified on Thu Jan  7 14:46:10 PST 1993 by steveg *)
(*      modified on Sat Aug  8 00:18:54 PDT 1992 by mhb    *)
(*      modified on Wed Jul 29 23:17:19 PDT 1992 by johnh  *)
<* PRAGMA LL *>

MODULE PackingView1;

IMPORT BinpackViewClass, Color, Filter, Fmt, GraphVBT, 
       PaintOp,  R2, Thread, VBT, View, ZeusPanel;

TYPE
  T = BinpackViewClass.T BRANDED OBJECT
        W : INTEGER;            (* number of weights *)
        wt: REAL;               (* size of current weight *)
        current: GraphVBT.Vertex;  (* current weight *)
      OVERRIDES
        <* LL=0 *>
        oeSetup     := Setup;
        oeNewWeight := NewWeight;
        oePack      := Pack;
        oeIgnore    := Ignore;
      END;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(GraphVBT.T).init())
  END New;

PROCEDURE Setup (view: T; nBins, nWts: INTEGER) =
  BEGIN
    view.W := nWts;
    WITH mg = NEW(GraphVBT.T, 
                world := 
                  GraphVBT.WorldRectangle{
                      w := -2.0, s := 0.0, e := FLOAT(nBins), n := 1.0},
                pixelSizeDivisor :=
                  ARRAY [0 .. 1] OF CARDINAL{nBins + 2, 1},
                preferredSize := R2.T{FLOAT(2 + nBins) * 10.0, 100.0} ).init() DO
      LOCK VBT.mu DO EVAL Filter.Replace(view, mg) END
    END
  END Setup;

VAR font: GraphVBT.WorldFont;

PROCEDURE NewWeight (view: T; id: INTEGER; wt: REAL) =
  VAR
    mg := NARROW(Filter.Child(view), GraphVBT.T);
    rgb := Color.FromHSV(Color.HSV{FLOAT(id+1) / FLOAT(view.W), 1.0, 1.0});
    op  := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  BEGIN
    IF font = NIL THEN
      font := mg.font(family := "Helvetica", weight := "Bold", slant := GraphVBT.Slant.Roman, size := 0.4);
    END;
    view.wt := wt;
    view.current := NEW(GraphVBT.Vertex, 
                     graph       := mg,
                     pos         := R2.T{-1.0, 0.5},
                     color       := op,
                     size        := R2.T{1.0, wt}, 
                     border      := 0.0025,
                     borderColor := PaintOp.Fg,
                     label       := Fmt.Int(id),
                     font        := font,
                     fontColor   := PaintOp.Fg).init();
    mg.redisplay()
  END NewWeight;

PROCEDURE Pack (view: T; bin: INTEGER; total: REAL) RAISES {Thread.Alerted} =
  VAR mg := NARROW(Filter.Child(view), GraphVBT.T);
  BEGIN
    LOCK mg.mu DO
      view.current.move(
        R2.T{0.5 + FLOAT(bin), total - view.wt / 2.0},
        animated := TRUE)
    END;
    mg.animate(0.0, 1.0);
  END Pack;

PROCEDURE Ignore (view: T) =
  VAR mg := NARROW(Filter.Child(view), GraphVBT.T);
  BEGIN
    LOCK mg.mu DO view.current.remove() END;
    mg.redisplay()
  END Ignore;

BEGIN
  ZeusPanel.RegisterView (New, "Packing Simple", "Binpack");
END PackingView1.


