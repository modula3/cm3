(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Thu Jan  5 15:35:05 PST 1995 by najork *)
(*      modified on Fri Jul  9 00:32:53 PDT 1993 by mhb    *)
(*      modified on Thu Jan  7 11:26:30 PST 1993 by steveg *)
(*      modified on Wed Jul 29 23:17:19 PDT 1992 by johnh  *)
<* PRAGMA LL *>

MODULE PackingView2;

IMPORT Color, Filter, Fmt, GraphVBT, IntList, PaintOp,
       R2, RealList, Thread, VBT, View, ZeusPanel;

REVEAL
  T = Public BRANDED OBJECT
        W: INTEGER;             (* number of weights *)
        weights: REF ARRAY OF Weight;  (* the weights *)
      OVERRIDES
        <* LL=0 *>
        oeSetup      := Setup;
        oeNewWeight  := NewWeight;
        oePack       := Pack;
        oeIgnore     := Ignore;
        createGraph  := CreateGraph;
        createWeight := CreateWeight;
        <* LL=VBT.mu *>
        ueRepackBin := RepackBin;
      END;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(GraphVBT.T).init())
  END New;

PROCEDURE Setup (view: T; nBins, nWts: INTEGER) =
  BEGIN
    view.W := nWts;
    view.weights := NEW(REF ARRAY OF Weight, nWts);
    WITH mg = view.createGraph(nBins, nWts) DO
      LOCK VBT.mu DO EVAL Filter.Replace(view, mg) END
    END
  END Setup;

PROCEDURE NewWeight (view: T; id: INTEGER; wt: REAL) =
  VAR mg: GraphVBT.T := Filter.Child(view);
  BEGIN
    view.curr := view.createWeight(id, wt);
    view.weights[id] := view.curr;
    mg.redisplay()
  END NewWeight;

PROCEDURE Pack (view: T; bin: INTEGER; total: REAL) =
  VAR mg: GraphVBT.T := Filter.Child(view);
  BEGIN
    LOCK mg.mu DO
      view.curr.move(
        R2.T{0.5 + FLOAT(bin), total - view.curr.amt / 2.0},
        animated := FALSE)
    END;
    mg.redisplay()
  END Pack;

PROCEDURE Ignore (view: T) =
  VAR mg: GraphVBT.T := Filter.Child(view);
  BEGIN
    LOCK mg.mu DO view.curr.remove() END;
    mg.redisplay()
  END Ignore;

PROCEDURE RepackBin (view    : T;
                     bin     : INTEGER;
                     old, new: IntList.T;
                     amts    : RealList.T ) RAISES {Thread.Alerted} =
  <* LL = VBT.mu *>
  VAR
    mg   : GraphVBT.T := Filter.Child(view);
    total: REAL       := 0.0;
    o, n : IntList.T;
    a    : RealList.T;
  BEGIN
    o := old;
    WHILE o # NIL DO
      IF NOT IntList.Member(new, o.head) THEN
        WITH id  = o.head,
             bar = NARROW(view.weights[id], Weight) DO
          bar.remove();
          view.weights[id] := NIL;
        END
      END;
      o := o.tail
    END;
    n := new;
    a := amts;
    WHILE n # NIL DO
      WITH id  = n.head,
           amt = a.head,
           bar = NARROW(view.weights[id], Weight) DO
        total := total + amt;
        bar.move(R2.T{0.5 + FLOAT(bin), total - amt / 2.0},
                 animated := TRUE)
      END;
      n := n.tail;
      a := a.tail
    END;
    mg.animate (0.0, 1.0);
  END RepackBin;

VAR
  font: GraphVBT.WorldFont;

PROCEDURE CreateGraph (<* UNUSED *> view : T;
                                    nBins: INTEGER;
                       <* UNUSED *> nWts : INTEGER  ):
  GraphVBT.T =
  VAR
    graph := NEW(GraphVBT.T, world := GraphVBT.WorldRectangle{
                                        w := 0.0, s := 0.0, e :=
                                        FLOAT(nBins), n := 1.0},
                 pixelSizeDivisor :=
                   ARRAY [0 .. 1] OF CARDINAL{nBins, 1}).init();
  BEGIN
    font :=
      graph.font(family := "Helvetica", weight := "bold",
                 slant := GraphVBT.Slant.Roman, size := 0.05);
    RETURN graph;
  END CreateGraph;

PROCEDURE CreateWeight (view: T; id: INTEGER; amt: REAL): Weight =
  VAR
    mg  := NARROW(Filter.Child(view), GraphVBT.T);
    rgb := Color.FromHSV(Color.HSV{FLOAT(id + 1) / FLOAT(view.W), 1.0, 1.0});
    op  := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  BEGIN 
    RETURN NEW(Weight, 
                     id          := id, 
                     amt         := amt,
                     graph       := mg,
                     pos         := R2.T{-100.0,-100.0},
                     color       := op,
                     size        := R2.T{1.0, amt},
                     border      := 0.0025,
                     borderColor := PaintOp.Fg,
                     label       := Fmt.Int(id),
                     font        := font,
                     fontColor   := PaintOp.Fg).init()
  END CreateWeight;

BEGIN
  ZeusPanel.RegisterView (New, "Packing", "Binpack");
END PackingView2.


