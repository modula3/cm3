(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Wed May  4 09:50:53 PDT 1994 by najork   *)
(*      modified on Wed Jan  6 15:34:16 PST 1993 by steveg   *)
(*      modified on Wed Aug  5 11:36:26 PDT 1992 by karsenty *)
(*      modified on Tue Aug  4 15:13:35 PDT 1992 by karlin   *)
(*      modified on Wed Jul 22 01:10:06 1992 by mhb          *)

MODULE MFLegendView;

IMPORT Filter, MFGraph, GraphVBT, MFViews, MaxflowViewClass, R2, TextVBT, 
       View, ZeusPanel;

TYPE
  T = MaxflowViewClass.T BRANDED OBJECT
        gvbt: GraphVBT.T;
      OVERRIDES
        startrun := Startrun;
        oeSetup := Setup;
      END;

PROCEDURE Startrun (view: T) =
  (* sleazy hack: remove the old GraphVBT and just ignore it;
     heck, what else are VM and GC good for? *)
  BEGIN
    EVAL Filter.Replace(view, NEW(GraphVBT.T).init());
    (* call the superclass startrun in ZeusClass.T *)
    MaxflowViewClass.T.startrun(view); 
  END Startrun;


PROCEDURE Setup (             view   : T; 
                 <* UNUSED *> g      : MFGraph.T; 
                 <* UNUSED *> source : MFGraph.Vertex; 
                 <* UNUSED *> sink   : MFGraph.Vertex) =
  VAR
    wc := GraphVBT.WorldRectangle{w := 0.0, s := 0.0, e := 1.0, n := 1.0};
    stdsize, bigsize: R2.T;
    v  : GraphVBT.Vertex;
    f  : GraphVBT.WorldFont;
  BEGIN
    view.gvbt := NEW(GraphVBT.T, world := wc).init();
    EVAL Filter.Replace(view, view.gvbt);

    stdsize := R2.T{0.08, 0.08};
    bigsize := R2.T{0.6, 0.2};

    (* cornflowerblue *)

    EVAL
      NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.25, 0.9},
          size := stdsize, color := MFViews.Color("cornflowerblue")).init();

    f := view.gvbt.font (family := "helvetica", weight := "bold",
                         slant := GraphVBT.Slant.Roman, size := 0.05);

    EVAL
      NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.75, 0.9},
          size := bigsize,
          label :=
            "Forward flow along path (color saturation = flow/capacity)",
          font := f, fontColor := MFViews.Color("black"),
          color := MFViews.Color("white")).init();

    (* limegreen *)

    EVAL NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.25, 0.7},
             size := stdsize, color := MFViews.Color("limegreen")).init();

    EVAL NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.75, 0.7},
             size := bigsize, label := "Backward flow along path",
             font := f, fontColor := MFViews.Color("black"),
             color := MFViews.Color("white")).init();

    (* magenta *)

    EVAL NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.25, 0.5},
             size := stdsize, color := MFViews.Color("magenta")).init();

    EVAL NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.75, 0.5},
             size := bigsize, label := "Flow = capacity", font := f,
             fontColor := MFViews.Color("black"),
             color := MFViews.Color("white")).init();

    (* black *)

    v := NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.25, 0.3},
             size := stdsize, shape := GraphVBT.VertexShape.Ellipse,
             color := MFViews.Color("gray")).init();

    EVAL
      NEW(
        GraphVBT.VertexHighlight, vertex := v,
        color := MFViews.Color("black"), border := R2.T{0.01, 0.01}).init();

    EVAL NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.75, 0.3},
             size := bigsize, label := "Augmenting path vertex", font := f,
             fontColor := MFViews.Color("black"),
             color := MFViews.Color("white")).init();


    (* yellow *)

    v := NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.25, 0.1},
             size := stdsize, shape := GraphVBT.VertexShape.Ellipse,
             color := MFViews.Color("gray")).init();

    EVAL
      NEW(
        GraphVBT.VertexHighlight, vertex := v,
        color := MFViews.Color("yellow"), border := R2.T{0.01, 0.01}).init();

    EVAL NEW(GraphVBT.Vertex, graph := view.gvbt, pos := R2.T{0.75, 0.1},
             size := bigsize, label := "currentVertex", font := f,
             fontColor := MFViews.Color("black"),
             color := MFViews.Color("white")).init();


    view.gvbt.redisplay();
  END Setup;

PROCEDURE New (): View.T =
  VAR a : T :=  NEW(T).init(TextVBT.New("Legend"));
  BEGIN
    RETURN a;
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Legend", "Maxflow");

END MFLegendView.
