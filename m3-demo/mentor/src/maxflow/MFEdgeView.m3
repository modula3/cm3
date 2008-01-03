(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue Jan 31 14:40:26 PST 1995 by kalsow     *)
(*      modified on Wed May  4 11:09:53 PDT 1994 by najork     *)
(*      modified on Wed Jan  6 15:20:03 PST 1993 by steveg     *)
(*      modified on Thu Sep  3 19:00:02 PDT 1992 by johnh      *)
(*      modified on Thu Aug  6 14:38:18 PDT 1992 by karsenty   *)
(*      modified on Wed Aug  5 13:02:07 PDT 1992 by karlin     *)
(*      modified on Wed Jul 22 01:10:06 1992 by mhb            *)

MODULE MFEdgeView;

IMPORT MaxflowViewClass, Filter, GraphVBT, R2, ColorName, View, ZeusPanel, 
       MFGraph, RefList, MFAlgs, PaintOp;

TYPE
  T = MaxflowViewClass.T BRANDED OBJECT
	gvbt : GraphVBT.T;
        nameOfVertex : REF ARRAY OF GraphVBT.Vertex;

      OVERRIDES
        startrun := Startrun;
        oeSetup := Setup;
        oeHighlightPath := HighlightPath;
      END;


PROCEDURE Color(color: TEXT): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  VAR rgb := ColorName.ToRGB (color);
  BEGIN
    RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END Color;


PROCEDURE Startrun (view: T) =
  (* sleazy hack: remove the old GraphVBT and just ignore it;
     heck, what else are VM and GC good for? *)
  BEGIN
    EVAL Filter.Replace(view, NEW(GraphVBT.T).init());
    (* call the superclass startrun in ZeusClass.T *)
    MaxflowViewClass.T.startrun(view); 
  END Startrun;


PROCEDURE Setup (view: T; <*UNUSED*> g: MFGraph.T;  <*UNUSED*> source, sink: MFGraph.Vertex) =
  VAR
      wc := GraphVBT.WorldRectangle{
            w := 0.0, s := 0.0, e := 1.0, n := 1.0};

  BEGIN
    view.gvbt  := NEW(GraphVBT.T, world := wc).init();
    EVAL Filter.Replace(view, view.gvbt);
    view.gvbt.redisplay();
  END Setup;

PROCEDURE New (): View.T =
  VAR a : T :=  NEW(T).init(NEW(GraphVBT.T).init());
  BEGIN
    RETURN a;
  END New;

PROCEDURE HighlightPath (view: T; path : RefList.T; <*UNUSED*> maxCap : REAL)=
  VAR 
      len : CARDINAL;
      width, h: REAL;
      i : CARDINAL := 0;
      curedge : MFAlgs.MFEdge;
      from, to, curvertex : MFAlgs.MFVertex;
      arrowto := ARRAY [0 .. 1] OF BOOLEAN{FALSE, TRUE};
      arrowfrom := ARRAY [0 .. 1] OF BOOLEAN{TRUE, FALSE};
      font := view.gvbt.font(family := "helvetica", weight := "bold", slant := GraphVBT.Slant.Roman, size := 0.05);
 BEGIN
    view.gvbt.clear();
    view.gvbt.redisplay();
    len := RefList.Length(path);
    width := 1.0/FLOAT(len);
    view.nameOfVertex := NEW(REF ARRAY OF GraphVBT.Vertex, len+1);
    h :=(width/2.0) + FLOAT(i)*width;
    curedge := path.head;
    from := NARROW( curedge.from, MFAlgs.MFVertex);
    to := NARROW( curedge.to, MFAlgs.MFVertex);
    view.nameOfVertex[i] := NEW (GraphVBT.Vertex, graph := view.gvbt,
                            pos := R2.T{h, 0.05},
                            shape := GraphVBT.VertexShape.Rectangle,
                            size := R2.T{width/4.0, 0.1},
                            color := Color("white"),
                            fontColor := Color("black"),
                            font := font,
                            label :=  from.label).init(); 
    curvertex := from;
    INC(i);
    path := path.tail;
    WHILE path # NIL DO
      h :=(width/2.0) + FLOAT(i)*width;
      IF (i# 0) THEN 
        curedge := path.head;
        from := NARROW( curedge.from, MFAlgs.MFVertex);
        to := NARROW( curedge.to, MFAlgs.MFVertex);
      END;
      IF (curvertex = from) THEN
        view.nameOfVertex[i] := NEW (GraphVBT.Vertex, graph := view.gvbt,
                            pos := R2.T{h, 0.05},
                            shape := GraphVBT.VertexShape.Rectangle,
                            size := R2.T{width/4.0, 0.1},
                            color := Color("white"),
                            fontColor := Color("black"),
                            font := font,
                            label :=  to.label).init();
         EVAL NEW(GraphVBT.Edge, 
                 vertex0 :=view.nameOfVertex[i-1],
                 vertex1 :=view.nameOfVertex[i],
                 color := Color ("cornflowerblue"),
(*                 label := Fmt.Real(curedge.capacity - curedge.flow),*)
                 arrow := arrowto).init();  
         curvertex := to;
       ELSIF (curvertex = to) THEN
        view.nameOfVertex[i] := NEW (GraphVBT.Vertex, graph := view.gvbt,
                            pos := R2.T{h, 0.05},
                            shape := GraphVBT.VertexShape.Rectangle,
                            size := R2.T{width/4.0, 0.1},
                            color := Color("white"),
                            fontColor := Color("black"),
                            font := font,
                            label :=  from.label).init();
         EVAL NEW(GraphVBT.Edge, 
                 vertex0 :=view.nameOfVertex[i-1],
                 vertex1 :=view.nameOfVertex[i],
                 color := Color("limegreen"),
(*                 label := Fmt.Real(curedge.flow),*)
                 arrow :=arrowfrom).init();  
        curvertex := from; 
      END;
      INC(i); 
      path := path.tail;
    END;
    IF (i=1) THEN
      view.nameOfVertex[i] := NEW (GraphVBT.Vertex, graph := view.gvbt,
                            pos := R2.T{h, 0.05},
                            shape := GraphVBT.VertexShape.Rectangle,
                            size := R2.T{width/4.0, 0.1},
                            color := Color("white"),
                            fontColor := Color("black"),
                            font := font,
                            label :=  to.label).init();
       EVAL NEW(GraphVBT.Edge, 
                 vertex0 :=view.nameOfVertex[i-1],
                 vertex1 :=view.nameOfVertex[i],
                 arrow :=arrowfrom).init();
      END;
      view.gvbt.redisplay();
  END HighlightPath;

BEGIN
  ZeusPanel.RegisterView (New, "Edge  View", "Maxflow");

END MFEdgeView.
