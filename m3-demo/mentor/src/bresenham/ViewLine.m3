(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Jan  5 16:17:18 PST 1995 by najork   *)
(*      modified on Wed Aug 18 08:41:02 PDT 1993 by comba    *)
(*      modified on Mon Aug 16 19:12:56 PDT 1993 by harrison *)
(*      modified on Sun Jul 11 21:00:11 PDT 1993 by mhb *)
<* PRAGMA LL *>

MODULE ViewLine;

IMPORT BresenhamViewClass, Filter, GraphVBT, 
       PaintOp, R2, VBT, View, ZeusPanel, Thread;

IMPORT AnimationPath;

IMPORT Math ;

CONST
    NPOINTS = 64 ;

VAR
    bgColor    := PaintOp.FromRGB(0.6, 0.6, 0.6);
    pixelColor := PaintOp.FromRGB(0.0, 0.0, 0.0);
    lineColor := PaintOp.FromRGB(0.2, 1.0, 0.2);
    gridColor := PaintOp.FromRGB(0.0, 0.0, 0.0);  
    lensColor := PaintOp.FromRGB(0.0, 0.0, 0.0); 
    borderClr := PaintOp.FromRGB(0.0, 0.0, 0.0) ;
    fontColor := PaintOp.FromRGB(0.0, 0.0, 0.0) ;
    errorColor1l := PaintOp.FromRGB(1.0, 0.0, 0.0);
    errorColor1d := PaintOp.FromRGB(0.5, 0.0, 0.0);
    errorColor2l := PaintOp.FromRGB(1.0, 1.0, 0.0);
    errorColor2d := PaintOp.FromRGB(0.5, 0.5, 0.0);

    lensPt   : ARRAY [0..NPOINTS-1] OF R2.T ;
    lensPol  : ARRAY [0..NPOINTS-1] OF REFANY ; 

TYPE
  T = BresenhamViewClass.T BRANDED OBJECT
        graph: GraphVBT.T;         (* drawing canvas that fills the view *)
        width, height: INTEGER;
	x1, y1, x2, y2: INTEGER;   (* Current line *)
        nrPixels : INTEGER ;
        x, y : INTEGER ;           (* Current pixel *)
        wIncr, hIncr : REAL ;
        showError : BOOLEAN ;
        font : GraphVBT.WorldFont;
      OVERRIDES
        <* LL=0 *>
        oeSetup   := Setup;
        oeNewLine := NewLine;
        oeShowPixel := ShowPixel;
        oeMove := Move ;
      END;

PROCEDURE New (): View.T =
  BEGIN RETURN NEW(T).init(NIL) END New;

PROCEDURE PixelEdge(view: T; x, y: INTEGER): R2.T =
  BEGIN
    RETURN R2.T{FLOAT(x) / FLOAT(view.width),
                FLOAT(y) / FLOAT(view.height)};
  END PixelEdge;

PROCEDURE PixelCenter(view: T; x, y: INTEGER): R2.T =
  BEGIN
    RETURN R2.T{(FLOAT(x) + 0.5) / FLOAT(view.width),
                (FLOAT(y) + 0.5) / FLOAT(view.height)};
  END PixelCenter;

PROCEDURE DrawEdge (view: T; width, y : INTEGER) =
  BEGIN
    NEW(GraphVBT.Edge,
        vertex0 := NEW(GraphVBT.Vertex, 
                       graph := view.graph,
                       pos := PixelEdge(view, 0, y)).init(),
        vertex1 := NEW(GraphVBT.Vertex,
                       graph := view.graph,
                       pos := PixelEdge(view, width, y)).init(),
        width := 0.004,
        color := gridColor).init().toFront (GraphVBT.ZOrder.Foreground);
  END DrawEdge ;

PROCEDURE DrawText (view: T; x, y : INTEGER; t : TEXT) =
  BEGIN
    NEW(GraphVBT.Vertex,
        graph := view.graph,
        label := t,
        font := view.font,
        size := R2.T{0.08, 0.09},
        fontColor := fontColor,
        color := bgColor,
        pos   := R2.T{(FLOAT(x) + 0.2) / FLOAT(view.width),
                      (FLOAT(y)) / FLOAT(view.height)}
    ).init().toFront (GraphVBT.ZOrder.Foreground);
  END DrawText ;

PROCEDURE DrawLongText (view: T; y : INTEGER; t : TEXT) =
  BEGIN
    NEW(GraphVBT.Vertex,
        graph := view.graph,
        label := t,
        font := view.font,
        size := R2.T{0.9, 0.07},
        fontColor := fontColor,
        color := bgColor,
        pos   := PixelCenter(view, view.width DIV 2, y)
    ).init().toFront (GraphVBT.ZOrder.Foreground);
  END DrawLongText ;

PROCEDURE DrawError(
    view : T; x, y, p : INTEGER; scale : REAL;color: PaintOp.T) = 
  VAR
    error := ((FLOAT(p)/(2.0*FLOAT(view.width)))+ 0.5) * 2.0 * scale ;
    posError := R2.T{
      (FLOAT(x) + 0.5) / FLOAT(view.width),
      (FLOAT(y)) / FLOAT(view.height) + (error/2.0)
    } ;
    sizeError := R2.T{1.0 / FLOAT(view.width), error} ;
  BEGIN
    NEW(GraphVBT.Vertex,
             graph := view.graph,
             pos := posError,
             size := sizeError,
             border := 0.001,
             fontColor := borderClr,
             color := color).init().toFront(GraphVBT.ZOrder.Normal);    
  END DrawError ;

PROCEDURE Setup (view: T; width, height: INTEGER; show : BOOLEAN) =
  VAR 
    south : REAL ;
    east  : REAL ;
    north := 1.1 ;
    west  : REAL ;
  BEGIN
    IF show THEN
      west  := -0.2 ;
      south := -2.0 ;
      east  := 1.1 ;
    ELSE
      west  := -0.1 ;
      south := -0.1 ;
      east  := 1.1 ;
    END ; 
    view.graph := 
      NEW(GraphVBT.T, 
        world := GraphVBT.WorldRectangle{
             w := west, s := south, e := east, n := north}).init();
    view.width := width;
    view.height := height;
    view.wIncr  := 1.0 / FLOAT(width) ;
    view.hIncr  := 1.0 / FLOAT(height) ;
    view.nrPixels := 0 ;
    view.showError := show ;
     
    view.font := view.graph.font(
                          family := "Helvetica", weight := "bold",
                          slant := GraphVBT.Slant.Roman, size := 0.08) ;

    LOCK VBT.mu DO 
      EVAL Filter.Replace(view, view.graph) 
    END;

    NEW(GraphVBT.Vertex,
             graph := view.graph,
             pos := R2.T{0.5, 0.5},
             size := R2.T{1.5, 5.0},
             color := bgColor).init().toBack(GraphVBT.ZOrder.Background);

    FOR w := 0 TO width DO
      VAR
        v0 := NEW(GraphVBT.Vertex,
                  graph := view.graph,
                  pos := PixelEdge(view, w, 0)).init();
        v1 := NEW(GraphVBT.Vertex,
                  graph := view.graph,
                  pos := PixelEdge(view, w, height)).init();
      BEGIN
        NEW(GraphVBT.Edge,
                    vertex0 := v0,
                    vertex1 := v1,
                    width := 0.001,
                    color := gridColor).init().toFront (GraphVBT.ZOrder.Foreground) ;
      END;
    END;

    FOR h := 0 TO height DO
      VAR
        v0 := NEW(GraphVBT.Vertex,
                  graph := view.graph,
                  pos := PixelEdge(view, 0, h)).init();
        v1 := NEW(GraphVBT.Vertex,
                  graph := view.graph,
                  pos := PixelEdge(view, width, h)).init();
      BEGIN
        NEW(GraphVBT.Edge,
                    vertex0 := v0,
                    vertex1 := v1,
                    width := 0.001,
                    color := gridColor).init().toFront (GraphVBT.ZOrder.Foreground); 
      END;
    END;

    IF view.showError THEN
      DrawEdge (view, width, -2) ;
      DrawEdge (view, width, -3) ;
      DrawEdge (view, width, -4) ;
      DrawEdge (view, width, -7) ;
      DrawEdge (view, width, -9) ;
      DrawEdge (view, width, -11) ;
      DrawText (view, -1, -4, "0.0") ;
      DrawText (view, -1, -3, "0.5") ;
      DrawText (view, -1, -2, "1.0") ;

      DrawText (view, -1, -11, "0 ") ;
      DrawText (view, -1, -9, "dx") ;
      DrawText (view, -1, -7, "2dx") ;
      DrawLongText (view, -5, 
                    "Error = dy/dx, i1 = Error, i2 = 1-Error") ;
      DrawLongText (view, -12, 
                    "Error = 2dy, i1 = Error, i2 = 1-Error") ;
    END ;

    view.graph.redisplay()
  END Setup;

PROCEDURE NewLine (view: T; x1, y1, x2, y2: INTEGER) =
  VAR
    v0 := NEW(GraphVBT.Vertex,
	      graph := view.graph,
	      pos := PixelCenter(view, x1, y1)).init();
    v1 := NEW(GraphVBT.Vertex,
	      graph := view.graph,
	      pos := PixelCenter(view, x2, y2)).init();
    angIncr := (2.0d0 * 3.1415926535d0) / FLOAT(NPOINTS, LONGREAL) ;
    ang : LONGREAL ;
  BEGIN
    NEW(GraphVBT.Edge,
      vertex0 := v0,
      vertex1 := v1, width := 0.01,
      color := lineColor).init().toFront (GraphVBT.ZOrder.Foreground); 

    VAR
      xC := view.wIncr ;
      yC := view.hIncr ;
      rX := 1.3 * view.wIncr ;
      rY := 1.3 * view.hIncr ;
    BEGIN
      FOR i := 0 TO NPOINTS-1 DO
        ang := FLOAT(i, LONGREAL) * angIncr ;
        lensPt[i] := R2.T{
             FLOAT(Math.cos(ang)) * rX + xC, 
             FLOAT(Math.sin(ang)) * rY + yC
          } ;
        lensPol[i]:=
          NEW(GraphVBT.Vertex, graph:=view.graph, 
            pos:=lensPt[i],
            color := lensColor).init();
        IF (i > 0) THEN
          NEW(GraphVBT.Edge,
                  vertex0 := lensPol[i-1],
                  vertex1 := lensPol[i], 
                  color := lensColor,
                  width := 0.001).init().toFront (GraphVBT.ZOrder.Foreground);
        END ; 
      END
    END ;

    NEW(GraphVBT.Edge,
                  vertex0 := lensPol[NPOINTS-1],
                  vertex1 := lensPol[0], 
                  color := lensColor,
                  width := 0.001).init().toFront (GraphVBT.ZOrder.Foreground);

    view.x1 := x1;
    view.y1 := y1;
    view.x2 := x2;
    view.y2 := y2;
    view.graph.redisplay()
  END NewLine;

PROCEDURE ShowPixel(view: T; 
                    x : INTEGER;
                    y : INTEGER;
       <* UNUSED *> p1: INTEGER;
                    p2: INTEGER) RAISES {Thread.Alerted} =
  VAR
    v : GraphVBT.Vertex ;
  BEGIN
    IF p2 < 0 THEN
      DrawError (view, x, -4, p2, 1.0 / FLOAT(view.height), errorColor1d) ;
      DrawError (view, x, -11, p2, 2.0 / FLOAT(view.height), errorColor2d) ;
    ELSE
      DrawError (view, x, -4, p2, 1.0 / FLOAT(view.height), errorColor1l) ;
      DrawError (view, x, -11, p2, 2.0 / FLOAT(view.height), errorColor2l) ;
    END ;
    IF (view.nrPixels = 0) THEN
      v := NEW(GraphVBT.Vertex,
	      graph := view.graph,
	      pos := PixelCenter(view, x, y),
	      size := R2.Scale(2.0, PixelCenter(view, 0, 0)),
              color := pixelColor,
              shape := GraphVBT.VertexShape.Ellipse).init();          
      view.x := x ;
      view.y := y ;
    ELSE
      VAR
        pos0   := PixelCenter(view, view.x, view.y) ;
        pos1   := PixelCenter(view, x, y) ;
        path01 := NEW(AnimationPath.StraightPath).init(pos0, pos1) ;
      BEGIN
        v := NEW(GraphVBT.Vertex,
	      graph := view.graph,
	      pos := pos0,
	      size := R2.Scale(2.0, PixelCenter(view, 0, 0)),
              color := pixelColor,
              shape := GraphVBT.VertexShape.Ellipse).init();
        v.move (        
          pos := pos1,
          animated := TRUE,
          start := 0.0,
          stop := 1.0,
          path := path01) ;  
        view.graph.animate(0.0, 1.0) ;      
        view.x := x ;
        view.y := y ;
      END ;
    END ;   
    INC (view.nrPixels) ;
    view.graph.redisplay() ;
  END ShowPixel;

PROCEDURE Move (view : T; p : INTEGER) RAISES {Thread.Alerted} =
  VAR
    incr    : R2.T ;
    oldPt,
    newPt   : R2.T ;
    newVert : GraphVBT.Vertex ;
    newPath : AnimationPath.StraightPath ;
  BEGIN
    IF (p < 0) THEN
      incr := R2.T {view.wIncr, 0.0} 
    ELSE
      incr := R2.T {view.wIncr, view.hIncr}
    END ;
    FOR i := 0 TO NPOINTS-1 DO
      oldPt := lensPt[i] ;
      newPt := R2.T {oldPt[0]+incr[0],oldPt[1]+incr[1]} ;
      newPath := NEW(AnimationPath.StraightPath).init(oldPt, newPt) ;
      newVert := lensPol[i] ;
      newVert.move (pos := newPt, animated := TRUE, path := newPath) ;
      lensPol[i] := newVert ;
      lensPt[i] := newPt ;
    END ;
    view.graph.animate(0.0, 1.0) ;          
    view.graph.redisplay()  
  END Move ;

BEGIN
  ZeusPanel.RegisterView (New, "Line", "Bresenham");
END ViewLine.



