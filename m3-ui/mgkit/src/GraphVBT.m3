(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Tue Aug 27 11:17:19 PDT 1996 by najork *)
(*      modified on Tue Jan 31 11:56:02 PST 1995 by kalsow *)
(*      modified on Fri Aug 19 16:41:56 PDT 1994 by steveg *)
(*      modified on Mon Jul  4 23:42:36 PDT 1994 by mhb    *)
(*      modified on Tue Aug  4 07:42:08 PDT 1992 by jdd *)

MODULE GraphVBT EXPORTS GraphVBT, GraphVBTExtras;

IMPORT Animate, Axis, FloatMode, Fmt, Font, Math, MG, MGV, MultiFilter,
       PaintOp, Point, Pts, R2, Random, Rd, RealFloat, R2Path, Rect,
       RefList, RefListUtils,
       Region, ScaleFilter, TextRd, TextWr, Thread, VBT, VBTClass, Wr;

<* PRAGMA LL *>

REVEAL
  TPrivate = ScaleFilter.T BRANDED OBJECT END;
  T =
    TPublic BRANDED OBJECT
      <* LL >= {SELF.mu} *>
      rect: Rect.T;             (* the display rectangle in pixels *)
      res := ARRAY Axis.T OF REAL{1.0, 1.0}; (* copied from screentype *)
      realMarginMM: R2.T;       (* enough at left and bottom to center the
                                   display rectangle *)
      vertexGroup: MG.Group;    (* all the MG.Group objects for the
                                   vertices *)
      edgeGroup: MG.Group;      (* all the MG.Group objects for the
                                   edges *)
      vertexHighlightGroup: MG.Group;  (* all the MG.Group objects for the
                                          vertex highlights *)
      polygonGroup: MG.Group;   (* all the MG.Group objects for the
                                   polygons *)
      fgGroup, bgGroup: MG.Group; (* all the MG.Group objects for objects
                                     who have been "toFront" or "toBack"
                                     with a "ZOrder" of "Foreground" or
                                     "Background" *)
      animations := 0;          (* number of animations in progress *)
      needRefresh := FALSE;     (* if objects moved or screen resolution
                                   while an animation was in effect.   *)
      needReshape := FALSE;     (* Reshape call should refresh even if shape
                                   hasn't changed. *)
      mgv: MGV.V;               (* the actual MGV containing the graphics *)
      initialized := FALSE;
    OVERRIDES
      init               := InitGraph;
      setWorld           := SetWorldGraph;
      setMargin          := SetMarginGraph;
      setPixelSizeDivisor := SetPixelSizeDivisorGraph;
      setAspect          := SetAspectGraph;
      setPreferredSize   := SetPreferredSizeGraph;
      rescreen           := Rescreen;
      reshape            := Reshape;
      shape              := Shape;
      redisplay          := RedisplayGraph;
      animate            := AnimateGraph;
      clear              := ClearGraph;
      verticesAt         := GraphVerticesAt;
      edgesAt            := GraphEdgesAt;
      vertexHighlightsAt := GraphVertexHighlightsAt;
      polygonsAt         := GraphPolygonsAt;
      font               := MakeWorldFont;
    END;

REVEAL
  Vertex = VertexPublic BRANDED OBJECT
             <* LL >= {SELF.graph.mu} *>
             mg   : MG.T;       (* the corresponding MG.T *)
             group: MG.Group;   (* containing the MG.T *)
             currentGroup: MG.Group;   (* graph group containing self.group *)
             colorScheme: PaintOp.ColorScheme;  (* containing the color and
                                                   the fontColor *)
             animated: BOOLEAN;
             path    : AnimationPath;
             new: RECORD
                    pos: R2.T;  (* position after next animation *)
                  END;
             initialized := FALSE;
           OVERRIDES
             init           := InitVertex;
             move           := MoveVertex;
             setSize      := SetVertexSize;
             setShape       := SetVertexShape;
             setColor       := SetVertexColor;
             setLabel       := SetVertexLabel;
             setFont        := SetVertexFont;
             setFontColor   := SetVertexFontColor;
             setBorder    := SetVertexBorder;
             setBorderColor := SetVertexBorderColor;
             toFront        := VertexToFront;
             toBack         := VertexToBack;
             remove         := RemoveVertex;
           END;

REVEAL
  Edge =
    EdgePublic BRANDED OBJECT
      <* LL >= {SELF.vertex0.graph.mu} *>
      graph   : T;              (* from the vertices *)
      straight: BOOLEAN;        (* whether the line is straight *)
      mg      : MG.T;           (* the corresponding MG.Line or MG.Shape *)
      isLine  : BOOLEAN;        (* whether the mg is an MG.Line *)
      end: ARRAY [0 .. 1] OF MG.LineEnd;  (* the ends of the MG.Line, or
                                             NIL *)
      arrowLine: ARRAY [0 .. 1], [0 .. 1] OF MG.Line;
      (* the arrowheads, or NIL.  the first index is which arrowhead
         (forward then backward); the second index is which which line (the
         one on the left in its direction, then the one on the right). *)
      arrowEnd: ARRAY [0 .. 1] OF ARRAY [0 .. 1], [0 .. 1] OF MG.LineEnd;
      (* their ends, or NIL.  the third index is the end on the edge, then
         the far end. *)
      arrowPos: ARRAY [0 .. 1] OF ARRAY [0 .. 1], [0 .. 1] OF R2.T;
      (* the positions of the ends, in world coordinates. *)
      group: MG.Group;          (* a group for all the endpoints *)
      currentGroup: MG.Group;   (* graph group containing self.group *)
      pos: ARRAY [0 .. 1] OF R2.T;  (* current positions of the endpoints,
                                       in world coordinates *)
      cpos: ARRAY [0 .. 1] OF R2.T;  (* current positions of control
                                        points, in world coordinates, or
                                        same as pos if no control points *)
      new: RECORD
             vertex0, vertex1: Vertex;  (* vertices after next animation *)
             control0, control1: Vertex;  (* control vertices after next
                                             animation *)
           END;
      colorScheme: PaintOp.ColorScheme;
      initialized                        := FALSE;
    OVERRIDES
      init       := InitEdge;
      move       := MoveEdge;
      setWidth := SetEdgeWidth;
      setColor   := SetEdgeColor;
      setArrow   := SetEdgeArrow;
      toFront    := EdgeToFront;
      toBack     := EdgeToBack;
      remove     := RemoveEdge;
    END;

REVEAL
  VertexHighlight =
    VertexHighlightPublic BRANDED OBJECT
      <* LL >= {SELF.vertex.graph.mu} *>
      graph: T;                 (* from the vertex *)
      mg   : MG.T;              (* the corresponding MG.T *)
      group: MG.Group;          (* containing the MG.T *)
      currentGroup: MG.Group;   (* graph group containing self.group *)
      colorScheme: PaintOp.ColorScheme;  (* the ColorScheme for the MG.T *)
      shape      : VertexShape;          (* current shape *)
      pos: R2.T;                (* current position of the vertex
                                   highlight, in world coordinates *)
      size: R2.T;             (* dimensions of the vertex highlight
                                   rectangle *)
      new: RECORD
             vertex: Vertex;    (* vertex after next animation *)
           END;
      initialized := FALSE;
    OVERRIDES
      init        := InitVertexHighlight;
      move        := MoveVertexHighlight;
      setBorder := SetVertexHighlightBorder;
      setColor    := SetVertexHighlightColor;
      toFront     := VertexHighlightToFront;
      toBack      := VertexHighlightToBack;
      remove      := RemoveVertexHighlight;
    END;

REVEAL
  Polygon =
    PolygonPublic BRANDED OBJECT
      <* LL >= {RefList.First(SELF).graph.mu} *>
      graph: T;                 (* from the vertices *)
      mg   : MG.T;              (* the corresponding MG.Shape *)
      pos: RefList.T (* OF REF R2.T *);  (* current positions of the corners,
                                         in world coordinates *)
      group: MG.Group;          (* a group for the MG.T *)
      currentGroup: MG.Group;   (* graph group containing self.group *)
      new: RECORD
             vertices: RefList.T (* OF Vertex *);  (* vertices after next
                                                   animation *)
           END;
      colorScheme: PaintOp.ColorScheme;
      initialized                        := FALSE;
    OVERRIDES
      init     := InitPolygon;
      move     := MovePolygon;
      setColor := SetPolygonColor;
      toFront  := PolygonToFront;
      toBack   := PolygonToBack;
      remove   := RemovePolygon;
    END;

REVEAL
  WorldFont = BRANDED REF Font.T;

<* LL.sup < graph.mu *>

PROCEDURE InitGraph (graph: T): T =
  BEGIN
    IF graph.initialized THEN RETURN graph; END;

    graph.mgv := NEW(MGV.V).init();
    EVAL ScaleFilter.T.init(graph, graph.mgv);
    graph.mu := NEW(MUTEX);

    LOCK graph.mu DO
      graph.realMarginMM := R2.Origin;
      graph.fgGroup := NEW(MG.Group).init();
      graph.bgGroup := NEW(MG.Group).init();
      graph.vertexGroup := NEW(MG.Group).init();
      graph.edgeGroup := NEW(MG.Group).init();
      graph.vertexHighlightGroup := NEW(MG.Group).init();
      graph.polygonGroup := NEW(MG.Group).init();

      LOCK graph.mgv.mu DO
        graph.mgv.border := ARRAY Axis.T OF REAL{0.0, 0.0}; (* no MG border *)
        graph.mgv.displayList.addAfter(graph.mgv, graph.fgGroup, NIL);
        graph.mgv.displayList.addAfter(graph.mgv, graph.vertexGroup, NIL);
        graph.mgv.displayList.addAfter(graph.mgv, graph.edgeGroup, NIL);
        graph.mgv.displayList.addAfter(
          graph.mgv, graph.vertexHighlightGroup, NIL);
        graph.mgv.displayList.addAfter(graph.mgv, graph.polygonGroup, NIL);
        graph.mgv.displayList.addAfter(graph.mgv, graph.bgGroup, NIL);
      END;

      graph.initialized := TRUE;
    END;

    RETURN graph;
  END InitGraph;

PROCEDURE SetWorldGraph(graph: T; world: WorldRectangle) =
  BEGIN
    graph.world := world;
    graph.needReshape := TRUE;
    VBT.Mark(graph);
  END SetWorldGraph;

PROCEDURE SetMarginGraph(graph: T; margin: REAL) =
  BEGIN
    graph.margin := margin;
    graph.needReshape := TRUE;
    VBT.Mark(graph);
  END SetMarginGraph;

PROCEDURE SetPixelSizeDivisorGraph(graph: T; psd: ARRAY [0 .. 1] OF CARDINAL) =
  BEGIN
    graph.pixelSizeDivisor := psd;
    graph.needReshape := TRUE;
    VBT.Mark(graph);
  END SetPixelSizeDivisorGraph;

PROCEDURE SetAspectGraph(graph: T; aspect: REAL) =
  BEGIN
    graph.aspect := aspect;
    graph.needReshape := TRUE;
    VBT.Mark(graph);
  END SetAspectGraph;

PROCEDURE SetPreferredSizeGraph (graph: T; ps: R2.T) =
  BEGIN
    graph.preferredSize := ps;
    graph.needReshape := TRUE;
    VBT.Mark(graph);
  END SetPreferredSizeGraph;

<* LL.sup >= graph.mu *>

PROCEDURE Rescreen (graph: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    IF cd.st # NIL THEN graph.res := cd.st.res; graph.rect := Rect.Empty END;
    TPublic.rescreen(graph, cd);
  END Rescreen;

<* LL.sup = VBT.mu *>

PROCEDURE Reshape (graph: T; READONLY cd: VBT.ReshapeRec) =
  VAR
    redisplay := FALSE;
    reshape   := FALSE;
  BEGIN
    IF NOT Rect.IsEmpty(cd.new) THEN
      LOCK graph.mu DO
        IF (graph.rect # cd.new) OR graph.needReshape THEN
          graph.rect := cd.new;
          graph.needReshape := FALSE;

          (* avoid arithmetic problems *)
          IF graph.rect.east = graph.rect.west THEN
            graph.rect.east := graph.rect.east + 1;
          END;
          IF graph.rect.south = graph.rect.north THEN
            graph.rect.south := graph.rect.south + 1;
          END;

          LOCK graph.mgv.mu DO
            RefreshGraph(graph);
          END;
          reshape := TRUE;

          IF graph.animations > 0 THEN graph.needRefresh := TRUE; END;
        ELSE
          redisplay := TRUE;
        END;
      END;
      IF reshape THEN
        TPublic.reshape(graph, cd);
      ELSIF redisplay THEN
        graph.mgv.mgRedisplay(Region.Full);
      END;
    END;
  END Reshape;

<* LL.sup = VBT.mu *>

PROCEDURE Shape (graph: T; ax: Axis.T; <* UNUSED *> n: CARDINAL):
  VBT.SizeRange =
  BEGIN
    LOCK graph.mu DO
      RETURN VBT.SizeRange{
               VBT.DefaultShape.lo,
               MAX(ROUND(graph.preferredSize[ORD(ax)] * graph.res[ax]),
                   VBT.DefaultShape.lo), VBT.DefaultShape.hi};
    END;
  END Shape;

<* LL = VBT.mu.graph *>

PROCEDURE RedisplayGraph (graph: T) =
  BEGIN
    graph.mgv.mgRedisplay(Region.Full);
  END RedisplayGraph;

<* LL.sup < vertex.graph.mu *>

PROCEDURE InitVertex (vertex: Vertex): Vertex =
  BEGIN
    IF vertex.initialized THEN RETURN vertex; END;

    WITH graph = vertex.graph,
         mgv   = graph.mgv     DO
      LOCK graph.mu DO
        LOCK mgv.mu DO
          vertex.colorScheme :=
            PaintOp.MakeColorScheme(vertex.color, vertex.fontColor);
          IF vertex.font = NIL THEN vertex.font := DefaultFont END;
          CASE vertex.shape OF
          | VertexShape.Rectangle =>
              vertex.mg := NEW(MG.Rectangle, color := vertex.colorScheme,
                               label := vertex.label, font := vertex.font^,
                               weight := 0.0).init(R2.Origin, R2.Origin);
          | VertexShape.Ellipse =>
              vertex.mg := NEW(MG.Ellipse, color := vertex.colorScheme,
                               label := vertex.label, font := vertex.font^,
                               weight := 0.0).init(R2.Origin, R2.Origin);
          END;
          vertex.group := NEW(MG.Group).init();
          vertex.currentGroup := graph.vertexGroup;

          vertex.vertexHighlights := NIL;
          vertex.new.pos := vertex.pos;
          vertex.animated := FALSE;
          vertex.group.addBefore(mgv, vertex.mg, NIL);
          vertex.currentGroup.addBefore(mgv, vertex.group, NIL);
          RefListUtils.Push(graph.vertices, vertex);

          VAR centerPP := WorldPosToPts(graph, vertex.pos);
          BEGIN
            MG.TranslateToLocked(
              vertex.mg, mgv, Finite2(centerPP), center := TRUE);
            AdjustVertex(vertex);
          END;
          vertex.initialized := TRUE;
        END;
      END;
    END;
    RETURN vertex;
  END InitVertex;

<* LL.sup < edge.vertex0.graph.mu *>

PROCEDURE InitEdge (edge: Edge): Edge =
  BEGIN
    IF edge.initialized THEN RETURN edge; END;

    WITH graph = edge.vertex0.graph,
         mgv   = graph.mgv           DO
      LOCK graph.mu DO
        LOCK mgv.mu DO
          edge.graph := graph;
          <*ASSERT edge.vertex1.graph = graph*>

          edge.straight := (edge.control0 = NIL) AND (edge.control1 = NIL);
          IF edge.control0 # NIL THEN
            <*ASSERT edge.control0.graph = graph*>
          END;
          IF edge.control1 # NIL THEN
            <*ASSERT edge.control1.graph = graph*>
          END;

          edge.colorScheme :=
            PaintOp.MakeColorScheme(edge.color, edge.color);

          IF edge.straight THEN
            edge.mg :=
              NEW(MG.Line, weight := Pts.FromMM(edge.width),
                  color := edge.colorScheme).init(R2.Origin, R2.Origin);
            edge.end[0] := NEW(MG.LineEnd, line := edge.mg,
                               controlsFrom := TRUE).init();
            edge.end[1] := NEW(MG.LineEnd, line := edge.mg,
                               controlsFrom := FALSE).init();
            edge.isLine := TRUE;
          ELSE
            VAR path := NEW(R2Path.T);
            BEGIN
              path.init();
              edge.mg := NEW(MG.Shape, weight := Pts.FromMM(edge.width),
                             color := edge.colorScheme).init(
                           R2.Origin, path, fill := FALSE);
            END;
            edge.end[0] := NIL;
            edge.end[1] := NIL;
            edge.isLine := FALSE;
          END;

          FOR i := 0 TO 1 DO
            IF edge.arrow[i] THEN
              FOR j := 0 TO 1 DO
                edge.arrowLine[i][j] := NEW(
                                          MG.Line,
                                          weight := Pts.FromMM(edge.width),
                                          color := edge.colorScheme).init(
                                          R2.Origin, R2.Origin);
                edge.arrowEnd[i][j][0] :=
                  NEW(MG.LineEnd, line := edge.arrowLine[i][j],
                      controlsFrom := TRUE).init();
                edge.arrowEnd[i][j][1] :=
                  NEW(MG.LineEnd, line := edge.arrowLine[i][j],
                      controlsFrom := FALSE).init();
              END;
            END;
          END;

          FOR i := 0 TO 1 DO
            FOR j := 0 TO 1 DO
              FOR k := 0 TO 1 DO edge.arrowPos[i][j][k] := R2.Origin; END;
            END;
          END;

          edge.new.vertex0 := edge.vertex0;
          edge.new.vertex1 := edge.vertex1;

          RefListUtils.Push(edge.vertex0.edges, edge);
          RefListUtils.Push(edge.vertex1.edges, edge);

          edge.new.control0 := edge.control0;
          edge.new.control1 := edge.control1;

          IF edge.control0 # NIL THEN
            RefListUtils.Push(edge.control0.edges, edge);
          END;
          IF edge.control1 # NIL THEN
            RefListUtils.Push(edge.control1.edges, edge);
          END;

          edge.group := NEW(MG.Group).init();
          edge.currentGroup := graph.edgeGroup;
          edge.currentGroup.addBefore(mgv, edge.group, NIL);
          IF edge.isLine THEN
            edge.group.addBefore(mgv, edge.end[0], NIL);
            edge.group.addBefore(mgv, edge.end[1], NIL);
          ELSE
            edge.group.addBefore(mgv, edge.mg, NIL);
          END;

          FOR i := 0 TO 1 DO
            IF edge.arrow[i] THEN
              FOR j := 0 TO 1 DO
                FOR k := 0 TO 1 DO
                  edge.group.addBefore(mgv, edge.arrowEnd[i][j][k], NIL);
                END;
              END;
            END;
          END;

          RefListUtils.Push(graph.edges, edge);

          RefreshEdge(edge);
          edge.initialized := TRUE;
        END;
      END;
    END;
    RETURN edge;
  END InitEdge;

<* LL.sup < vertexHighlight.vertex.graph.mu *>

PROCEDURE InitVertexHighlight (vertexHighlight: VertexHighlight):
  VertexHighlight =
  BEGIN
    IF vertexHighlight.initialized THEN RETURN vertexHighlight; END;

    WITH graph = vertexHighlight.vertex.graph,
         mgv   = graph.mgv                     DO
      LOCK graph.mu DO
        LOCK mgv.mu DO
          vertexHighlight.graph := graph;

          vertexHighlight.colorScheme :=
            PaintOp.MakeColorScheme(vertexHighlight.color, PaintOp.Fg);

          vertexHighlight.size := R2.Origin;

          vertexHighlight.shape := vertexHighlight.vertex.shape;
          CASE vertexHighlight.shape OF
          | VertexShape.Rectangle =>
              vertexHighlight.mg :=
                NEW(MG.Rectangle, color := vertexHighlight.colorScheme,
                    weight := 0.0).init(R2.Origin, R2.Origin);
          | VertexShape.Ellipse =>
              vertexHighlight.mg :=
                NEW(MG.Ellipse, color := vertexHighlight.colorScheme,
                    weight := 0.0).init(R2.Origin, R2.Origin);
          END;

          vertexHighlight.group := NEW(MG.Group).init();
          vertexHighlight.currentGroup := graph.vertexHighlightGroup;

          vertexHighlight.pos := vertexHighlight.vertex.pos;

          vertexHighlight.new.vertex := vertexHighlight.vertex;
          RefListUtils.Push(
            vertexHighlight.vertex.vertexHighlights, vertexHighlight);

          vertexHighlight.group.addBefore(mgv, vertexHighlight.mg, NIL);
          vertexHighlight.currentGroup.addBefore(
            mgv, vertexHighlight.group, NIL);

          RefListUtils.Push(graph.vertexHighlights, vertexHighlight);

          VAR centerPP := WorldPosToPts(graph, vertexHighlight.vertex.pos);
          BEGIN
            MG.TranslateToLocked(
              vertexHighlight.mg, mgv, Finite2(centerPP), center := TRUE);
          END;
          EVAL AdjustVertexHighlightSizeandShape(vertexHighlight);
          vertexHighlight.initialized := TRUE;
        END;
      END;
    END;
    RETURN vertexHighlight;
  END InitVertexHighlight;

<* LL.sup < RefList.First(polygon.vertices).graph.mu *>

PROCEDURE InitPolygon (polygon: Polygon): Polygon =
  BEGIN
    IF polygon.initialized THEN RETURN polygon; END;

    <*ASSERT polygon.vertices # NIL*>
    WITH graph = NARROW(polygon.vertices.head, Vertex).graph,
         mgv   = graph.mgv                                    DO
      LOCK graph.mu DO
        LOCK mgv.mu DO
          polygon.graph := graph;
          VAR vertices := polygon.vertices.tail;
          BEGIN
            WHILE vertices # NIL DO
              TYPECASE RefListUtils.Pop(vertices) OF
              | Vertex (vertex) =>
                <*ASSERT vertex.graph = graph*>
              | RefList.T (v2) =>
                  <* ASSERT RefList.Length(v2) = 3 *>
                  WHILE v2 # NIL DO
                    VAR vertex: Vertex := RefListUtils.Pop(v2);
                    BEGIN
                      <*ASSERT vertex.graph = graph*>
                    END;
                  END;
              ELSE               <* ASSERT FALSE *>
              END;
            END;
          END;

          polygon.colorScheme :=
            PaintOp.MakeColorScheme(polygon.color, polygon.color);

          VAR path := NEW(R2Path.T);
          BEGIN
            path.init();
            polygon.mg := NEW(MG.Shape, weight := 0.0,
                              color := polygon.colorScheme).init(
                            R2.Origin, path, fill := FALSE);
          END;

          polygon.new.vertices := polygon.vertices;

          VAR vertices := polygon.vertices;
          BEGIN
            WHILE vertices # NIL DO
              TYPECASE RefListUtils.Pop(vertices) OF
              | Vertex (vertex) =>
                RefListUtils.Push(vertex.polygons, polygon);
              | RefList.T (v2) =>
                  WHILE v2 # NIL DO
                    VAR vertex: Vertex := RefListUtils.Pop(v2);
                    BEGIN
                      RefListUtils.Push(vertex.polygons, polygon);
                    END;
                  END;
              ELSE               <* ASSERT FALSE *>
              END;
            END;
          END;

          polygon.group := NEW(MG.Group).init();
          polygon.currentGroup := graph.polygonGroup;

          polygon.group.addBefore(mgv, polygon.mg, NIL);
          polygon.currentGroup.addBefore(mgv, polygon.group, NIL);
          RefListUtils.Push(graph.polygons, polygon);

          RefreshPolygon(polygon);
          polygon.initialized := TRUE;
        END;
      END;
    END;
    RETURN polygon;
  END InitPolygon;

<* LL.sup < graph.mu *>

PROCEDURE AnimateGraph (graph: T; t0, t1: REAL) RAISES {Thread.Alerted} =
  BEGIN
    LOCK graph.mu DO
      LOCK graph.mgv.mu DO
        graph.animations := graph.animations + 1;
        VAR
          tz := t0 / t1;
          a  := 1.0 / (1.0 - tz);
          b  := -tz / (1.0 - tz);
        BEGIN
          IF NOT RealFloat.Finite(a + b) THEN a := 1.0; b := 0.0; END;
          VAR tf := NEW(AffineTimeFunction, a := a, b := b);
          BEGIN
            AnimateGraphVertices(graph, tf);
            AnimateGraphEdges(graph, tf);
            AnimateGraphVertexHighlights(graph, tf);
            AnimateGraphPolygons(graph, tf);
          END;
        END;
      END;
    END;
    MGV.Animation(graph.mgv, t1);
    VAR redisplay := FALSE;
    BEGIN
      LOCK graph.mu DO
        LOCK graph.mgv.mu DO
          graph.animations := graph.animations - 1;
          IF graph.animations > 0 THEN
            graph.needRefresh := TRUE;
          ELSIF graph.needRefresh THEN
            RefreshGraph(graph, FALSE);
          END;
          PostAnimateGraphVertices(graph);
          IF PostAnimateGraphEdges(graph) THEN redisplay := TRUE; END;
          IF PostAnimateGraphVertexHighlights(graph) THEN
            redisplay := TRUE;
          END;
          IF PostAnimateGraphPolygons(graph) THEN redisplay := TRUE; END;
        END;
      END;
      IF redisplay THEN graph.redisplay(); END;
    END;
  END AnimateGraph;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE AnimateGraphVertices (graph: T; tf: Animate.TimeFunction) =
  VAR vertices := graph.vertices;
  BEGIN
    WHILE vertices # NIL DO
      VAR vertex: Vertex := RefListUtils.Pop(vertices);
      BEGIN
        IF vertex.animated THEN
          IF vertex.path # NIL THEN
            VAR
              path: AlongGivenPath := NEW(AlongGivenPath, graph := graph,
                                          pos := vertex.pos,
                                          path := vertex.path).init(tf);
            BEGIN
              MGV.AddAnimationLocked(graph.mgv, path, vertex.mg);
            END;
          ELSE
            VAR linear: Animate.Linear := NEW(Animate.Linear).init(tf);
            BEGIN
              linear.setVector(
                graph.mgv,
                WorldSizeToPts(graph, R2.Sub(vertex.new.pos, vertex.pos)));
              MGV.AddAnimationLocked(graph.mgv, linear, vertex.mg);
            END;
          END;
          vertex.pos := vertex.new.pos;
        END;
      END;
    END;
  END AnimateGraphVertices;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE AnimateGraphEdges (graph: T; tf: Animate.TimeFunction) =
  VAR edges := graph.edges;
  BEGIN
    WHILE edges # NIL DO
      VAR
        edge       : Edge := RefListUtils.Pop(edges);
        oldVertex0        := edge.vertex0;
        oldVertex1        := edge.vertex1;
        oldControl0       := edge.control0;
        oldControl1       := edge.control1;
        oldPos0           := edge.pos[0];
        oldPos1           := edge.pos[1];
      BEGIN
        IF edge.vertex0 # edge.new.vertex0
             OR edge.vertex1 # edge.new.vertex1
             OR edge.control0 # edge.new.control0
             OR edge.control1 # edge.new.control1 THEN

          RefListUtils.DeleteQ(edge.vertex0.edges, edge);
          RefListUtils.DeleteQ(edge.vertex1.edges, edge);

          IF edge.control0 # NIL THEN
            RefListUtils.DeleteQ(edge.control0.edges, edge);
          END;
          IF edge.control1 # NIL THEN
            RefListUtils.DeleteQ(edge.control1.edges, edge);
          END;

          edge.vertex0 := edge.new.vertex0;
          edge.vertex1 := edge.new.vertex1;

          RefListUtils.Push(edge.vertex0.edges, edge);
          RefListUtils.Push(edge.vertex1.edges, edge);

          edge.control0 := edge.new.control0;
          edge.control1 := edge.new.control1;
          edge.straight := (edge.control0 = NIL) AND (edge.control1 = NIL);

          IF edge.control0 # NIL THEN
            RefListUtils.Push(edge.control0.edges, edge);
          END;
          IF edge.control1 # NIL THEN
            RefListUtils.Push(edge.control1.edges, edge);
          END;
        END;

        IF NOT ((edge.vertex0 = oldVertex0 AND NOT edge.vertex0.animated)
                  AND (edge.vertex1 = oldVertex1
                         AND NOT edge.vertex1.animated)
                  AND (edge.control0 = oldControl0
                         AND (edge.control0 = NIL
                                OR NOT edge.control0.animated))
                  AND (edge.control1 = oldControl1
                         AND (edge.control1 = NIL
                                OR NOT edge.control1.animated))
                  AND edge.isLine = edge.straight) THEN
          IF NOT edge.straight AND edge.isLine THEN
            (* was straight, now curved.  change to straight bezier before
               animation. *)
            edge.group.remove(graph.mgv, edge.end[0]);
            edge.group.remove(graph.mgv, edge.end[1]);
            VAR path := NEW(R2Path.T);
            BEGIN
              path.init();
              edge.mg := NEW(MG.Shape, weight := Pts.FromMM(edge.width),
                             color := edge.colorScheme).init(
                           R2.Origin, path, fill := FALSE);
            END;
            edge.end[0] := NIL;
            edge.end[1] := NIL;
            edge.isLine := FALSE;
            edge.group.addBefore(graph.mgv, edge.mg, NIL);
          END;

          IF edge.isLine THEN
            IF edge.vertex0 = oldVertex0 AND edge.vertex0.animated
                 AND edge.vertex0.path # NIL THEN
              VAR
                path: AlongGivenPath := NEW(
                                          AlongGivenPath, graph := graph,
                                          pos := edge.pos[0],
                                          path := edge.vertex0.path).init(
                                          tf);
              BEGIN
                MGV.AddAnimationLocked(graph.mgv, path, edge.end[0]);
              END;
            ELSIF edge.vertex0 # oldVertex0 OR edge.vertex0.animated THEN
              VAR linear: Animate.Linear := NEW(Animate.Linear).init(tf);
              BEGIN
                linear.setVector(
                  graph.mgv, WorldSizeToPts(graph, R2.Sub(edge.vertex0.new.pos,
                                                      edge.pos[0])));
                MGV.AddAnimationLocked(graph.mgv, linear, edge.end[0]);
              END;
            END;
            IF edge.vertex1 = oldVertex1 AND edge.vertex1.animated
                 AND edge.vertex1.path # NIL THEN
              VAR
                path: AlongGivenPath := NEW(
                                          AlongGivenPath, graph := graph,
                                          pos := edge.pos[1],
                                          path := edge.vertex1.path).init(
                                          tf);
              BEGIN
                MGV.AddAnimationLocked(graph.mgv, path, edge.end[1]);
              END;
            ELSIF edge.vertex1 # oldVertex1 OR edge.vertex1.animated THEN
              VAR linear: Animate.Linear := NEW(Animate.Linear).init(tf);
              BEGIN
                linear.setVector(
                  graph.mgv, WorldSizeToPts(graph, R2.Sub(edge.vertex1.new.pos,
                                                      edge.pos[1])));
                MGV.AddAnimationLocked(graph.mgv, linear, edge.end[1]);
              END;
            END;
          ELSE
            VAR bezierAnimation: BezierAnimation;
            BEGIN
              bezierAnimation := NEW(BezierAnimation, graph := graph);
              IF edge.vertex0 = oldVertex0 AND edge.vertex0.animated
                   AND edge.vertex0.path # NIL THEN
                bezierAnimation.pathA := edge.vertex0.path;
              ELSE
                bezierAnimation.pathA :=
                  NEW(StraightPath, p0 := edge.pos[0],
                      p1 := edge.vertex0.new.pos);
              END;
              IF edge.control0 # NIL THEN
                IF edge.control0 = oldControl0 AND edge.control0.animated
                     AND edge.control0.path # NIL THEN
                  bezierAnimation.pathB := edge.control0.path;
                ELSE
                  bezierAnimation.pathB :=
                    NEW(StraightPath, p0 := edge.cpos[0],
                        p1 := edge.control0.new.pos);
                END;
              ELSE
                bezierAnimation.pathB :=
                  NEW(StraightPath, p0 := edge.cpos[0],
                      p1 := edge.vertex0.new.pos);
              END;
              IF edge.control1 # NIL THEN
                IF edge.control1 = oldControl1 AND edge.control1.animated
                     AND edge.control1.path # NIL THEN
                  bezierAnimation.pathC := edge.control1.path;
                ELSE
                  bezierAnimation.pathC :=
                    NEW(StraightPath, p0 := edge.cpos[1],
                        p1 := edge.control1.new.pos);
                END;
              ELSE
                bezierAnimation.pathC :=
                  NEW(StraightPath, p0 := edge.cpos[1],
                      p1 := edge.vertex1.new.pos);
              END;
              IF edge.vertex1 = oldVertex1 AND edge.vertex1.animated
                   AND edge.vertex1.path # NIL THEN
                bezierAnimation.pathD := edge.vertex1.path;
              ELSE
                bezierAnimation.pathD :=
                  NEW(StraightPath, p0 := edge.pos[1],
                      p1 := edge.vertex1.new.pos);
              END;
              bezierAnimation := bezierAnimation.init(tf);
              MGV.AddAnimationLocked(graph.mgv, bezierAnimation, edge.mg);
            END;
          END;
          edge.pos[0] := edge.vertex0.new.pos;
          edge.pos[1] := edge.vertex1.new.pos;
          IF edge.control0 # NIL THEN
            edge.cpos[0] := edge.control0.new.pos;
          ELSE
            edge.cpos[0] := edge.pos[0];
          END;
          IF edge.control1 # NIL THEN
            edge.cpos[1] := edge.control1.new.pos;
          ELSE
            edge.cpos[1] := edge.pos[1];
          END;
          VAR
            new    := ArrowPos(edge);
            vertex := ARRAY [0 .. 1] OF Vertex{edge.vertex0, edge.vertex1};
            oldVertex := ARRAY [0 .. 1] OF Vertex{oldVertex0, oldVertex1};
            oldPos    := ARRAY [0 .. 1] OF R2.T{oldPos0, oldPos1};
          BEGIN
            FOR i := 0 TO 1 DO
              IF edge.arrow[i] THEN
                IF vertex[i] = oldVertex[i] AND vertex[i].animated
                     AND vertex[i].path # NIL THEN
                  VAR
                    oldOffset, newOffset: ARRAY [0 .. 1], [0 .. 1] OF R2.T;
                  BEGIN
                    FOR j := 0 TO 1 DO
                      FOR k := 0 TO 1 DO
                        oldOffset[j][k] :=
                          R2.Sub(edge.arrowPos[i][j][k], oldPos[i]);
                        newOffset[j][k] :=
                          R2.Sub(new[i][j][k], edge.pos[i]);
                      END;
                    END;
                    FOR j := 0 TO 1 DO
                      FOR k := 0 TO 1 DO
                        VAR
                          path: AlongGivenPath := NEW(AlongGivenPath,
                                                      graph := graph,
                                                      pos := edge.arrowPos[
                                                               i][j][k],
                                                      path :=
                                                        NEW(OffsetPath,
                                                            path :=
                                                              vertex[
                                                                i].path,
                                                            offset0 :=
                                                              oldOffset[j][
                                                                k],
                                                            offset1 :=
                                                              newOffset[j][
                                                                k])).init(
                                                    tf);
                        BEGIN
                          MGV.AddAnimationLocked(
                            graph.mgv, path, edge.arrowEnd[i][j][k]);
                        END;
                      END;
                    END;
                  END;
                ELSE
                  FOR j := 0 TO 1 DO
                    FOR k := 0 TO 1 DO
                      VAR
                        linear: Animate.Linear := NEW(Animate.Linear).init(
                                                    tf);
                      BEGIN
                        linear.setVector(
                          graph.mgv, WorldSizeToPts(graph, R2.Sub(new[i][j][k],
                                                 edge.arrowPos[i][j][k])));
                        MGV.AddAnimationLocked(
                          graph.mgv, linear, edge.arrowEnd[i][j][k]);
                      END;
                    END;
                  END;
                END;
              END;
            END;
            edge.arrowPos := new;
          END;
        END;
      END;
    END;
  END AnimateGraphEdges;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE AnimateGraphVertexHighlights (graph: T; tf: Animate.TimeFunction) =
  VAR vertexHighlights := graph.vertexHighlights;
  BEGIN
    WHILE vertexHighlights # NIL DO
      VAR vertexHighlight: VertexHighlight := RefListUtils.Pop(vertexHighlights);
      BEGIN
        IF vertexHighlight.new.vertex # vertexHighlight.vertex THEN
          VAR animation: Animate.T;
          BEGIN
            VAR
              linearResize: LinearResize;
              center0PP := WorldPosToPts(graph, vertexHighlight.pos);
              center1PP := WorldPosToPts(
                             graph, vertexHighlight.new.vertex.new.pos);
              size0 := R2.T{
                           MAX(ABS(vertexHighlight.vertex.size[0])
                                 + 2.0 * vertexHighlight.border[0], 0.0),
                           MAX(ABS(vertexHighlight.vertex.size[1])
                                 + 2.0 * vertexHighlight.border[1], 0.0)};
              size1 := R2.T{
                           MAX(ABS(vertexHighlight.new.vertex.size[0])
                                 + 2.0 * vertexHighlight.border[0], 0.0),
                           MAX(ABS(vertexHighlight.new.vertex.size[1])
                                 + 2.0 * vertexHighlight.border[1], 0.0)};
            BEGIN
              linearResize := NEW(LinearResize);
              linearResize.graph := graph;
              linearResize.shape := vertexHighlight.vertex.shape;
              linearResize.corner0[0] := SWFromCenter(center0PP, size0);
              linearResize.corner0[1] := SWFromCenter(center1PP, size1);
              linearResize.corner1[0] := NEFromCenter(center0PP, size0);
              linearResize.corner1[1] := NEFromCenter(center1PP, size1);
              EVAL linearResize.init(tf);
              animation := linearResize;
            END;
            MGV.AddAnimationLocked(graph.mgv, animation, vertexHighlight.mg);
          END;
          vertexHighlight.pos := vertexHighlight.new.vertex.new.pos;
        ELSIF vertexHighlight.vertex.animated THEN
          IF vertexHighlight.vertex.path # NIL THEN
            VAR
              path: AlongGivenPath := NEW(
                                        AlongGivenPath, graph := graph,
                                        pos := vertexHighlight.pos,
                                        path := vertexHighlight.vertex.path).init(
                                        tf);
            BEGIN
              MGV.AddAnimationLocked(graph.mgv, path, vertexHighlight.mg);
            END;
          ELSE
            VAR linear: Animate.Linear := NEW(Animate.Linear).init(tf);
            BEGIN
              linear.setVector(
                graph.mgv, WorldSizeToPts(graph, R2.Sub(vertexHighlight.vertex.new.pos,
                                       vertexHighlight.pos)));
              MGV.AddAnimationLocked(graph.mgv, linear, vertexHighlight.mg);
            END;
          END;
          vertexHighlight.pos := vertexHighlight.vertex.new.pos;
        END;
        IF vertexHighlight.vertex # vertexHighlight.new.vertex THEN
          RefListUtils.DeleteQ(
              vertexHighlight.vertex.vertexHighlights, vertexHighlight);
          vertexHighlight.vertex := vertexHighlight.new.vertex;
          RefListUtils.Push(
            vertexHighlight.vertex.vertexHighlights, vertexHighlight);
        END;
      END;
    END;
  END AnimateGraphVertexHighlights;

PROCEDURE StraightAnimatePath ( newV : Vertex;
                                newP : REF R2.T;
                                oldV : Vertex;
                                oldP : REF R2.T   ): AnimationPath =
  BEGIN
    IF newV = oldV AND newV.animated AND newV.path # NIL THEN
      RETURN newV.path;
    ELSE
      RETURN NEW(StraightPath, p0 := oldP^, p1 := newP^);
    END;
  END StraightAnimatePath;

PROCEDURE CurvedAnimatePath (newVs, newPs, oldVs, oldPs: RefList.T):
  RefList.T =                    (* of AnimationPath *)
  VAR res: RefList.T := NIL;
  BEGIN
    FOR i := 0 TO 2 DO
      RefListUtils.Push(
        res, StraightAnimatePath(
               RefListUtils.Pop(newVs), RefListUtils.Pop(newPs),
               RefListUtils.Pop(oldVs), RefListUtils.Pop(oldPs)))
    END;
    RETURN RefList.ReverseD(res);
  END CurvedAnimatePath;

PROCEDURE VList(v1, v2: Vertex): RefList.T =
  BEGIN
  RETURN RefList.List3(v1, v2, v2)
  END VList;

PROCEDURE PList(p1, p2: REF R2.T): RefList.T =
  BEGIN
  RETURN RefList.List3(p1, p2, p2)
  END PList;

<* LL.sup >= graph.mu, graph.mgv.mu *>
PROCEDURE AnimateGraphPolygons (graph: T; tf: Animate.TimeFunction) =
  VAR polygons := graph.polygons;
  BEGIN
    WHILE polygons # NIL DO
      VAR
        polygon     : Polygon := RefListUtils.Pop(polygons);
        oldVertices           := polygon.vertices;
        oldPositions          := polygon.pos;
      BEGIN
        IF NOT RefListUtils.Equal(polygon.vertices, polygon.new.vertices) THEN
          VAR vertices := polygon.vertices;
          BEGIN
            WHILE vertices # NIL DO
              TYPECASE RefListUtils.Pop(vertices) OF
              | Vertex (vertex) =>
                  RefListUtils.DeleteQ(vertex.polygons, polygon);
              | RefList.T (v2) =>
                  WHILE v2 # NIL DO
                    VAR vertex: Vertex := RefListUtils.Pop(v2);
                    BEGIN
                      RefListUtils.DeleteQ(vertex.polygons, polygon);
                    END;
                  END;
              ELSE               <* ASSERT FALSE *>
              END;
            END;
          END;

          polygon.vertices := polygon.new.vertices;

          VAR vertices := polygon.vertices;
          BEGIN
            WHILE vertices # NIL DO
              TYPECASE RefListUtils.Pop(vertices) OF
              | Vertex (vertex) =>
                  RefListUtils.Push(vertex.polygons, polygon);
              | RefList.T (v2) =>
                  WHILE v2 # NIL DO
                    VAR vertex: Vertex := RefListUtils.Pop(v2);
                    BEGIN
                      RefListUtils.Push(vertex.polygons, polygon);
                    END;
                  END;
              ELSE               <* ASSERT FALSE *>
              END;
            END;
          END;
        END;

        VAR
          vertices             := polygon.vertices;
          positions: RefList.T := NIL;
        BEGIN
          WHILE vertices # NIL DO
            TYPECASE RefListUtils.Pop(vertices) OF
            | Vertex (vertex) =>
                RefListUtils.Push(positions, NewR2(vertex.new.pos));
            | RefList.T (v2) =>
                VAR pos2: RefList.T := NIL;
                BEGIN
                  WHILE v2 # NIL DO
                    VAR vertex: Vertex := RefListUtils.Pop(v2);
                    BEGIN
                      RefListUtils.Push(pos2, NewR2(vertex.new.pos));
                    END;
                  END;
                  RefListUtils.Push(positions, RefList.ReverseD(pos2));
                END;
            ELSE                 <* ASSERT FALSE *>
            END;
          END;
          polygon.pos := RefList.ReverseD(positions);
        END;

        VAR anyAnimated := FALSE;
        BEGIN
          VAR vertices := polygon.vertices;
          BEGIN
            WHILE vertices # NIL AND NOT anyAnimated DO
              TYPECASE RefListUtils.Pop(vertices) OF
              | Vertex (vertex) =>
                  IF vertex.animated THEN anyAnimated := TRUE; EXIT; END;
              | RefList.T (v2) =>
                  <* ASSERT RefList.Length(v2) = 3 *>
                  WHILE v2 # NIL DO
                    VAR vertex: Vertex := RefListUtils.Pop(v2);
                    BEGIN
                      IF vertex.animated THEN
                        anyAnimated := TRUE;
                        EXIT;
                      END;
                    END;
                  END;
              ELSE               <* ASSERT FALSE *>
              END;
            END;
          END;
          IF NOT RefListUtils.Equal(polygon.vertices, oldVertices)
               OR anyAnimated THEN
            VAR paths: RefList.T := NIL;
            BEGIN
              VAR
                list                           := polygon.vertices;
                oldList                        := oldVertices;
                positionList                   := polygon.pos;
                oldPositionList                := oldPositions;
                old, new, oldP, newP: REFANY;
                prevNewV, prevOldV  : Vertex;
                prevNewP, prevOldP  : REF R2.T;
              BEGIN
                WHILE list # NIL OR oldList # NIL DO
                  IF list # NIL THEN
                    new := RefListUtils.Pop(list);
                    newP := RefListUtils.Pop(positionList);
                  END;
                  IF oldList # NIL THEN
                    old := RefListUtils.Pop(oldList);
                    oldP := RefListUtils.Pop(oldPositionList);
                  END;
                  TYPECASE new OF
                  | Vertex (newV) =>
                      TYPECASE old OF
                      | Vertex (oldV) =>
                          RefListUtils.Push(
                            paths,
                            StraightAnimatePath(newV, newP, oldV, oldP));
                          prevOldV := oldV;
                          prevOldP := oldP;
                      | RefList.T =>
                          RefListUtils.Push(
                            paths, CurvedAnimatePath(
                                     VList(prevNewV, newV),
                                     PList(prevNewP, newP), old, oldP));
                          prevOldV := RefList.Nth(old, 2);
                          prevOldP := RefList.Nth(oldP, 2);
                      ELSE       <* ASSERT FALSE *>
                      END;
                      prevNewV := newV;
                      prevNewP := newP;
                  | RefList.T =>
                      TYPECASE old OF
                      | Vertex (oldV) =>
                          RefListUtils.Push(
                            paths, CurvedAnimatePath(
                                     new, newP, VList(prevOldV, oldV),
                                     PList(prevOldP, oldP)));
                          prevOldV := oldV;
                          prevOldP := oldP;
                      | RefList.T =>
                          RefListUtils.Push(
                            paths, CurvedAnimatePath(new, newP, old, oldP));
                          prevOldV := RefList.Nth(old, 2);
                          prevOldP := RefList.Nth(oldP, 2);
                      ELSE       <* ASSERT FALSE *>
                      END;
                      prevNewV := RefList.Nth(new, 2);
                      prevNewP := RefList.Nth(newP, 2);
                  ELSE           <* ASSERT FALSE *>
                  END;
                END;
              END;
              paths := RefList.ReverseD(paths);
              VAR
                polygonAnimation := NEW(PolygonAnimation, graph := graph,
                                        paths := paths);
              BEGIN
                polygonAnimation := polygonAnimation.init(tf);
                MGV.AddAnimationLocked(
                  graph.mgv, polygonAnimation, polygon.mg);
              END;
            END;
          END;
        END;
      END;
    END;
  END AnimateGraphPolygons;
<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE PostAnimateGraphVertices (graph: T) =
  VAR vertices := graph.vertices;
  BEGIN
    WHILE vertices # NIL DO
      VAR vertex: Vertex := RefListUtils.Pop(vertices);
      BEGIN
        vertex.animated := FALSE;
      END;
    END;
  END PostAnimateGraphVertices;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE PostAnimateGraphEdges (graph: T): BOOLEAN =
  VAR
    edges     := graph.edges;
    redisplay := FALSE;
  BEGIN
    WHILE edges # NIL DO
      VAR edge: Edge := RefListUtils.Pop(edges);
      BEGIN
        IF edge.straight AND NOT edge.isLine THEN
          (* was curved, now straight.  replace with straight line. *)
          edge.group.remove(graph.mgv, edge.mg);
          edge.mg :=
            NEW(MG.Line, weight := Pts.FromMM(edge.width),
                color := edge.colorScheme).init(R2.Origin, R2.Origin);
          edge.end[0] :=
            NEW(MG.LineEnd, line := edge.mg, controlsFrom := TRUE).init();
          edge.end[1] :=
            NEW(MG.LineEnd, line := edge.mg, controlsFrom := FALSE).init();
          edge.isLine := TRUE;
          edge.group.addBefore(graph.mgv, edge.end[0], NIL);
          edge.group.addBefore(graph.mgv, edge.end[1], NIL);
          RefreshEdge(edge);
          redisplay := TRUE;
        END;
      END;
    END;
    RETURN redisplay;
  END PostAnimateGraphEdges;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE PostAnimateGraphVertexHighlights (graph: T): BOOLEAN =
  VAR
    vertexHighlights := graph.vertexHighlights;
    redisplay        := FALSE;
  BEGIN
    WHILE vertexHighlights # NIL DO
      VAR vertexHighlight: VertexHighlight := RefListUtils.Pop(vertexHighlights);
      BEGIN
        IF AdjustVertexHighlightSizeandShape(vertexHighlight) THEN
          redisplay := TRUE;
        END;
      END;
    END;
    RETURN redisplay;
  END PostAnimateGraphVertexHighlights;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE PostAnimateGraphPolygons (<* UNUSED *> graph: T): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END PostAnimateGraphPolygons;

<* LL.sup >= graph.mu *>

PROCEDURE ClearGraph (graph: T) =
  BEGIN
    WHILE graph.vertices # NIL DO
      VAR vertex: Vertex := graph.vertices.head;
      BEGIN
        vertex.remove();
      END;
    END;
    LOCK graph.mgv.mu DO
      graph.mgv.animations := NIL
    END;
  END ClearGraph;

PROCEDURE PixelRectToWorld (              graph       : T;
                            READONLY      r           : Rect.T;
                            VAR (* OUT *) hRect, vRect: R2.T    ) =
  VAR
    c1 := ScreenPtToWorldPos(graph, Rect.NorthWest(r));
    c2 := ScreenPtToWorldPos(graph, Rect.SouthEast(r));
  BEGIN
    hRect[0] := MIN(c1[0], c2[0]);
    hRect[1] := MAX(c1[0], c2[0]);
    vRect[0] := MIN(c1[1], c2[1]);
    vRect[1] := MAX(c1[1], c2[1]);
  END PixelRectToWorld;

<* LL.sup >= graph.mu *>

PROCEDURE GraphVerticesAt (graph: T; pixelRect: Rect.T):
  RefList.T (* OF Vertex *) =
  VAR
    vertices             := graph.vertices;
    list        : RefList.T := NIL;
    hRect, vRect: R2.T;
  BEGIN
    LOCK graph.mgv.mu DO
      PixelRectToWorld(graph, pixelRect, hRect, vRect);
      WHILE vertices # NIL DO
        VAR
          vertex: Vertex := RefListUtils.Pop(vertices);
          center         := vertex.pos;
          size           := Abs(vertex.size);
          hVertex := R2.T{center[0] - size[0] / 2.0,
                          center[0] + size[0] / 2.0};
          vVertex := R2.T{center[1] - size[1] / 2.0,
                          center[1] + size[1] / 2.0};
        BEGIN
          IF R2Intersect(hRect, hVertex) AND R2Intersect(vRect, vVertex) THEN
            CASE vertex.shape OF
            | VertexShape.Rectangle => RefListUtils.Push(list, vertex);
            | VertexShape.Ellipse =>
                IF hRect[0] < hVertex[0] AND hVertex[1] < hRect[1]
                     OR vRect[0] < vVertex[0] AND vVertex[1] < vRect[1] THEN
                  RefListUtils.Push(list, vertex);
                ELSE
                  VAR
                    dx0 := (center[0] - hRect[0]) / size[0];
                    dx1 := (center[0] - hRect[1]) / size[0];
                    dy0 := (center[1] - vRect[0]) / size[1];
                    dy1 := (center[1] - vRect[1]) / size[1];
                  BEGIN
                    IF dx0 * dx0 + dy0 * dy0 < 0.25
                         OR dx0 * dx0 + dy1 * dy1 < 0.25
                         OR dx1 * dx1 + dy0 * dy0 < 0.25
                         OR dx1 * dx1 + dy1 * dy1 < 0.25 THEN
                      RefListUtils.Push(list, vertex);
                    END;
                  END;
                END;
            END;
          END;
        END;
      END;
    END;
    RETURN RefList.ReverseD(list);
  END GraphVerticesAt;

<* LL.sup >= graph.mu *>

PROCEDURE GraphEdgesAt (graph: T; pixelRect: Rect.T):
  RefList.T (* OF Edge *) =
  VAR
    edges                := graph.edges;
    list        : RefList.T := NIL;
    hRect, vRect: R2.T;
  BEGIN
    LOCK graph.mgv.mu DO
      PixelRectToWorld(graph, pixelRect, hRect, vRect);
      WHILE edges # NIL DO
        VAR edge: Edge := RefListUtils.Pop(edges);
        BEGIN
          IF EdgeInBox(edge, hRect, vRect) THEN RefListUtils.Push(list, edge); END;
        END;
      END;
    END;
    RETURN list;
  END GraphEdgesAt;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE EdgeInBox (edge: Edge; hBox, vBox: R2.T): BOOLEAN =
  BEGIN
    WITH graph = edge.graph,
         slop  = ScreenPtToWorldPos(graph, Point.T{2, 2}) DO
      hBox := R2.T{hBox[0] - MAX(ABS(edge.width) / 2.0, slop[0]),
                   hBox[1] + MAX(ABS(edge.width) / 2.0, slop[0])};
      vBox := R2.T{vBox[0] - MAX(ABS(edge.width) / 2.0, slop[1]),
                   vBox[1] + MAX(ABS(edge.width) / 2.0, slop[1])};
      IF edge.straight THEN
        RETURN StraightEdgeInBox(edge, hBox, vBox);
      ELSE
        RETURN BezierEdgeInBox(edge, hBox, vBox);
      END;
    END;
  END EdgeInBox;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE StraightEdgeInBox (edge: Edge; READONLY hBox, vBox: R2.T):
  BOOLEAN =
  BEGIN
    RETURN LineInBox(edge.pos[0], edge.pos[1], hBox, vBox)
  END StraightEdgeInBox;

PROCEDURE LineInBox (READONLY pos0, pos1, hBox, vBox: R2.T):
  BOOLEAN =
  BEGIN
    VAR
      hEdge := R2.T{MIN(pos0[0], pos1[0]), MAX(pos0[0], pos1[0])};
      vEdge := R2.T{MIN(pos0[1], pos1[1]), MAX(pos0[1], pos1[1])};
    BEGIN
      IF NOT (R2Intersect(hBox, hEdge) AND R2Intersect(vBox, vEdge)) THEN
        RETURN FALSE;
      END;
    END;
    VAR
      x0 := pos0[0];
      y0 := pos0[1];
      x1 := pos1[0];
      y1 := pos1[1];
    BEGIN
      IF hBox[0] <= x0 AND x0 <= hBox[1] AND vBox[0] <= y0
           AND y0 <= vBox[1] OR hBox[0] <= x1 AND x1 <= hBox[1]
                                    AND vBox[0] <= y1 AND y1 <= vBox[1] THEN
        RETURN TRUE;
      END;
      VAR t := (hBox[0] - x0) / (x1 - x0);
      BEGIN
        IF 0.0 <= t AND t <= 1.0 THEN
          VAR y := y0 + (y1 - y0) * t;
          BEGIN
            IF vBox[0] <= y AND y <= vBox[1] THEN RETURN TRUE; END;
          END;
        END;
      END;
      VAR t := (hBox[1] - x0) / (x1 - x0);
      BEGIN
        IF 0.0 <= t AND t <= 1.0 THEN
          VAR y := y0 + (y1 - y0) * t;
          BEGIN
            IF vBox[0] <= y AND y <= vBox[1] THEN RETURN TRUE; END;
          END;
        END;
      END;
      VAR t := (vBox[0] - y0) / (y1 - y0);
      BEGIN
        IF 0.0 <= t AND t <= 1.0 THEN
          VAR x := x0 + (x1 - x0) * t;
          BEGIN
            IF hBox[0] <= x AND x <= hBox[1] THEN RETURN TRUE; END;
          END;
        END;
      END;
    END;
    RETURN FALSE;
  END LineInBox;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE BezierEdgeInBox (edge: Edge; hBox, vBox: R2.T): BOOLEAN =
  PROCEDURE BezierStep (c0, c1, c2, c3: R2.T): BOOLEAN =
    BEGIN
      VAR end := c0;
      BEGIN
        IF (hBox[0] <= end[0] AND end[0] <= hBox[1])
             AND (vBox[0] <= end[1] AND end[1] <= vBox[1]) THEN
          RETURN TRUE;
        END;
      END;
      VAR end := c3;
      BEGIN
        IF (hBox[0] <= end[0] AND end[0] <= hBox[1])
             AND (vBox[0] <= end[1] AND end[1] <= vBox[1]) THEN
          RETURN TRUE;
        END;
      END;
      VAR hBounds := CubicBounds(c0[0], c1[0], c2[0], c3[0]);
      BEGIN
        IF NOT R2Intersect(hBox, hBounds) THEN RETURN FALSE; END;
        VAR vBounds := CubicBounds(c0[1], c1[1], c2[1], c3[1]);
        BEGIN
          IF NOT R2Intersect(vBox, vBounds) THEN RETURN FALSE; END;
          IF ABS(hBounds[1] - hBounds[0]) < hHalfPixel
               AND ABS(vBounds[1] - vBounds[0]) < hHalfPixel THEN
            RETURN FALSE;
          END;
        END;
      END;
      VAR
        d0 := c0;
        d1 := c1;
        d2 := c2;
        d3 := c3;
      BEGIN
        SubCubic2(d0, d1, d2, d3, half := 0);
        IF BezierStep(d0, d1, d2, d3) THEN RETURN TRUE; END;
      END;
      VAR
        d0 := c0;
        d1 := c1;
        d2 := c2;
        d3 := c3;
      BEGIN
        SubCubic2(d0, d1, d2, d3, half := 1);
        IF BezierStep(d0, d1, d2, d3) THEN RETURN TRUE; END;
      END;
      RETURN FALSE;
    END BezierStep;
  VAR hHalfPixel, vHalfPixel: REAL;
  BEGIN
    WITH graph = edge.graph, slop = ScreenPtToWorldPos(graph, Point.T{1, 1}) DO
      hHalfPixel := slop[0] / 2.0;
      vHalfPixel := slop[1] / 2.0;
      RETURN
        BezierStep(edge.pos[0], edge.cpos[0], edge.cpos[1], edge.pos[1]);
    END;
  END BezierEdgeInBox;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE GraphVertexHighlightsAt (graph: T; pixelRect: Rect.T):
  RefList.T (* OF VertexHighlight *) =
  VAR
    vertexHighlights         := graph.vertexHighlights;
    list            : RefList.T := NIL;
    hRect, vRect    : R2.T;
  BEGIN
    LOCK graph.mgv.mu DO
      PixelRectToWorld(graph, pixelRect, hRect, vRect);
      WHILE vertexHighlights # NIL DO
        VAR
          vertexHighlight: VertexHighlight := RefListUtils.Pop(vertexHighlights);
          center                           := vertexHighlight.pos;
          size := R2.T{ABS(vertexHighlight.vertex.size[0])
                         + 2.0 * vertexHighlight.border[0],
                       ABS(vertexHighlight.vertex.size[1])
                         + 2.0 * vertexHighlight.border[1]};
          hHighlight := R2.T{center[0] - size[0] / 2.0,
                             center[0] + size[0] / 2.0};
          vHighlight := R2.T{center[1] - size[1] / 2.0,
                             center[1] + size[1] / 2.0};
        BEGIN
          IF R2Intersect(hRect, hHighlight)
               AND R2Intersect(vRect, vHighlight) THEN
            CASE vertexHighlight.shape OF
            | VertexShape.Rectangle => RefListUtils.Push(list, vertexHighlight);
            | VertexShape.Ellipse =>
                IF hRect[0] < hHighlight[0] AND hHighlight[1] < hRect[1]
                     OR vRect[0] < vHighlight[0]
                          AND vHighlight[1] < vRect[1] THEN
                  RefListUtils.Push(list, vertexHighlight);
                ELSE
                  VAR
                    dx0 := (center[0] - hRect[0]) / (size[0]);
                    dx1 := (center[0] - hRect[1]) / (size[0]);
                    dy0 := (center[1] - vRect[0]) / (size[1]);
                    dy1 := (center[1] - vRect[1]) / (size[1]);
                  BEGIN
                    IF dx0 * dx0 + dy0 * dy0 < 0.25
                         OR dx0 * dx0 + dy1 * dy1 < 0.25
                         OR dx1 * dx1 + dy0 * dy0 < 0.25
                         OR dx1 * dx1 + dy1 * dy1 < 0.25 THEN
                      RefListUtils.Push(list, vertexHighlight);
                    END;
                  END;
                END;
            END;
          END;
        END;
      END;
    END;
    RETURN RefList.ReverseD(list);
  END GraphVertexHighlightsAt;

PROCEDURE GraphPolygonsAt (graph: T; pixelRect: Rect.T):
  RefList.T (* OF Polygon *) =
  VAR
    polygons             := graph.polygons;
    list        : RefList.T := NIL;
    hRect, vRect: R2.T;
  BEGIN
    LOCK graph.mgv.mu DO
      PixelRectToWorld(graph, pixelRect, hRect, vRect);
      WHILE polygons # NIL DO
        VAR polygon: Polygon := RefListUtils.Pop(polygons);
        BEGIN
          IF PolygonInBox(polygon, hRect, vRect) THEN
            RefListUtils.Push(list, polygon);
          END;
        END;
      END;
      RETURN list;
    END;
  END GraphPolygonsAt;

PROCEDURE Flatten (list: RefList.T): RefList.T =
  VAR res: RefList.T := NIL;
  BEGIN
    WHILE list # NIL DO
      TYPECASE list.head OF
      | Vertex(v) => res := RefList.Cons(v, res);
      | RefList.T(l) =>
          WHILE l # NIL DO
            res := RefList.Cons(l.head, res);
            l := l.tail;
          END;
      ELSE                       <* ASSERT FALSE *>
      END;
      list := list.tail
    END;
    RETURN RefList.ReverseD(res);
  END Flatten;

(* Winding = Odd.  Likeliest case is point within a polygon.  Check for odd
   winding.  Check for vertex in rectangle.  Check for edge crossing any
   side of rectangle. *)
PROCEDURE PolygonInBox (polygon: Polygon; hBox, vBox: R2.T): BOOLEAN =
  VAR
    vertices         : RefList.T;
    winding                   := 0;
    vertex           : Vertex;
    first, pos0, pos1: R2.T;
    windingPt                  := R2.T{hBox[0], vBox[0]};
  BEGIN
    (* Temporarily (hah!) treat a polygon with bezier edges as if it
       were a polygon with straight edges to the control points (SCG 21 July 1993) *)
    vertices := Flatten(polygon.vertices);
    vertex := RefListUtils.Pop(vertices);
    pos0 := vertex.pos;
    first := pos0;
    IF PosInBox(pos0, hBox, vBox) THEN RETURN TRUE END;
    WHILE vertices # NIL DO
      vertex := RefListUtils.Pop(vertices);
      pos1 := vertex.pos;
      IF PosInBox(pos1, hBox, vBox) THEN RETURN TRUE END;
      IF LineInBox(pos0, pos1, hBox, vBox) THEN RETURN TRUE END;
      IF Winding(pos0, pos1, windingPt) THEN INC(winding) END;
      pos0 := pos1;
    END;
    pos1 := first;
    IF LineInBox(pos0, pos1, hBox, vBox) THEN RETURN TRUE END;
    IF Winding(pos0, pos1, windingPt) THEN INC(winding) END;
    RETURN (winding MOD 2) = 1
  END PolygonInBox;

PROCEDURE PosInBox (READONLY pos, hBox, vBox: R2.T): BOOLEAN =
  BEGIN
    RETURN hBox[0] <= pos[0] AND pos[0] <= hBox[1] AND vBox[0] <= pos[1]
             AND pos[1] <= vBox[1]
  END PosInBox;

(* Winding return TRUE is the line p1, p2 crosses the ray extending east
   from w.  RETURN FALSE if the ray crosses exactly through p2. *)
PROCEDURE Winding (READONLY p1, p2, w: R2.T): BOOLEAN =
  VAR
    hw := w[0];
    vw := w[1];
    h1 := p1[0];
    v1 := p1[1];
    h2 := p2[0];
    v2 := p2[1];
  BEGIN
    IF (v1 < vw AND v2 <= vw) OR (v1 > vw AND v2 >= vw) THEN
      RETURN FALSE
    END;
    IF h1 >= hw AND h2 > hw THEN RETURN TRUE; END;
    RETURN h1 + (h2 - h1) * (vw - v1) / (v2 - v1) >= hw
  END Winding;

CONST
  SlantToText = ARRAY Slant OF TEXT{"r", "i", "o", "ri", "ro", "ot", "*"};

<* LL arbitrary *>

PROCEDURE MakeWorldFont (<* UNUSED *> t      : T;
                                      family : TEXT  := "Helvetica";
                                      size   : REAL  := 0.0353;
                                      slant  : Slant := Slant.Roman;
                                      weight : TEXT  := "Medium";
                                      foundry: TEXT  := "*"          ):
  WorldFont =
  BEGIN
    RETURN WorldFontFromFont(Font.FromName(ARRAY OF TEXT{
        Fmt.F("-%s-%s-%s-%s-*-*-*-%s-*-*-*-*-*-*", foundry, family, weight,
              SlantToText[slant], Fmt.Real(Pts.FromMM(size)))}));
  END MakeWorldFont;

PROCEDURE ScaleFontToPts (name: TEXT): TEXT =
  VAR
    rd              := TextRd.New(name);
    wr              := TextWr.New();
    ch       : CHAR;
    int             := 0;
    res, frac: REAL;
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    TRY
      (* copy up to pointsize *)
      FOR i := 1 TO 8 DO
        ch := Rd.GetChar(rd);
        WHILE ch # '-' DO Wr.PutChar(wr, ch); ch := Rd.GetChar(rd); END;
        Wr.PutChar(wr, ch);
      END;
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN name;
    END;

    TRY
      ch := Rd.GetChar(rd);
      WHILE ORD(ch) >= ORD('0') AND ORD(ch) <= ORD('9') DO
        int := 10 * int + ORD(ch) - ORD('0');
        ch := Rd.GetChar(rd);
      END;
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN name
    END;

    (* slightly inaccurate conversion to floating pt *)
    res := FLOAT(int);
    IF ch = '.' THEN
      TRY
        ch := Rd.GetChar(rd);
        frac := 0.1;
        WHILE ORD(ch) >= ORD('0') AND ORD(ch) <= ORD('9') DO
          res := res + FLOAT(ORD(ch) - ORD('0')) * frac;
          frac := frac / 10.0;
          ch := Rd.GetChar(rd);
        END;
      EXCEPT
        Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN name
      END;
    END;
    IF ch # '-' THEN RETURN name END;
    Wr.PutText(wr, Fmt.Real(Pts.FromMM(res)));
    Wr.PutChar(wr, '-');
    TRY
      FOR i := 1 TO 5 DO
        ch := Rd.GetChar(rd);
        WHILE ch # '-' DO Wr.PutChar(wr, ch); ch := Rd.GetChar(rd); END;
        Wr.PutChar(wr, ch);
      END;
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN name
    END;

    LOOP
      TRY
        (* copy charset - no trailing "-" *)
        ch := Rd.GetChar(rd);
        Wr.PutChar(wr, ch);
      EXCEPT
      | Rd.EndOfFile => EXIT;
      | Rd.Failure, Thread.Alerted => RETURN name;
      END;
    END;

    RETURN TextWr.ToText(wr);
  END ScaleFontToPts;

PROCEDURE WorldFontFromText (font: TEXT): WorldFont =
  BEGIN
    (* need to resize the pointsize (yuck!) *)
    RETURN WorldFontFromFont(
             Font.FromName(ARRAY OF TEXT{ScaleFontToPts(font)}));
  END WorldFontFromText;

PROCEDURE WorldFontFromFont (font: Font.T): WorldFont =
  VAR res := NEW(WorldFont);
  BEGIN
    res^ := font;
    RETURN res;
  END WorldFontFromFont;

PROCEDURE FontFromWorldFont (wf: WorldFont): Font.T =
  BEGIN
    RETURN wf^
  END FontFromWorldFont;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE MoveVertex (vertex  : Vertex;
                      pos     : R2.T;
                      animated: BOOLEAN       := FALSE;
                      <* UNUSED *> start := 0.0; <* UNUSED *> stop := 1.0;
                      path    : AnimationPath := NIL    ) =
  BEGIN
    WITH graph = vertex.graph DO
      vertex.new.pos := pos;
      IF animated THEN
        vertex.animated := TRUE;
        vertex.path := path;
      ELSE
        vertex.animated := FALSE;
        LOCK graph.mgv.mu DO
          MG.TranslateToLocked(
            vertex.mg, graph.mgv, Finite2(WorldPosToPts(graph, pos)),
            center := TRUE);
        END;
        vertex.pos := pos;
        VAR edges := vertex.edges;
        BEGIN
          WHILE edges # NIL DO
            VAR edge: Edge := RefListUtils.Pop(edges);
            BEGIN
              MoveEdge(edge, edge.vertex0, edge.vertex1, edge.control0,
                       edge.control1, animated := FALSE);
            END;
          END;
        END;
        VAR vertexHighlights := vertex.vertexHighlights;
        BEGIN
          WHILE vertexHighlights # NIL DO
            VAR
              vertexHighlight: VertexHighlight := RefListUtils.Pop(
                                                    vertexHighlights);
            BEGIN
              MoveVertexHighlight(
                vertexHighlight, vertexHighlight.vertex, animated := FALSE);
            END;
          END;
        END;
        VAR polygons := vertex.polygons;
        BEGIN
          WHILE polygons # NIL DO
            VAR polygon: Polygon := RefListUtils.Pop(polygons);
            BEGIN
              MovePolygon(polygon, polygon.vertices, animated := FALSE);
            END;
          END;
        END;
        IF graph.animations > 0 THEN graph.needRefresh := TRUE; END;
      END;
    END;
  END MoveVertex;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE SetVertexSize (vertex: Vertex; size: R2.T) =
  BEGIN
    vertex.size := size;
    LOCK vertex.graph.mgv.mu DO AdjustVertex(vertex) END;
  END SetVertexSize;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE SetVertexShape (vertex: Vertex; shape: VertexShape) =
  BEGIN
    IF shape = vertex.shape THEN RETURN; END;
    WITH graph = vertex.graph DO
      LOCK graph.mgv.mu DO
        vertex.shape := shape;
        vertex.group.remove(graph.mgv, vertex.mg);
        CASE vertex.shape OF
        | VertexShape.Rectangle =>
            vertex.mg := NEW(MG.Rectangle, color := vertex.colorScheme,
                             label := vertex.label, font := vertex.font^,
                             weight := 0.0).init(R2.Origin, R2.Origin);
        | VertexShape.Ellipse =>
            vertex.mg := NEW(MG.Ellipse, color := vertex.colorScheme,
                             label := vertex.label, font := vertex.font^,
                             weight := 0.0).init(R2.Origin, R2.Origin);
        END;
        vertex.group.addBefore(graph.mgv, vertex.mg, NIL);
        AdjustVertex(vertex);
      END;
    END;
  END SetVertexShape;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE SetVertexColor (vertex: Vertex; color: PaintOp.T) =
  BEGIN
    WITH graph = vertex.graph DO
      LOCK graph.mgv.mu DO
        vertex.color := color;
        vertex.colorScheme :=
          PaintOp.MakeColorScheme(vertex.color, vertex.fontColor);
        vertex.mg.setColor(graph.mgv, vertex.colorScheme);
      END;
    END;
  END SetVertexColor;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE SetVertexLabel (vertex: Vertex; label: TEXT) =
  BEGIN
    vertex.label := label;
    WITH graph = vertex.graph DO
      LOCK graph.mgv.mu DO vertex.mg.setLabel(graph.mgv, label); END;
    END;
  END SetVertexLabel;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE SetVertexFont (vertex: Vertex; font: WorldFont) =
  BEGIN
    IF font = NIL THEN font := DefaultFont END;
    WITH graph = vertex.graph DO
      LOCK graph.mgv.mu DO
        vertex.font := font;
        vertex.mg.setFont(graph.mgv, vertex.font^);
      END;
    END;
  END SetVertexFont;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE SetVertexFontColor (vertex: Vertex; fontColor: PaintOp.T) =
  BEGIN
    WITH graph = vertex.graph DO LOCK graph.mgv.mu DO
      vertex.fontColor := fontColor;
      vertex.colorScheme :=
        PaintOp.MakeColorScheme(vertex.color, vertex.fontColor);
      vertex.mg.setColor(graph.mgv, vertex.colorScheme);
    END; END;
  END SetVertexFontColor;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE SetVertexBorder (vertex: Vertex; border: REAL) =
  BEGIN
    vertex.border := border;
     LOCK vertex.graph.mgv.mu DO AdjustVertex(vertex) END;
  END SetVertexBorder;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE SetVertexBorderColor (vertex: Vertex; borderColor: PaintOp.T) =
  BEGIN
    WITH graph = vertex.graph DO
      LOCK graph.mgv.mu DO vertex.borderColor := borderColor; END;
    END;
  END SetVertexBorderColor;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE VertexToFront (vertex: Vertex; zOrder := ZOrder.Normal) =
  BEGIN
    WITH graph = vertex.graph DO
      LOCK graph.mgv.mu DO
        vertex.currentGroup.remove(graph.mgv, vertex.group);
        CASE zOrder OF
        | ZOrder.Normal => vertex.currentGroup := graph.vertexGroup;
        | ZOrder.Foreground => vertex.currentGroup := graph.fgGroup;
        | ZOrder.Background => vertex.currentGroup := graph.bgGroup;
        END;
        vertex.currentGroup.addBefore(graph.mgv, vertex.group, NIL);
        RefListUtils.DeleteQ(graph.vertices, vertex);
        RefListUtils.Push(graph.vertices, vertex);
      END;
    END;
  END VertexToFront;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE VertexToBack (vertex: Vertex; zOrder := ZOrder.Normal) =
  BEGIN
    WITH graph = vertex.graph DO
      LOCK graph.mgv.mu DO
        vertex.currentGroup.remove(graph.mgv, vertex.group);
        CASE zOrder OF
        | ZOrder.Normal => vertex.currentGroup := graph.vertexGroup;
        | ZOrder.Foreground => vertex.currentGroup := graph.fgGroup;
        | ZOrder.Background => vertex.currentGroup := graph.bgGroup;
        END;
        vertex.currentGroup.addAfter(graph.mgv, vertex.group, NIL);
        RefListUtils.DeleteQ(graph.vertices, vertex);
        graph.vertices := RefList.Append(graph.vertices, RefList.List1(vertex));
      END;
    END;
  END VertexToBack;

<* LL.sup >= vertex.graph.mu *>

PROCEDURE RemoveVertex (vertex: Vertex) =
  BEGIN
    IF NOT vertex.initialized THEN RETURN; END;
    WHILE vertex.edges # NIL DO
      VAR edge: Edge := vertex.edges.head;
      BEGIN
        RemoveEdge(edge);
      END;
    END;
    WHILE vertex.vertexHighlights # NIL DO
      VAR
        vertexHighlight: VertexHighlight := vertex.vertexHighlights.head;
      BEGIN
        RemoveVertexHighlight(vertexHighlight);
      END;
    END;
    WHILE vertex.polygons # NIL DO
      VAR polygon: Polygon := vertex.polygons.head;
      BEGIN
        RemovePolygon(polygon);
      END;
    END;
    WITH graph = vertex.graph DO
      LOCK graph.mgv.mu DO
        RefListUtils.DeleteQ(graph.vertices, vertex);
        vertex.currentGroup.remove(graph.mgv, vertex.group);
      END;
    END;
    vertex.initialized := FALSE;
  END RemoveVertex;

<* LL.sup >= edge.vertex0.graph.mu *>

PROCEDURE SetEdgeWidth (edge: Edge; width: REAL) =
  BEGIN
    WITH graph = edge.graph DO
      LOCK graph.mgv.mu DO
        edge.width := width;
        edge.mg.setWeight(graph.mgv, Pts.FromMM(edge.width));
        VAR arrows := FALSE;
        BEGIN
          FOR i := 0 TO 1 DO
            IF edge.arrow[i] THEN
              FOR j := 0 TO 1 DO
                edge.arrowLine[i][j].setWeight(
                  graph.mgv, Pts.FromMM(edge.width));
                arrows := TRUE;
              END;
            END;
          END;
          IF arrows THEN
            RefreshEdge(edge);
          ELSE
            (* work around MG bug *)
            IF NOT edge.isLine THEN RefreshEdge(edge); END;
          END;
        END;
      END;
    END;
  END SetEdgeWidth;

<* LL.sup >= edge.vertex0.graph.mu *>

PROCEDURE SetEdgeColor (edge: Edge; color: PaintOp.T) =
  BEGIN
    WITH graph = edge.graph DO
      LOCK graph.mgv.mu DO
        edge.color := color;
        edge.colorScheme :=
          PaintOp.MakeColorScheme(edge.color, edge.color);
        edge.mg.setColor(graph.mgv, edge.colorScheme);
        edge.mg.setWeight(graph.mgv, Pts.FromMM(edge.width)); (* work
                                                                 around MG
                                                                 bug *)
        FOR i := 0 TO 1 DO
          IF edge.arrow[i] THEN
            FOR j := 0 TO 1 DO
              edge.arrowLine[i][j].setColor(graph.mgv, edge.colorScheme);
              edge.arrowLine[i][j].setWeight(
                graph.mgv, Pts.FromMM(edge.width));
            END;
          END;
        END;
      END;
    END;
  END SetEdgeColor;

<* LL.sup >= edge.vertex0.graph.mu *>

PROCEDURE SetEdgeArrow (edge: Edge; arrow: ARRAY [0 .. 1] OF BOOLEAN) =
  BEGIN
    WITH graph = edge.graph DO
      LOCK graph.mgv.mu DO
        FOR i := 0 TO 1 DO
          IF arrow[i] AND NOT edge.arrow[i] THEN
            FOR j := 0 TO 1 DO
              edge.arrowLine[i][j] :=
                NEW(MG.Line, weight := Pts.FromMM(edge.width),
                    color := edge.colorScheme).init(R2.Origin, R2.Origin);
              edge.arrowEnd[i][j][0] :=
                NEW(MG.LineEnd, line := edge.arrowLine[i][j],
                    controlsFrom := TRUE).init();
              edge.arrowEnd[i][j][1] :=
                NEW(MG.LineEnd, line := edge.arrowLine[i][j],
                    controlsFrom := FALSE).init();
              FOR k := 0 TO 1 DO
                edge.group.addBefore(
                  graph.mgv, edge.arrowEnd[i][j][k], NIL);
                edge.arrowPos[i][j][k] := R2.Origin;
              END;
            END;
          ELSIF edge.arrow[i] AND NOT arrow[i] THEN
            FOR j := 0 TO 1 DO
              edge.arrowLine[i][j] := NIL;
              FOR k := 0 TO 1 DO
                edge.group.remove(graph.mgv, edge.arrowEnd[i][j][k]);
                edge.arrowEnd[i][j][k] := NIL;
              END;
            END;
          END;
        END;
        edge.arrow := arrow;
        RefreshEdge(edge);
      END;
    END;
  END SetEdgeArrow;

<* LL.sup >= edge.vertex0.graph.mu *>

PROCEDURE MoveEdge (edge              : Edge;
                    vertex0, vertex1  : Vertex;
                    control0, control1: Vertex   := NIL;
                    animated          : BOOLEAN  := FALSE;
                    <* UNUSED *> start := 0.0; <* UNUSED *> stop := 1.0) =
  VAR straight := (control0 = NIL) AND (control1 = NIL);
  BEGIN
    WITH graph = edge.graph DO
      LOCK graph.mgv.mu DO
        <*ASSERT vertex0.graph = graph*>
        <*ASSERT vertex1.graph = graph*>

        IF straight THEN
          <*ASSERT control1 = NIL*>
        ELSE
          <*ASSERT control1 # NIL*>
          <*ASSERT control0.graph = graph*>
          <*ASSERT control1.graph = graph*>
        END;

        edge.new.vertex0 := vertex0;
        edge.new.vertex1 := vertex1;
        edge.new.control0 := control0;
        edge.new.control1 := control1;

        IF NOT animated THEN

          RefListUtils.DeleteQ(edge.vertex0.edges, edge);
          RefListUtils.DeleteQ(edge.vertex1.edges, edge);

          IF edge.control0 # NIL THEN
            RefListUtils.DeleteQ(edge.control0.edges, edge);
          END;
          IF edge.control1 # NIL THEN
            RefListUtils.DeleteQ(edge.control1.edges, edge);
          END;

          edge.vertex0 := edge.new.vertex0;
          edge.vertex1 := edge.new.vertex1;

          RefListUtils.Push(edge.vertex0.edges, edge);
          RefListUtils.Push(edge.vertex1.edges, edge);

          edge.control0 := edge.new.control0;
          edge.control1 := edge.new.control1;
          edge.straight := (edge.control0 = NIL) AND (edge.control1 = NIL);

          IF edge.control0 # NIL THEN
            RefListUtils.Push(edge.control0.edges, edge);
          END;
          IF edge.control1 # NIL THEN
            RefListUtils.Push(edge.control1.edges, edge);
          END;

          IF NOT edge.straight AND edge.isLine THEN
            (* was straight, now curved. *)
            edge.group.remove(graph.mgv, edge.end[0]);
            edge.group.remove(graph.mgv, edge.end[1]);
            VAR path := NEW(R2Path.T);
            BEGIN
              path.init();
              edge.mg := NEW(MG.Shape, weight := Pts.FromMM(edge.width),
                             color := edge.colorScheme).init(
                           R2.Origin, path, fill := FALSE);
            END;
            edge.end[0] := NIL;
            edge.end[1] := NIL;
            edge.isLine := FALSE;
            edge.group.addBefore(graph.mgv, edge.mg, NIL);
          ELSIF edge.straight AND NOT edge.isLine THEN
            (* was curved, now straight. *)
            edge.group.remove(graph.mgv, edge.mg);
            edge.mg :=
              NEW(MG.Line, weight := Pts.FromMM(edge.width),
                  color := edge.colorScheme).init(R2.Origin, R2.Origin);
            edge.end[0] := NEW(MG.LineEnd, line := edge.mg,
                               controlsFrom := TRUE).init();
            edge.end[1] := NEW(MG.LineEnd, line := edge.mg,
                               controlsFrom := FALSE).init();
            edge.isLine := TRUE;
            edge.group.addBefore(graph.mgv, edge.end[0], NIL);
            edge.group.addBefore(graph.mgv, edge.end[1], NIL);
          END;
          RefreshEdge(edge);
          IF graph.animations > 0 THEN graph.needRefresh := TRUE; END;
        END;
      END;
    END;
  END MoveEdge;

<* LL.sup >= edge.graph.mu *>

PROCEDURE EdgeToFront (edge: Edge; zOrder := ZOrder.Normal) =
  BEGIN
    WITH graph = edge.graph DO
      LOCK graph.mgv.mu DO
        edge.currentGroup.remove(graph.mgv, edge.group);
        CASE zOrder OF
        | ZOrder.Normal => edge.currentGroup := graph.edgeGroup;
        | ZOrder.Foreground => edge.currentGroup := graph.fgGroup;
        | ZOrder.Background => edge.currentGroup := graph.bgGroup;
        END;
        edge.currentGroup.addBefore(graph.mgv, edge.group, NIL);
        RefListUtils.DeleteQ(graph.edges, edge);
        RefListUtils.Push(graph.edges, edge);
      END;
    END;
  END EdgeToFront;

<* LL.sup >= edge.graph.mu *>

PROCEDURE EdgeToBack (edge: Edge; zOrder := ZOrder.Normal) =
  BEGIN
    WITH graph = edge.graph DO
      LOCK graph.mgv.mu DO
        edge.currentGroup.remove(graph.mgv, edge.group);
        CASE zOrder OF
        | ZOrder.Normal => edge.currentGroup := graph.edgeGroup;
        | ZOrder.Foreground => edge.currentGroup := graph.fgGroup;
        | ZOrder.Background => edge.currentGroup := graph.bgGroup;
        END;
        edge.currentGroup.addAfter(graph.mgv, edge.group, NIL);
        RefListUtils.DeleteQ(graph.edges, edge);
        graph.edges := RefList.Append(graph.edges, RefList.List1(edge));
      END;
    END;
  END EdgeToBack;

<* LL.sup >= edge.vertex0.graph.mu *>

PROCEDURE RemoveEdge (edge: Edge) =
  BEGIN
    IF NOT edge.initialized THEN RETURN; END;
    WITH graph = edge.graph DO
      LOCK graph.mgv.mu DO
        RefListUtils.DeleteQ(graph.edges, edge);
        RefListUtils.DeleteQ(edge.vertex0.edges, edge);
        RefListUtils.DeleteQ(edge.vertex1.edges, edge);
        IF edge.control0 # NIL THEN
          RefListUtils.DeleteQ(edge.control0.edges, edge);
        END;
        IF edge.control1 # NIL THEN
          RefListUtils.DeleteQ(edge.control1.edges, edge);
        END;
        edge.currentGroup.remove(graph.mgv, edge.group);
      END;
    END;
    edge.initialized := FALSE;
  END RemoveEdge;

<* LL.sup >= vertex.vertex.graph.mu *>

PROCEDURE MoveVertexHighlight (vertexHighlight: VertexHighlight;
                               vertex         : Vertex;
                               animated       : BOOLEAN           := FALSE;
                      <* UNUSED *> start := 0.0; <* UNUSED *> stop := 1.0) =
  BEGIN
    WITH graph = vertex.graph DO
      LOCK graph.mgv.mu DO
        <*ASSERT vertex.graph = graph*>
        vertexHighlight.new.vertex := vertex;
        IF NOT animated THEN
          vertexHighlight.pos := vertexHighlight.new.vertex.pos;
          RefListUtils.DeleteQ(
              vertexHighlight.vertex.vertexHighlights, vertexHighlight);
          vertexHighlight.vertex := vertexHighlight.new.vertex;
          RefListUtils.Push(
            vertexHighlight.vertex.vertexHighlights, vertexHighlight);
          VAR centerPP := WorldPosToPts(graph, vertexHighlight.vertex.pos);
          BEGIN
            MG.TranslateToLocked(vertexHighlight.mg, graph.mgv,
                                 Finite2(centerPP), center := TRUE);
          END;
          EVAL AdjustVertexHighlightSizeandShape(vertexHighlight);
          IF graph.animations > 0 THEN graph.needRefresh := TRUE; END;
        END;
      END;
    END;
  END MoveVertexHighlight;

<* LL.sup >= vertexHighlight.vertex.graph.mu *>

PROCEDURE SetVertexHighlightBorder (vertexHighlight: VertexHighlight;
                                    border         : R2.T             ) =
  BEGIN
    BEGIN
      vertexHighlight.border := border;
      LOCK vertexHighlight.vertex.graph.mgv.mu DO
        EVAL AdjustVertexHighlightSizeandShape(vertexHighlight);
      END;
    END;
  END SetVertexHighlightBorder;

<* LL.sup >= vertexHighlight.vertex.graph.mu *>

PROCEDURE SetVertexHighlightColor (vertexHighlight: VertexHighlight;
                                   color          : PaintOp.T        ) =
  BEGIN
    vertexHighlight.color := color;
    vertexHighlight.colorScheme :=
      PaintOp.MakeColorScheme(vertexHighlight.color, PaintOp.Fg);
    WITH graph = vertexHighlight.graph DO
      LOCK graph.mgv.mu DO
        vertexHighlight.mg.setColor(graph.mgv, vertexHighlight.colorScheme);
      END;
    END;
  END SetVertexHighlightColor;

<* LL.sup >= vertexHighlight.graph.mu *>

PROCEDURE VertexHighlightToFront (vertexHighlight: VertexHighlight; zOrder := ZOrder.Normal) =
  BEGIN
    WITH graph = vertexHighlight.graph DO
      LOCK graph.mgv.mu DO
        vertexHighlight.currentGroup.remove(graph.mgv, vertexHighlight.group);
        CASE zOrder OF
        | ZOrder.Normal => vertexHighlight.currentGroup := graph.vertexHighlightGroup;
        | ZOrder.Foreground => vertexHighlight.currentGroup := graph.fgGroup;
        | ZOrder.Background => vertexHighlight.currentGroup := graph.bgGroup;
        END;
        vertexHighlight.currentGroup.addBefore(
          graph.mgv, vertexHighlight.group, NIL);
        RefListUtils.DeleteQ(graph.vertexHighlights, vertexHighlight);
        RefListUtils.Push(graph.vertexHighlights, vertexHighlight);
      END;
    END;
  END VertexHighlightToFront;

<* LL.sup >= vertexHighlight.graph.mu *>

PROCEDURE VertexHighlightToBack (vertexHighlight: VertexHighlight; zOrder := ZOrder.Normal) =
  BEGIN
    WITH graph = vertexHighlight.graph DO
      LOCK graph.mgv.mu DO
        vertexHighlight.currentGroup.remove(graph.mgv, vertexHighlight.group);
        CASE zOrder OF
        | ZOrder.Normal => vertexHighlight.currentGroup := graph.vertexHighlightGroup;
        | ZOrder.Foreground => vertexHighlight.currentGroup := graph.fgGroup;
        | ZOrder.Background => vertexHighlight.currentGroup := graph.bgGroup;
        END;
        vertexHighlight.currentGroup.addAfter(
          graph.mgv, vertexHighlight.group, NIL);
        RefListUtils.DeleteQ(graph.vertexHighlights, vertexHighlight);
        graph.vertexHighlights :=
          RefList.Append(graph.vertexHighlights, RefList.List1(vertexHighlight));
      END;
    END;
  END VertexHighlightToBack;

<* LL.sup >= vertexHighlight.vertex.graph.mu *>

PROCEDURE RemoveVertexHighlight (vertexHighlight: VertexHighlight) =
  BEGIN
    IF NOT vertexHighlight.initialized THEN RETURN; END;
    WITH graph = vertexHighlight.graph DO
      LOCK graph.mgv.mu DO
        RefListUtils.DeleteQ(graph.vertexHighlights, vertexHighlight);
        RefListUtils.DeleteQ(
            vertexHighlight.vertex.vertexHighlights, vertexHighlight);
        vertexHighlight.currentGroup.remove(graph.mgv, vertexHighlight.group);
      END;
    END;
    vertexHighlight.initialized := FALSE;
  END RemoveVertexHighlight;

<* LL.sup >= RefList.First(polygon.vertices).graph.mu *>

PROCEDURE SetPolygonColor (polygon: Polygon; color: PaintOp.T) =
  BEGIN
    WITH graph = polygon.graph DO
      LOCK graph.mgv.mu DO
        polygon.color := color;
        polygon.colorScheme :=
          PaintOp.MakeColorScheme(polygon.color, polygon.color);
        polygon.mg.setColor(graph.mgv, polygon.colorScheme);
      END;
    END;
  END SetPolygonColor;

<* LL.sup >= RefList.First(polygon.vertices).graph.mu *>

PROCEDURE MovePolygon (polygon : Polygon;
                       vertices: RefList.T (* OF Vertex *);
                       animated: BOOLEAN := FALSE;
                       <* UNUSED *> start := 0.0;
                       <* UNUSED *> stop := 1.0) =
  BEGIN
    WITH graph = polygon.graph DO
      LOCK graph.mgv.mu DO
        <*ASSERT vertices # NIL*>
        VAR list := vertices;
        BEGIN
          WHILE list # NIL DO
            TYPECASE RefListUtils.Pop(list) OF
            | Vertex (vertex) =>
              <*ASSERT vertex.graph = graph*>
            | RefList.T (v2) =>
                <* ASSERT RefList.Length(v2) = 3 *>
                WHILE v2 # NIL DO
                  VAR vertex: Vertex := RefListUtils.Pop(v2);
                  BEGIN
                    <*ASSERT vertex.graph = graph*>
                  END;
                END;
            ELSE                 <* ASSERT FALSE *>
            END;
          END;
        END;

        polygon.new.vertices := vertices;

        IF NOT animated THEN
          VAR vertices := polygon.vertices;
          BEGIN
            WHILE vertices # NIL DO
              TYPECASE RefListUtils.Pop(vertices) OF
              | Vertex (vertex) =>
                  RefListUtils.DeleteQ(vertex.polygons, polygon);
              | RefList.T (v2) =>
                  <* ASSERT RefList.Length(v2) = 3 *>
                  WHILE v2 # NIL DO
                    VAR vertex: Vertex := RefListUtils.Pop(v2);
                    BEGIN
                      RefListUtils.DeleteQ(vertex.polygons, polygon);
                    END;
                  END;
              ELSE               <* ASSERT FALSE *>
              END;
            END;
          END;

          polygon.vertices := polygon.new.vertices;

          VAR vertices := polygon.vertices;
          BEGIN
            WHILE vertices # NIL DO
              TYPECASE RefListUtils.Pop(vertices) OF
              | Vertex (vertex) =>
                  RefListUtils.Push(vertex.polygons, polygon);
              | RefList.T (v2) =>
                  <* ASSERT RefList.Length(v2) = 3 *>
                  WHILE v2 # NIL DO
                    VAR vertex: Vertex := RefListUtils.Pop(v2);
                    BEGIN
                      RefListUtils.Push(vertex.polygons, polygon);
                    END;
                  END;
              ELSE               <* ASSERT FALSE *>
              END;
            END;
          END;

          RefreshPolygon(polygon);
          IF graph.animations > 0 THEN graph.needRefresh := TRUE; END;
        END;
      END;
    END;
  END MovePolygon;

<* LL.sup >= polygon.graph.mu *>

PROCEDURE PolygonToFront (polygon: Polygon; zOrder := ZOrder.Normal) =
  BEGIN
    WITH graph = polygon.graph DO
      LOCK graph.mgv.mu DO
        polygon.currentGroup.remove(graph.mgv, polygon.group);
        CASE zOrder OF
        | ZOrder.Normal => polygon.currentGroup := graph.polygonGroup;
        | ZOrder.Foreground => polygon.currentGroup := graph.fgGroup;
        | ZOrder.Background => polygon.currentGroup := graph.bgGroup;
        END;
        polygon.currentGroup.addBefore(graph.mgv, polygon.group, NIL);
        RefListUtils.DeleteQ(graph.polygons, polygon);
        RefListUtils.Push(graph.polygons, polygon);
      END;
    END;
  END PolygonToFront;

<* LL.sup >= polygon.graph.mu *>

PROCEDURE PolygonToBack (polygon: Polygon; zOrder := ZOrder.Normal) =
  BEGIN
    WITH graph = polygon.graph DO
      LOCK graph.mgv.mu DO
        polygon.currentGroup.remove(graph.mgv, polygon.group);
        CASE zOrder OF
        | ZOrder.Normal => polygon.currentGroup := graph.polygonGroup;
        | ZOrder.Foreground => polygon.currentGroup := graph.fgGroup;
        | ZOrder.Background => polygon.currentGroup := graph.bgGroup;
        END;
        polygon.currentGroup.addAfter(graph.mgv, polygon.group, NIL);
        RefListUtils.DeleteQ(graph.polygons, polygon);
        graph.polygons := RefList.Append(graph.polygons, RefList.List1(polygon));
      END;
    END;
  END PolygonToBack;

<* LL.sup >= RefList.First(polygon.vertices).graph.mu *>

PROCEDURE RemovePolygon (polygon: Polygon) =
  BEGIN
    IF NOT polygon.initialized THEN RETURN; END;
    WITH graph = polygon.graph DO
      LOCK graph.mgv.mu DO
        RefListUtils.DeleteQ(graph.polygons, polygon);
        VAR vertices := polygon.vertices;
        BEGIN
          WHILE vertices # NIL DO
            TYPECASE RefListUtils.Pop(vertices) OF
            | Vertex (vertex) =>
                RefListUtils.DeleteQ(vertex.polygons, polygon);
            | RefList.T (vs) =>
                WHILE vs # NIL DO
                  VAR vertex: Vertex := RefListUtils.Pop(vs);
                  BEGIN
                    RefListUtils.DeleteQ(vertex.polygons, polygon);
                  END;
                END;
            ELSE                 <* ASSERT FALSE *>
            END;
          END;
        END;
        polygon.currentGroup.remove(graph.mgv, polygon.group);
      END;
    END;
    polygon.initialized := FALSE;
  END RemovePolygon;

(* REDISPLAY *)

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE RefreshGraph (graph: T; all := TRUE) =
  VAR pixels, borderPixels: ARRAY [0 .. 1] OF INTEGER;
  BEGIN
    IF VBT.ScreenTypeOf(graph) = NIL OR Rect.IsEmpty(graph.rect) THEN
      RETURN
    END;
    IF all THEN
      pixels[0] := graph.rect.east - graph.rect.west;
      pixels[1] := graph.rect.south - graph.rect.north;

      borderPixels[0] := CEILING(graph.margin * graph.res[Axis.T.Hor]);
      borderPixels[1] := CEILING(graph.margin * graph.res[Axis.T.Ver]);

      pixels[0] := pixels[0] - 2 * borderPixels[0];
      IF pixels[0] < 0 THEN
        pixels[0] := 0;
        borderPixels[0] := (graph.rect.east - graph.rect.west) DIV 2;
      END;
      pixels[1] := pixels[1] - 2 * borderPixels[1];
      IF pixels[1] < 0 THEN
        pixels[1] := 0;
        borderPixels[1] := (graph.rect.south - graph.rect.north) DIV 2;
      END;

      IF pixels[0] >= graph.pixelSizeDivisor[0] THEN
        pixels[0] := pixels[0] - pixels[0] MOD graph.pixelSizeDivisor[0];
      END;
      IF pixels[1] >= graph.pixelSizeDivisor[1] THEN
        pixels[1] := pixels[1] - pixels[1] MOD graph.pixelSizeDivisor[1];
      END;

      IF graph.aspect # 0.0 THEN
        IF FLOAT(pixels[0]) * graph.res[Axis.T.Ver] * graph.aspect
             > FLOAT(pixels[1]) * graph.res[Axis.T.Hor] THEN
          pixels[0] :=
            ROUND(FLOAT(pixels[1])
                    * (graph.res[Axis.T.Hor] / graph.res[Axis.T.Ver])
                    / graph.aspect / FLOAT(graph.pixelSizeDivisor[0]))
              * graph.pixelSizeDivisor[0];
          IF pixels[0] + 2 * borderPixels[0]
               > graph.rect.east - graph.rect.west THEN
            pixels[0] := pixels[0] - graph.pixelSizeDivisor[0];
          END;
          IF pixels[0] = 0 THEN
            pixels[0] :=
              (graph.rect.east - graph.rect.west) - 2 * borderPixels[0];
          END;
        ELSE
          pixels[1] :=
            ROUND(FLOAT(pixels[0])
                    * (graph.res[Axis.T.Ver] / graph.res[Axis.T.Hor])
                    * graph.aspect / FLOAT(graph.pixelSizeDivisor[1]))
              * graph.pixelSizeDivisor[1];
          IF pixels[1] + 2 * borderPixels[1]
               > graph.rect.south - graph.rect.north THEN
            pixels[1] := pixels[1] - graph.pixelSizeDivisor[1];
          END;
          IF pixels[1] = 0 THEN
            pixels[1] :=
              (graph.rect.south - graph.rect.north) - 2 * borderPixels[1];
          END;
        END;
      END;

      borderPixels[0] :=
        ((graph.rect.east - graph.rect.west) - pixels[0]) DIV 2;
      borderPixels[1] :=
        ((graph.rect.south - graph.rect.north) - pixels[1]) DIV 2;

      VAR
        hs := ABS(FLOAT(pixels[0]) / (graph.world.e - graph.world.w));
        vs := ABS(FLOAT(pixels[1]) / (graph.world.s - graph.world.n));
      BEGIN
        graph.realMarginMM[0] := FLOAT(borderPixels[0]) / hs;
        graph.realMarginMM[1] := FLOAT(borderPixels[1]) / vs;

        hs := hs / graph.res[Axis.T.Hor];
        vs := vs / graph.res[Axis.T.Ver];

        Thread.Release(graph.mgv.mu);
        Thread.Release(graph.mu);
        TRY
          ScaleFilter.Scale(graph, hs, vs);
        FINALLY
          Thread.Acquire(graph.mu);
          Thread.Acquire(graph.mgv.mu);
        END;
      END;
    END;

    VAR vertices := graph.vertices;
    BEGIN
      WHILE vertices # NIL DO
        VAR vertex: Vertex := RefListUtils.Pop(vertices);
        BEGIN
          RefreshVertex(vertex);
        END;
      END;
    END;

    VAR edges := graph.edges;
    BEGIN
      WHILE edges # NIL DO
        VAR edge: Edge := RefListUtils.Pop(edges);
        BEGIN
          RefreshEdge(edge);
        END;
      END;
    END;

    VAR polygons := graph.polygons;
    BEGIN
      WHILE polygons # NIL DO
        VAR polygon: Polygon := RefListUtils.Pop(polygons);
        BEGIN
          RefreshPolygon(polygon);
        END;
      END;
    END;
  END RefreshGraph;

<* LL.sup >= vertex.graph.mu, vertex.graph.mgv.mu *>

PROCEDURE RefreshVertex (vertex: Vertex) =
  BEGIN
    WITH graph = vertex.graph DO
      VAR centerPP := WorldPosToPts(graph, vertex.pos);
      BEGIN
        MG.TranslateToLocked(
          vertex.mg, graph.mgv, Finite2(centerPP), center := TRUE);
        VAR vertexHighlights := vertex.vertexHighlights;
        BEGIN
          WHILE vertexHighlights # NIL DO
            VAR
              vertexHighlight: VertexHighlight := RefListUtils.Pop(
                                                    vertexHighlights);
            BEGIN
              MG.TranslateToLocked(vertexHighlight.mg, graph.mgv,
                                   Finite2(centerPP), center := TRUE);
            END;
          END;
        END;
      END;
    END;
  END RefreshVertex;

<* LL.sup >= edge.vertex0.graph.mu, edge.vertex0.graph.mgv.mu *>

PROCEDURE RefreshEdge (edge: Edge) =
  BEGIN
    WITH graph = edge.graph DO
      edge.pos[0] := edge.vertex0.pos;
      edge.pos[1] := edge.vertex1.pos;
      IF edge.control0 # NIL THEN
        edge.cpos[0] := edge.control0.pos;
      ELSE
        edge.cpos[0] := edge.vertex0.pos;
      END;
      IF edge.control1 # NIL THEN
        edge.cpos[1] := edge.control1.pos;
      ELSE
        edge.cpos[1] := edge.vertex1.pos;
      END;
      IF edge.isLine THEN
        MG.TranslateToLocked(
          edge.end[0], graph.mgv, Finite2(WorldPosToPts(graph, edge.pos[0])));
        MG.TranslateToLocked(
          edge.end[1], graph.mgv, Finite2(WorldPosToPts(graph, edge.pos[1])));
      ELSE
        VAR
          origin := WorldPosToPts(graph, edge.pos[0]);
          path   := NEW(R2Path.T);
        BEGIN
          path.init();
          path.moveTo(R2.Origin);
          path.curveTo(
            Finite2(R2.Sub(WorldPosToPts(graph, edge.cpos[0]), origin)),
            Finite2(R2.Sub(WorldPosToPts(graph, edge.cpos[1]), origin)),
            Finite2(R2.Sub(WorldPosToPts(graph, edge.pos[1]), origin)));
          NARROW(edge.mg, MG.Shape).reshape(
            graph.mgv, origin, path, fill := FALSE);
        END;
      END;
      edge.arrowPos := ArrowPos(edge);
      FOR i := 0 TO 1 DO
        IF edge.arrow[i] THEN
          FOR j := 0 TO 1 DO
            FOR k := 0 TO 1 DO
              MG.TranslateToLocked(
                edge.arrowEnd[i][j][k], graph.mgv,
                Finite2(WorldPosToPts(graph, edge.arrowPos[i][j][k])));
            END;
          END;
        END;
      END;
    END;
  END RefreshEdge;

CONST
  (* arrowhead lines extend 22.5 degrees from the edge *)
  SinTheta = 0.38268343;
  CosTheta = 0.92387953;

<* LL.sup >= edge.vertex0.graph.mu *>

PROCEDURE ArrowPos (edge: Edge):
  ARRAY [0 .. 1], [0 .. 1], [0 .. 1] OF R2.T =
  VAR new: ARRAY [0 .. 1], [0 .. 1], [0 .. 1] OF R2.T;
  BEGIN
    IF edge.arrow[0] THEN
      VAR tip, delta: R2.T;
      BEGIN
        ComputeArrowTip(edge, TRUE, tip, delta);
        new[0][0][0] := tip;
        new[0][1][0] := new[0][0][0];
        delta := R2.Scale(5.0 * edge.width, delta);
        new[0][0][1] :=
            R2.Add(
                new[0][0][0],
                R2.T{CosTheta * delta[0] + SinTheta * delta[1],
                     -SinTheta * delta[0] + CosTheta * delta[1]});
        new[0][1][1] :=
            R2.Add(
                new[0][1][0],
                R2.T{CosTheta * delta[0] - SinTheta * delta[1],
                     SinTheta * delta[0] + CosTheta * delta[1]});
      END;
    ELSE
      FOR j := 0 TO 1 DO
        FOR k := 0 TO 1 DO new[0][j][k] := edge.pos[0]; END;
      END;
    END;
    IF edge.arrow[1] THEN
      VAR tip, delta: R2.T;
      BEGIN
        ComputeArrowTip(edge, FALSE, tip, delta);
        new[1][0][0] := tip;
        new[1][1][0] := new[1][0][0];
        delta := R2.Scale(5.0 * edge.width, delta);
        new[1][0][1] :=
            R2.Add(
                new[1][0][0],
                R2.T{CosTheta * delta[0] + SinTheta * delta[1],
                     -SinTheta * delta[0] + CosTheta * delta[1]});
        new[1][1][1] :=
            R2.Add(
                new[1][1][0],
                R2.T{CosTheta * delta[0] - SinTheta * delta[1],
                     SinTheta * delta[0] + CosTheta * delta[1]});
      END;
    ELSE
      FOR j := 0 TO 1 DO
        FOR k := 0 TO 1 DO new[1][j][k] := edge.pos[1]; END;
      END;
    END;
    RETURN new;
  END ArrowPos;

<* LL.sup >= edge.vertex0.graph.mu, edge.vertex0.graph.mgv.mu *>

PROCEDURE ComputeArrowTip (            edge      : Edge;
                                       forward   : BOOLEAN;
                           VAR (*OUT*) tip, delta: R2.T     ) =
  BEGIN
    IF edge.isLine THEN
      ComputeArrowTipOfLine(edge, forward, tip, delta);
    ELSE
      ComputeArrowTipOfBezier(edge, forward, tip, delta);
    END;
  END ComputeArrowTip;

<* LL.sup >= edge.vertex0.graph.mu, edge.vertex0.graph.mgv.mu *>

PROCEDURE ComputeArrowTipOfLine (            edge      : Edge;
                                             forward   : BOOLEAN;
                                 VAR (*OUT*) tip, delta: R2.T     ) =
  VAR
    vertex0   : Vertex;
    pos0, pos1: R2.T;
  BEGIN
    IF forward THEN
      vertex0 := edge.vertex0;
      pos0 := edge.pos[0];
      pos1 := edge.pos[1];
    ELSE
      vertex0 := edge.vertex1;
      pos0 := edge.pos[1];
      pos1 := edge.pos[0];
    END;
    VAR
      d      := R2.Sub(pos1, pos0);
      length := R2.Length(d);
    VAR t: REAL;
    BEGIN
      WITH size = vertex0.size DO
        CASE vertex0.shape OF
        | VertexShape.Rectangle =>
          t := MIN(ABS(size[0] / (2.0 * d[0])),
                   ABS(size[1] / (2.0 * d[1])));
        | VertexShape.Ellipse =>
            t :=
              FLOAT(Math.sqrt(
                      FLOAT(0.25 / (d[0] * d[0] / (size[0] * size[0])
                                      + d[1] * d[1]
                                          / (size[1] * size[1])),
                            LONGREAL)), REAL);
        END;
        tip :=
          R2.Add(pos0,
                 R2.Scale(MIN(MAX(t + ABS(edge.width) / (2.0 * length),
                                  0.0), 1.0), d));
        delta := R2.Scale(1.0 / length, d);
      END;
    END;
    IF NOT RealFloat.Finite(tip[0] + tip[1]) THEN tip := pos0; END;
    IF NOT RealFloat.Finite(delta[0] + delta[1]) THEN
      delta := R2.T{1.0, 0.0};
    END;
  END ComputeArrowTipOfLine;

<* LL.sup >= edge.vertex0.graph.mu, edge.vertex0.graph.mgv.mu *>

PROCEDURE ComputeArrowTipOfBezier (            edge      : Edge;
                                               forward   : BOOLEAN;
                                   VAR (*OUT*) tip, delta: R2.T     ) =
  VAR
    vertex0                 : Vertex;
    pos0, pos1, cpos0, cpos1: R2.T;
  BEGIN
    WITH graph = edge.graph DO
      IF forward THEN
        vertex0 := edge.vertex0;
        pos0 := edge.pos[0];
        pos1 := edge.pos[1];
        cpos0 := edge.cpos[0];
        cpos1 := edge.cpos[1];
      ELSE
        vertex0 := edge.vertex1;
        pos0 := edge.pos[1];
        pos1 := edge.pos[0];
        cpos0 := edge.cpos[1];
        cpos1 := edge.cpos[0];
      END;
      VAR
        t : REAL;
        x1     := cpos0[0] - pos0[0];
        x2     := cpos1[0] - pos0[0];
        x3     := pos1[0] - pos0[0];
        y1     := cpos0[1] - pos0[1];
        y2     := cpos1[1] - pos0[1];
        y3     := pos1[1] - pos0[1];
        width  := (ABS(vertex0.size[0]) + ABS(edge.width) / 2.0);
        height := (ABS(vertex0.size[1]) + ABS(edge.width) / 2.0);
      BEGIN
        PROCEDURE Inside (t: REAL): BOOLEAN =
          VAR x := Cubic(0.0, x1, x2, x3, t);
          BEGIN
            x := (x + x) / width;
            IF ABS(x) > 1.0 THEN RETURN FALSE; END;
            VAR y := Cubic(0.0, y1, y2, y3, t := t);
            BEGIN
              y := (y + y) / height;
              IF ABS(y) > 1.0 THEN RETURN FALSE; END;
              CASE vertex0.shape OF
              | VertexShape.Rectangle => RETURN TRUE;
              | VertexShape.Ellipse => RETURN x * x + y * y < 1.0;
              END;
            END;
          END Inside;
        BEGIN
          VAR i := 0;
              rnd := NEW(Random.Default).init();
          BEGIN
            LOOP
              t := rnd.real();
              IF NOT Inside(t) THEN EXIT; END;
              i := i + 1;
              IF i = 100 THEN t := 0.0; EXIT; END;
            END;
          END;
          VAR
            t0          := 0.0;
            t1          := t;
            pixelLength := PixelLength(graph, x1, x2, x3, y1, y2, y3);
          BEGIN
            REPEAT
              t := (t0 + t1) / 2.0;
              IF Inside(t) THEN t0 := t; ELSE t1 := t; END;
              pixelLength := pixelLength DIV 2;
            UNTIL pixelLength = 0;
            t := t1;
          END;
        END;
        tip := R2.T{pos0[0] + Cubic(0.0, x1, x2, x3, t := t),
                    pos0[1] + Cubic(0.0, y1, y2, y3, t := t)};
        VAR
          d := R2.T{DCubic(0.0, x1, x2, x3, t := t),
                    DCubic(0.0, y1, y2, y3, t := t)};
          length := R2.Length(d);
        BEGIN
          delta := R2.Scale(1.0 / length, d);
          IF NOT RealFloat.Finite(delta[0] + delta[1]) THEN
            delta := R2.T{1.0, 0.0};
          END;
        END;
      END;
    END;
  END ComputeArrowTipOfBezier;

<* LL.sup >= graph.mu, graph.mgv.mu *>

PROCEDURE PixelLength (graph: T; x1, x2, x3, y1, y2, y3: REAL): INTEGER =
  VAR
    horScale := FLOAT(graph.rect.east - graph.rect.west) / ABS(
                  graph.world.e - graph.world.w);
    verScale := FLOAT(graph.rect.south - graph.rect.north) / ABS(
                  graph.world.s - graph.world.n);
  BEGIN
    RETURN CEILING((ABS(x1) + ABS(x2 - x1) + ABS(x3 - x2)) * horScale
                     + (ABS(y1) + ABS(y2 - y1) + ABS(y3 - y2)) * verScale);
  END PixelLength;

<* LL.sup >= RefList.First(polygon.vertices).graph.mu, RefList.First(polygon.vertices).graph.mgv.mu *>

PROCEDURE RefreshPolygon (polygon: Polygon) =
  BEGIN
    WITH graph = polygon.graph DO
      VAR
        vertices             := polygon.vertices;
        positions: RefList.T := NIL;
      BEGIN
        WHILE vertices # NIL DO
          TYPECASE RefListUtils.Pop(vertices) OF
          | Vertex (vertex) =>
              RefListUtils.Push(positions, NewR2(vertex.pos));
          | RefList.T (v2) =>
              VAR p2: RefList.T;
              BEGIN
                WHILE v2 # NIL DO
                  VAR vertex: Vertex := RefListUtils.Pop(v2);
                  BEGIN
                    RefListUtils.Push(p2, NewR2(vertex.pos));
                  END;
                END;
                RefListUtils.Push(positions, RefList.ReverseD(p2));
              END;
          ELSE                   <* ASSERT FALSE *>
          END;
        END;
        positions := RefList.ReverseD(positions);
        polygon.pos := positions;
      END;
      VAR
        origin: R2.T;
        path         := NEW(R2Path.T);
      BEGIN
        path.init();
        path.moveTo(R2.Origin);
        VAR positions := polygon.pos;
        BEGIN
          VAR pos := NARROW(RefListUtils.Pop(positions), REF R2.T)^;
          BEGIN
            origin := WorldPosToPts(graph, pos);
          END;
          VAR previous := origin;
          BEGIN
            WHILE positions # NIL DO
              TYPECASE RefListUtils.Pop(positions) OF
              | REF R2.T (r2) =>
                  VAR pos := WorldPosToPts(graph, r2^);
                  BEGIN
                    path.lineTo(Finite2(R2.Sub(pos, origin)));
                    previous := pos;
                  END;
              | RefList.T (r2s) =>
                  VAR
                    pos0 := WorldPosToPts(
                              graph, NARROW(RefList.Nth(r2s, 0), REF R2.T)^);
                    pos1 := WorldPosToPts(
                              graph, NARROW(RefList.Nth(r2s, 1), REF R2.T)^);
                    pos2 := WorldPosToPts(
                              graph, NARROW(RefList.Nth(r2s, 2), REF R2.T)^);
                  BEGIN
                    path.curveTo(Finite2(R2.Sub(pos0, origin)),
                                 Finite2(R2.Sub(pos1, origin)),
                                 Finite2(R2.Sub(pos2, origin)));
                    previous := pos2;
                  END;
              ELSE               <* ASSERT FALSE *>
              END;
            END;
          END;
        END;
        path.close();
        NARROW(polygon.mg, MG.Shape).reshape(
          graph.mgv, origin, path, fill := TRUE);
      END;
    END;
  END RefreshPolygon;

(* CONSISTENCY UTLITIES *)

<* LL.sup >= vertex.graph.mu, vertex.graph.mgv.mu *>

PROCEDURE AdjustVertex (vertex: Vertex) =
  BEGIN
    WITH graph = vertex.graph DO
      VAR centerPP := WorldPosToPts(graph, vertex.pos);
      BEGIN
        VAR
          border := MIN(MIN(vertex.border, vertex.size[0] / 2.0),
                        vertex.size[1] / 2.0);
        BEGIN
          CASE vertex.shape OF
          | VertexShape.Rectangle =>
              NARROW(vertex.mg, MG.Rectangle).reshape(
                graph.mgv, Finite2(SWFromCenter(centerPP, vertex.size)),
                Finite2(NEFromCenter(centerPP, vertex.size)));
              NARROW(vertex.mg, MG.Rectangle).setWeight(
                graph.mgv, Pts.FromMM(border));
          | VertexShape.Ellipse =>
              NARROW(vertex.mg, MG.Ellipse).reshape(
                graph.mgv, Finite2(SWFromCenter(centerPP, vertex.size)),
                Finite2(NEFromCenter(centerPP, vertex.size)));
              NARROW(vertex.mg, MG.Ellipse).setWeight(
                graph.mgv, Pts.FromMM(border));
          END;
        END;
        VAR edges := vertex.edges;
        BEGIN
          WHILE edges # NIL DO
            VAR edge: Edge := RefListUtils.Pop(edges);
            BEGIN
              RefreshEdge(edge);
            END;
          END;
        END;
        VAR vertexHighlights := vertex.vertexHighlights;
        BEGIN
          WHILE vertexHighlights # NIL DO
            VAR
              vertexHighlight: VertexHighlight := RefListUtils.Pop(
                                                    vertexHighlights);
            BEGIN
              EVAL AdjustVertexHighlightSizeandShape(vertexHighlight);
            END;
          END;
        END;
        VAR polygons := vertex.polygons;
        BEGIN
          WHILE polygons # NIL DO
            VAR polygon: Polygon := RefListUtils.Pop(polygons);
            BEGIN
              RefreshPolygon(polygon);
            END;
          END;
        END;
      END;
    END;
  END AdjustVertex;

<* LL.sup >= vertexHighlight.vertex.graph.mu, vertexHighlight.vertex.graph.mgv.mu *>

PROCEDURE AdjustVertexHighlightSizeandShape (vertexHighlight: VertexHighlight):
  BOOLEAN =
  BEGIN
    WITH graph = vertexHighlight.graph DO
      VAR
        size := R2.T{MAX(ABS(vertexHighlight.vertex.size[0])
                             + 2.0 * vertexHighlight.border[0], 0.0),
                       MAX(ABS(vertexHighlight.vertex.size[1])
                             + 2.0 * vertexHighlight.border[1], 0.0)};
      BEGIN
        IF (vertexHighlight.size[0] = size[0]
              AND vertexHighlight.size[1] = size[1])
             AND vertexHighlight.shape = vertexHighlight.vertex.shape THEN
          RETURN FALSE;
        END;
        vertexHighlight.size := size;
        IF vertexHighlight.shape # vertexHighlight.vertex.shape THEN
          vertexHighlight.group.remove(graph.mgv, vertexHighlight.mg);
          vertexHighlight.shape := vertexHighlight.vertex.shape;
          CASE vertexHighlight.shape OF
          | VertexShape.Rectangle =>
              vertexHighlight.mg :=
                NEW(MG.Rectangle, color := vertexHighlight.colorScheme,
                    weight := 0.0).init(R2.Origin, R2.Origin);
          | VertexShape.Ellipse =>
              vertexHighlight.mg :=
                NEW(MG.Ellipse, color := vertexHighlight.colorScheme,
                    weight := 0.0).init(R2.Origin, R2.Origin);
          END;
          vertexHighlight.group.addBefore(graph.mgv, vertexHighlight.mg, NIL);
        END;
      END;
      VAR centerPP := WorldPosToPts(graph, vertexHighlight.vertex.pos);
      BEGIN
        CASE vertexHighlight.shape OF
        | VertexShape.Rectangle =>
            NARROW(vertexHighlight.mg, MG.Rectangle).reshape(
              graph.mgv,
              Finite2(SWFromCenter(centerPP, vertexHighlight.size)),
              Finite2(NEFromCenter(centerPP, vertexHighlight.size)));
        | VertexShape.Ellipse =>
            NARROW(vertexHighlight.mg, MG.Ellipse).reshape(
              graph.mgv,
              Finite2(SWFromCenter(centerPP, vertexHighlight.size)),
              Finite2(NEFromCenter(centerPP, vertexHighlight.size)));
        END;
      END;
    END;
    RETURN TRUE;
  END AdjustVertexHighlightSizeandShape;

(* UTILITIES ON COORDINATES *)

<* LL.sup >= graph.mu *>

PROCEDURE ScreenPtToWorldPos (graph: T; READONLY pt: Point.T): R2.T =
  VAR
    res := VBT.ScreenTypeOf(MultiFilter.Child(graph)).res;
    pos := R2.T{(FLOAT(pt.h - graph.rect.west) / res[Axis.T.Hor])
                  - graph.realMarginMM[0],
                (FLOAT(graph.rect.south - pt.v) / res[Axis.T.Ver])
                  - graph.realMarginMM[1]};
  BEGIN
    IF graph.world.e < graph.world.w THEN
      pos[0] := graph.world.w - pos[0]
    ELSE
      pos[0] := graph.world.w + pos[0]
    END;
    IF graph.world.s < graph.world.n THEN
      pos[1] := graph.world.s + pos[1]
    ELSE
      pos[1] := graph.world.s - pos[1]
    END;
    RETURN pos;
  END ScreenPtToWorldPos;

<* LL.sup >= graph.mu *>

PROCEDURE WorldPosToPts (graph: T; READONLY posW: R2.T): R2.T =
  VAR h, v: REAL;
  BEGIN
    IF graph.world.e < graph.world.w THEN
      h := graph.realMarginMM[0] + (graph.world.w - posW[0])
    ELSE
      h := graph.realMarginMM[0] + (posW[0] - graph.world.w)
    END;
    IF graph.world.s < graph.world.n THEN
      v := graph.realMarginMM[1] + (posW[1] - graph.world.s)
    ELSE
      v := graph.realMarginMM[1] + (graph.world.s - posW[1])
    END;
    RETURN R2.T{Pts.FromMM(h), Pts.FromMM(v)}
  END WorldPosToPts;

<* LL.sup >= graph.mu *>

PROCEDURE WorldSizeToPts (graph: T; READONLY sizeW: R2.T): R2.T =
  VAR h := Pts.FromMM(sizeW[0]); v := Pts.FromMM(sizeW[1]);
  BEGIN
    IF graph.world.e < graph.world.w THEN h := -h END;
    IF graph.world.s > graph.world.n THEN v := -v END;
    RETURN R2.T{h, v};
  END WorldSizeToPts;

<* LL arbitrary *>

PROCEDURE SWFromCenter (READONLY centerPP: R2.T; READONLY size: R2.T): R2.T =
  BEGIN
    RETURN R2.T{centerPP[0] - Pts.FromMM(size[0]) / 2.0,
                centerPP[1] - Pts.FromMM(size[1]) / 2.0};
  END SWFromCenter;

<* LL arbitrary *>

PROCEDURE NEFromCenter (READONLY centerPP: R2.T; READONLY size: R2.T): R2.T =
  BEGIN
    RETURN R2.T{centerPP[0] + Pts.FromMM(size[0]) / 2.0,
                centerPP[1] + Pts.FromMM(size[1]) / 2.0};
  END NEFromCenter;

(* MISCELLANEOUS UTILITIES *)

<* LL arbitrary *>

PROCEDURE Abs (READONLY d: R2.T): R2.T =
  BEGIN
    RETURN R2.T{ABS(d[0]), ABS(d[1])};
  END Abs;

<* LL arbitrary *>

PROCEDURE Finite2 (z: R2.T): R2.T =
  BEGIN
    IF NOT RealFloat.Finite(z[0]) THEN z[0] := 0.0; END;
    IF NOT RealFloat.Finite(z[1]) THEN z[1] := 0.0; END;
    RETURN z;
  END Finite2;

(* TIME FUNCTION *)

TYPE
  AffineTimeFunction = Animate.TimeFunction OBJECT
                         a, b: REAL;
                       OVERRIDES
                         map := AffineMap;
                       END;

<* LL arbitrary *>

PROCEDURE AffineMap (self: AffineTimeFunction; t: REAL): REAL =
  BEGIN
    RETURN MIN(MAX(self.a * t + self.b, 0.0), 1.0);
  END AffineMap;

(* ANIMATION PATHS *)

TYPE
  StraightPath = AnimationPath OBJECT
                   p0, p1: R2.T;
                 OVERRIDES
                   pos := StraightPathPos;
                 END;

<* LL arbitrary *>

PROCEDURE StraightPathPos (self: StraightPath; t: REAL): R2.T =
  BEGIN
    RETURN R2.Add(self.p0, R2.Scale(t, R2.Sub(self.p1, self.p0)));
  END StraightPathPos;

TYPE
  OffsetPath = AnimationPath OBJECT
                 path            : AnimationPath;
                 offset0, offset1: R2.T;
               OVERRIDES
                 pos := OffsetPathPos;
               END;

<* LL vertex.graph.mu *>

PROCEDURE OffsetPathPos (self: OffsetPath; t: REAL): R2.T =
  BEGIN
    RETURN R2.Add(self.path.pos(t),
                  R2.Add(self.offset0,
                         R2.Scale(t, R2.Sub(self.offset1, self.offset0))));
  END OffsetPathPos;

(* ANIMATIONS *)

TYPE
  AlongGivenPath =
    Animate.T BRANDED OBJECT
      (* READONLY after initialization: MUST be initialized by client *)
      (* CONST *)
      graph: T;
      pos: R2.T;                (* the current center, in world
                                   coordinates *)
      path: AnimationPath;
    OVERRIDES
      length := LengthAlongGivenPath;
      doStep := DoStepAlongGivenPath;
    END;

<* LL <= VBT.mu *>

PROCEDURE LengthAlongGivenPath (<* UNUSED *> alongGivenPath: AlongGivenPath;
                                <* UNUSED *> v : MG.V;
                                <* UNUSED *> mg: MG.T  ): INTEGER =
  BEGIN
    RETURN 100;
  END LengthAlongGivenPath;

<* LL <= VBT.mu *>

PROCEDURE DoStepAlongGivenPath (alongGivenPath: AlongGivenPath;
                                time          : REAL;
                                <* UNUSED *> timePrev: REAL;
                                             v       : MG.V;
                                             mg      : MG.T  ) =
  VAR newPos: R2.T;
  BEGIN
    LOCK alongGivenPath.graph.mu DO
      LOCK v.mu DO
        newPos := alongGivenPath.path.pos(time);
        MG.RTranslateLocked(
          mg, v,
          Finite2(WorldSizeToPts(alongGivenPath.graph,
                                 R2.Sub(newPos, alongGivenPath.pos))));
      END;
      alongGivenPath.pos := newPos;
    END;
  END DoStepAlongGivenPath;

TYPE
  BezierAnimation =
    Animate.T BRANDED OBJECT
      (* READONLY after initialization: MUST be initialized by client *)
      (* CONST *)
      graph                     : T;
      pathA, pathB, pathC, pathD: AnimationPath;
    OVERRIDES
      length := LengthBezierAnimation;
      doStep := DoStepBezierAnimation;
    END;

<* LL<= VBT.mu *>

PROCEDURE LengthBezierAnimation (<* UNUSED *> bezierAnimation: BezierAnimation;
                                 <* UNUSED *> v : MG.V;
                                 <* UNUSED *> mg: MG.T  ): INTEGER =
  BEGIN
    RETURN 100;
  END LengthBezierAnimation;

<* LL<= VBT.mu *>

PROCEDURE DoStepBezierAnimation (bezierAnimation: BezierAnimation;
                                 time           : REAL;
                                 <* UNUSED *> timePrev: REAL;
                                              v       : MG.V;
                                              mg      : MG.T  ) =
  VAR
    a, b, c, d: R2.T;
    path := NEW(R2Path.T);
  BEGIN
    path.init();
    path.moveTo(R2.Origin);
    LOCK bezierAnimation.graph.mu DO
      LOCK bezierAnimation.graph.mgv.mu DO
        a := WorldPosToPts(bezierAnimation.graph, bezierAnimation.pathA.pos(time));
        b := WorldPosToPts(bezierAnimation.graph, bezierAnimation.pathB.pos(time));
        c := WorldPosToPts(bezierAnimation.graph, bezierAnimation.pathC.pos(time));
        d := WorldPosToPts(bezierAnimation.graph, bezierAnimation.pathD.pos(time));
        path.curveTo(Finite2(R2.Sub(b, a)), Finite2(R2.Sub(c, a)),
                     Finite2(R2.Sub(d, a)));
        NARROW(mg, MG.Shape).reshape(v, a, path, fill := FALSE);
      END;
    END
  END DoStepBezierAnimation;

TYPE
  LinearResize =
    Animate.T BRANDED OBJECT
      (* READONLY after initialization: MUST be initialized by client *)
      (* CONST *)
      graph: T;
      shape: VertexShape;
      corner0, corner1: ARRAY [0 .. 1] (* time *)
                          OF
                          R2.T;  (* coordinates, in points *)
    OVERRIDES
      length := LengthLinearResize;
      doStep := DoStepLinearResize;
    END;

<* LL <= VBT.mu *>

PROCEDURE LengthLinearResize (             linearResize: LinearResize;
                                           v           : MG.V;
                              <* UNUSED *> mg          : MG.T          ):
  INTEGER =
  BEGIN
    RETURN
      ROUND(
        MAX(Pts.ToPixels(
              v, MAX(ABS(linearResize.corner0[1][0]
                           - linearResize.corner0[0][0]),
                     ABS(linearResize.corner1[1][0]
                           - linearResize.corner1[0][0])), Axis.T.Hor),
            Pts.ToPixels(
              v, MAX(ABS(linearResize.corner0[1][1]
                           - linearResize.corner0[0][1]),
                     ABS(linearResize.corner1[1][1]
                           - linearResize.corner1[0][1])), Axis.T.Ver)));
  END LengthLinearResize;

<* LL <= VBT.mu *>

PROCEDURE DoStepLinearResize (             linearResize: LinearResize;
                                           time        : REAL;
                              <* UNUSED *> timePrev    : REAL;
                                           v           : MG.V;
                                           mg          : MG.T          ) =
  BEGIN
    LOCK v.mu DO
      CASE linearResize.shape OF
      | VertexShape.Rectangle =>
          NARROW(mg, MG.Rectangle).reshape(
            v, Finite2(
                 R2.Add(linearResize.corner0[0],
                        R2.Scale(time, R2.Sub(linearResize.corner0[1],
                                              linearResize.corner0[0])))),
            Finite2(
              R2.Add(linearResize.corner1[0],
                     R2.Scale(time, R2.Sub(linearResize.corner1[1],
                                           linearResize.corner1[0])))));
      | VertexShape.Ellipse =>
          NARROW(mg, MG.Ellipse).reshape(
            v, Finite2(
                 R2.Add(linearResize.corner0[0],
                        R2.Scale(time, R2.Sub(linearResize.corner0[1],
                                              linearResize.corner0[0])))),
            Finite2(
              R2.Add(linearResize.corner1[0],
                     R2.Scale(time, R2.Sub(linearResize.corner1[1],
                                           linearResize.corner1[0])))));
      END;
    END;
  END DoStepLinearResize;

TYPE
  PolygonAnimation =
    Animate.T BRANDED OBJECT
      (* READONLY after initialization: MUST be initialized by client *)
      (* CONST *)
      graph: T;
      paths: RefList.T (* OF AnimationPath or list of 3 AnimationPaths *);
    OVERRIDES
      length := LengthPolygonAnimation;
      doStep := DoStepPolygonAnimation;
    END;

<* LL <= VBT.mu *>

PROCEDURE LengthPolygonAnimation (<* UNUSED *> polygonAnimation: PolygonAnimation;
                                  <* UNUSED *> v : MG.V;
                                  <* UNUSED *> mg: MG.T  ): INTEGER =
  BEGIN
    RETURN 100;
  END LengthPolygonAnimation;

<* LL <= VBT.mu *>

PROCEDURE DoStepPolygonAnimation (polygonAnimation: PolygonAnimation;
                                  time            : REAL;
                                  <* UNUSED *> timePrev: REAL;
                                               v       : MG.V;
                                               mg      : MG.T  ) =
  VAR
    p                                        := NEW(R2Path.T);
    paths : RefList.T (* OF AnimationPath *) := polygonAnimation.paths;
    origin: R2.T;
  BEGIN
    LOCK polygonAnimation.graph.mu DO
      LOCK v.mu DO
        p.init();
        p.moveTo(R2.Origin);
        VAR path: AnimationPath := RefListUtils.Pop(paths);
        BEGIN
          origin :=
            Finite2(WorldPosToPts(polygonAnimation.graph, path.pos(time)));
        END;
        WHILE paths # NIL DO
          TYPECASE RefListUtils.Pop(paths) OF
          | AnimationPath (path) =>
              BEGIN
                VAR
                  posPts := WorldPosToPts(
                              polygonAnimation.graph, path.pos(time));
                BEGIN
                  p.lineTo(Finite2(R2.Sub(posPts, origin)));
                END;
              END;
          | RefList.T (ps) =>
              <* ASSERT RefList.Length(ps) = 3 *>
              BEGIN
                VAR
                  pos0Pts := WorldPosToPts(polygonAnimation.graph,
                                           NARROW(RefList.Nth(ps, 0),
                                                  AnimationPath).pos(time));
                  pos1Pts := WorldPosToPts(polygonAnimation.graph,
                                           NARROW(RefList.Nth(ps, 1),
                                                  AnimationPath).pos(time));
                  pos2Pts := WorldPosToPts(polygonAnimation.graph,
                                           NARROW(RefList.Nth(ps, 2),
                                                  AnimationPath).pos(time));
                BEGIN
                  p.curveTo(Finite2(R2.Sub(pos0Pts, origin)),
                            Finite2(R2.Sub(pos1Pts, origin)),
                            Finite2(R2.Sub(pos2Pts, origin)));
                END;
              END;
          ELSE                   <* ASSERT FALSE *>
          END;
        END;
        p.close();
        NARROW(mg, MG.Shape).reshape(v, origin, p, fill := TRUE);
      END;
    END;
  END DoStepPolygonAnimation;

(* BEZIER UTILITIES *)

<* LL arbitrary *>

PROCEDURE Cubic (c0, c1, c2, c3: REAL; t: REAL): REAL =
  BEGIN
    RETURN c0 + t * (3.0 * (c1 - c0)
                       + t * (3.0 * ((c2 - c1) - (c1 - c0))
                                + t * ((c3 - c0) - 3.0 * (c2 - c1))));
  END Cubic;

<* LL arbitrary *>

PROCEDURE DCubic (c0, c1, c2, c3: REAL; t: REAL): REAL =
  BEGIN
    RETURN 3.0 * ((c1 - c0) + t * (2.0 * ((c2 - c1) - (c1 - c0))
                                     + t * ((c3 - c0) - 3.0 * (c2 - c1))));
  END DCubic;

<* LL arbitrary *>

PROCEDURE SubCubic (VAR (*INOUT*) c0, c1, c2, c3: REAL; half: [0 .. 1]) =
  BEGIN
    CASE half OF
    | 0 =>
        c3 := (c0 + 3.0 * (c1 + c2) + c3) / 8.0;
        c2 := (c0 + 2.0 * c1 + c2) / 4.0;
        c1 := (c0 + c1) / 2.0;
    | 1 =>
        c0 := (c0 + 3.0 * (c1 + c2) + c3) / 8.0;
        c1 := (c1 + 2.0 * c2 + c3) / 4.0;
        c2 := (c2 + c3) / 2.0;
    END;
  END SubCubic;

<* LL arbitrary *>

PROCEDURE SubCubic2 (VAR (*INOUT*) c0, c1, c2, c3: R2.T; half: [0 .. 1]) =
  BEGIN
    SubCubic(c0[0], c1[0], c2[0], c3[0], half);
    SubCubic(c0[1], c1[1], c2[1], c3[1], half);
  END SubCubic2;

<* LL arbitrary *>

PROCEDURE CubicBounds (c0, c1, c2, c3: REAL): R2.T =
  BEGIN
    RETURN
      R2.T{MIN(MIN(c0, c1), MIN(c2, c3)), MAX(MAX(c0, c1), MAX(c2, c3))};
  END CubicBounds;

(* RANDOM UTILITIES *)

<* LL arbitrary *>

PROCEDURE R2Intersect (x, y: R2.T): BOOLEAN =
  BEGIN
    RETURN MAX(x[0], y[0]) <= MIN(x[1], y[1]);
  END R2Intersect;

PROCEDURE NewR2 (x: R2.T): REF R2.T =
  VAR a := NEW(REF R2.T);
  BEGIN
    a^ := x;
    RETURN a;
  END NewR2;

PROCEDURE GetMG(graph: T): MG.V =
  BEGIN
    RETURN graph.mgv
  END GetMG;

BEGIN
  <* ASSERT FloatMode.IEEE *>
  DefaultFont := MakeWorldFont(NIL);
END GraphVBT.
