(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue Jan 31 14:53:49 PST 1995 by kalsow *)
(*      modified on Tue Aug 10 20:52:12 PDT 1993 by weber *)
<* PRAGMA LL *>

MODULE ViewGameBoard;

IMPORT MinimaxViewClass, Filter, RefList, GraphVBT, PaintOp, R2, VBT, View,
       ZeusPanel, GameBoard, Rect, MinimaxIE, Thread;

TYPE
  T = MinimaxViewClass.T BRANDED OBJECT
        graph: GraphVBT.T;       (* set by Setup *)
        cells: ARRAY [0 .. GameBoard.BoardSize],
                 [0 .. GameBoard.BoardSize] OF
                 GraphVBT.Vertex;
        contents: ARRAY [0 .. GameBoard.BoardSize],
                    [0 .. GameBoard.BoardSize] OF
                    GraphVBT.Vertex;
        selected     : GraphVBT.Vertex;
        message      : GraphVBT.Vertex;
        messagePlayer: GraphVBT.Vertex;
      OVERRIDES
        <* LL = VBT.mu *>
        startrun := DoStartRun;
        <* LL = 0 *>
        oeSetup             := DoSetup;
        oePlayerMove        := DoPlayerMove;
        oeHumanCellSelected := DoHumanCellSelected;
        oeHumanIllegalMove  := DoHumanIllegalMove;
        oeFinished          := DoFinished;
        oePlayerThinking    := DoPlayerThinking;
      END;

  Graph = GraphVBT.T BRANDED OBJECT
          OVERRIDES
            <* LL = VBT.mu *>
            mouse := Mouse;
          END;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NIL)
  END New;

PROCEDURE DoStartRun (view: T) =
  BEGIN
    view.graph := NEW(Graph).init();
    EVAL Filter.Replace(view, view.graph);
  END DoStartRun;

PROCEDURE Mouse (g: Graph; READONLY cd: VBT.MouseRec) =
  <*FATAL Thread.Alerted *>
  VAR
    view             := NARROW(VBT.Parent(g), T);
    vList: RefList.T;
  BEGIN
    IF cd.clickType # VBT.ClickType.FirstDown THEN RETURN; END;
    LOCK g.mu DO vList := g.verticesAt(Rect.FromPoint(cd.cp.pt)) END;
    FOR x := 0 TO GameBoard.BoardSize - 1 DO
      FOR y := 0 TO GameBoard.BoardSize - 1 DO
        IF RefList.Member(vList, view.cells[x, y]) THEN
          MinimaxIE.HumanSelection(view, x, y);
        END;
      END;
    END;
  END Mouse;

PROCEDURE DoSetup (view: T) =
  BEGIN
    LOCK view.graph.mu DO
      view.graph.setWorld(GraphVBT.WorldRectangle{
                            w := 0.0, s := FLOAT(GameBoard.BoardSize + 1),
                            e := FLOAT(GameBoard.BoardSize), n := 0.0});
      view.graph.setAspect(
        FLOAT(GameBoard.BoardSize + 1) / FLOAT(GameBoard.BoardSize));
    END;
    FOR x := 0 TO GameBoard.BoardSize - 1 DO
      FOR y := 0 TO GameBoard.BoardSize - 1 DO
        view.cells[x, y] :=
          NEW(GraphVBT.Vertex, graph := view.graph,
              pos := R2.T{FLOAT(x) + 0.5, FLOAT(y) + 0.5},
              size := R2.T{0.95, 0.95},
              color := PaintOp.FromRGB(1.0, 1.0, 1.0)).init();
        view.cells[x, y].toBack(GraphVBT.ZOrder.Background);
      END;
    END;
    (* draw lines *)
    FOR line := 1 TO GameBoard.BoardSize - 1 DO
      EVAL NEW(GraphVBT.Edge,
               vertex0 := NEW(GraphVBT.Vertex, graph := view.graph,
                              pos := R2.T{0.0, FLOAT(line)},
                              size := R2.T{0.0, 0.0}).init(),
               vertex1 :=
                 NEW(GraphVBT.Vertex, graph := view.graph,
                     pos := R2.T{FLOAT(GameBoard.BoardSize), FLOAT(line)},
                     size := R2.T{0.0, 0.0}).init(), width := 0.1,
               color := PaintOp.FromRGB(0.0, 0.0, 0.0)).init();
      EVAL NEW(GraphVBT.Edge,
               vertex0 := NEW(GraphVBT.Vertex, graph := view.graph,
                              pos := R2.T{FLOAT(line), 0.0},
                              size := R2.T{0.0, 0.0}).init(),
               vertex1 :=
                 NEW(GraphVBT.Vertex, graph := view.graph,
                     pos := R2.T{FLOAT(line), FLOAT(GameBoard.BoardSize)},
                     size := R2.T{0.0, 0.0}).init(), width := 0.1,
               color := PaintOp.FromRGB(0.0, 0.0, 0.0)).init();
    END;
    FOR x := 0 TO GameBoard.BoardSize - 1 DO
      view.contents[x, 0] :=
        NEW(GraphVBT.Vertex, graph := view.graph,
            pos := R2.T{FLOAT(x) + 0.5, 0.5},
            shape := GraphVBT.VertexShape.Ellipse, size := R2.T{0.7, 0.7},
            color := PaintOp.FromRGB(0.0, 1.0, 0.0), label := "A",
            font := view.graph.font(
                      "Helvetica", 0.2, GraphVBT.Slant.Roman, "bold", "*"),
            fontColor := PaintOp.FromRGB(0.0, 0.0, 0.0)).init();
      view.contents[x, GameBoard.BoardSize - 1] :=
        NEW(GraphVBT.Vertex, graph := view.graph,
            pos := R2.T{FLOAT(x) + 0.5, FLOAT(GameBoard.BoardSize) - 0.5},
            shape := GraphVBT.VertexShape.Ellipse, size := R2.T{0.7, 0.7},
            color := PaintOp.FromRGB(0.0, 0.0, 1.0), label := "B",
            font := view.graph.font(
                      "Helvetica", 0.2, GraphVBT.Slant.Roman, "bold", "*"),
            fontColor := PaintOp.FromRGB(0.0, 0.0, 0.0)).init();
    END;
    FOR y := 1 TO GameBoard.BoardSize - 2 DO
      FOR x := 0 TO GameBoard.BoardSize - 1 DO
        view.contents[x, y] := NIL;
      END;
      view.message :=
        NEW(GraphVBT.Vertex, graph := view.graph,
            pos := R2.T{1.0, FLOAT(GameBoard.BoardSize) + 0.5},
            size := R2.T{2.0, 0.8},
            color := PaintOp.FromRGB(1.0, 1.0, 1.0), label := "",
            font := view.graph.font(
                      "Helvetica", 0.2, GraphVBT.Slant.Roman, "bold", "*"),
            fontColor := PaintOp.FromRGB(0.0, 0.0, 0.0)).init();
    END;
    view.graph.redisplay();
  END DoSetup;

PROCEDURE DoHumanCellSelected (view: T; xPos, yPos: INTEGER) =
  BEGIN
    view.selected := view.cells[xPos, yPos];
    LOCK view.graph.mu DO
      view.selected.setColor(PaintOp.FromRGB(0.5, 0.5, 0.5));
    END;
    view.graph.redisplay();
  END DoHumanCellSelected;

PROCEDURE DoHumanIllegalMove (view: T) =
  BEGIN
    LOCK view.graph.mu DO
      view.selected.setColor(PaintOp.FromRGB(1.0, 1.0, 1.0));
    END;
    view.graph.redisplay();
  END DoHumanIllegalMove;

PROCEDURE DoPlayerMove (           view                  : T;
                        <*UNUSED*> player                : INTEGER;
                                   fromX, fromY, toX, toY: INTEGER  )
  RAISES {Thread.Alerted} =
  BEGIN
    IF view.contents[toX, toY] # NIL THEN
      view.contents[toX, toY].remove();
    END;
    LOCK view.graph.mu DO
      IF view.selected # NIL THEN
        view.selected.setColor(PaintOp.FromRGB(1.0, 1.0, 1.0));
      END;
      view.contents[fromX, fromY].move(
        R2.T{FLOAT(toX) + 0.5, FLOAT(toY) + 0.5}, animated := TRUE);
    END;
    view.selected := NIL;
    view.contents[toX, toY] := view.contents[fromX, fromY];
    view.contents[fromX, fromY] := NIL;
    view.graph.animate(0.0, 1.0);
  END DoPlayerMove;

PROCEDURE MakePiece (view: T; player: INTEGER; x, y: REAL):
  GraphVBT.Vertex =
  BEGIN
    IF player = 0 THEN
      RETURN
        NEW(GraphVBT.Vertex, graph := view.graph, pos := R2.T{x, y},
            shape := GraphVBT.VertexShape.Ellipse, size := R2.T{0.7, 0.7},
            color := PaintOp.FromRGB(0.0, 1.0, 0.0), label := "A",
            font := view.graph.font(
                      "Helvetica", 0.2, GraphVBT.Slant.Roman, "bold", "*"),
            fontColor := PaintOp.FromRGB(0.0, 0.0, 0.0)).init();
    ELSE
      RETURN
        NEW(GraphVBT.Vertex, graph := view.graph, pos := R2.T{x, y},
            shape := GraphVBT.VertexShape.Ellipse, size := R2.T{0.7, 0.7},
            color := PaintOp.FromRGB(0.0, 0.0, 1.0), label := "B",
            font := view.graph.font(
                      "Helvetica", 0.2, GraphVBT.Slant.Roman, "bold", "*"),
            fontColor := PaintOp.FromRGB(0.0, 0.0, 0.0)).init();
    END;
  END MakePiece;

PROCEDURE DoFinished (view: T; player: INTEGER) =
  BEGIN
    IF view.messagePlayer # NIL THEN
      LOCK view.graph.mu DO view.messagePlayer.remove(); END;
    END;
    IF player = 2 THEN
      LOCK view.graph.mu DO view.message.setLabel("Game is tied."); END;
    ELSE
      LOCK view.graph.mu DO view.message.setLabel("Winner is "); END;
      view.messagePlayer :=
        MakePiece(view, player, 2.5, FLOAT(GameBoard.BoardSize) + 0.5);
    END;
    view.graph.redisplay();
  END DoFinished;

PROCEDURE DoPlayerThinking (           view  : T;
                                       player: INTEGER;
                            <*UNUSED*> board : GameBoard.T) =
  BEGIN
    IF view.messagePlayer # NIL THEN
      LOCK view.graph.mu DO view.messagePlayer.remove(); END;
    END;
    LOCK view.graph.mu DO view.message.setLabel("Player to move: "); END;
    view.messagePlayer :=
      MakePiece(view, player, 2.5, FLOAT(GameBoard.BoardSize) + 0.5);
    view.graph.redisplay();
  END DoPlayerThinking;

BEGIN
  ZeusPanel.RegisterView(New, "Game Board", "Minimax");
END ViewGameBoard.
