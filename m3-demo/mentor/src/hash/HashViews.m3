(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(*      modified on Wed Jul 22 00:37:01 1992 by mhb        *)

MODULE HashViews;

IMPORT Axis, ColorName, Filter, FormsVBT, GraphVBT, HashIE, HashViewClass, 
       IntList, PaintOp, R2, Rect, Text, TextVBT, Thread, VBT, View, ZeusPanel;

TYPE
  Level = [0..3];
  T = HashViewClass.T BRANDED OBJECT
        mg: MyGraphVBT;
        bucketContents : REF ARRAY OF GraphVBT.Vertex;
        bucketState: REF ARRAY OF GraphVBT.Vertex;
        oldColors: REF ARRAY OF ARRAY Level OF PaintOp.T;
        level: Level;
        opInsert, opDelete, opFind, item: GraphVBT.Vertex;
        operationHighlight, itemHighlight: GraphVBT.VertexHighlight;
        normalColor, compareColor, newColor, checkDeletableColor,
          checkHashPositionColor, operationHighlightColor,
          itemColor, nilColor, emptyColor: PaintOp.T;
      OVERRIDES
        oeSetup := Setup;
        oeInsert := Insert;
        oeDelete := Delete;
        oeFind := Find;
        oeCompare := Compare;
        oeAddToBucket := AddToBucket;
        oeDeleteFromBucket := DeleteFromBucket;
        oeCheckDeletable := CheckDeletable;
        oeCheckHashPosition := CheckHashPosition;
        ueFindReport := FindReport;
        ueStopFindReport := StopFindReport;
      END;

  MyGraphVBT = GraphVBT.T OBJECT
      view: T;
      showingFindPath: BOOLEAN := FALSE;
    OVERRIDES
      mouse := Mouse;
    END;

PROCEDURE Mouse (self: MyGraphVBT; READONLY cd: VBT.MouseRec) =
  <*FATAL Thread.Alerted*>
  BEGIN
    LOCK self.mu DO
      IF (cd.clickType = VBT.ClickType.FirstDown) AND NOT cd.cp.gone
           AND NOT cd.cp.offScreen AND NOT self.showingFindPath THEN
        (* Show find path *)
        WITH list = self.verticesAt(Rect.FromPoint(cd.cp.pt)) DO
          IF list = NIL THEN RETURN END;
          WITH label = NARROW (list.head, GraphVBT.Vertex).label DO
            IF label = NIL OR Text.Empty(label) OR Text.Equal(label, "NIL")
                 OR Text.Equal(label, "EMPTY") THEN
              RETURN;
            END;
            Thread.Release(self.mu);
            TRY
              HashIE.ReportFind(self.view, label);
            FINALLY
              Thread.Acquire(self.mu);
            END;
          END;
        END;
      ELSIF self.showingFindPath THEN
        Thread.Release(self.mu);
        TRY
          HashIE.StopReportFind(self.view);
        FINALLY
          Thread.Acquire(self.mu);
        END;
      END;
    END;
  END Mouse;

PROCEDURE FindReport (view: T; buckets: IntList.T) =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      FOR i := 0 TO LAST(view.bucketState^) DO
        SetBucketColor(view, i, PaintOp.Bg, 3);
      END;
      WHILE buckets # NIL DO
        SetBucketColor(view, buckets.head, view.compareColor, 3);
        buckets := buckets.tail;
      END;
      view.mg.showingFindPath := TRUE;
    END;
    view.mg.redisplay();
  END FindReport;

PROCEDURE StopFindReport (view: T) =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      ResetColors(view, 2);
      view.mg.showingFindPath := FALSE;
    END;
    view.mg.redisplay();
  END StopFindReport;

PROCEDURE Setup (view: T; data: FormsVBT.T; nBuckets: INTEGER) =

  PROCEDURE GetColor (name: TEXT): PaintOp.T =
    <* FATAL ColorName.NotFound, FormsVBT.Error, FormsVBT.Unimplemented *>
    VAR rgb := ColorName.ToRGB(FormsVBT.GetText(data, name));
    BEGIN
      RETURN PaintOp.FromRGB(
               rgb.r, rgb.g, rgb.b,
               PaintOp.Mode.Accurate);
    END GetColor;

  BEGIN
    view.normalColor := GetColor("NormalColor");
    view.nilColor := GetColor("NilColor");
    view.emptyColor := GetColor("EmptyColor");
    view.compareColor := GetColor("CompareColor");
    view.newColor := GetColor("NewColor");
    view.checkDeletableColor := GetColor("CheckDeletableColor");
    view.checkHashPositionColor := GetColor("CheckHashPositionColor");
    view.operationHighlightColor := GetColor("OperationHighlightColor");
    view.itemColor := GetColor("ItemColor");

    view.mg := NEW(MyGraphVBT, world := GraphVBT.WorldRectangle{
                                          w := -3.0, e := 2.5, n :=
                                          FLOAT(nBuckets + 1), s := -1.0},
                   pixelSizeDivisor :=
                     ARRAY [0 .. 1] OF CARDINAL{1, 2 * (nBuckets + 2)},
                   view := view).init();
    view.bucketContents := NEW(REF ARRAY OF GraphVBT.Vertex, nBuckets);
    FOR i := 0 TO LAST(view.bucketContents^) DO
      view.bucketContents[i] :=
        NEW(GraphVBT.Vertex, graph := view.mg, pos := R2.T{0.0, FLOAT(i)},
            label := "NIL", color := view.nilColor, border := 0.005,
            borderColor := PaintOp.Bg,
            shape := GraphVBT.VertexShape.Rectangle,
            fontColor := PaintOp.Fg, size := R2.T{1.0, 1.0}).init();
    END;
    view.bucketState := NEW(REF ARRAY OF GraphVBT.Vertex, nBuckets);
    FOR i := 0 TO LAST(view.bucketState^) DO
      view.bucketState[i] :=
        NEW(GraphVBT.Vertex, graph := view.mg, pos := R2.T{1.25, FLOAT(i)},
            color := PaintOp.Bg, shape := GraphVBT.VertexShape.Ellipse,
            size := R2.T{1.0, 1.0}).init();
    END;
    WITH opFont = view.mg.font(weight := "bold", size := 0.05) DO
      view.opInsert :=
        NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{-2.25, FLOAT(nBuckets)}, font := opFont,
            size := R2.T{1.0, 1.0}, fontColor := PaintOp.Fg,
            label := "INS", color := view.newColor).init();
      view.opDelete :=
        NEW(
          GraphVBT.Vertex, graph := view.mg,
          pos := R2.T{-2.25, FLOAT(nBuckets - 2)}, size := R2.T{1.0, 1.0},
          fontColor := PaintOp.Fg, label := "DEL", font := opFont,
          color := view.checkDeletableColor).init();
      view.opFind := NEW(GraphVBT.Vertex, graph := view.mg,
                         pos := R2.T{-2.25, FLOAT(nBuckets - 4)},
                         size := R2.T{1.0, 1.0}, fontColor := PaintOp.Fg,
                         font := opFont, label := "FIND",
                         color := view.compareColor).init();
    END;
    view.item :=
      NEW(GraphVBT.Vertex, graph := view.mg, color := view.itemColor,
          fontColor := PaintOp.Fg, pos := R2.T{-1.25, FLOAT(nBuckets)},
          size := R2.T{1.0, 1.0}).init();
    view.itemHighlight := NEW(
                            GraphVBT.VertexHighlight, vertex := view.item,
                            color := view.operationHighlightColor,
                            border := R2.T{0.25, 0.5}).init();
    view.oldColors := NEW(REF ARRAY OF ARRAY Level OF PaintOp.T, nBuckets);
    view.level := 0;
    view.operationHighlight := NIL;
    EVAL Filter.Replace(view, view.mg);
  END Setup;

PROCEDURE ResetColors (view: T; level: Level) (* LL = view.mg.mu *) =
  BEGIN
    IF view.operationHighlight = NIL THEN
      Thread.Release(view.mg.mu);
      TRY
        view.operationHighlight :=
          NEW(GraphVBT.VertexHighlight, vertex := view.opInsert,
              color := view.operationHighlightColor,
              border := R2.T{0.25, 0.5}).init();
      FINALLY
        Thread.Acquire(view.mg.mu);
      END;
    END;
    IF view.level <= level THEN RETURN; END;
    FOR i := 0 TO LAST(view.bucketState^) DO
      view.bucketState[i].setColor(view.oldColors[i, level]);
    END;
    view.level := level;
  END ResetColors;

PROCEDURE SetBucketColor (view  : T;
                          bucket: INTEGER;
                          color : PaintOp.T;
                          level : CARDINAL   ) =
  BEGIN
    IF level < view.level THEN
      FOR i := 0 TO LAST(view.bucketState^) DO
        view.bucketState[i].setColor(view.oldColors[i, level]);
      END;
    ELSIF level > view.level THEN
      FOR i := 0 TO LAST(view.bucketState^) DO
        FOR l := view.level TO level - 1 DO
          view.oldColors[i, l] := view.bucketState[i].color;
        END;
      END;
    END;
    view.bucketState[bucket].setColor(color);
    view.level := level;
  END SetBucketColor;

PROCEDURE Insert (view: T; item: TEXT) RAISES {Thread.Alerted} =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      ResetColors(view, 0);
      view.operationHighlight.move(view.opInsert);
      view.item.move(pos := R2.T{view.item.pos[0], view.opInsert.pos[1]});
      view.item.setLabel(item);
    END;
    view.mg.animate(0.0, 0.5);
  END Insert;

PROCEDURE Delete (view: T; item: TEXT) RAISES {Thread.Alerted} =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      ResetColors(view, 0);
      view.operationHighlight.move(view.opDelete);
      view.item.move(pos := R2.T{view.item.pos[0], view.opDelete.pos[1]});
      view.item.setLabel(item);
    END;
    view.mg.animate(0.0, 0.5);
  END Delete;

PROCEDURE Find (view: T; item: TEXT) RAISES {Thread.Alerted} =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      ResetColors(view, 0);
      view.operationHighlight.move(view.opFind);
      view.item.move(pos := R2.T{view.item.pos[0], view.opFind.pos[1]});
      view.item.setLabel(item);
    END;
    view.mg.animate(0.0, 0.5);
  END Find;

PROCEDURE Compare (view: T; bucket: INTEGER) =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      SetBucketColor(view, bucket, view.compareColor, 1);
    END;
    view.mg.redisplay();
  END Compare;

PROCEDURE AddToBucket (view: T; key: TEXT; bucket: INTEGER) =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      view.bucketContents[bucket].setLabel(key);
      view.bucketContents[bucket].setColor(view.normalColor);
      SetBucketColor(view, bucket, view.newColor, 2);
    END;
    view.mg.redisplay();
  END AddToBucket;

PROCEDURE DeleteFromBucket (view     : T;
               <* UNUSED *> key      : TEXT;
                            bucket   : INTEGER;
                            markEmpty: BOOLEAN  ) =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      IF markEmpty THEN
        view.bucketContents[bucket].setLabel("EMPTY");
        view.bucketContents[bucket].setColor(view.emptyColor);
      ELSE
        view.bucketContents[bucket].setLabel("NIL");
        view.bucketContents[bucket].setColor(view.nilColor);
      END;
      SetBucketColor(view, bucket, view.newColor, 2);
    END;
    view.mg.redisplay();
  END DeleteFromBucket;

PROCEDURE CheckDeletable (view: T; bucket: INTEGER) =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      SetBucketColor(view, bucket, view.checkDeletableColor, 1);
    END;
    view.mg.redisplay();
  END CheckDeletable;

PROCEDURE CheckHashPosition (view: T; bucket: INTEGER) =
  BEGIN
    IF view.mg = NIL THEN RETURN; END;
    LOCK view.mg.mu DO
      ResetColors(view, 1);
      SetBucketColor(view, bucket, view.checkHashPositionColor, 2);
    END;
    view.mg.redisplay();
  END CheckHashPosition;

TYPE Shape = TextVBT.T OBJECT OVERRIDES shape := MyShape END;

PROCEDURE MyShape (<* UNUSED *> self: Shape; 
                                axis: Axis.T; 
                   <* UNUSED *> n:    CARDINAL): VBT.SizeRange =
  VAR pref: CARDINAL;
  BEGIN
    IF axis = Axis.T.Hor THEN pref := 300 ELSE pref := 800 END;
    RETURN VBT.SizeRange{0, pref, VBT.DefaultShape.hi};
  END MyShape;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(Shape).init("New view"));
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Buckets", "Hash");
END HashViews.
