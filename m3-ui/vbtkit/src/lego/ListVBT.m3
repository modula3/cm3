(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 17 11:02:46 PDT 1996 by mhb            *)
(*      modified on Fri Jul  9 00:10:05 1993 by gnelson        *)
(*      modified on Mon Jun 14 18:51:57 PDT 1993 by meehan     *)
(*      modified on Tue Jun 16 13:08:43 PDT 1992 by muller     *)
(*      modified on Mon Apr 27 16:08:25 PDT 1992 by birrell    *)
(* ListVBT.m3 *)

MODULE ListVBT EXPORTS ListVBT;

IMPORT Axis, Font, HVSplit, PaintOp, Pixmap, Point, Rect, Region,
       ScrollerVBTClass, Split, TextureVBT, VBT, VBTKitEnv;

(* *)
(* Types and such *)
(* *)

TYPE

  CellContents = RECORD
      value: REFANY;
      selected: BOOLEAN;
    END;

  Scroller = ScrollerVBTClass.T OBJECT
      list: T;
    OVERRIDES
      scroll := Scroll;
      autoScroll := AutoScroll;
      thumb := Thumb;
    END;
    
  Bar = TextureVBT.T OBJECT
      size: REAL := 0.25
    OVERRIDES
      shape := BarShape;
    END;

  Contents = VBT.Leaf OBJECT
      (* The VBT wherein the cells get painted. All painting and mouse
         interpretation is done by this class, including all calls of painter
         and selector methods. *)
      list: T;
      haveScreen: BOOLEAN := FALSE;
      height: INTEGER := 1; (* cached result of painter.height *)
      hasFocus: BOOLEAN := FALSE; (* while we have mouse focus *)
    METHODS
      cellForCP(cp: VBT.CursorPosition;
                VAR cage: VBT.Cage): INTEGER := CellForCP;
        (* LL.sup < list *)
        (* Returns cell number for given cursor position, or -1; if
           cp is above or below "contents", auto-scrolls; in any
           case, assigns to "cage" the minimal cage that would cause
           cellForCP to do something different. *)
      scrollContents(this: INTEGER) := ScrollContents;
        (* LL.sup = list *)
        (* Sets list.firstVisible to boundFirstVisible(this), and does
            scroll/repaints. *)
      boundFirstVisible() := BoundFirstVisible;
        (* LL.sup = list *)
        (* Sets firstVisible to a value that satisfies the firstVisible
           invariant and is as close as possible to the current firstVisible.
           Also calls updateScroller. *)
      moveCells(at: Cell; delta: INTEGER) := MoveCells;
        (* LL.sup = list *)
        (* Fixup the firstVisible invariant, and update the screen for cells
           [ MIN(at, at+delta), ... ).  The pixels at cell positions [at, ...)
           are suitable for cells [at+delta, ...).  Delta might be negative. 
           Also calls updateScroller. *)
      paintCells(at:Cell; n: INTEGER; bad: Rect.T) := PaintCells;
        (* LL.sup = list *)
        (* Repaint the cells; if at+n >= nCells, erase the blank space *)
      selectCell(this: Cell) := SelectCell;
        (* LL.sup = list *)
        (* Uses painter.select to adjust the cell's appearance on-screen. *)
    OVERRIDES
      repaint := Repaint;
      reshape := Reshape;
      rescreen := Rescreen;
      redisplay := Redisplay;
      mouse := Mouse;
      position := Position;
    END;

REVEAL

  Private = HVSplit.T BRANDED "ListVBT.Private 1.0" OBJECT
    END;

  T = Public BRANDED "ListVBT.T 1.0" OBJECT
      mu: MUTEX;                    (* protects all the fields *)
      vScroll: Scroller;            (* sub-window containg the scroll bar *)
      contents: Contents;           (* sub-window containing the cells *)
      cells: REF ARRAY OF CellContents;
      nCells: CARDINAL := 0;        (* total number of cells *)
      nSelections: CARDINAL := 0;   (* total number of selected cells *)
      firstSelected: Cell := 0;     (* first selected cell, for efficiency;
                                       valid iff nSelections > 0 *)
      firstVisible: Cell := 0;      (* first visible cell *)
        (* Invariant: firstVisible+nVisible>nCells iff nCells<nVisible *)
      nVisible: CARDINAL := 0;      (* screen space for visible cells *)
    METHODS
      getNextSelected(VAR this: Cell) := GetNextSelected;
        (* LL.sup = list;
           PRE: list.nSelections > 0 and there exists a selected cell >= this;
           sets "this" to first selected cell >= this *)
      updateScroller() := UpdateScroller;
        (* LL.sup = list; informs the scroller of current position and size *)
    OVERRIDES
      init := Init;
      setValue := SetValue;
      getValue := GetValue;
      count := Count;
      insertCells := InsertCells;
      removeCells := RemoveCells;
      selectNone := SelectNone;
      selectOnly := SelectOnly;
      select := Select;
      isSelected := IsSelected;
      getAllSelected := GetAllSelected;
      getFirstSelected := GetFirstSelected;
      scrollTo := ScrollTo;
      scrollToShow := ScrollToShow;
      redisplay := TRedisplay;
      reportVisible := ReportVisible;
    END;

  TextPainter = TextPainterPublic BRANDED "ListVBT.TextPainter 1.0" OBJECT
      mu: MUTEX;
      eraseColor, textColor, hiliteColor, hiliteTextColor: PaintOp.T;
      font: Font.T;
      ascent, descent: INTEGER;
    OVERRIDES
      init := TextPainterInit;
      height := TextPainterHeight;
      paint := TextPainterPaint;
      select := TextPainterSelect;
      erase := TextPainterErase;
      setFont := TextPainterSetFont;
    END;

  UniSelector = Selector BRANDED "ListVBT.UniSelector 1.0" OBJECT
      list: T;
    OVERRIDES
      init := UniSelectorInit;
      insideClick := UniSelectorInsideClick;
      outsideClick := UniSelectorOutsideClick;
      insideDrag := UniSelectorInsideDrag;
      outsideDrag := UniSelectorOutsideDrag;
    END;

  MultiSelector = Selector BRANDED "ListVBT.MultiSelector 1.0" OBJECT
      list: T;
      anchor: Cell := 0;
      prev: Cell := 0;
      adding: BOOLEAN := FALSE;
    OVERRIDES
      init := MultiSelectorInit;
      insideClick := MultiSelectorInsideClick;
      outsideClick := MultiSelectorOutsideClick;
      insideDrag := MultiSelectorInsideDrag;
      outsideDrag := MultiSelectorOutsideDrag;
    END;


(* *)
(* Implementations of ListVBT methods *)
(* *)

PROCEDURE Init (list: T; colors: PaintOp.ColorQuad): T =
  VAR bar := NEW (Bar).init (colors.fg, Pixmap.Solid);
  BEGIN
    EVAL HVSplit.T.init (list, Axis.T.Hor);
    list.mu := NEW (MUTEX);
    LOCK list.mu DO
      list.cells := NEW (REF ARRAY OF CellContents, 100);
      list.vScroll := NEW (Scroller, list := list).init (Axis.T.Ver, colors);
      list.contents := NEW (Contents, list := list);
      IF VBTKitEnv.ScrollbarWest THEN
        Split.AddChild (list, list.vScroll);
        Split.AddChild (list, bar);
        Split.AddChild (list, list.contents);
      ELSE
        Split.AddChild (list, list.contents);
        Split.AddChild (list, bar);
        Split.AddChild (list, list.vScroll);
      END;
      IF list.painter = NIL THEN
        list.painter :=
          NEW (TextPainter).init (colors.bg, colors.fg, colors.fg, colors.bg)
      END;
      IF list.selector = NIL THEN
        list.selector := NEW (UniSelector).init (list)
      END;
    END;
    RETURN list
  END Init;

PROCEDURE SetValue(list: T; this: Cell; value: REFANY) =
  (* LL.sup < list *)
  BEGIN
    LOCK list.mu DO
      IF (this >= 0) AND (this < list.nCells) THEN
        list.cells[this].value := value;
        list.contents.paintCells(this, 1, Rect.Full);
      END;
    END;
  END SetValue;

PROCEDURE GetValue(list: T; this: Cell): REFANY =
  (* LL.sup < list *)
  BEGIN
    LOCK list.mu DO
      IF this < 0 THEN
        RETURN NIL
      ELSIF this >= list.nCells THEN
        RETURN NIL
      ELSE
        RETURN list.cells[this].value
      END;
    END;
  END GetValue;

PROCEDURE Count(list: T): CARDINAL =
  (* LL.sup < list *)
  BEGIN
    LOCK list.mu DO
      RETURN list.nCells
    END;
  END Count;

PROCEDURE GetNextSelected(list: T; VAR this: Cell) =
  (* LL.sup = list;
     PRE: list.nSelections > 0 and there exists a selected cell >= this *)
  BEGIN
    LOOP
      <* ASSERT(this < list.nCells) *>
      IF list.cells^[this].selected THEN EXIT END;
      INC(this);
    END;
  END GetNextSelected;

PROCEDURE InsertCells(list: T; at: Cell; n: CARDINAL) =
  (* LL.sup < list *)
  VAR first: Cell; oldCells: REF ARRAY OF CellContents;
  BEGIN
    LOCK list.mu DO
      first := MAX(0, MIN(at, list.nCells));
      IF list.firstSelected >= first THEN INC(list.firstSelected, n) END;
      IF n + list.nCells > NUMBER(list.cells^) THEN
        oldCells := list.cells;
        list.cells := NEW(REF ARRAY OF CellContents,
            MAX(n + list.nCells,
                NUMBER(oldCells^) + NUMBER(oldCells^) DIV 2));
        SUBARRAY(list.cells^, 0, NUMBER(oldCells^)) := oldCells^;
      END;
      SUBARRAY(list.cells^, first+n, list.nCells-first) :=
          SUBARRAY(list.cells^, first, list.nCells-first);
      FOR i := first TO first + n - 1 DO
        list.cells^[i] := CellContents{ value := NIL, selected := FALSE };
      END;
      INC(list.nCells, n);
      list.contents.moveCells(first, n);
    END;
  END InsertCells;

PROCEDURE RemoveCells(list: T; at: Cell; n: CARDINAL) =
  (* LL.sup < list *)
  VAR first, this: Cell; amount: CARDINAL;
  BEGIN
    LOCK list.mu DO
      first := MAX(0, MIN(at, list.nCells));
      amount := MIN(at+n, list.nCells) - first;
      IF amount > 0 THEN
        this := first;
        WHILE (list.nSelections > 0) AND (this < first + amount) DO
          IF list.cells^[this].selected THEN
            list.cells^[this].selected := FALSE;
            DEC(list.nSelections);
          END;
          INC(this);
        END;
        (* Now list.firstSelected might be wrong, either because we just
           deselected it, or because it's beyond first+amount and must be
           relocated. *)
        IF list.nSelections > 0 THEN
          IF list.firstSelected >= first THEN
            this := list.firstSelected;
            list.getNextSelected(this);
            list.firstSelected := this - amount;
          END;
        END;
        SUBARRAY(list.cells^, first, list.nCells-(first+amount)) :=
            SUBARRAY(list.cells^, first+amount, list.nCells-(first+amount));
        DEC(list.nCells, amount);
        list.contents.moveCells(first+amount, -amount);
      END;
    END;
  END RemoveCells;

PROCEDURE SelectNone(list: T) =
  (* LL.sup < list *)
  VAR this: INTEGER;
  BEGIN
    LOCK list.mu DO
      this := list.firstSelected;
      WHILE list.nSelections > 0 DO
        list.getNextSelected(this);
        list.cells^[this].selected := FALSE;
        DEC(list.nSelections);
        list.contents.selectCell(this);
      END;
    END;
  END SelectNone;

PROCEDURE SelectOnly(list: T; this: Cell) =
  (* LL.sup < list *)
  BEGIN
    LOCK list.mu DO (* optimise the no-op case, to reduce flicker *)
      IF (this >= 0) AND (this < list.nCells) AND
         (list.nSelections = 1) AND list.cells^[this].selected THEN
        RETURN
      END;
    END;
    list.selectNone();
    list.select(this, TRUE);
  END SelectOnly;

PROCEDURE Select(list: T; this: Cell; selected: BOOLEAN) =
  (* LL.sup < list *)
  BEGIN
    LOCK list.mu DO
      IF (this >= 0) AND (this < list.nCells) THEN
        IF list.cells^[this].selected # selected THEN
          list.cells^[this].selected := selected;
          IF selected THEN
            INC(list.nSelections);
            IF (list.nSelections = 1) OR (this < list.firstSelected) THEN
              list.firstSelected := this;
            END;
          ELSE
            DEC(list.nSelections);
            IF (list.nSelections > 0) AND (this = list.firstSelected) THEN
              list.getNextSelected(list.firstSelected);
            END;
          END;
          list.contents.selectCell(this);
        END;
      END;
    END;
  END Select;

PROCEDURE IsSelected(list: T; this: Cell): BOOLEAN =
  (* LL.sup < list *)
  BEGIN
    LOCK list.mu DO
      IF this < 0 THEN
        RETURN FALSE
      ELSIF this >= list.nCells THEN
        RETURN FALSE
      ELSE
        RETURN list.cells^[this].selected
      END;
    END;
  END IsSelected;

PROCEDURE GetAllSelected(list: T): REF ARRAY OF Cell =
  (* LL.sup < list *)
  VAR sel: REF ARRAY OF Cell; this: Cell;
  BEGIN
    LOCK list.mu DO
      sel := NEW(REF ARRAY OF Cell, list.nSelections);
      this := list.firstSelected;
      FOR i := 0 TO NUMBER(sel^)-1 DO
        list.getNextSelected(this);
        sel^[i] := this;
        INC(this);
      END;
      RETURN sel
    END;
  END GetAllSelected;

PROCEDURE GetFirstSelected(list: T; VAR this: Cell): BOOLEAN =
  (* LL.sup < list *)
  BEGIN
    LOCK list.mu DO
      IF list.nSelections > 0 THEN
        this := list.firstSelected;
        <* ASSERT(list.cells^[this].selected) *>
        RETURN TRUE
      ELSE
        RETURN FALSE
      END;
    END;
  END GetFirstSelected;

PROCEDURE ScrollTo(list: T; this: Cell) =
  (* LL.sup < list *)
  BEGIN
    LOCK list.mu DO
      list.contents.scrollContents(this);
    END;
  END ScrollTo;

PROCEDURE ScrollToShow(list: T; this: Cell) =
  (* LL.sup < list *)
  BEGIN
    LOCK list.mu DO
      IF (this < list.firstVisible) OR
          (this >= list.firstVisible + list.nVisible) THEN
        list.contents.scrollContents(this - list.nVisible DIV 2);
      END;
    END;
  END ScrollToShow;

PROCEDURE TRedisplay(list: T) =
  (* LL.sup = VBT.mu *)
  BEGIN
    HVSplit.T.redisplay(list);
    list.contents.redisplay();
  END TRedisplay;

PROCEDURE ReportVisible(
   <*UNUSED*>list: T; 
   <*UNUSED*>first: Cell; 
   <*UNUSED*>num: CARDINAL) =
  BEGIN
  END ReportVisible;
  
PROCEDURE UpdateScroller(list: T) =
  (* LL.sup = list *)
  BEGIN
    IF list.vScroll # NIL THEN
      ScrollerVBTClass.Update(list.vScroll, list.firstVisible,
                         MIN(list.firstVisible+list.nVisible, list.nCells),
                         list.nCells);
    END;
    list.reportVisible(list.firstVisible, 
      MIN(list.nVisible, list.nCells - list.firstVisible))
  END UpdateScroller;


(* *)
(* Implementations of Contents methods *)
(* *)

PROCEDURE Mouse(contents: Contents; READONLY cd: VBT.MouseRec) =
  (* LL.sup = VBT.mu *)
  VAR
    this: INTEGER; cage: VBT.Cage;
  BEGIN
    IF (cd.clickType = VBT.ClickType.FirstDown) OR contents.hasFocus THEN
      this := contents.cellForCP(cd.cp, cage);
      IF cd.clickType = VBT.ClickType.LastUp THEN
        VBT.SetCage(contents, VBT.EverywhereCage);
        contents.hasFocus := FALSE;
      ELSE
        VBT.SetCage(contents, cage);
        contents.hasFocus := TRUE;
      END;
      IF this >= 0 THEN
        contents.list.selector.insideClick(cd, this);
      ELSE
        contents.list.selector.outsideClick(cd);
      END;
    END;
  END Mouse;

PROCEDURE Position(contents: Contents; READONLY cd: VBT.PositionRec) =
  (* LL.sup = VBT.mu *)
  VAR
    this: INTEGER; cage: VBT.Cage;
  BEGIN
    IF contents.hasFocus THEN
      this := contents.cellForCP(cd.cp, cage);
      VBT.SetCage(contents, cage);
      IF this >= 0 THEN
        contents.list.selector.insideDrag(cd, this);
      ELSE
        contents.list.selector.outsideDrag(cd);
      END;
    END;
  END Position;

PROCEDURE Redisplay(contents: Contents) =
  (* LL.sup = mu *)
  BEGIN
    WITH list = contents.list DO
      LOCK list.mu DO
        IF contents.haveScreen THEN
          contents.height := list.painter.height(contents)
        ELSE
          contents.height := 1;
        END;
      END;
    END;
    VBT.Leaf.redisplay(contents);
  END Redisplay;

PROCEDURE Reshape(contents: Contents; READONLY cd: VBT.ReshapeRec) =
  (* LL.sup = mu.contents *)
  VAR
    wasNormalized: BOOLEAN;
    needsRepaint: Rect.T;
    delta: Point.T;
    oldFirstVisible: Cell;
    firstVisibleDelta: INTEGER;
  BEGIN
    WITH list = contents.list DO
      LOCK list.mu DO
        IF cd.new = Rect.Empty THEN
          list.nVisible := 0;
          firstVisibleDelta := 0;
          list.reportVisible(list.firstVisible, 0)
        ELSE
          wasNormalized := (list.nSelections>0) AND
              (list.firstSelected >= list.firstVisible) AND
              (list.firstSelected < list.firstVisible+list.nVisible);
          list.nVisible := Rect.VerSize(cd.new) DIV contents.height;
          IF wasNormalized AND
              (list.firstSelected >= list.firstVisible+list.nVisible) THEN
            list.firstVisible := list.firstSelected - list.nVisible DIV 2;
          END;
          (* in any case, fix up the firstVisible invariant *)
          oldFirstVisible := list.firstVisible;
          contents.boundFirstVisible();
          firstVisibleDelta :=
              (oldFirstVisible - list.firstVisible) * contents.height;
        END;
      END;
    END;
    (* Salvage old pixels; but RWT's whiteboard says salvage never succeeds. *)
    IF (cd.saved.west <= cd.prev.west) AND
        (cd.saved.east >= cd.prev.east) AND
        (Rect.HorSize(cd.prev) >= Rect.HorSize(cd.new)) THEN
      (* If we don't have full width, we'll repaint the cells anyway *)
      delta := Point.Sub(Rect.NorthWest(cd.new), Rect.NorthWest(cd.prev));
      INC(delta.v, firstVisibleDelta);
      IF delta # Point.Origin THEN
        VBT.Scroll(contents, cd.new, delta);
      END;
      needsRepaint := cd.new;
      needsRepaint.south := needsRepaint.north +
          (cd.saved.north - cd.prev.north) + firstVisibleDelta;
      IF needsRepaint.south > needsRepaint.north THEN
        contents.repaint(Region.FromRect(needsRepaint));
      END;
      needsRepaint := cd.new;
      INC(needsRepaint.north,
          cd.saved.south - cd.prev.north + firstVisibleDelta);
      IF needsRepaint.south > needsRepaint.north THEN
        contents.repaint(Region.FromRect(needsRepaint));
      END;
    ELSE
      contents.repaint(Region.FromRect(cd.new));
    END;
  END Reshape;

PROCEDURE Rescreen(contents: Contents; READONLY cd: VBT.RescreenRec) =
  (* LL.sup = mu.contents *)
  BEGIN
    WITH list = contents.list DO
      LOCK list.mu DO
        IF cd.st = NIL THEN
          contents.haveScreen := FALSE;
          contents.height := 1;
          list.nVisible := 0;
          list.reportVisible(list.firstVisible, 0)
        ELSE
          contents.haveScreen := TRUE;
          contents.height := list.painter.height(contents);
        END;
      END;
    END;
  END Rescreen;

PROCEDURE Repaint(contents: Contents; READONLY rgn: Region.T) =
  (* LL.sup = mu.contents *)
  VAR
    domain := VBT.Domain(contents);
    firstHit, lastHit: Cell;
  BEGIN
    WITH list = contents.list DO
      LOCK contents.list.mu DO
        IF rgn.r.north < domain.north THEN
          firstHit := list.firstVisible
        ELSE
          firstHit := (rgn.r.north-domain.north) DIV contents.height +
                       list.firstVisible;
        END;
        IF rgn.r.south > domain.south THEN
          lastHit := list.firstVisible + list.nVisible
        ELSE
          lastHit := (rgn.r.south-domain.north-1) DIV contents.height +
                     list.firstVisible;
        END;
        contents.paintCells(firstHit, lastHit-firstHit+1, rgn.r);
      END;
    END;
  END Repaint;

PROCEDURE CellForCP(contents: Contents; cp: VBT.CursorPosition;
                    VAR cage: VBT.Cage): INTEGER =
  (* LL.sup < list *)
  VAR
    domain := VBT.Domain(contents);
    cellInDomain: INTEGER; (* cell number relative to list.firstVisible *)
    r := domain;
  BEGIN
    WITH list = contents.list DO
      LOCK list.mu DO
        IF cp.gone THEN
          cage := VBT.EmptyCage;
          IF NOT cp.offScreen THEN
            IF cp.pt.v < domain.north THEN
              contents.scrollContents(list.firstVisible-1);
              IF list.nCells > 0 THEN
                RETURN list.firstVisible
              END;
            ELSIF cp.pt.v >= domain.south THEN
              contents.scrollContents(list.firstVisible+1);
              IF list.nCells > 0 THEN
                RETURN MIN(list.nCells, list.firstVisible + list.nVisible)-1
              END;
            END;
          END;
          RETURN -1
        ELSE
          <* ASSERT(cp.pt.v >= domain.north) *>
          cellInDomain := (cp.pt.v - domain.north) DIV contents.height;
          r.north := domain.north + cellInDomain * contents.height;
          r.south := MIN(r.north + contents.height, domain.south);
          cage := VBT.Cage{ r, VBT.InOut{FALSE}, VBT.AllScreens };
          IF list.firstVisible + cellInDomain >= list.nCells THEN
            RETURN -1
          ELSE
            RETURN list.firstVisible + cellInDomain;
          END;
        END;
      END;
    END;
  END CellForCP;

PROCEDURE ScrollContents(contents: Contents; this: INTEGER) =
  (* LL.sup = list *)
  VAR delta: INTEGER;
  BEGIN
    WITH list = contents.list DO
      delta := list.firstVisible - this;
      list.firstVisible := this;
      contents.moveCells(this, delta);
    END;
  END ScrollContents;

PROCEDURE BoundFirstVisible(contents: Contents) =
  (* LL.sup = list *)
  BEGIN
    WITH list = contents.list DO
      list.firstVisible :=
          MAX(0, MIN(list.firstVisible, list.nCells-list.nVisible) );
      list.updateScroller();
    END;
  END BoundFirstVisible;

PROCEDURE MoveCells(contents: Contents; at: Cell; delta: INTEGER) =
  (* LL.sup = list *)
        (* Fixup the firstVisible invariant, and update the screen for cells
           [ MIN(at, at+delta), ... ).  The pixels at cell positions [at, ...)
           are suitable for cells [at+delta, ...).  Delta might be negative. 
           Also calls updateScroller. *)
  VAR
    oldFirst, adjustment, boundedFirst, boundedDelta: INTEGER;
    boundedAt: Cell;
    domain := VBT.Domain(contents);
    clip: Rect.T;
  BEGIN
    WITH list = contents.list DO
      oldFirst := list.firstVisible;
      contents.boundFirstVisible();
      boundedFirst := list.firstVisible;
      adjustment := oldFirst - boundedFirst;
      boundedDelta := delta + adjustment;
      boundedAt := at - adjustment;
      (* NOTE: at+delta = boundedAt+boundedDelta *)
      IF (adjustment # 0) AND (MIN(boundedAt, at+delta) > boundedFirst) THEN
        (* extra repaint caused by bounding firstVisible *)
        (* repaint [list.firstVisible .. MIN(boundedAt, at+delta) ) *)
        clip := domain;
        clip.south := clip.north +
                (MIN(boundedAt, at+delta)-boundedFirst) * contents.height;
        VBT.Scroll(contents,
                   clip,
                   Point.T{h :=  0, v := adjustment * contents.height}
                   );
        IF adjustment > 0 THEN
          (* repaint newly exposed cells at top *)
          contents.paintCells(boundedFirst, adjustment, Rect.Full);
        END;
      END;
      IF boundedDelta # 0 THEN
        (* repaint [MIN(boundedAt, at+delta) .. ) *)
        clip := domain;
        INC(clip.north,
            (boundedAt+boundedDelta-boundedFirst) * contents.height);
        clip.south := domain.north + list.nVisible * contents.height;
        IF clip.north < clip.south THEN
          (* scroll into [at+delta .. ) *)
          VBT.Scroll(contents,
                     clip,
                     Point.T{h := 0, v := boundedDelta * contents.height}
                     );
        END;
        IF boundedDelta > 0 THEN
          (* repaint [boundedAt .. at+delta) *)
          contents.paintCells(boundedAt, boundedDelta, Rect.Full);
        END;
        IF boundedDelta < 0 THEN
          (* repaint newly exposed cells at bottom *)
          contents.paintCells(
            boundedFirst+list.nVisible+boundedDelta, -boundedDelta, Rect.Full);
        END;
      END;
    END;
  END MoveCells;

PROCEDURE PaintCells(contents: Contents; at: Cell; n: INTEGER; bad: Rect.T) =
  (* LL.sup = list *)
  VAR
    domain := VBT.Domain(contents);
    r := domain;
    start, limit: Cell;
  BEGIN
    WITH list = contents.list DO
      start := MAX(at, list.firstVisible);
      limit := MIN(MIN(at+n, list.firstVisible+list.nVisible), list.nCells);
      FOR this := start TO limit-1 DO
        r.north := domain.north + (this-list.firstVisible) * contents.height;
        r.south := r.north + contents.height;
        list.painter.paint(contents, r, list.cells^[this].value, this,
                                        list.cells^[this].selected,
                                        Rect.Meet (r, bad));
      END;
      IF limit < at+n THEN
        (* erase the rest of the cell positions *)
        r.north := domain.north + (limit-list.firstVisible) * contents.height;
        r.south := domain.north + (at+n-list.firstVisible) * contents.height;
        list.painter.erase(contents, r);
      END;
    END;
  END PaintCells;

PROCEDURE SelectCell(contents: Contents; this: Cell) =
  (* LL.sup = list *)
  VAR r, domain: Rect.T;
  BEGIN
    WITH list = contents.list DO
      domain := VBT.Domain(contents);
      IF domain # Rect.Empty THEN
        IF (this >= list.firstVisible) AND
                                (this < list.firstVisible + list.nVisible) THEN
          r := domain;
          INC(r.north, (this-list.firstVisible) * contents.height);
          r.south := r.north + contents.height;
          list.painter.select(contents, r, list.cells^[this].value, this,
                                           list.cells^[this].selected);
        END;
      END;
    END;
  END SelectCell;


(* *)
(* Implementations of Scroller methods *)
(* *)

PROCEDURE Scroll(scroller: Scroller;
                 <*UNUSED*> READONLY cd: VBT.MouseRec;
                 part: INTEGER;
                 height: INTEGER;
                 towardsEOF: BOOLEAN) =
  (* LL.sup < list *)
  VAR distance: INTEGER;
  BEGIN
    WITH list = scroller.list DO
      LOCK list.mu DO
        distance := MAX(1, (part * list.nVisible) DIV height);
        IF NOT towardsEOF THEN distance := -distance END;
        list.contents.scrollContents(list.firstVisible+distance);
      END;
    END;
  END Scroll;

PROCEDURE AutoScroll (scroller: Scroller;
                      <*UNUSED*> READONLY cd: VBT.MouseRec;
                      linesToScroll: CARDINAL;
                      towardsEOF: BOOLEAN) =
  (* LL.sup < list *)
  VAR distance: INTEGER;
  BEGIN
    WITH list = scroller.list DO
      LOCK list.mu DO
        distance := linesToScroll;
        IF NOT towardsEOF THEN distance := -distance END;
        list.contents.scrollContents(list.firstVisible+distance);
      END;
    END;
  END AutoScroll;

CONST NearEdge = 13;
    (* Thumbing closer than this to top/bottom of scroll bar is treated as
       being exactly at the top/bottom. *)

PROCEDURE Thumb (scroller: Scroller;
                 <*UNUSED*> READONLY cd: VBT.MouseRec;
                 part: INTEGER;
                 height: INTEGER) =
  (* LL.sup < list *)
  VAR position: INTEGER;
  BEGIN
    WITH list = scroller.list DO
      LOCK list.mu DO
        IF part < NearEdge THEN
          position := 0
        ELSIF part + NearEdge > height THEN
          position := list.nCells
        ELSE
          position := (part * list.nCells) DIV height
        END;
        list.contents.scrollContents(position);
      END;
    END;
  END Thumb;


(* *)
(* Implementation of Bar method *)
(* *)

PROCEDURE BarShape(bar: Bar; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  (* LL.sup = VBT.mu.bar *)
  VAR
    sr: VBT.SizeRange;
  BEGIN
    WITH hv = HVSplit.AxisOf(VBT.Parent(bar)) DO
      IF hv = ax THEN
        sr.lo := ROUND(VBT.MMToPixels(bar, bar.size, hv));
        sr.pref := sr.lo;
        sr.hi := sr.lo + 1;
        RETURN sr
      ELSE
        RETURN TextureVBT.T.shape(bar, ax, n)
      END
    END
  END BarShape;


(* *)
(* Implementations of TextPainter methods *)
(* *)

CONST
  Leading = 0;
  LMargin = 2;

PROCEDURE TextPainterInit(painter: TextPainter;
                          bg: PaintOp.T := PaintOp.Bg;
                          fg: PaintOp.T := PaintOp.Fg;
                          hiliteBg: PaintOp.T := PaintOp.Fg;
                          hiliteFg: PaintOp.T := PaintOp.Bg;
                          font: Font.T := Font.BuiltIn): TextPainter =
  BEGIN
    painter.mu := NEW(MUTEX);
    painter.eraseColor := bg;
    painter.textColor := PaintOp.Pair(PaintOp.Transparent, fg);
    painter.hiliteColor := hiliteBg;
    painter.hiliteTextColor := PaintOp.Pair(PaintOp.Transparent, hiliteFg);
    LOCK painter.mu DO
      painter.font := font;
    END;
    RETURN painter
  END TextPainterInit;

PROCEDURE TextPainterHeight(painter: TextPainter; v: VBT.T): INTEGER =
  (* LL.sup = list *)
  VAR bBox: Rect.T;
  BEGIN
    LOCK painter.mu DO
      bBox := VBT.BoundingBox(v, "X", painter.font);
      painter.ascent := -bBox.north;
      painter.descent := bBox.south;
    END;
    RETURN Leading + Rect.VerSize(bBox)
  END TextPainterHeight;

PROCEDURE TextPainterPaint (painter : TextPainter;
                            v       : VBT.T;
                            r       : Rect.T;
                            value   : REFANY;
                 <*UNUSED*> index   : CARDINAL;
                            selected: BOOLEAN;
                            bad     : Rect.T       ) =
  (* LL.sup = list *)
  VAR tintColor, textColor: PaintOp.T;
  BEGIN
    IF selected THEN
      tintColor := painter.hiliteColor;
      textColor := painter.hiliteTextColor;
    ELSE
      tintColor := painter.eraseColor;
      textColor := painter.textColor;
    END;
    VBT.PaintTint (v, bad, tintColor);
    IF value # NIL THEN
      LOCK painter.mu DO
        VBT.PaintText (
          v := v,
          pt := Point.T {h := r.west + LMargin, v :=
                         r.south - painter.descent - Leading},
          fnt := painter.font, op := textColor,
          t := NARROW (value, TEXT));
      END;
    END;
  END TextPainterPaint;

PROCEDURE TextPainterSelect(painter: TextPainter; v: VBT.T; r: Rect.T;
                            value: REFANY; index: CARDINAL; selected: BOOLEAN) =
  (* LL.sup = list *)
  BEGIN
    painter.paint(v, r, value, index, selected, r);
  END TextPainterSelect;

PROCEDURE TextPainterErase(painter: TextPainter; v: VBT.T; r: Rect.T) =
  (* LL.sup = list *)
  BEGIN
    VBT.PaintTint(v, r, painter.eraseColor);
  END TextPainterErase;

PROCEDURE TextPainterSetFont(painter: TextPainter; v: VBT.T; font: Font.T) =
  (* LL.sup < v *)
  BEGIN
    LOCK painter.mu DO
      painter.font := font;
      VBT.Mark(v);
    END;
  END TextPainterSetFont;


(* *)
(* Implementations of UniSelector methods *)
(* *)

PROCEDURE UniSelectorInit(selector: UniSelector; l: T): Selector =
  BEGIN
    selector.list := l;
    RETURN selector
  END UniSelectorInit;

PROCEDURE UniSelectorInsideClick (                    selector: UniSelector;
                                  <*UNUSED*> READONLY cd      : VBT.MouseRec;
                                                      this    : Cell          ) =
  (* LL.sup = VBT.mu *)
  BEGIN
    selector.list.selectOnly (this);
  END UniSelectorInsideClick;

PROCEDURE UniSelectorOutsideClick (<*UNUSED*>          selector: UniSelector;
                                   <*UNUSED*> READONLY cd      : VBT.MouseRec ) =
  (* LL.sup = VBT.mu *)
  BEGIN
  END UniSelectorOutsideClick;

PROCEDURE UniSelectorInsideDrag (selector: UniSelector;
                                 <*UNUSED*> READONLY cd  : VBT.PositionRec;
                                                     this: Cell             ) =
  (* LL.sup = VBT.mu *)
  BEGIN
    selector.list.selectOnly (this);
  END UniSelectorInsideDrag;

PROCEDURE UniSelectorOutsideDrag (<*UNUSED*> selector: UniSelector;
                                  <*UNUSED*> READONLY cd: VBT.PositionRec) =
  (* LL.sup = VBT.mu *)
  BEGIN
  END UniSelectorOutsideDrag;


(* *)
(* Implementations of MultiSelector methods *)
(* *)

PROCEDURE MultiSelectorInit(selector: MultiSelector; l: T): Selector =
  BEGIN
    selector.list := l;
    RETURN selector
  END MultiSelectorInit;

PROCEDURE MultiSelectorInsideClick (         selector: MultiSelector;
                                    READONLY cd      : VBT.MouseRec;
                                             this    : Cell           ) =
  (* LL.sup = VBT.mu *)
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      WITH list = selector.list DO
        selector.anchor := this;
        IF VBT.Modifier.Shift IN cd.modifiers THEN
          selector.adding := NOT list.isSelected (this);
        ELSE
          selector.adding := TRUE;
          list.selectNone ();
        END;
        list.select (this, selector.adding);
        selector.prev := this;
      END;
    END;
  END MultiSelectorInsideClick;

PROCEDURE MultiSelectorOutsideClick (<*UNUSED*> selector: MultiSelector;
                                     <*UNUSED*> READONLY cd: VBT.MouseRec) =
  (* LL.sup = VBT.mu *)
  BEGIN
  END MultiSelectorOutsideClick;

PROCEDURE MultiSelectorInsideDrag (selector: MultiSelector;
                                   <*UNUSED*> READONLY cd  : VBT.PositionRec;
                                                       this: Cell             ) =
  (* LL.sup = VBT.mu *)
  BEGIN
    WITH list = selector.list DO
      (* There are numerous cases; either first or last loop is empty. *)
      FOR i := selector.prev TO MIN (this, selector.anchor) - 1 DO
        (* prev < this and prev < anchor: undo after prev *)
        list.select (i, NOT selector.adding);
      END;
      FOR i := MIN (this, selector.anchor + 1)
          TO MAX (selector.anchor - 1, this) DO
        (* apply between this and anchor, in either order *)
        list.select (i, selector.adding);
      END;
      FOR i := MAX (this, selector.anchor) + 1 TO selector.prev DO
        (* prev > this and prev > anchor: undo up to prev *)
        list.select (i, NOT selector.adding);
      END;
      selector.prev := this;
    END;
  END MultiSelectorInsideDrag;

PROCEDURE MultiSelectorOutsideDrag (<*UNUSED*> selector: MultiSelector;
                                    <*UNUSED*> READONLY cd: VBT.PositionRec) =
  (* LL.sup = VBT.mu *)
  BEGIN
  END MultiSelectorOutsideDrag;


BEGIN
END ListVBT.
