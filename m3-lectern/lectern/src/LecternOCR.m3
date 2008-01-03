(* Copyright 1995 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: interface for accessing OCR data *)

(* Last modified on Tue Aug 20 08:12:56 PDT 1996 by mcjones   *)
(*      modified on Fri Apr  7 16:02:52 PDT 1995 by birrell   *)

MODULE LecternOCR;

IMPORT Cursor, Images, ImageVBT, LecternDoc, PaintOp, Point, PolyRegion,
       Rd, Rect, Region, TextF, TextWr, Thread, VBT, Wr;

TYPE
  Rects = REF ARRAY OF Rect.T;
  Tracking = { None, Selection, DelayedDrag, Drag };

REVEAL T = Public BRANDED OBJECT
    rd: Rd.T;
    dir: LecternDoc.Dir;
    v: ImageVBT.T;
    grab: Cursor.T;                (* cursor while dragging image *)
    rects: Rects := NIL;
    words: Words := NIL;
    curPage: INTEGER := 0;         (* page for which rects & words are valid *)
    tracking := Tracking.None;     (* mouse tracking in progress *)
    lastPos: Point.T;              (* Mouse tracking *)
    selStart: SelPos := NoSelPos;  (* first selected word, or NoSelPos *)
    selEnd: SelPos := NoSelPos;    (* 1+last selected word, if any *)
    selAnchor: SelPos := NoSelPos; (* initially selected word, if any *)
  METHODS
    readOCRData(page: INTEGER) RAISES { Thread.Alerted } := ReadOCRData;
    findPoint(pt: Point.T; page: INTEGER): INTEGER := FindPoint;
    selRegion(from, to: SelPos; gaps: BOOLEAN;
              page: INTEGER; class: LecternDoc.Class): Region.T := SelRegion;
    selRepaint(from, to: SelPos;
               page: INTEGER; class: LecternDoc.Class) := SelRepaint;
    deSelect(page: INTEGER; class: LecternDoc.Class) := DeSelect;
    select(cp: VBT.CursorPosition; time: VBT.TimeStamp;
           page: INTEGER; class: LecternDoc.Class;
           newSel, newAnchor: BOOLEAN) := Select;
  OVERRIDES
    init := Init;
    mouse := Mouse;
    position := Position;
    paint := Paint;
    close := Close;
    read := Read;
    setSelection := SetSelection;
    selectWords := SelectWords;
    getSelection := GetSelection;
    getRect := GetRect;
    getWords := GetWords;
    getWordSeq := GetWordSeq;
  END;

PROCEDURE Scale(class: LecternDoc.Class): INTEGER =
    (* Returns amount by which images of given class are reduced from OCR *)
  BEGIN
    <* ASSERT LecternDoc.DefaultScales[LecternDoc.Class.OCRWords] = 1 *>
    RETURN LecternDoc.DefaultScales[class];
  END Scale;

PROCEDURE ImageNW(v: ImageVBT.T): Point.T =
    (* Returns the coordinates of the NW corner of the image, in v's coords. *)
  VAR
    domainV := VBT.Domain(v);
    domainI := v.get().domain(v);
  BEGIN
    RETURN Point.Sub(Point.Add(Rect.Middle(domainV), v.getDelta()),
                     Point.Sub(Rect.Middle(domainI), Rect.NorthWest(domainI)));
  END ImageNW;

PROCEDURE ToOCRSpace(v: ImageVBT.T; cp: VBT.CursorPosition;
                     class: LecternDoc.Class): Point.T =
    (* Converts a point in v's coordinate system to a point in OCR space.
       Maps offscreen and gone to (-1,-1) *)
  VAR
    scale := Scale(class);
    imageNW := ImageNW(v);
    relINW := Point.Sub(cp.pt, imageNW);
  BEGIN
    IF cp.offScreen THEN
      RETURN Point.T{ -1, -1}
    ELSIF cp.gone THEN
      RETURN Point.T{-1, -1}
    ELSE
      RETURN Point.T{relINW.h * scale, relINW.v * scale}
    END;
  END ToOCRSpace;

PROCEDURE PointFromOCRSpace(v: ImageVBT.T; pt: Point.T;
                            class: LecternDoc.Class): Point.T =
    (* Converts a point from OCR space to v's coordinate system. *)
  VAR
    scale := Scale(class);
    round := scale DIV 2;
    imageNW := ImageNW(v);
    scaled := Point.T{h:=(pt.h+round) DIV scale, v:=(pt.v+round) DIV scale};
  BEGIN
    RETURN Point.Add(scaled, imageNW)
  END PointFromOCRSpace;

PROCEDURE RectFromOCRSpace(v: ImageVBT.T; r: Rect.T;
                           class: LecternDoc.Class): Rect.T =
    (* Converts a rectangle from OCR space to v's coordinate system. *)
  BEGIN
    IF Rect.IsEmpty(r) THEN
      RETURN Rect.Empty
    ELSE
      RETURN Rect.FromCorners(PointFromOCRSpace(v, Rect.NorthWest(r), class),
                              PointFromOCRSpace(v, Rect.SouthEast(r), class));
    END;
  END RectFromOCRSpace;

PROCEDURE IncPos(pos: SelPos): SelPos =
  BEGIN
    RETURN SelPos{ page := pos.page, word := pos.word+1 };
  END IncPos;

PROCEDURE DecPos(pos: SelPos): SelPos =
  BEGIN
    RETURN SelPos{ page := pos.page, word := pos.word-1 };
  END DecPos;

PROCEDURE SameLine(left, right: Rect.T): BOOLEAN =
  (* Returns TRUE iff left and right are plausibly adjacent words on a line. *)
  BEGIN
    WITH rightMid = (right.north+right.south) DIV 2 DO
      RETURN (NOT Rect.IsEmpty(left)) AND
             (NOT Rect.IsEmpty(right)) AND
             right.west >= left.west AND
             rightMid >= left.north AND
             rightMid <= left.south;
    END;
  END SameLine;

PROCEDURE ReadInt4(rd: Rd.T): INTEGER RAISES { Rd.Failure, Thread.Alerted,
                                               Rd.EndOfFile } =
    (* Read an integer represented as four bytes on rd, L.S. first. *)
  VAR n := 0; mul := 1;
  BEGIN
    FOR i := 0 TO 3 DO
      INC(n, ORD(Rd.GetChar(rd)) * mul);
      IF i < 3 THEN mul := mul * 256 END;
    END;
    RETURN n
  END ReadInt4;


(* *)
(* Internal methods *)
(* *)

CONST RectSizeInFile = 16; (* not equal to BYTESIZE(Rect.T) on Alpha *)

PROCEDURE ReadOCRData(t: T; page: INTEGER) RAISES { Thread.Alerted } =
  (* Update "t" to have the OCR data for "page" *)
<* FATAL Rd.Failure, Rd.EndOfFile *>
  BEGIN
    IF t.rects = NIL OR t.curPage # page THEN
      WITH
        words = t.dir.pages[page+t.dir.origin][LecternDoc.Class.OCRWords],
        rects = t.dir.pages[page+t.dir.origin][LecternDoc.Class.OCRRects] DO
        VAR nWords := rects.length DIV RectSizeInFile;
        BEGIN
          t.words := NEW(Words, nWords);
          t.rects := NEW(Rects, nWords);
          Rd.Seek(t.rd, words.start);
          FOR i := 0 TO LAST(t.words^) DO
            t.words[i] := Rd.GetLine(t.rd);
          END;
          Rd.Seek(t.rd, rects.start);
          FOR i := 0 TO LAST(t.rects^) DO
            WITH r = t.rects[i] DO
              r.west := ReadInt4(t.rd);
              r.south := ReadInt4(t.rd);
              r.east := ReadInt4(t.rd);
              r.north := ReadInt4(t.rd);
            END;
          END;
        END;
      END;
      t.curPage := page;
    END;
  END ReadOCRData;

PROCEDURE FindPoint(t: T; pt: Point.T; page: INTEGER): INTEGER =
  (* Find the bounding rectangle of the word containing (or closest to) "pt",
     if any.  Everything is in OCR coordinates. *)
  <* FATAL Thread.Alerted *>
  VAR
    closestDistance := LAST(INTEGER);
    closestIndex := -1;
  BEGIN
    t.readOCRData(page);
    FOR i := 0 TO LAST(t.rects^) DO
      WITH distance = Point.DistSquare(Rect.Project(t.rects[i], pt), pt) DO
        IF distance = 0 THEN RETURN i END;
        IF distance < closestDistance THEN
          closestDistance := distance;
          closestIndex := i;
        END;
      END;
    END;
    RETURN closestIndex
  END FindPoint;

PROCEDURE SelRegion(t: T;
                    from, to: SelPos;
                    gaps: BOOLEAN;
                    page: INTEGER; class: LecternDoc.Class): Region.T =
  (* Returns a region containing all of the words in [from..to) that are
     on the given page (which might be none of them).  Result is in screen
     coords.  Iff "gaps", result includes gaps between start/end points and
     adjacent words on the same lines. *)
  <* FATAL Thread.Alerted *>
  VAR
    startWord, endWord: INTEGER;
    prgn := PolyRegion.Empty;
    lineR := Rect.Empty;
    prevR := Rect.Empty;
  BEGIN
    IF t.selStart.word < 0 THEN
      RETURN Region.Empty
    ELSIF page < from.page OR page > to.page THEN
      RETURN Region.Empty
    ELSE
      t.readOCRData(page);
      IF page = from.page THEN
        startWord := from.word
      ELSE
        startWord := 0;
      END;
      IF page = to.page THEN
        endWord := to.word
      ELSE
        endWord := NUMBER(t.words^);
      END;
      IF startWord < endWord THEN
        FOR i := 0 TO LAST(t.words^) DO
          WITH newR = RectFromOCRSpace(t.v, t.rects[i], class) DO
            IF SameLine(prevR, newR) THEN
              lineR.north := MIN(lineR.north, newR.north);
              lineR.south := MAX(lineR.south, newR.south);
              IF gaps AND i = startWord THEN
                lineR.west := lineR.east;
              ELSIF i <= startWord THEN
                lineR.west := newR.west;
              END;
              IF gaps AND i = endWord THEN
                lineR.east := newR.west;
              ELSIF i < endWord THEN
                lineR.east := newR.east;
              END;
            ELSIF i >= endWord THEN
              EXIT
            ELSE
              IF i > startWord THEN
                PolyRegion.JoinRect(prgn, Rect.Inset(lineR, -1));
              END;
              lineR := newR;
            END;
            prevR := newR;
          END;
        END;
        PolyRegion.JoinRect(prgn, Rect.Inset(lineR, -1));
      END;
      RETURN PolyRegion.ToRegion(prgn);
    END;
  END SelRegion;

PROCEDURE SelRepaint(t: T;
                     from, to: SelPos;
                     page: INTEGER; class: LecternDoc.Class) =
    (* Uses VBT.ForceRepaint to provoke a repaint of the selection. *)
  BEGIN
    VBT.ForceRepaint(t.v, Region.FromRect(t.selRegion(from,
                                                      to,
                                                      TRUE,
                                                      page, class).r));
  END SelRepaint;

PROCEDURE DeSelect(t: T; page: INTEGER; class: LecternDoc.Class) =
    (* Remove any selection and provoke any needed repaint *)
  BEGIN
    t.selRepaint(t.selStart, t.selEnd, page, class);
    IF t.selStart.word >= 0 THEN VBT.Release(t.v, VBT.Source) END;
    t.selStart := NoSelPos; t.selEnd := NoSelPos; t.selAnchor := NoSelPos;
  END DeSelect;

PROCEDURE Select(t: T;
                 cp: VBT.CursorPosition; time: VBT.TimeStamp;
                 page: INTEGER; class: LecternDoc.Class;
                 newSel, newAnchor: BOOLEAN) =
    (* Do selection action at cp; "new" => new selection; otherwise extend. *)
(* Neater, but slower (see code at end of this procedure):
  VAR
    oldRgn := t.selRegion(t.selStart, t.selEnd, FALSE, page, class);
*)
  BEGIN
    IF newSel THEN t.deSelect(page, class) END;
    WITH found = SelPos{ word := t.findPoint(ToOCRSpace(t.v, cp, class), page),
                         page := page } DO
      IF found.word >= 0 THEN
        IF newSel THEN
          (* New selection *)
          TRY
            VBT.Acquire(t.v, VBT.Source, time);
            t.selStart := found;
            t.selAnchor := t.selStart;
            t.selEnd := IncPos(t.selStart);
            t.selRepaint(t.selStart, t.selEnd, page, class);
          EXCEPT VBT.Error =>
          END;
        ELSIF t.selStart.word >= 0 THEN
          (* Extending existing selection *)
          IF page < t.selAnchor.page OR
            (page = t.selAnchor.page AND found.word < t.selAnchor.word) THEN
            (* New point is earlier than selAnchor *)
            IF newAnchor THEN t.selAnchor := DecPos(t.selEnd) END;
            WITH newEnd = IncPos(t.selAnchor), newStart = found DO
              IF t.selStart # newStart OR t.selEnd # newEnd THEN
                t.selRepaint(newStart, t.selStart, page, class); (* expand *)
                t.selRepaint(t.selStart, newStart, page, class); (* shrink *)
                t.selRepaint(newEnd, t.selEnd, page, class);     (* anchor *)
                t.selStart := newStart;
                t.selEnd := newEnd;
              END;
            END;
          ELSE
            (* New point is beyond or at selAnchor *)
            IF newAnchor THEN t.selAnchor := t.selStart END;
            WITH newStart = t.selAnchor, newEnd = IncPos(found) DO
              IF t.selStart # newStart OR t.selEnd # newEnd THEN
                t.selRepaint(t.selEnd, newEnd, page, class);     (* expand *)
                t.selRepaint(newEnd, t.selEnd, page, class);     (* shrink *)
                t.selRepaint(t.selStart, newStart, page, class); (* anchor *)
                t.selStart := newStart;
                t.selEnd := newEnd;
              END;
            END;
          END;
        END;
      END;
    END;
(* Neater, but slower:
    WITH newRgn = t.selRegion(t.selStart, t.selEnd, FALSE, page, class) DO
      VBT.ForceRepaint(t.v,
                Region.FromRect(Region.SymmetricDifference(newRgn, oldRgn).r));
    END;
*)
  END Select;


(* *)
(* Public Methods *)
(* *)

PROCEDURE Init(t: T; rd: Rd.T; READONLY dir: LecternDoc.Dir; v: ImageVBT.T;
               grab: Cursor.T := Cursor.DontCare): T =
  BEGIN
    t.rd := rd;
    t.dir := dir;
    t.v := v;
    t.grab := grab;
    t.selStart := NoSelPos; t.selEnd := NoSelPos; t.selAnchor := NoSelPos;
    RETURN t;
  END Init;

TYPE DelayedDrag = Thread.Closure OBJECT
    t: T;
  OVERRIDES
    apply := DoDelayedDrag;
  END;

PROCEDURE DoDelayedDrag(self: DelayedDrag): REFANY =
  BEGIN
    Thread.Pause(0.20D0);
    LOCK VBT.mu DO
      IF self.t.tracking = Tracking.DelayedDrag THEN
        VBT.SetCursor(self.t.v, self.t.grab);
        self.t.tracking := Tracking.Drag;
      END;
    END;
    RETURN NIL
  END DoDelayedDrag;

PROCEDURE Mouse(t: T; READONLY cd: VBT.MouseRec;
                page: INTEGER; class: LecternDoc.Class) =
  VAR
    middle := cd.whatChanged = VBT.Modifier.MouseM OR
              VBT.Modifier.Control IN cd.modifiers;
    right := cd.whatChanged = VBT.Modifier.MouseR OR
              VBT.Modifier.Shift IN cd.modifiers;
    left :=  cd.whatChanged = VBT.Modifier.MouseL AND NOT middle AND NOT right;
    drag := cd.clickCount = 0 AND middle;
    newSel := ( cd.clickCount = 2 AND middle ) OR left;
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      t.lastPos := cd.cp.pt;
      VBT.SetCage(t.v, VBT.CageFromPosition(cd.cp));
      IF drag THEN
        t.tracking := Tracking.DelayedDrag;
        EVAL Thread.Fork(NEW(DelayedDrag, t := t));
      ELSE
        t.tracking := Tracking.Selection;
        t.select(cd.cp, cd.time, page, class, newSel, TRUE);
      END;
    ELSIF cd.clickType = VBT.ClickType.LastUp THEN
      IF t.tracking = Tracking.Drag THEN
        VBT.SetCursor(t.v, Cursor.DontCare);
      END;
      t.tracking := Tracking.None;
    END;
  END Mouse;

PROCEDURE Position(t: T; READONLY cd: VBT.PositionRec;
                   page: INTEGER; class: LecternDoc.Class) =
    (* LL = VBT.mu *)
  BEGIN
    IF t.tracking # Tracking.None THEN
      IF t.tracking # Tracking.DelayedDrag AND NOT cd.cp.gone THEN
        IF t.tracking = Tracking.Drag THEN
          t.v.moveTo(Point.Add(t.v.getDelta(),
                               Point.Sub(cd.cp.pt, t.lastPos)));
        ELSE
          t.select(cd.cp, cd.time, page, class, FALSE, FALSE);
        END;
        t.lastPos := cd.cp.pt;
      END;
      VBT.SetCage(t.v, VBT.CageFromPosition(cd.cp));
    END;
  END Position;

PROCEDURE Paint(t: T; READONLY rgn: Region.T;
                page: INTEGER; class: LecternDoc.Class)
               RAISES { Thread.Alerted, Images.Error } =
  BEGIN
    ImageVBT.T.paint(t.v, rgn);
    WITH paintRgn = Region.Meet(rgn, t.selRegion(t.selStart, t.selEnd, FALSE,
                                                 page, class)) DO
      IF NOT Region.IsEmpty(paintRgn) THEN
        VBT.PaintRegion(t.v, paintRgn, PaintOp.Swap);
      END;
    END;
  END Paint;

PROCEDURE Close(t: T) =
  BEGIN
    TRY
      Rd.Close(t.rd);
    EXCEPT Rd.Failure, Thread.Alerted =>
    END;
  END Close;

PROCEDURE Read(t: T; s: VBT.Selection): TEXT RAISES { VBT.Error } =
  <*FATAL Wr.Failure, Thread.Alerted *>
  VAR
    startWord, endWord: INTEGER;
    prevR: Rect.T;
    wr: Wr.T;
  BEGIN
    IF s = VBT.Source AND t.selStart.word >= 0 THEN
      wr := TextWr.New();
      FOR page := t.selStart.page TO t.selEnd.page DO
        t.readOCRData(page);
        IF page = t.selStart.page THEN
          startWord := t.selStart.word
        ELSE
          startWord := 0;
        END;
        IF page = t.selEnd.page THEN
          endWord := t.selEnd.word
        ELSE
          endWord := NUMBER(t.words^);
        END;
        prevR := Rect.Empty;
        FOR i := startWord TO endWord-1 DO
          WITH
            newR = RectFromOCRSpace(t.v, t.rects[i], LecternDoc.Class.Print),
            newW = t.words[i] DO
            IF Wr.Index(wr) = 0 THEN
              Wr.PutText(wr, newW);
            ELSIF SameLine(prevR, newR) THEN
              Wr.PutChar(wr, ' '); Wr.PutText(wr, newW);
            ELSE
              Wr.PutChar(wr, '\n'); Wr.PutText(wr, newW);
            END;
            prevR := newR;
          END;
        END;
      END;
      RETURN TextWr.ToText(wr)
    ELSE
      RAISE VBT.Error(VBT.ErrorCode.UnownedSelection)
    END;
  END Read;

PROCEDURE SetSelection(t: T; selStart, selEnd: SelPos; time: VBT.TimeStamp;
                       page: INTEGER; class: LecternDoc.Class) =
  PROCEDURE BoundSelPos(selPos: SelPos): SelPos =
    (* Ensure selPos is within document's page range.  This matters if it
       came from a link, which might have arbitrary garbage.  In particular,
       changing a document's origin can cause problems with links. *)
    BEGIN
      IF selPos.word >= 0 THEN
        WITH relPage = selPos.page + t.dir.origin DO
          IF relPage < 0 OR relPage > LAST(t.dir.pages^) THEN
            RETURN NoSelPos;
          ELSE
            RETURN selPos;
          END;
        END;
      ELSE
        RETURN NoSelPos;
      END;
    END BoundSelPos;
  BEGIN
    t.deSelect(page, class);
    TRY
      VBT.Acquire(t.v, VBT.Source, time);
      t.selStart := BoundSelPos(selStart);
      t.selAnchor := t.selStart;
      IF t.selStart = NoSelPos THEN
        t.selEnd := NoSelPos
      ELSE
        t.selEnd := BoundSelPos(selEnd);
      END;
      t.selRepaint(t.selStart, t.selEnd, page, class);
    EXCEPT VBT.Error =>
      (* Well, never mind. *)
    END;
  END SetSelection;

PROCEDURE SelectWords(t: T;
                      from, for: INTEGER; time: VBT.TimeStamp;
                      page: INTEGER; class: LecternDoc.Class): BOOLEAN =
  PROCEDURE FindWord(n: INTEGER): SelPos =
    (* Find the page contaning word number "n", and its relative word number *)
    VAR
      total := 0;
    BEGIN
      FOR page := 0 TO LAST(t.dir.pages^) DO
        WITH
          rects = t.dir.pages[page][LecternDoc.Class.OCRRects],
          nWords = rects.length DIV RectSizeInFile DO
          IF n < total+nWords THEN
            RETURN SelPos{ page := page-t.dir.origin, word := n - total }
          ELSE
            INC(total, nWords);
          END;
        END;
      END;
      RETURN NoSelPos
    END FindWord;
  BEGIN
    WITH
      fromPos = FindWord(from),
      toPos = FindWord(from + MAX(for, 1) - 1) DO
      IF fromPos.word < 0 THEN
        RETURN FALSE
      ELSE
        IF toPos.word < 0 THEN
          t.setSelection(fromPos, IncPos(fromPos), time, page, class);
        ELSE
          t.setSelection(fromPos, IncPos(toPos), time, page, class);
        END;
        RETURN TRUE
      END;
    END;
  END SelectWords;

PROCEDURE GetSelection(t: T; VAR selStart, selEnd: SelPos) =
  BEGIN
    IF t.selStart.word < 0 THEN
      selStart := NoSelPos;
      selEnd := selStart;
    ELSE
      selStart := t.selStart;
      selEnd := t.selEnd;
    END;
  END GetSelection;

PROCEDURE GetRect(t: T; pos: SelPos; class: LecternDoc.Class): Rect.T
                  RAISES { Thread.Alerted } =
    (* LL = VBT.mu *)
  BEGIN
    IF pos.word < 0 THEN
      RETURN Rect.Empty
    ELSE
      t.readOCRData(pos.page);
      RETURN RectFromOCRSpace(t.v, t.rects[pos.word], class);
    END;
  END GetRect;

PROCEDURE GetWords(t: T; page: INTEGER): Words RAISES { Thread.Alerted } =
  BEGIN
    IF page+t.dir.origin < 0 OR page+t.dir.origin > LAST(t.dir.pages^) THEN
      RETURN NIL
    ELSE
      t.readOCRData(page);
      RETURN t.words;
    END;
  END GetWords;

PROCEDURE GetWordSeq(t: T; page: INTEGER): WordSeq RAISES { Thread.Alerted } =
  <* FATAL Rd.Failure *>
  VAR
    seq: TEXT;
  BEGIN
    LOCK VBT.mu DO
      IF page+t.dir.origin < 0 OR page+t.dir.origin > LAST(t.dir.pages^) THEN
        RETURN NIL
      ELSE
        WITH
          words = t.dir.pages[page+t.dir.origin][LecternDoc.Class.OCRWords] DO
          IF words.start <= 0 THEN
            RETURN NIL
          ELSE
            seq := TextF.New(words.length);
            Rd.Seek(t.rd, words.start);
            EVAL Rd.GetSub(t.rd, SUBARRAY(seq^, 0, words.length));
            RETURN seq
          END;
        END;
      END;
    END;
  END GetWordSeq;

BEGIN
END LecternOCR.
