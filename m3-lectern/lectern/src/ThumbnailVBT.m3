(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* VBT for viewing and selecting thumbnail images *)

(* Last modified on Wed Mar 22 15:02:39 PST 1995 by birrell   *)

MODULE ThumbnailVBT EXPORTS ThumbnailVBT;

IMPORT Axis, HVSplit, ImageRd, Images, LecternDoc, PaintOp, Point,
       Rd, Rect, Region, ScrollerVBTClass, Split, Thread, VBT;

(* *)
(* Types and constants *)
(* *)

CONST BarWidth = 1;

TYPE
  ImageArray = REF ARRAY OF Images.T;

  Scroller = ScrollerVBTClass.T OBJECT
      thumbnail: T;
    OVERRIDES
      scroll := Scroll;
      autoScroll := AutoScroll;
      thumb := Thumb;
    END;
    
  Contents = VBT.Leaf OBJECT
      (* This is the leaf where the rectangles get painted *)
      thumbnail: T;
      tracking := FALSE;
      oldSel := -1;
    METHODS
      trackCP(cp: VBT.CursorPosition): VBT.Cage := TrackCP;
    OVERRIDES
      repaint := Repaint;
      reshape := Reshape;
      mouse := Mouse;
      position := Position;
    END;

REVEAL Private = HVSplit.T BRANDED OBJECT END;

REVEAL T = Public BRANDED OBJECT
    bg: PaintOp.T;
    total: INTEGER;
    size, gap: Point.T;
    vScroll: Scroller := NIL;
    contents: Contents := NIL;
    images: ImageArray := NIL;
    columns: INTEGER := 1; (* number of columns, 1 if domain is empty *)
    firstVisible: INTEGER; (* first visible row *)
    selected: INTEGER;
  METHODS
    visible(): REAL := Visible;
      (* returns number of visible rows; typically includes a partial row *)
    rows(): INTEGER := Rows;
      (* returns number of rows *)
    updateScroller() := UpdateScroller;
      (* clip firstVisible and ensure scroller is consistent with our state *)
    scrollTo(this: INTEGER) := ScrollTo;
      (* make "this" be firstVisible, subject to clipping by updateScroller *)
  OVERRIDES
    init := Init;
    setContents := SetContents;
    set := Set;
    get := Get;
    scrollToShow := ScrollToShow;
    acquire := Acquire;
    hit := Hit;
  END;


(* *)
(* Implementations of Scroller methods *)
(* *)

PROCEDURE Scroll(scroller: Scroller;
                 <*UNUSED*> READONLY cd: VBT.MouseRec;
                 part: INTEGER;
                 height: INTEGER;
                 towardsEOF: BOOLEAN) =
  VAR distance: INTEGER;
  BEGIN
    WITH v = scroller.thumbnail DO
      distance := MAX(1, FLOOR((FLOAT(part) * v.visible()) / FLOAT(height)));
      IF NOT towardsEOF THEN distance := -distance END;
      v.scrollTo(v.firstVisible + distance);
    END;
  END Scroll;

PROCEDURE AutoScroll (scroller: Scroller;
                      <*UNUSED*> READONLY cd: VBT.MouseRec;
                      linesToScroll: CARDINAL;
                      towardsEOF: BOOLEAN) =
  VAR distance: INTEGER;
  BEGIN
    WITH v = scroller.thumbnail DO
      distance := linesToScroll;
      IF NOT towardsEOF THEN distance := -distance END;
      v.scrollTo(v.firstVisible + distance);
    END;
  END AutoScroll;

CONST NearEdge = 13;
    (* Thumbing closer than this to top/bottom of scroll bar is treated as
       being exactly at the top/bottom. *)

PROCEDURE Thumb (scroller: Scroller;
                 <*UNUSED*> READONLY cd: VBT.MouseRec;
                 part: INTEGER;
                 height: INTEGER) =
  VAR position: INTEGER;
  BEGIN
    WITH v = scroller.thumbnail DO
      IF part < NearEdge THEN
        position := 0
      ELSIF part + NearEdge > height THEN
        position := v.rows()
      ELSE
        position := (part * v.rows()) DIV height
      END;
      v.scrollTo(position);
    END;
  END Thumb;


(* *)
(* Implementation of Contents methods *)
(* *)

PROCEDURE PaintDifference(v: VBT.T; with, without: Rect.T; op: PaintOp.T) =
    (* Paint with minus without using given tint *)
  VAR parts: Rect.Partition;
  BEGIN
    Rect.Factor(with, without, parts, 1, 1);
    FOR i := 0 TO 4 DO
      IF i#2 THEN VBT.PaintTint(v, parts[i], op) END;
    END;
  END PaintDifference;

CONST
  ShadowSize = 1; (* width of image shadow *)

PROCEDURE ImageArea(contents: Contents; n: INTEGER): Rect.T =
    (* Returns the rectangle in which image n may be painted *)
  VAR
    v := contents.thumbnail;
    cols := v.columns;
    domain := VBT.Domain(contents);
    firstOrigin := Point.MoveHV(Rect.NorthWest(domain),
                                BarWidth + v.gap.h, v.gap.v);
    myOrigin := Point.MoveHV(firstOrigin,
                           (n MOD cols) * (v.size.h+v.gap.h),
                           ((n DIV cols)-v.firstVisible) * (v.size.v+v.gap.v));
  BEGIN
    RETURN Rect.FromCorner(myOrigin, v.size.h, v.size.v);
  END ImageArea;

PROCEDURE Frame(contents: Contents; n: INTEGER): Rect.T =
    (* Paint gray area around the image, and return the
       rectangle in which the image should be painted. *)
  VAR
    v := contents.thumbnail;
    image := ImageArea(contents, n);
    outside := Rect.Change(image,
                           -v.gap.h DIV 2,
                           v.gap.h - v.gap.h DIV 2,
                           -v.gap.v DIV 2,
                           v.gap.v - v.gap.v DIV 2);
  BEGIN
    PaintDifference(contents, outside, image, v.bg);
    RETURN image
  END Frame;

PROCEDURE RepaintOne(contents: Contents; rgn: Region.T; n: INTEGER) =
  (* LL.sup = mu.contents *)
  (* Paint image n and its frame, to the extent these intersect rgn *)
  VAR
    v := contents.thumbnail;
    image := Frame(contents, n);
    area := Rect.Meet(rgn.r, image);
  BEGIN
    IF Rect.IsEmpty(VBT.Domain(contents)) THEN RETURN END;
    IF n >= v.total THEN
      VBT.PaintTint(contents, area, v.bg)
    ELSIF v.images[n] = NIL THEN
      VBT.PaintTint(contents, area, PaintOp.Bg);
    ELSE
      WITH
        imDomain = v.images[n].domain(contents),
        offset = Point.Sub(Rect.Middle(image), Rect.Middle(imDomain)),
        imDomainInV = Rect.Add(imDomain, offset),
        clippedImage = Rect.Meet(imDomainInV, image) DO
        IF NOT Rect.IsEmpty(area) THEN
          PaintDifference(contents, area,
                          imDomainInV,
                          v.bg);
          TRY
            v.images[n].paint(contents, area, offset);
          EXCEPT Thread.Alerted, Images.Error =>
          END;
          IF n = v.selected THEN
            VBT.PaintTint(contents, Rect.Meet(area,imDomainInV), PaintOp.Swap);
          END;
        END;
        IF v.shadow THEN
          (* Paint a border and shadow, if they're perhaps in area *)
          WITH
            top = Rect.T{west := clippedImage.west-1,
                               east := clippedImage.east+1,
                               north := clippedImage.north-1,
                               south := clippedImage.north},
            right1 = Rect.T{west := top.east-1,
                                  east := top.east,
                                  north := top.south,
                                  south := top.north+ShadowSize},
            right2 = Rect.T{west := right1.west,
                                  east := right1.east+ShadowSize,
                                  north := right1.south,
                                  south := clippedImage.south},
            left = Rect.T{west := top.west,
                                east := top.west+1,
                                north := top.south,
                                south := right2.south},
            bottom1 = Rect.T{west := left.west,
                                   east := left.west+ShadowSize,
                                   north := left.south,
                                   south := left.south+1},
            bottom2 = Rect.T{west := bottom1.east,
                                   east := right2.east,
                                   north := bottom1.north,
                                   south := bottom1.south+ShadowSize} DO
            VBT.PaintTint(contents, top, PaintOp.Fg);
            VBT.PaintTint(contents, right1, PaintOp.Fg);
            VBT.PaintTint(contents, right2, PaintOp.Fg);
            VBT.PaintTint(contents, bottom1, PaintOp.Fg);
            VBT.PaintTint(contents, bottom2, PaintOp.Fg);
            VBT.PaintTint(contents, left, PaintOp.Fg);
          END;
        END;
      END;
    END;
  END RepaintOne;

PROCEDURE Repaint(contents: Contents; READONLY rgn: Region.T) =
  (* LL.sup = mu.contents *)
  VAR
    v := contents.thumbnail;
    domain := VBT.Domain(contents);
    firstOrigin := Point.MoveHV(Rect.NorthWest(domain),
                                BarWidth + v.gap.h, v.gap.v);
    cols := contents.thumbnail.columns;
    rows := CEILING(contents.thumbnail.visible());
    surroundFill := Rect.Change(domain, BarWidth, 0, 0, 0);
    frameFill := Rect.FromCorner(
                   Point.MoveHV(firstOrigin, -v.gap.h DIV 2, -v.gap.v DIV 2),
                   cols * (v.size.h+v.gap.h),
                   rows * (v.size.v+v.gap.v));
  BEGIN
    IF Rect.IsEmpty(domain) THEN RETURN END;
    PaintDifference(contents, domain, surroundFill, PaintOp.Fg);
    PaintDifference(contents, surroundFill, frameFill, v.bg);
    FOR row := 0 TO rows-1 DO
      FOR col := 0 TO cols-1 DO
        RepaintOne(contents, rgn, (v.firstVisible + row) * cols + col);
      END;
    END;
  END Repaint;

PROCEDURE Reshape(contents: Contents; READONLY cd: VBT.ReshapeRec) =
  (* LL.sup = mu.contents *)
  VAR
    v := contents.thumbnail;
    firstVisibleImage := v.firstVisible * v.columns;
  PROCEDURE Cols(): INTEGER =
    BEGIN
      RETURN MAX(1,
                 (Rect.HorSize(VBT.Domain(contents)) - v.gap.v - BarWidth) DIV
                                                          (v.size.h + v.gap.h))
    END Cols;
  BEGIN
    v.columns := Cols();
    v.firstVisible := firstVisibleImage DIV v.columns;
    v.updateScroller(); (* clip firstVisible for new shape *)
    VBT.Leaf.reshape(contents, cd);
  END Reshape;

PROCEDURE Mouse(contents: Contents; READONLY cd: VBT.MouseRec) =
  (* LL.sup = VBT.mu *)
  VAR cage: VBT.Cage;
  BEGIN
    WITH v = contents.thumbnail DO
      IF cd.clickType = VBT.ClickType.FirstDown THEN
        contents.oldSel := v.selected;
        contents.tracking := TRUE;
      END;
      IF contents.tracking THEN
        cage := contents.trackCP(cd.cp);
        IF cd.clickType = VBT.ClickType.LastUp THEN
          contents.tracking := FALSE;
          VBT.SetCage(contents, VBT.EverywhereCage);
          IF v.selected # contents.oldSel THEN v.hit(v.selected, cd.time) END;
        ELSE
          VBT.SetCage(contents, cage);
        END;
      END;
    END;
  END Mouse;

PROCEDURE Position(contents: Contents; READONLY cd: VBT.PositionRec) =
  (* LL.sup = VBT.mu *)
  BEGIN
    IF contents.tracking THEN
      VBT.SetCage(contents, contents.trackCP(cd.cp));
    END;
  END Position;

PROCEDURE TrackCP(contents: Contents; cp: VBT.CursorPosition): VBT.Cage =
  VAR
    domain := VBT.Domain(contents);
    target :=  Point.T{ h := -1, v := -1 };
    newSel: INTEGER;
    cage: VBT.Cage;
  BEGIN
    WITH v = contents.thumbnail DO
      IF cp.gone THEN
        IF NOT cp.offScreen AND cp.pt # Point.Origin THEN
          IF cp.pt.v < domain.north THEN
            v.scrollTo(v.firstVisible-1);
            target.h := 0; target.v := 0;
          ELSIF cp.pt.v >= domain.south THEN
            v.scrollTo(v.firstVisible+1);
            target.h := v.columns - 1;
            target.v := FLOOR(v.visible()) - 1;
          END;
        END;
        cage := VBT.EmptyCage;
      ELSE
        target.h := MIN(v.columns-1,
            (cp.pt.h - domain.west - BarWidth) DIV (v.size.h + v.gap.h));
        target.v := (cp.pt.v - domain.north) DIV (v.size.v + v.gap.v);
        cage := VBT.CageFromPosition(cp)
      END;
      newSel := target.h + (target.v + v.firstVisible) * v.columns;
      IF newSel >= 0 THEN v.set(MIN(newSel, v.total-1)) END;
      RETURN cage
    END;
  END TrackCP;

(* *)
(* Implementation of T Methods *)
(* *)

PROCEDURE Init(v: T; colors: PaintOp.ColorQuad := NIL;
               size, gap: Point.T): T =
    (* LL = VBT.mu *)
  BEGIN
    IF colors = NIL THEN v.bg := PaintOp.Bg ELSE v.bg := colors.bg END;
    v.total := 0;
    v.images := NEW(ImageArray, 0);
    v.size := size;
    v.gap := Point.T{h := MAX(3, gap.h), v := MAX(3, gap.v)};
    v.firstVisible := 0;
    v.selected := -1;
    IF v.contents = NIL THEN
      EVAL HVSplit.T.init(v, Axis.T.Hor);
      v.vScroll := NEW(Scroller, thumbnail := v).init(Axis.T.Ver, colors);
      Split.AddChild(v, v.vScroll);
      v.contents := NEW(Contents, thumbnail := v);
      Split.AddChild(v, v.contents);
    ELSE
      VBT.Mark(v);
    END;
    RETURN v
  END Init;

PROCEDURE Acquire(v: T; n: INTEGER): Images.T =
    (* LL.sup = mu.v, i.e. v's share of VBT.mu, as for repaint method *)
  BEGIN
    IF n < 0 OR n >= v.total THEN RETURN NIL ELSE RETURN v.images[n] END;
  END Acquire;

PROCEDURE SetContents(v: T; rd: Rd.T;
                      READONLY dir: LecternDoc.Dir;
                      gamma := 1.0) =
    (* LL.sup = VBT.mu *)
  BEGIN
    v.total := NUMBER(dir.pages^);
    v.selected := MIN(v.total-1, v.selected);
    v.images := NEW(ImageArray, v.total);
    FOR i := 0 TO LAST(v.images^) DO
      WITH entry = dir.pages[i][LecternDoc.Class.Thumbnail] DO
        IF entry.start = 0 THEN
          v.images[i] := NIL
        ELSE
          v.images[i] := NEW(ImageRd.T).init(rd, entry.start,
                                 entry.length,
                                 PaintOp.Copy,
                                 NIL,
                                 gamma);
        END;
      END;
    END;
    (* Defer repaint *)
    VBT.Mark(v);
    VBT.Mark(v.contents);
    v.updateScroller(); (* might clip firstVisible *)
  END SetContents;

PROCEDURE Set(v: T; n: INTEGER) =
    (* LL.sup = VBT.mu *)
  VAR oldSel := v.selected;
  BEGIN
    IF n # v.selected THEN
      v.selected := n;
      IF NOT VBT.IsMarked(v.contents) THEN
        IF oldSel >= 0 THEN RepaintOne(v.contents, Region.Full, oldSel) END;
        IF n >= 0 THEN RepaintOne(v.contents, Region.Full, n) END;
      END;
    END;
  END Set;

PROCEDURE Get(v: T): INTEGER =
    (* LL.sup = VBT.mu *)
  BEGIN
    RETURN v.selected;
  END Get;

PROCEDURE ScrollToShow(v: T; n: INTEGER) =
    (* LL.sup = VBT.mu *)
  VAR
    row := n DIV v.columns;
    this := v.firstVisible;
  BEGIN
    IF row < v.firstVisible THEN
      (* make it visible at the bottom.  Good if we're scrolling back *)
      this := row - (FLOOR(v.visible()) - 1);
    ELSIF row >= v.firstVisible + FLOOR(v.visible()) THEN
      (* make it visible at the top.  Good if we're scrolling forward *)
      this := row;
    END;
    IF this # v.firstVisible THEN
      (* Update data structures, but defer painting *)
      v.firstVisible := this;
      v.updateScroller(); (* might clip firstVisible *)
      VBT.Mark(v.contents);
    END;
  END ScrollToShow;

PROCEDURE Hit(<*UNUSED*>v: T;
              <*UNUSED*>n: INTEGER;
              <*UNUSED*>time: VBT.TimeStamp) =
    (* LL.sup = VBT.mu *)
  BEGIN
  END Hit;

PROCEDURE Visible(v: T): REAL =
    (* Returns number of rows completely or partially visible *)
  BEGIN
    RETURN MAX(1.0,
               FLOAT(Rect.VerSize(VBT.Domain(v.contents))) /
               FLOAT(v.size.v + v.gap.v))
  END Visible;

PROCEDURE Rows(v: T): INTEGER =
  VAR cols := v.columns;
  BEGIN
    RETURN MAX(1, (v.total + cols - 1) DIV cols)
  END Rows;

PROCEDURE UpdateScroller(v: T) =
  VAR rows := v.rows();
  BEGIN
    v.firstVisible := MAX(0, MIN(v.firstVisible, rows - FLOOR(v.visible())));
    IF v.vScroll # NIL THEN
      ScrollerVBTClass.Update(v := v.vScroll,
                              start := v.firstVisible,
                              end := v.firstVisible + FLOOR(v.visible()),
                              length := rows);
    END;
  END UpdateScroller;

PROCEDURE ScrollTo(v: T; this: INTEGER) =
  VAR old := v.firstVisible;
  VAR delta: Point.T;
  VAR badR: Region.T;
  VAR domain := VBT.Domain(v.contents);
  BEGIN
    v.firstVisible := this;
    v.updateScroller(); (* might clip firstVisible *)
    IF v.firstVisible # old AND NOT Rect.IsEmpty(domain) THEN
      IF VBT.IsMarked(v.contents) THEN
        badR := Region.Full;
        VBT.Unmark(v.contents); (* we're about to explicitly repaint it *)
      ELSE
        delta := Point.T{ h := 0,
                          v := (old - v.firstVisible) * (v.size.v + v.gap.v) };
        badR := Region.Difference(Region.FromRect(domain),
                                  Region.FromRect(Rect.Add(domain, delta)));
        VBT.Scroll(v.contents, domain, delta);
      END;
      v.contents.repaint(badR);
    END;
  END ScrollTo;


(* *)
(* Initialization, such as it is *)
(* *)

BEGIN

END ThumbnailVBT.
