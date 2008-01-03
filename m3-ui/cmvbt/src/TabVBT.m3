(* Copyright 1996-2000, Critical Mass, Inc. All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE TabVBT;

IMPORT Axis, Filter, FilterClass, Font, PaintOp, Point;
IMPORT Rect, Region, Shadow, Split, TSplit, VBT, VBTClass;

CONST
  CW = 3;  (* width of a chiseled or beveled edge in pixels *)
  InitialTabOffset = 2 * CW;

TYPE
  LineDesc = RECORD h00, v00, h0n, v0n,  h10, v10, h1n, v1n: [-128 .. +127]; END;

VAR
  NorthEdge  := LineDesc { 0, 0, +1, +1,       0, 0, -1, +1 };
  EastEdge   := LineDesc { -CW, +CW, +1, -1,  -CW, -CW, +1, +1 };
  SouthEdge  := LineDesc { +CW, -CW, -1, +1,  -CW, -CW, +1, +1 };
  WestEdge   := LineDesc { 0, 0, +1, +1,       0, 0, +1, -1 };
  EastTab    := LineDesc { -CW, +CW, +1, -1,  -CW, +CW, +1, -1 };
  WestTab    := LineDesc { 0, 0, +1, +1,       0, 0, +1, +1 };
  TopLeft    := LineDesc { 0, 0, +1, +1,       0, 0, +1, +1 };
  TopRight   := LineDesc { 0, 0, -1, +1,       0, 0, -1, +1 };
  SolidNorth := LineDesc { 0, 0, 0, +1,        0, 0, 0, +1 };

TYPE
  Tab = RECORD
    title    : TEXT;
    vbt      : VBT.T;
    h_offset : INTEGER;  (* from west edge of parent *)
    width    : INTEGER;  (* in pixels of this tab *)
  END;

REVEAL
  T = Public BRANDED OBJECT
    tabs       : REF ARRAY OF Tab;
    tab_height : CARDINAL;       (* height of tab row in pixels *)
    tab_width  : CARDINAL;       (* width of tab row from west edge of domain *)
    cur_tab    : INTEGER;        (* index of current tab *)
    fnt        : Font.T;
    text_inset : CARDINAL;
    text_base  : CARDINAL;
    shadow     : Shadow.T;
  OVERRIDES
    init          := Init;
    mouse         := Mouse;
    reshape       := Reshape;
    rescreen      := Rescreen;
    repaint       := Repaint;
    shape         := Shape;
    locate        := Locate;
  END;

PROCEDURE New (READONLY tabs     : ARRAY OF TEXT;
               READONLY contents : ARRAY OF VBT.T;
                        fnt      :  Font.T := Font.BuiltIn;
                        shadow   : Shadow.T := NIL         ): T =
  BEGIN
    RETURN NEW (T).init (tabs, contents, fnt, shadow);
  END New;

PROCEDURE Init (self     : T;
       READONLY headings : ARRAY OF TEXT;
       READONLY contents : ARRAY OF VBT.T;
                tabfnt   : Font.T;
                shadow   : Shadow.T       ): T = 
  <* FATAL Split.NotAChild *>
  VAR
    prev: VBT.T := NIL;
    cnt := NUMBER(contents);
  BEGIN
    <* ASSERT cnt > 0 *>
    <* ASSERT cnt = NUMBER(headings) *>

    IF (shadow = NIL) THEN shadow := Shadow.None; END;

    self.tabs       := NEW (REF ARRAY OF Tab, cnt);
    self.tab_height := 2 * CW + 20; (* use BoundingBox later... *)
    self.cur_tab    := -1;
    self.fnt        := tabfnt;
    self.ch         := NEW(TSplit.T).init(fickle := FALSE);
    self.text_inset := 4;
    self.text_base  := 4;
    self.shadow     := shadow;

    prev := NIL;
    FOR i := FIRST(contents) TO LAST(contents) DO
      WITH z = self.tabs[i] DO
        z.title := headings [i];
        z.vbt   := contents [i];
        Split.Insert (self.ch, prev, z.vbt);
        prev := z.vbt;
      END;
    END;

    ResetTabs (self);

    self.cur_tab := 0;
    TSplit.SetCurrent (self.ch, contents[0]);
    EVAL Filter.T.init (self, self.ch);

    RETURN self;
  END Init;


PROCEDURE GetDomains (v: T;  VAR(*OUT*) dom, child: Rect.T) = 
  BEGIN
    dom := VBT.Domain (v);
    child := Rect.Change (dom, dn := v.tab_height + CW,
                          ds := -CW, dw := CW, de := -CW);
  END GetDomains;

CONST
  LocChild = -1;
  LocGone  = -2;

PROCEDURE Location (v: T;  READONLY pt: Point.T): INTEGER =
  (* Returns the logical location in "v"'s domain that contains "pt".
     The locations returned are:
  |      0..n     => in tab "n"
  |      LocChild => in child domain
  |      LocGone  => somewhere else...
  *)
  VAR
    dom   : Rect.T;
    chDom : Rect.T;
    x, y  : INTEGER;
    tab_base : INTEGER;
  BEGIN
    GetDomains (v, dom, chDom);
    IF Rect.Member (pt, chDom) THEN RETURN LocChild; END;

    tab_base := dom.north + v.tab_height;
    IF (dom.north <= pt.v) AND (pt.v < tab_base) THEN
      (* it's in the tab row *)
      FOR i := FIRST (v.tabs^) TO LAST (v.tabs^) DO
        WITH z = v.tabs[i] DO
          x := dom.west + z.h_offset;
          y := dom.north;  IF (i # v.cur_tab) THEN INC (y, CW); END;
          IF (x <= pt.h) AND (pt.h < x + z.width)
           AND (y <= pt.v) AND (pt.v < tab_base) THEN
            RETURN i;
          END;
        END;
      END;
    END;

    RETURN LocGone;
  END Location;

PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) RAISES {} =
  <* FATAL Split.NotAChild *>
  VAR loc: INTEGER;
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      loc := Location (v, cd.cp.pt);
      IF loc = LocChild THEN
        (* let the child see it *)
        VBTClass.Mouse (v.ch, cd);
      ELSIF loc = LocGone THEN
        (* ignore it *)
      ELSE (* in the tab row *)
        IF (loc # v.cur_tab) THEN
          PaintTab (v, v.cur_tab, FALSE, Rect.Full);
          PaintTab (v, loc,       TRUE,  Rect.Full);
          v.cur_tab := loc;
          TSplit.SetCurrent (v.ch, v.tabs[loc].vbt);
        END;
      END;
    ELSE (* let others handle it *)
      VBTClass.Mouse (v.ch, cd);
    END;
  END Mouse;

PROCEDURE PaintEdge (v    : T;
            READONLY clip : Rect.T;
                     a, b : Point.T;
            READONLY edge : LineDesc;
                     op   : PaintOp.T) =
  VAR r: Rect.T;
  BEGIN
    INC (a.h, edge.h00);  INC (a.v, edge.v00);
    INC (b.h, edge.h10);  INC (b.v, edge.v10);
    FOR i := 1 TO CW DO
      r.north := a.v;  r.south := b.v + 1;
      r.west  := a.h;  r.east  := b.h + 1;
      VBT.PaintTint (v, Rect.Meet (clip, r), op);
      (*** VBT.Line (v, clip, a, b, op := op); **)
      INC (a.h, edge.h0n);  INC (a.v, edge.v0n);
      INC (b.h, edge.h1n);  INC (b.v, edge.v1n);
    END;
  END PaintEdge;

PROCEDURE PaintTab (v: T;  i: CARDINAL;  up: BOOLEAN;  READONLY clip: Rect.T) =
  VAR
    dom := VBT.Domain(v);
    p, q : Point.T;
    r   : Rect.T;
  BEGIN
    IF (i >= NUMBER (v.tabs^)) THEN RETURN; END;

    WITH z = v.tabs[i] DO
      r.north := dom.north;
      r.south := dom.north + v.tab_height + CW;
      r.west  := dom.west + z.h_offset;
      r.east  := r.west + z.width;
      VBT.PaintTint (v, r, op := v.shadow.bg);
      r.south := dom.north + v.tab_height;

      IF up THEN
        p.h := r.west;   p.v := r.north;
        q.h := r.east;   q.v := r.north;
        PaintEdge (v, clip, p, q, NorthEdge, v.shadow.light);

        p.h := r.west;   p.v := r.north;
        q.h := r.west;   q.v := r.south;
        PaintEdge (v, clip, p, q, WestTab, v.shadow.light);

        p.h := r.east;   p.v := r.north;
        q.h := r.east;   q.v := r.south;
        PaintEdge (v, clip, p, q, EastTab, v.shadow.dark);

        p.h := dom.west;  p.v := r.south;
        q.h := r.west;    q.v := r.south;
        PaintEdge (v, clip, p, q, TopLeft, v.shadow.light);

        p.h := r.east;    p.v := r.south;
        q.h := dom.east;  q.v := r.south;
        PaintEdge (v, clip, p, q, TopRight, v.shadow.light);

        p.h := r.west + CW + v.text_inset;   p.v := r.south - CW - v.text_base;
        VBT.PaintText (v, clip, p, v.fnt, z.title, v.shadow.transparentFg);

      ELSE (* "down" *)
        p.h := r.west;   p.v := r.north;
        q.h := r.east;   q.v := r.north;
        PaintEdge (v, clip, p, q, SolidNorth, v.shadow.bg);

        p.h := r.west;   p.v := r.north + CW;
        q.h := r.east;   q.v := r.north + CW;
        PaintEdge (v, clip, p, q, NorthEdge, v.shadow.light);

        p.h := r.west;   p.v := r.north + CW;
        q.h := r.west;   q.v := r.south + CW;
        PaintEdge (v, clip, p, q, WestEdge, v.shadow.light);

        p.h := r.east;   p.v := r.north + CW;
        q.h := r.east;   q.v := r.south + CW;
        PaintEdge (v, clip, p, q, EastEdge, v.shadow.dark);

        p.h := r.west;   p.v := r.south;
        q.h := r.east;   q.v := r.south;
        PaintEdge (v, clip, p, q, SolidNorth, v.shadow.light);

        p.h := r.west + CW + v.text_inset;   p.v := r.south - v.text_base;
        VBT.PaintText (v, clip, p, v.fnt, z.title, v.shadow.transparentFg);
      END;
    END;
  END PaintTab;

PROCEDURE PaintTabs (v: T;  READONLY clip: Rect.T) =
  VAR r: Rect.T;  p, q: Point.T;
  BEGIN
    (* paint the space right of all the tabs *)
    r := VBT.Domain (v);
    r.south := r.north + v.tab_height;
    r.east  := r.west + InitialTabOffset;
    VBT.PaintTint (v, r, op := v.shadow.bg);
    p.h := r.west;       p.v := r.south;
    q.h := r.west + InitialTabOffset + CW;  q.v := r.south;
    PaintEdge (v, clip, p, q, NorthEdge, v.shadow.light);

    (* paint the space left of all the tabs *)
    r := VBT.Domain (v);
    r.south := r.north + v.tab_height;
    r.west  := r.west + v.tab_width;
    VBT.PaintTint (v, r, op := v.shadow.bg);
    p.h := r.west - CW;   p.v := r.south;
    q.h := r.east;   q.v := r.south;
    PaintEdge (v, clip, p, q, NorthEdge, v.shadow.light);

    FOR i := FIRST (v.tabs^) TO LAST (v.tabs^) DO
      PaintTab (v, i, (i = v.cur_tab), clip);
    END;
  END PaintTabs;

PROCEDURE RepaintBorder (v: T; READONLY br: Rect.T) =  
  VAR  r := VBT.Domain (v);  p, q: Point.T;
  BEGIN
    r.north := r.north + v.tab_height;

    p.h := r.west;   p.v := r.north;
    q.h := r.west;   q.v := r.south;
    PaintEdge (v, br, p, q, WestEdge, v.shadow.light);

    p.h := r.west;  p.v := r.south;
    q.h := r.east;  q.v := r.south;
    PaintEdge (v, br, p, q, SouthEdge, v.shadow.dark);

    p.h := r.east;   p.v := r.north;
    q.h := r.east;   q.v := r.south;
    PaintEdge (v, br, p, q, EastEdge, v.shadow.dark);

    PaintTabs(v, br);
  END RepaintBorder;

PROCEDURE Rescreen(v: T; READONLY cd: VBT.RescreenRec) RAISES {} =
  BEGIN
    Public.rescreen (v, cd);
    RepaintBorder (v, Rect.Full);
  END Rescreen;

PROCEDURE Repaint(v: T; READONLY badR: Region.T) RAISES {} =
  BEGIN
    Public.repaint(v, badR);
    RepaintBorder (v, badR.r);
  END Repaint;

PROCEDURE ResetTabs (v: T) =
  (* LL = VBT.mu *)
  VAR
    bbox       := VBT.BoundingBox (v, "Xy", v.fnt);
    txt_height := Rect.VerSize (bbox);
  BEGIN
    v.text_inset := MAX (1, txt_height DIV 4);
    v.text_base  := v.text_inset + bbox.south;
    v.tab_height := 2 * CW + 2 * v.text_inset + txt_height;
    v.tab_width := InitialTabOffset;
    FOR i := FIRST (v.tabs^) TO LAST (v.tabs^) DO
      WITH z = v.tabs[i] DO
        z.h_offset := v.tab_width;
        z.width    := VBT.TextWidth (v, z.title, v.fnt) + 2 * CW + 2 * v.text_inset;
        v.tab_width := z.h_offset + z.width;
      END;
    END;
  END ResetTabs;

PROCEDURE Reshape(v: T;  READONLY cd: VBT.ReshapeRec) =
  (* LL = VBT.mu *)
  VAR dom, chDom: Rect.T;
  BEGIN
    GetDomains (v, dom, chDom);
    VBTClass.Reshape(v.ch, new := chDom, saved := cd.saved);
    ResetTabs(v);
    RepaintBorder (v, Rect.Full);
  END Reshape;

PROCEDURE Shape(v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  (* LL = VBT.mu *)
  VAR sz: VBT.SizeRange;  extra, tab_width: INTEGER;
  BEGIN
    ResetTabs (v);

    (* get the child's shape *)
    sz := VBTClass.GetShape (v.ch, ax, n);

    (* add the borders & tab bar *)
    IF (ax = Axis.T.Ver)
      THEN extra := 2 * CW + v.tab_height;
      ELSE extra := 2 * CW;
    END;

    (* make room for all the tabs *)
    IF (ax = Axis.T.Hor) THEN
      tab_width := v.tab_width + 2 * CW (*on right*);
      IF (sz.lo < tab_width) THEN
        sz.lo   := tab_width;
        sz.pref := MAX (tab_width, sz.pref);
        sz.hi   := MAX (tab_width + 1, sz.hi);
      END;
    END;

    INC (sz.lo,   extra);
    INC (sz.pref, extra);
    INC (sz.hi,   extra);

    RETURN sz;
  END Shape;

PROCEDURE Locate(v: T; READONLY pt: Point.T; VAR r: Rect.T): VBT.T =
  BEGIN
    RETURN VBT.Split.locate(v.ch, pt, r)
  END Locate;

BEGIN
END TabVBT.
