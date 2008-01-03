(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 27 15:38:42 PDT 1996 by najork                   *)
(*      modified on Wed Jun 12 13:58:49 PDT 1996 by mhb                      *)

MODULE HTMLVBTG EXPORTS HTMLVBTG, HTMLVBTGRep;

IMPORT Axis, BooleanVBT, BorderedVBT, ResourceBundle, Bundle, ButtonVBT,
       CIText, Color, ColorName, Cursor, FeedbackVBT, Filter, FlexVBT, Fmt,
       Font, HighlightVBT, HTML, HTMLVBT, HVSplit, Image, Images, Lex, PaintOp,
       PackSplit, Pixmap, PixmapVBT, Point, Pts, Rd, ReactivityVBT,
       Rect, RefList, Rsrc, Shadow, ShadowedVBT, Split, SwitchVBT,
       Text, TextPort, TextRd, TextVBT, TextureVBT, Thread,
       TSplit, TypeinVBT, VBT, VBTClass, ViewportVBT, Web, Oblet;

<* FATAL Split.NotAChild, ColorName.NotFound *>

REVEAL
  T = Private BRANDED OBJECT
      OVERRIDES
        init := Init;
      END;

TYPE ImageVBT = RigidPixmapVBT OBJECT 
  ismap: BOOLEAN
END;

REVEAL 
  ImageInfo = PublicImageInfo BRANDED OBJECT 
    vbt: ImageVBT;
  OVERRIDES
    load := LoadImage;
  END;

PROCEDURE Init (    v         : T;
                    html      : HTML.T;
                    useAlt    : BOOLEAN;
                    useZippers: BOOLEAN;
                VAR list      : RefList.T;
                    scrollBar : BOOLEAN): T =
  VAR
    split := HVSplit.New(Axis.T.Ver, adjustable := FALSE);
  BEGIN
    EVAL HTMLVBT.T.init(v, html);
    v.baseURL := html.base;
    v.useAlt := useAlt;
    v.useZippers := useZippers;
    v.toLoad := NIL;
    v.hsplit := NIL;
    v.headers := NIL;
    v.ulDepth := 0;
    v.verbatim := FALSE;
    IF html.isIndex THEN DisplayIsIndex(v, split) END;
    IF html.body # NIL THEN
      v.page := split;
      WalkSequence(v, split, DefaultState, html.body);
      IF v.useZippers THEN
        NullifyEmptyHeaders(v);
        ExpandTopHeader(v);
      END;
    END;

    Split.AddChild(split, TextureVBT.New());

    IF scrollBar THEN
      WITH rim = BorderedVBT.New(split, size := Pts.ToMM(PageMarginAmt),
                           op := RegularColors.bg),
           hilite = HighlightVBT.New(rim),
           viewport = NEW(ViewportVBT.T).init(
                  ch := hilite, axis := Axis.T.Ver,
                  scrollStyle := ViewportVBT.ScrollStyle.VerOnly,
                  shapeStyle := ViewportVBT.ShapeStyle.Related,
                  shadow := RegularShadow) DO
        EVAL Filter.Replace(v, viewport);
      END;
    ELSE
      EVAL Filter.Replace(v, split);
    END;

    list := RefList.ReverseD(v.toLoad);
    RETURN v
  END Init;

PROCEDURE ExpandTopHeader (v: T) =
  (* look at all the headers.  find the min header level.  if there's only
     one at that level, expand it.  if there's only one header, nullify its
     toggler. *)
  VAR h: HeaderBox;
  VAR
    header := v.headers;
    min    := LAST(INTEGER);
    ct     := 0;
    total  := 0;
  BEGIN
    WHILE header # NIL DO
      IF NOT header.empty THEN
        INC(total);
        IF header.level = min THEN
          INC(ct)
        ELSIF header.level < min THEN
          h := header;
          min := header.level;
          ct := 1;
        END
      END;
      header := header.next;
    END;
    IF ct = 1 THEN
      ExpandHeader(h);
      IF total = 1 THEN NullifyToggler(h.toggler) END
    END;
    ExpandHeaders(0, v.headers, FALSE)
  END ExpandTopHeader;

PROCEDURE NullifyEmptyHeaders (v: T) =
  (* look at each header.  if it has no subheadings and it has no contents
     (other than glue), remove its toggle. *)

  PROCEDURE NoSubHeaders (h: HeaderBox): BOOLEAN =
    BEGIN
      RETURN h.next = NIL OR h.next.level <= h.level
    END NoSubHeaders;

  PROCEDURE NoContents (h: HeaderBox): BOOLEAN =
    VAR ch := Split.Succ(h.contents, NIL);
    BEGIN
      WHILE ch # NIL DO
        TYPECASE (ch) OF | VGlueVBT => ELSE RETURN FALSE END;
        ch := Split.Succ(h.contents, ch);
      END;
      RETURN TRUE;
    END NoContents;

  PROCEDURE DeleteContents (h: HeaderBox) =
    BEGIN
      WHILE Split.Succ(h.contents, NIL) # NIL DO
        WITH ch = Split.Succ(h.contents, NIL) DO
          Split.Delete(h.contents, ch);
          VBT.Discard(ch);
        END
      END
    END DeleteContents;

  VAR header, nextHeader: HeaderBox;
  BEGIN
    header := v.headers;
    WHILE header # NIL DO
      nextHeader := header.next;
      IF NoContents(header) AND NoSubHeaders(header) THEN
        DeleteContents(header);
        NullifyToggler(header.toggler);
        header.empty := TRUE;
      END;
      header := nextHeader;
    END;
  END NullifyEmptyHeaders;


PROCEDURE EnterHMode (v: T; parent: VBT.Split) =
  BEGIN
    IF v.hsplit = NIL THEN
      IF v.verbatim THEN 
        v.hsplit := HVSplit.New(hv := Axis.T.Hor, adjustable := FALSE);
      ELSE 
        v.hsplit := PackSplit.New(op := RegularColors.bg, hgap := 1.0, vgap := 0.0);
      END;
      Split.AddChild(parent, v.hsplit);
    END;
  END EnterHMode;

PROCEDURE ExitHMode (v: T) =
  BEGIN
    IF v.hsplit # NIL THEN
      IF Split.NumChildren(v.hsplit) = 0 THEN
        Split.Delete (VBT.Parent(v.hsplit), v.hsplit)
      ELSE
        Split.AddChild(v.hsplit, HFill())
      END;
      v.hsplit := NIL;
    END;
  END ExitHMode;

PROCEDURE WalkSequence (v: T; vsplit: VBT.T; s: State; seq: HTML.Sequence) =

  VAR savedState: State;

  PROCEDURE Push () =
    BEGIN
      savedState := s
    END Push;

  PROCEDURE Pop () =
    BEGIN
      s := savedState
    END Pop;

  BEGIN
    WHILE seq # NIL DO

      TYPECASE seq OF

      | HTML.Word (word) =>
          DisplayWord(v, vsplit, s, word);

      | HTML.Image (image) =>
          DisplayImage(v, vsplit, s, image);

      | HTML.Oblet (oblet) =>
          Oblet.DisplayOblet(v, vsplit, s, oblet);

      | HTML.Paragraph =>
          ExitHMode(v);
          Split.AddChild(vsplit, VGlue(ParSkipAmt));

      | HTML.LineBreak => 
          ExitHMode(v);

      | HTML.HorizontalRule =>
          ExitHMode(v);
          Split.AddChild(vsplit, VGlue(HRPreSkipAmt));
          Split.AddChild(vsplit, HBar(HRAmt));
          Split.AddChild(vsplit, VGlue(HRPostSkipAmt));

      | HTML.Typewriter (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, FontFamily.Fixed, s.size, s.weight, s.slant);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Boldface (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, s.family, s.size, FontWeight.Bold, s.slant);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Italic (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, s.family, s.size, s.weight, FontSlant.Slanted);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Underline (format) =>
          EnterHMode(v, vsplit);
          WalkSequence(v, vsplit, s, format.content);

      | HTML.Emphasis (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, s.family, s.size, s.weight, FontSlant.Slanted);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Strong (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, s.family, s.size, FontWeight.Bold, s.slant);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Code (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, FontFamily.Fixed, s.size, s.weight, s.slant);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Keyboard (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, FontFamily.Fixed, s.size, s.weight, s.slant);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Sample (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, FontFamily.Fixed, s.size, s.weight, s.slant);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Definition (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(
            s, s.family, s.size, FontWeight.Bold, FontSlant.Slanted);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Variable (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, FontFamily.Fixed, s.size, s.weight, s.slant);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Citation (format) =>
          EnterHMode(v, vsplit);
          Push();
          ChangeFont(s, s.family, s.size, s.weight, FontSlant.Slanted);
          WalkSequence(v, vsplit, s, format.content);
          Pop();

      | HTML.Glossary (glossary) =>
          WalkGlossary(v, vsplit, s, glossary);

      | HTML.List (list) =>
          WalkList(v, vsplit, s, list);

      | HTML.Preformatted (pre) =>
          ExitHMode(v);
          Split.AddChild(vsplit, VGlue(ParSkipAmt));
          v.verbatim := TRUE;
          Push();
          ChangeFont(s, FontFamily.Fixed, FontSize.Normal,
                     FontWeight.Normal, FontSlant.Normal);
          WalkSequence(v, vsplit, s, pre.content);
          Pop();
          v.verbatim := FALSE;
          ExitHMode(v);
          Split.AddChild(vsplit, VGlue(ParSkipAmt));

      | HTML.Anchor (anchor) =>
          WalkAnchor(v, vsplit, s, anchor);

      | HTML.Heading (heading) =>
          ExitHMode(v);
          Push();
          vsplit := WalkHeading(v, vsplit, s, heading);
          Pop();
          ExitHMode(v);

      | HTML.Address (addr) =>
          ExitHMode(v);
          Push();
          ChangeFont(
            s, s.family, s.size, FontWeight.Normal, FontSlant.Slanted);
          WalkSequence(v, vsplit, s, addr.content); 
          Pop();
          ExitHMode(v);

      | HTML.BlockQuote (quote) =>
          WalkBlockQuote(v, vsplit, s, quote);

      | HTML.Table (table) =>
          ExitHMode(v);
          WalkSequence(v, vsplit, s, table.content);

      | HTML.TableRow (table) =>
          ExitHMode(v);
          WalkSequence(v, vsplit, s, table.content);
          ExitHMode(v);

      ELSE
        EnterHMode(v, vsplit);
        Split.AddChild(v.hsplit, TextVBT.New("????", bgFg := ErrorColors))
      END;

      seq := seq.next;
    END;
  END WalkSequence;


PROCEDURE WalkGlossary (v: T; vsplit: VBT.T; s: State; glossary: HTML.Glossary) =
  VAR gs := glossary.content;
  BEGIN
    ExitHMode(v);
    Split.AddChild(vsplit, VGlue(ParSkipAmt)); 

    IF glossary.preContent # NIL THEN
       ExitHMode(v);
       WalkSequence(v, vsplit, s, glossary.preContent);
    END;

    WHILE gs # NIL DO

      IF gs.term # NIL THEN
         ExitHMode(v);
         WalkSequence(v, vsplit, s, gs.term);
      END;

      IF gs.definition # NIL THEN
        ExitHMode(v); 
        WITH hbox = HVSplit.New(hv := Axis.T.Hor, adjustable := FALSE) DO
          Split.AddChild(vsplit, hbox);
          Split.AddChild(hbox, HGlue(2.0*IndentAmt));
          WITH vbox = HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE) DO
            Split.AddChild(hbox, vbox);
            WalkSequence(v, vbox, s, gs.definition)
          END
        END
      END;
      gs := gs.next;
    END;
    ExitHMode(v);

  END WalkGlossary;


VAR
  Solid := GetPixmap("filledbullet.pbm", ResourceBundle.Get());
  Hollow := GetPixmap("hollowbullet.pbm", ResourceBundle.Get());

PROCEDURE WalkList (v: T; vsplit: VBT.T; s: State; list: HTML.List) =
  VAR
    item := list.content;
    listhbox := HVSplit.New(hv := Axis.T.Hor, adjustable := FALSE);
    listvbox := HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE);
    tick, tickbox, hbox, vbox: VBT.T;
    bullet: Pixmap.T;
    chCount: INTEGER;
  BEGIN
    IF list.preContent # NIL THEN
       ExitHMode(v);
       WalkSequence(v, vsplit, s, list.preContent);
    END;

    ExitHMode(v);
    IF list.kind = HTML.ListKind.Ordered THEN 
      chCount := 1;
    ELSE
      chCount := 0;
      IF v.ulDepth = 0 THEN bullet := Solid ELSE bullet := Hollow END;
      INC(v.ulDepth);
    END;

    Split.AddChild(vsplit, listhbox); 
    Split.AddChild(listhbox, HGlue(IndentAmt));
    Split.AddChild(listhbox, listvbox);
    WHILE item # NIL DO
      hbox := HVSplit.New(hv := Axis.T.Hor, adjustable := FALSE);
      Split.AddChild(listvbox, hbox);
      IF chCount = 0 THEN
        tick := NEW(RigidPixmapVBT).init(pm:=bullet, op:=s.bgFg.bgFg, bg:=s.bgFg.bg);
      ELSE
        tick := NEW(RigidTextVBT).init(Fmt.Pad(Fmt.Int(chCount), 2) & ". ", bgFg:=s.bgFg, fnt:=s.font);
        INC(chCount);
      END;
      tickbox := HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE);
      Split.AddChild(tickbox, NEW(FlexVBT.T).init(tick, FlexVBT.Fixed));
      Split.AddChild(tickbox, VFill());

      Split.AddChild(hbox, tickbox);
      vbox := HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE);
      Split.AddChild(hbox, vbox);
      EnterHMode(v, vbox);
        (* add either number or bullet *)
      WalkSequence(v, vbox, s, item.content);
      ExitHMode(v);
      item := item.next;
    END;

    IF list.kind # HTML.ListKind.Ordered THEN 
      DEC(v.ulDepth);
    END;
  END WalkList;


PROCEDURE WalkHeading (v: T; vsplit: VBT.T; s: State; heading: HTML.Heading):
  VBT.T =
  BEGIN
    IF v.useZippers THEN
      RETURN WalkZipperedHeading(v, s, heading)
    ELSE
      WITH h = headingInfo[heading.level] DO
        Split.AddChild(vsplit, VGlue(h.preGlue));
        ChangeFont(s, FontFamily.Normal, h.fontSize, FontWeight.Bold,
                   FontSlant.Normal);
        WalkSequence(v, vsplit, s, heading.content);
        Split.AddChild(vsplit, VGlue(h.postGlue));
      END;
      RETURN vsplit
    END
  END WalkHeading;

(* The VBT structure of a HeaderBox is as follows:

   (HeaderBoxTSplit
     empty1
     (VBox1 
       preglue
       (HBox 
         toggler 
         (VBox2 
           heading
           (HideawayTSplit 
             empty2
             (ContentsVBox postglue ..stuff..))))))

*)

(* HideawayTSplit - Like a TSplit, except when the current child is NIL, 
   the shape is empty rather than a VBT's default shape. *)

TYPE HideawayTSplit = TSplit.T OBJECT  
  METHODS 
    set(show: BOOLEAN) := HTSet;
  OVERRIDES 
    shape := HTShape; 
  END;

PROCEDURE HTSet (v: HideawayTSplit; show: BOOLEAN) =
  BEGIN
    IF show THEN 
      TSplit.SetCurrent(v, Split.Succ(v, NIL))
    ELSE
      TSplit.SetCurrent(v, NIL)
    END
  END HTSet;
 
PROCEDURE HTShape (v: HideawayTSplit; ax: Axis.T; n: CARDINAL): VBT.SizeRange
  RAISES {} =
  BEGIN
    IF TSplit.GetCurrent(v) = NIL THEN
      RETURN VBT.SizeRange{lo := 0, pref := 0, hi := 1};
    ELSE
      RETURN TSplit.T.shape(v, ax, n)
    END
  END HTShape;

REVEAL
  HeaderBox = HideawayTSplit BRANDED OBJECT
                empty: BOOLEAN; (* contents empty and no subheaders *)
                level: INTEGER;
                toExpand: BOOLEAN;
                toggler : Toggler; 
                contents: HVSplit.T;
                next: HeaderBox;
              END;
  
PROCEDURE WalkZipperedHeading (v: T; s: State; heading: HTML.Heading): VBT.T =
  VAR
    header   := NEW(HeaderBox);
    h        := headingInfo[heading.level];
    tsplit   := NEW(HideawayTSplit).init();
    feedback := NEW(TogglerFeedback).init();
    switch   := NEW(SwitchVBT.T).init(feedback);
    toggler := NEW(Toggler, header := header, tsplit := tsplit).init(
                 switch);
    vbox1  := HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE);
    vbox2  := HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE);
    vsplit := HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE);
    hbox   := HVSplit.New(hv := Axis.T.Hor, adjustable := FALSE);
  BEGIN
    VAR l := v.headers; BEGIN
      IF l = NIL THEN v.headers := header; 
      ELSE 
        WHILE l.next # NIL DO l := l.next END;
        l.next := header;
      END
    END;

    BooleanVBT.Put(toggler, TRUE);
    FeedbackVBT.Normal(feedback);

    header.empty := FALSE;
    header.level := heading.level;
    header.toExpand := FALSE;
    header.toggler := toggler;
    header.contents := vsplit;
    header.next := NIL;
    EVAL header.init();
    Split.AddChild(header, vbox1);

    Split.AddChild(vbox1, VGlue(h.preGlue));
    Split.AddChild(vbox1, hbox);

    Split.AddChild(hbox, toggler);
    Split.AddChild(hbox, vbox2);

    ChangeFont(
      s, FontFamily.Normal, h.fontSize, FontWeight.Bold, FontSlant.Normal);
    WalkSequence(v, vbox2, s, heading.content);

    Split.AddChild(vbox2, tsplit);
    Split.AddChild(tsplit, vsplit);
    Split.AddChild(vsplit, VGlue(h.postGlue));
    Split.AddChild(v.page, header);
    RETURN vsplit;

  END WalkZipperedHeading;

VAR
  ExpandIcon     := GetPixmap("expandArrow.pbm", ResourceBundle.Get());
  ExpandOnIcon   := GetPixmap("expandOnArrow.pbm", ResourceBundle.Get());
  ContractIcon   := GetPixmap("contractArrow.pbm", ResourceBundle.Get());
  ContractOnIcon := GetPixmap("contractOnArrow.pbm", ResourceBundle.Get());

  ToggleIcons   := ARRAY BOOLEAN OF Pixmap.T {ContractIcon,   ExpandIcon};
  ToggleOnIcons := ARRAY BOOLEAN OF Pixmap.T {ContractOnIcon, ExpandOnIcon};


TYPE
  TogglerFeedback = FeedbackVBT.T BRANDED OBJECT
                      pm: PixmapVBT.T;
                    METHODS
                      init (): TogglerFeedback := TogglerFeedbackInit;
                    OVERRIDES
                      normal  := TogglerFeedbackNormal;
                      excited := TogglerFeedbackExcited;
                    END;

PROCEDURE TogglerFeedbackInit (f: TogglerFeedback): TogglerFeedback =
  BEGIN
    f.pm :=
      NEW(RigidPixmapVBT).init(
        pm := Pixmap.Solid, op := RegularColors.bgFg, bg := RegularColors.bg,
        valign := 0.0, vmargin := 0.0, hmargin := 2.0);
    RETURN (FeedbackVBT.T).init(f, f.pm);
  END TogglerFeedbackInit;

PROCEDURE TogglerFeedbackNormal (f: TogglerFeedback) =
  BEGIN
    PixmapVBT.Put(f.pm, ToggleIcons[FeedbackVBT.GetState(f)])
  END TogglerFeedbackNormal;

PROCEDURE TogglerFeedbackExcited (f: TogglerFeedback) =
  BEGIN
    PixmapVBT.Put(f.pm, ToggleOnIcons[FeedbackVBT.GetState(f)])
  END TogglerFeedbackExcited;

PROCEDURE NullifyToggler (toggle: Toggler) =
  BEGIN
    VAR r := NEW(ReactivityVBT.T).init(ch := NIL, colors := RegularShadow); BEGIN
      Split.Replace(VBT.Parent(toggle), toggle, r);
      EVAL Filter.Replace(r, toggle);
      ReactivityVBT.Set(r, ReactivityVBT.State.Vanish, Cursor.DontCare);
    END
  END NullifyToggler;


TYPE
  Toggler = BooleanVBT.T BRANDED OBJECT
              header: HeaderBox;
              tsplit: HideawayTSplit;
            OVERRIDES
              callback := TreeToggle;
            END;

PROCEDURE TreeToggle (toggle: Toggler; READONLY cd: VBT.MouseRec) =
  (* open/contract the immediate logical
     children *)
  VAR
    h := toggle.header;
    all := (VBT.Modifier.Shift IN cd.modifiers)
             OR (VBT.Modifier.Control IN cd.modifiers);
  BEGIN
    IF BooleanVBT.Get(toggle) THEN
      CollapseHeader(h);
      CollapseHeaders(h.level, h.next, all);
    ELSE
      ExpandHeader(h);
      ExpandHeaders(h.level, h.next, all);
    END;
  END TreeToggle;


PROCEDURE CollapseHeader (h: HeaderBox) =
  BEGIN
    h.toExpand := FALSE;
    h.toggler.tsplit.set(FALSE);
    BooleanVBT.Put(h.toggler, TRUE);
    FeedbackVBT.Normal(Filter.Child(Filter.Child(h.toggler)));
  END CollapseHeader;

PROCEDURE CollapseHeaders (level      : INTEGER;
                           start      : HeaderBox;
                           collapseAll: BOOLEAN    ) =
  VAR header := start;
  BEGIN
    WHILE header # NIL DO
      IF header.level <= level THEN RETURN END;
      header.set(FALSE);
      IF collapseAll THEN CollapseHeader(header) END;
      header := header.next
    END
  END CollapseHeaders;

  
PROCEDURE ExpandHeader (h: HeaderBox) =
  BEGIN
    h.toExpand := TRUE;
    h.toggler.tsplit.set(TRUE);
    BooleanVBT.Put(h.toggler, FALSE);
    FeedbackVBT.Normal(Filter.Child(Filter.Child(h.toggler)));
  END ExpandHeader;

PROCEDURE ExpandHeaders (level    : INTEGER;
                         start    : HeaderBox;
                         expandAll: BOOLEAN    ) =
  VAR header := start;
  BEGIN
    WHILE header # NIL DO
      IF header.level <= level THEN RETURN END;
      header.set(TRUE);
      IF expandAll THEN ExpandHeader(header) END;
      IF header.toExpand THEN
        header := header.next
      ELSE (* skip subheaders *)
        VAR level := header.level; BEGIN
          header := header.next;  
          WHILE header # NIL AND header.level > level DO 
            header := header.next
          END
        END
      END
    END;
  END ExpandHeaders;


PROCEDURE WalkAnchor (v: T; vsplit: VBT.T; s: State; anchor: HTML.Anchor) =
  VAR href := anchor.href;
  BEGIN
    EnterHMode(v, vsplit);
    IF href = NIL THEN
      (* Probably a NAME anchor -- ignore it. *)
      WalkSequence(v, vsplit, s, anchor.content);
      RETURN;
    END;
    VAR pos := Text.FindChar(href, '#', 0);
    BEGIN
      IF pos = 0 THEN
        (* '#' is first char: defines a destination link in current page; ignore it *)
        WalkSequence(v, vsplit, s, anchor.content);
        RETURN
      END;
      IF pos > 0 THEN
        (* links to a place on href; kill off what's after the '#'. *)
        href := Text.Sub(href, 0, pos);
      END;
      VAR
        button: ButtonVBT.T;
        vbox := HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE);
        hbox := v.hsplit;
      BEGIN
        button :=
          NEW(Anchor, v := v, href := href).init(vbox, AnchorAction);
        Split.AddChild(v.hsplit, button);
        v.hsplit := HVSplit.New(hv := Axis.T.Hor, adjustable := FALSE);
        Split.AddChild(vbox, v.hsplit);
        ChangeColors(s, AnchorColors);
        WalkSequence(v, vbox, s, anchor.content);
        IF v.hsplit # NIL THEN v.hsplit := hbox END;
      END;
    END
  END WalkAnchor;

TYPE Anchor = ButtonVBT.T OBJECT
    v: T;
    href: TEXT
  END;

PROCEDURE AnchorAction (self: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  VAR anchor := NARROW(self, Anchor); where: Point.T;
  BEGIN
    IF IsIsMap(anchor, cd.cp.pt, where) THEN
      anchor.v.ismap(anchor.href, where, cd)
    ELSE
      anchor.v.hotlink(anchor.href, cd)
    END
  END AnchorAction;

PROCEDURE IsIsMap (anchor: Anchor; READONLY pt: Point.T; VAR where: Point.T):
  BOOLEAN =
  VAR v := Split.Locate(anchor, pt);
  BEGIN
    WHILE v # NIL DO
      TYPECASE v OF
      | ImageVBT (im) =>
          IF im.ismap THEN
            where := Rect.GlobToLoc(VBT.Domain(im), pt);
            RETURN TRUE;
          ELSE
            RETURN FALSE;
          END;
      | Split.T => v := Split.Locate(v, pt);
      ELSE
        RETURN FALSE
      END;
    END;
    RETURN FALSE
  END IsIsMap;


PROCEDURE WalkBlockQuote (v: T; vsplit: VBT.T; s: State; quote: HTML.BlockQuote) =
  VAR
    hbox := HVSplit.New(hv := Axis.T.Hor, adjustable := FALSE);
    vbox := HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE);
  BEGIN
    ExitHMode(v);
    Split.AddChild(vsplit, VGlue(ParSkipAmt)); 
    Split.AddChild(vsplit, hbox); 
    Split.AddChild(hbox, HGlue(IndentAmt));
    Split.AddChild(hbox, vbox);
    WalkSequence(v, vbox, s, quote.content);
    ExitHMode(v);
    Split.AddChild(vsplit, VGlue(ParSkipAmt));
  END WalkBlockQuote;


PROCEDURE DisplayIsIndex (v: T; vsplit: VBT.T) =
  VAR hbox := HVSplit.New(hv := Axis.T.Hor, adjustable := FALSE);
  BEGIN
    ExitHMode(v);
    Split.AddChild(vsplit, HBar(HRAmt));
    Split.AddChild(vsplit, VGlue(HRPostSkipAmt));
    Split.AddChild(vsplit, hbox);
    WITH prompt = NEW(RigidTextVBT).init(
                    txt := "This is a searchable index. Enter search keywords: ",
                    hmargin := 0.0, halign := 0.5, 
                    vmargin := 0.0, valign := 0.0, 
                    fnt := DefaultState.font, bgFg := DefaultState.bgFg) DO
      Split.AddChild(hbox, prompt)
    END;
    WITH typein = NEW(IsIndexTypeinVBT, v:=v).init(
                    expandOnDemand := TRUE, font := IsIndexFont,
                    colorScheme := IsIndexShadow),
         shadow = NEW(ShadowedVBT.T).init(typein, IsIndexShadow, Shadow.Style.Lowered) DO
      Split.AddChild(hbox, shadow)
    END;
    Split.AddChild(vsplit, VGlue(HRPreSkipAmt));
    Split.AddChild(vsplit, HBar(HRAmt));
    Split.AddChild(vsplit, VGlue(HRPostSkipAmt));
  END DisplayIsIndex;

TYPE
  IsIndexTypeinVBT = TypeinVBT.T OBJECT 
    v: T;
  OVERRIDES
    returnAction := IsIndexAction;
  END;

PROCEDURE IsIndexAction (                    vbt: IsIndexTypeinVBT; 
                         <*UNUSED*> READONLY cd : VBT.KeyRec) =
  <* FATAL Thread.Alerted *>  
     (* Not sure if fataling Thread.Alerted is appropriate -- najork 8/27/96 *)
  BEGIN
    vbt.v.isindex(Web.EncodeURL(TextPort.GetText(vbt)))
  END IsIndexAction;


PROCEDURE DisplayWord (v: T; vsplit: VBT.T; s: State; word: HTML.Word) =

  PROCEDURE AddText (t: TEXT) =
    VAR
      vbt := NEW(RigidTextVBT).init(
               txt := t, hmargin := 0.0, halign := 0.5, vmargin := 0.0,
               valign := 0.0, fnt := s.font, bgFg := s.bgFg);
    BEGIN
      Split.AddChild(v.hsplit, vbt)
    END AddText;

  CONST
    AllChars  = SET OF CHAR{FIRST(CHAR).. LAST(CHAR)};
    NonBlanks = AllChars - Lex.Blanks;
    NL        = '\n';
    NonNL     = AllChars - SET OF CHAR{NL};

    <* FATAL Rd.EndOfFile, Rd.Failure, Thread.Alerted *>  
         (* Not sure if fataling everything is appropriate -- najork 8/27/96 *)
  BEGIN
    EnterHMode(v, vsplit);

    IF v.verbatim THEN
      WITH rd = TextRd.New(word.word) DO
        IF Rd.GetChar(rd) = NL THEN
          ExitHMode(v);
          EnterHMode(v, vsplit)
        ELSE
          Rd.UnGetChar(rd)
        END;
        WHILE NOT Rd.EOF(rd) DO
          AddText(Lex.Scan(rd, NonNL));
          IF NOT Rd.EOF(rd) THEN
            EVAL Rd.GetChar(rd);
            ExitHMode(v);
            EnterHMode(v, vsplit)
          END
        END
      END

    ELSE
      TYPECASE v.hsplit OF

      | HVSplit.T => AddText(word.word);

      | PackSplit.T =>
          WITH rd = TextRd.New(word.word) DO
            Lex.Skip(rd);
            WHILE NOT Rd.EOF(rd) DO
              AddText(Lex.Scan(rd, NonBlanks));
              IF NOT Rd.EOF(rd) THEN Lex.Skip(rd) END
            END
          END

      ELSE                       <*ASSERT FALSE*>
      END
    END
  END DisplayWord;

PROCEDURE DisplayImage (v: T; vsplit: VBT.T; s: State; image: HTML.Image) =
  BEGIN
    EnterHMode(v, vsplit);
    IF v.useAlt OR image.source = NIL THEN
      VAR alt := image.alternate;
      BEGIN
        IF alt = NIL THEN alt := "[image]" END;
        WITH vbt = NEW(RigidTextVBT).init(
                     txt := alt, hmargin := 0.0, halign := 0.5,
                     vmargin := 0.0, valign := 0.0, fnt := s.font,
                     bgFg := s.bgFg) DO
          Split.AddChild(v.hsplit, vbt)
        END
      END
    ELSE
      VAR
        vbt := NEW(ImageVBT, ismap := image.ismap).init(
                 pm := EmptyImage, 
                 op := (* PaintOp.Copy *) RegularColors.bgFg,
                 bg := RegularColors.bg);
        border := BorderedVBT.New(
                    vbt, size := Pts.ToMM(0.5), op := s.bgFg.fg);
        pm : Pixmap.T;
        url: TEXT;
      BEGIN
        IF s.bgFg # AnchorColors THEN BorderedVBT.SetSize(border, 0.0) END;
        Split.AddChild(v.hsplit, border);
        url := Web.AbsoluteURL(image.source, v.baseURL);
        IF Images.FromCache(url, pm) THEN
          PixmapVBT.Put(vbt, pm)
        ELSE
          v.toLoad :=
            RefList.Cons(NEW(ImageInfo, url := url, align := image.align,
                             vbt := vbt), v.toLoad)
        END
      END
    END
  END DisplayImage;

(* 
PROCEDURE LoadImage (info: ImageInfo; page: Web.Page)
  RAISES {Thread.Alerted} =
  VAR pm: Pixmap.T;
  BEGIN
    TRY
      IF page.header.contentType # Web.MIMEType.Image THEN
        RAISE Images.Error
      END;
      IF CIText.Equal(page.header.contentSubType, "xxgif") THEN
         WITH rd = TextRd.New(page.contents),
              pic = GifPic.New(rd),
              vbt = PicVBT.New(pic)
         DO
           Split.Replace(VBT.Parent(info.vbt), info.vbt, vbt)
         END
      ELSE
        IF CIText.Equal(page.header.contentSubType, "jpeg") THEN
          pm := Images.FromJPEG(page.contents);
        ELSIF CIText.Equal(page.header.contentSubType, "gif") THEN
          pm := Images.FromGIF(page.contents);
        ELSIF CIText.Equal(page.header.contentSubType, "x-xbitmap") THEN
          pm := Images.FromXBM(page.contents);
        ELSIF CIText.Equal(page.header.contentSubType, "ppm") OR
              CIText.Equal(page.header.contentSubType, "pnm") OR
              CIText.Equal(page.header.contentSubType, "pbm") OR
              CIText.Equal(page.header.contentSubType, "pgm") THEN
          WITH rd = TextRd.New(page.contents) DO
            pm := Image.Unscaled(Image.FromRd(rd));
          END;
        ELSE
          RAISE Images.Error
        END;  
        Images.ToCache(info.url, pm);
        LOCK VBT.mu DO PixmapVBT.Put(info.vbt, pm) END
      END
    EXCEPT GifPic.Error, Images.Error, Rd.Failure =>
        LOCK VBT.mu DO
          PixmapVBT.Put(info.vbt, ErrorImage);
          PixmapVBT.SetColors(
            info.vbt, op := ErrorColors.bgFg, bg := ErrorColors.bg)
        END
    END;
  END LoadImage;
*)
PROCEDURE LoadImage (info: ImageInfo; page: Web.Page)
  RAISES {Thread.Alerted} =
  VAR pm: Pixmap.T;
  BEGIN
    TRY
      IF page.header.contentType # Web.MIMEType.Image THEN
        RAISE Images.Error
      END;
      IF CIText.Equal(page.header.contentSubType, "jpeg") THEN
        pm := Images.FromJPEG(page.contents);
      ELSIF CIText.Equal(page.header.contentSubType, "gif") THEN
        pm := Images.FromGIF(page.contents);
      ELSIF CIText.Equal(page.header.contentSubType, "x-xbitmap") THEN
        pm := Images.FromXBM(page.contents);
      ELSIF CIText.Equal(page.header.contentSubType, "ppm") OR
            CIText.Equal(page.header.contentSubType, "pnm") OR
            CIText.Equal(page.header.contentSubType, "pbm") OR
            CIText.Equal(page.header.contentSubType, "pgm") THEN
        WITH rd = TextRd.New(page.contents) DO
          pm := Image.Unscaled(Image.FromRd(rd));
        END;
      ELSE
        RAISE Images.Error
      END;
      Images.ToCache(info.url, pm);
      LOCK VBT.mu DO PixmapVBT.Put(info.vbt, pm) END
    EXCEPT
      Image.Error, Images.Error, Rd.Failure =>
        LOCK VBT.mu DO
          PixmapVBT.Put(info.vbt, ErrorImage);
          PixmapVBT.SetColors(
            info.vbt, op := ErrorColors.bgFg, bg := ErrorColors.bg)
        END
    END;
  END LoadImage;


PROCEDURE VFill (): VBT.T =
  VAR txt := TextureVBT.New(op := RegularColors.bg);
  BEGIN
    RETURN FlexVBT.FromAxis(txt, Axis.T.Ver, FlexVBT.StretchyRange)
  END VFill;


TYPE VGlueVBT = FlexVBT.T BRANDED OBJECT END;

PROCEDURE VGlue (amt: REAL): VBT.T =
  VAR txt := TextureVBT.New(op := RegularColors.bg);
  BEGIN
    RETURN NEW(VGlueVBT).init(
             txt, FlexVBT.Shape{FlexVBT.DefaultRange,
                                FlexVBT.RigidRange(Pts.ToMM(amt))})
  END VGlue;

PROCEDURE HFill (): VBT.T =
  VAR txt := TextureVBT.New(op := RegularColors.bg);
  BEGIN
    RETURN FlexVBT.FromAxis(txt, Axis.T.Hor, FlexVBT.StretchyRange)
  END HFill;

PROCEDURE HGlue (amt: REAL): VBT.T =
  VAR txt := TextureVBT.New(op := RegularColors.bg);
  BEGIN
    RETURN FlexVBT.FromAxis(txt, Axis.T.Hor, FlexVBT.RigidRange(Pts.ToMM(amt)))
  END HGlue;

PROCEDURE HBar (amt: REAL): VBT.T =
  VAR txt := TextureVBT.New(op := RegularColors.fg);
  BEGIN
    RETURN FlexVBT.FromAxis(txt, Axis.T.Ver, FlexVBT.RigidRange(Pts.ToMM(amt)))
  END HBar;



REVEAL RigidPixmapVBT = PixmapVBT.T BRANDED OBJECT 
  OVERRIDES shape := RigidPixmapVBTShape;
END;

PROCEDURE RigidPixmapVBTShape (v: RigidPixmapVBT; ax: Axis.T; n: CARDINAL): VBT.SizeRange
  RAISES {} =
  VAR sr := PixmapVBT.T.shape(v, ax, n);
  BEGIN
    sr.hi := sr.pref + 1;
    RETURN sr
  END RigidPixmapVBTShape;


REVEAL RigidTextVBT = TextVBT.T BRANDED OBJECT 
  OVERRIDES shape := RigidTextVBTShape;
END;

PROCEDURE RigidTextVBTShape (v: RigidTextVBT; ax: Axis.T; n: CARDINAL): VBT.SizeRange
  RAISES {} =
  VAR sr := TextVBT.T.shape(v, ax, n);
  BEGIN
    sr.hi := sr.pref + 1;
    RETURN sr
  END RigidTextVBTShape;


PROCEDURE LookupShadow(bgName, fgName: TEXT): Shadow.T =
  VAR
    rgb: Color.T;
    bg, fg, light, dark: PaintOp.T;
  BEGIN
    rgb := ColorNameToRGB(bgName);
    bg := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b, mode:=PaintOp.Mode.Accurate, bw:=PaintOp.BW.UseBg);
    rgb := ColorNameToRGB(fgName);
    fg := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b, mode:=PaintOp.Mode.Accurate, bw:=PaintOp.BW.UseFg);
    rgb := ColorNameToRGB("Light" & bgName);
    light := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b, mode:=PaintOp.Mode.Accurate, bw:=PaintOp.BW.UseFg);
    rgb := ColorNameToRGB("Dark" & bgName);
    dark := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b, mode:=PaintOp.Mode.Accurate, bw:=PaintOp.BW.UseFg);
    RETURN Shadow.New(ShadowAmt, bg, fg, light, dark)
  END LookupShadow;

PROCEDURE LookupColors(bgName, fgName: TEXT): PaintOp.ColorQuad =
  VAR
    bg := ColorNameToRGB(bgName);
    bgOp := PaintOp.FromRGB(bg.r, bg.g, bg.b, mode:=PaintOp.Mode.Accurate, bw:=PaintOp.BW.UseBg);
    fg := ColorNameToRGB(fgName);
    fgOp := PaintOp.FromRGB(fg.r, fg.g, fg.b, mode:=PaintOp.Mode.Accurate, bw:=PaintOp.BW.UseFg);
    quad := PaintOp.MakeColorQuad(bgOp, fgOp);
(*
  TYPE 
    ColorQuint = PaintOp.ColorQuad OBJECT
      bgBg: PaintOp.T;
    END;
*)
  BEGIN
(*
     RETURN NEW(ColorQuint, 
          bg := quad.bg, 
          fg := quad.fg, 
          bgFg := quad.bgFg, 
          transparentFg := quad.transparentFg,
          bgBg := PaintOp.Pair(bgOp, bgOp))
*)    
    RETURN quad;
  END LookupColors;    

PROCEDURE ColorNameToRGB (name: TEXT): Color.T =
  (* gross hack here to ensure that the background color matches that of
     FormsVBT. *)
  BEGIN
    IF Text.Equal(name, BackgroundColor) THEN
      RETURN Color.T{0.8, 0.8, 0.8}
    ELSE
      RETURN ColorName.ToRGB(name)
    END
  END ColorNameToRGB;

PROCEDURE LookupFont (family: FontFamily;
                      size  : FontSize;
                      weight: FontWeight;
                      slant : FontSlant   ): Font.T =
  VAR
    base, suffix, name: TEXT;
    style             : FontStyle;
  BEGIN
    IF weight = FontWeight.Normal THEN
      IF slant = FontSlant.Normal THEN
        style := FontStyle.Plain
      ELSE
        style := FontStyle.Slanted
      END
    ELSE
      IF slant = FontSlant.Normal THEN
        style := FontStyle.Bold
      ELSE
        style := FontStyle.BoldSlanted
      END
    END;
    IF family = FontFamily.Normal THEN
      base := NormalFontNames[style];
      suffix := NormalFontSizes[size];
    ELSE
      base := FixedFontNames[style];
      suffix := FixedFontSizes[size];
    END;
    name := base & suffix;
    RETURN Font.FromName(ARRAY OF TEXT{name})
  END LookupFont;


VAR DefaultState: State;

PROCEDURE ChangeColors (VAR s: State; bgFg: PaintOp.ColorQuad) =
  BEGIN
    s.bgFg := bgFg;
  END ChangeColors;

PROCEDURE ChangeFont (VAR s     : State;
                          family: FontFamily;
                          size  : FontSize;
                          weight: FontWeight;
                          slant : FontSlant   ) =
  BEGIN
    s.family := family;
    s.size := size;
    s.weight := weight;
    s.slant := slant;
    s.font := LookupFont(family, size, weight, slant);
  END ChangeFont;

PROCEDURE GetPixmap (name: TEXT; bundle: Bundle.T): Pixmap.T =
  <* FATAL Image.Error, Rsrc.NotFound, Rd.Failure, Thread.Alerted *>
  VAR
    rd := Rsrc.Open(name, Rsrc.BuildPath(bundle));
    pm := Image.Scaled(Image.FromRd(rd));
  BEGIN
    Rd.Close(rd);
    RETURN pm
  END GetPixmap;


(********** initialization **********)

VAR
  RegularShadow  : Shadow.T;

  IsIndexShadow: Shadow.T;
  IsIndexFont  : Font.T;


BEGIN
  EmptyImage := GetPixmap("emptyimage.pbm", ResourceBundle.Get());
  ErrorImage := GetPixmap("errorimage.pbm", ResourceBundle.Get());

  RegularShadow := LookupShadow(BackgroundColor, RegularColor);
  RegularColors := LookupColors(BackgroundColor, RegularColor);
  RegularBgColors := LookupColors(BackgroundColor, BackgroundColor);
  AnchorColors := LookupColors(BackgroundColor, AnchorColor);
  ErrorColors := LookupColors(BackgroundColor, ErrorColor);

  IsIndexShadow := LookupShadow(IsIndexBgColor, RegularColor);
  IsIndexFont :=
    LookupFont(family := FontFamily.Fixed, size := FontSize.Normal,
               weight := FontWeight.Normal, slant := FontSlant.Normal);

  DefaultState.bgFg := RegularColors;
  ChangeFont(
    DefaultState, family := FontFamily.Normal, size := FontSize.Normal,
    weight := FontWeight.Normal, slant := FontSlant.Normal);

END HTMLVBTG.
