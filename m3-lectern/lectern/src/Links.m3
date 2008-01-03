(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* Managing links *)

(* Last modified on Fri Apr 14 14:55:58 PDT 1995 by birrell   *)
MODULE Links EXPORTS Links;

IMPORT AnyEvent, ASCII, Env, File, FileRd, FileWr, Fmt, FormsVBT, FS, FVTypes,
       KeyboardKey, LecternDoc, LecternOCR, ListVBT, OSError, Pathname,
       Rd, TempFiles, Text, TextF, Thread, Time, TypeinVBT, VBT, Wr;

<*FATAL FormsVBT.Error*>
<*FATAL FormsVBT.Unimplemented*>

CONST MaxBookmarks = 10;
  (* Maximum number of bookmarks *)

REVEAL T = Public BRANDED OBJECT
    fv: FormsVBT.T;
    browser: ListVBT.T;
    variant: Variant;
    viewer, path: TEXT := NIL;
    dir: LecternDoc.Dir;
    ocr: LecternOCR.T;
    modified: ARRAY Class OF BOOLEAN;
    showHyper := FALSE;
    class: Class := Class.Bookmark;
    images := ARRAY Class OF Rd.T { NIL, .. };
    mtime := ARRAY Class OF Time.T{ 0.0d0, .. };
    links: ARRAY Class OF REF ARRAY OF Link;
  OVERRIDES
    init := Init;
    update := Update;
    popup := Popup;
    key := Key;
    browserHit := BrowserHit;
    popDown := PopDown;
    appendLink := AppendLink;
  END;


(* *)
(* Read/Write *)
(* *)

PROCEDURE FileName(class: Class): TEXT RAISES { Error } =
  VAR
    home := Env.Get("HOME");
    suffix: TEXT;
  BEGIN
    CASE class OF
    | Class.Bookmark => suffix := ".bookmarks.lect";
    | Class.Diary => suffix := ".diary.lect";
    ELSE
      <*ASSERT FALSE*>
    END;
    IF home = NIL THEN
      RAISE Error("Can't use diary or bookmarks: $HOME not defined");
    END;
    RETURN Pathname.Join(home, suffix, NIL);
  END FileName;

PROCEDURE PutSelPos(wr: Wr.T; READONLY selPos: LecternOCR.SelPos)
                  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    LecternDoc.PutInt4(wr, selPos.page);
    LecternDoc.PutInt4(wr, selPos.word); 
  END PutSelPos;

CONST ThisFile = "";
  (* On-disk encoding of links that refer to the current file. *)

PROCEDURE PutLink(wr: Wr.T; READONLY link: Link; path: TEXT)
                  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    LecternDoc.PutText(wr, link.name);
    IF Text.Equal(link.file, path) THEN
      LecternDoc.PutText(wr, ThisFile)
    ELSE
      LecternDoc.PutText(wr, link.file);
    END;
    LecternDoc.PutInt4(wr, link.origin);
    PutSelPos(wr, link.selStart);
    PutSelPos(wr, link.selEnd);
  END PutLink;

PROCEDURE PutLinks(wr: Wr.T; links: REF ARRAY OF Link; path := ThisFile)
                   RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    LecternDoc.PutInt4(wr, NUMBER(links^));
    FOR i := 0 TO LAST(links^) DO PutLink(wr, links[i], path) END;
  END PutLinks;

PROCEDURE PutOutline(wr: Wr.T;
                     VAR dir: LecternDoc.Dir;
                     path: TEXT;
                     outline: LinkList)
                    RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    dir.outline.start := Wr.Index(wr);
    PutLinks(wr, outline, path);
    dir.outline.length := Wr.Index(wr) - dir.outline.start;
  END PutOutline;

PROCEDURE PutLinksFile(t: T; class: Class) RAISES { Error } =
  VAR
    wr: Wr.T;
    dir := LecternDoc.Dir{
            outline := LecternDoc.Component{},
            original := LecternDoc.Component{},
            pages := NEW(LecternDoc.DirPages, 1),
            attributes := NEW(LecternDoc.Attributes, 0),
            gammas := NEW(LecternDoc.Gammas, 1)
          };
    dirStart: CARDINAL;
    finalName := FileName(class); (* get any error early *)
    tempName := TempFiles.Get(Pathname.Prefix(finalName), ",links-");
  BEGIN
    TRY
      IF t.modified[class] THEN
        wr := FileWr.Open(tempName);
        TempFiles.Note(tempName);
        TRY
          LecternDoc.WriteHeader(wr);
          <*FATAL Rd.Failure*>
          VAR
            c: LecternDoc.Component;
          BEGIN
            Rd.Seek(t.images[class], 0);
            WITH p = dir.pages[0] DO
              c := LecternDoc.CopyRd(t.images[class],
                                     Rd.Length(t.images[class]),wr);
              p[LecternDoc.Class.Thumbnail] := c;
              p[LecternDoc.Class.Small] := c;
              p[LecternDoc.Class.Normal] := c;
              p[LecternDoc.Class.Large] := c;
              p[LecternDoc.Class.Print] := c;
              dir.gammas[0] := 1.0;
            END;
          END;
          (* Write links, but don't encode any as "this file"; that encoding
             isn't honored in ReadLinksFile. *)
          PutOutline(wr, dir, ThisFile, t.links[class]);
          dirStart := Wr.Index(wr);
          LecternDoc.WriteDir(wr, dir);
          LecternDoc.WriteTrailer(wr, dirStart);
        FINALLY
          TRY
            Wr.Close(wr);
          EXCEPT Wr.Failure, Thread.Alerted =>
          END;
          TempFiles.Forget(tempName);
        END;
        FS.Rename(tempName, finalName);
        t.modified[class] := FALSE;
      END;
    EXCEPT
      | Thread.Alerted =>
      | OSError.E =>
          RAISE Error("Can't open diary or bookmark file for writing");
      | Wr.Failure =>
          RAISE Error("Can't write diary or bookmark file");
    END;
  END PutLinksFile;

PROCEDURE ReadSelPos(rd: Rd.T; VAR selPos: LecternOCR.SelPos)
                   RAISES { Rd.Failure, Thread.Alerted, Rd.EndOfFile } =
  BEGIN
    selPos.page := LecternDoc.ReadInt4(rd);
    selPos.word := LecternDoc.ReadInt4(rd);
  END ReadSelPos;

PROCEDURE ReadLink(rd: Rd.T; VAR link: Link)
                   RAISES { Rd.Failure, Thread.Alerted,
                            Rd.EndOfFile, LecternDoc.NotLectern } =
  BEGIN
    link.name := LecternDoc.ReadText(rd);
    link.file := LecternDoc.ReadText(rd);
    link.origin := LecternDoc.ReadInt4(rd);
    ReadSelPos(rd, link.selStart);
    ReadSelPos(rd, link.selEnd);
  END ReadLink;

PROCEDURE ReadLinks(rd: Rd.T): REF ARRAY OF Link
                     RAISES { Rd.Failure, Thread.Alerted,
                              Rd.EndOfFile, LecternDoc.NotLectern } =
  VAR
    links := NEW(REF ARRAY OF Link, LecternDoc.ReadInt4(rd));
  BEGIN
    FOR i := 0 TO LAST(links^) DO ReadLink(rd, links[i]) END;
    RETURN links;
  END ReadLinks;

PROCEDURE ReadOutline(rd: Rd.T;
                      READONLY dir: LecternDoc.Dir;
                      path: TEXT): LinkList
                     RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    fake, result: LinkList;
    index := 0;
  PROCEDURE AddFixedLink(name: TEXT; p: INTEGER) =
    VAR
      selStart := LecternOCR.SelPos{ page := p - dir.origin, word := 0 };
    BEGIN
      fake[index] := Link{name := name,
                           file := ThisFile,
                           origin := dir.origin,
                           selStart := selStart,
                           selEnd := selStart};
      INC(index);
    END AddFixedLink;
  BEGIN
    IF dir.outline.start > 0 THEN
      TRY
        Rd.Seek(rd, dir.outline.start);
        result := ReadLinks(rd);
      EXCEPT LecternDoc.NotLectern =>
        result := NEW(LinkList, 0);
      END;
    ELSE
      fake := NEW(LinkList, 5);
      AddFixedLink("Beginning", 0);
      WITH p = dir.contents DO
        IF p >= 0 THEN AddFixedLink("Contents", p) END;
      END;
      WITH p = dir.origin+1 DO
        IF p >= 0 THEN AddFixedLink("Page 1", p) END;
      END;
      WITH p = dir.index DO
        IF p >= 0 THEN AddFixedLink("Index", p) END;
      END;
      AddFixedLink("End", LAST(dir.pages^));
      result := NEW(LinkList, index);
      result^ := SUBARRAY(fake^, 0, index);
    END;
    FOR i := 0 TO LAST(result^) DO
      IF Text.Equal(result[i].file, ThisFile) THEN result[i].file := path END;
    END;
    RETURN result;
  END ReadOutline;

PROCEDURE ReadLinksFile(t: T; class: Class; h: File.T)
                        RAISES { Error, OSError.E } =
    (* On normal exit or Error, "h" has been closed; on OSError.E it hasn't. *)
  VAR
    rd: Rd.T;
    dir: LecternDoc.Dir;
  BEGIN
    rd := NEW(FileRd.T).init(h);
    TRY
      TRY
        TRY
          LecternDoc.ReadDir(rd, dir);
          IF dir.outline.start > 0 THEN
            Rd.Seek(rd, dir.outline.start);
            t.links[class] := ReadLinks(rd);
          ELSE
            t.links[class] := NEW(LinkList, 0);
          END;
        EXCEPT LecternDoc.NotLectern =>
          (*TEMP: old format*)
          Rd.Seek(rd, 0);
          t.links[class] := ReadLinks(rd);
        END;
      FINALLY
        TRY
          Rd.Close(rd);
        EXCEPT Rd.Failure =>
        END;
      END;
    EXCEPT
    | Thread.Alerted =>
    | Rd.Failure => RAISE Error("Couldn't read diary or bookmark file");
    | Rd.EndOfFile, LecternDoc.NotLectern =>
        RAISE Error("Malformed diary or bookmark file");
    END;
  END ReadLinksFile;

PROCEDURE ReadFile(t: T; class: Class) =
  (* Read the links file if it's more recent than last mtime; and if so,
     report the update to the client *)
  (* LL = VBT.mu + t *)
  VAR
    h: File.T;
    newMTime: Time.T;
  PROCEDURE Close() = (* This is a very silly piece of code to need. *)
    BEGIN
      TRY
        h.close();
      EXCEPT OSError.E => (* what do they expect me to do here? *)
      END;
    END Close;
  BEGIN
    TRY
      h := FS.OpenFileReadonly(FileName(class));
    EXCEPT OSError.E, Error => RETURN;
    END;
    TRY
      newMTime := h.status().modificationTime;
    EXCEPT OSError.E => Close(); RETURN;
    END;
    IF newMTime <= t.mtime[class] THEN Close(); RETURN; END;
    TRY
      ReadLinksFile(t, class, h);
    EXCEPT
    | Error => RETURN;
    | OSError.E => Close(); RETURN;
    END;
    t.mtime[class] := newMTime;
    ReportUpdate(t, class);
  END ReadFile;


(* *)
(* Various subroutines *)
(* *)

PROCEDURE BeginsWith(a, b: TEXT): BOOLEAN =
    (* Return TRUE iff "a" begins with "b" *)
  VAR
    len := Text.Length(b);
  BEGIN
    IF Text.Length(a) < len THEN RETURN FALSE END;
    FOR i := 0 TO len-1 DO
      IF ASCII.Lower[a[i]] # ASCII.Lower[b[i]] THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END BeginsWith;

PROCEDURE QuietPutText(fv: FormsVBT.T; key: TEXT; value: TEXT) =
    (* Like FormsVBT.PutText, but does nothing if the text is already there *)
    (* LL = VBT.mu *)
  BEGIN
    IF NOT Text.Equal(value, FormsVBT.GetText(fv, key)) THEN
      FormsVBT.PutText(fv, key, value);
    END;
  END QuietPutText;

PROCEDURE NewLink(path: TEXT;
                  READONLY dir: LecternDoc.Dir;
                  ocr: LecternOCR.T;
                  name := ""): Link =
    (* Return a link defined by the current position and selection *)
    (* LL = VBT.mu *)
  VAR
    selStart, selEnd: LecternOCR.SelPos;
  BEGIN
    IF ocr = NIL OR path = NIL THEN RETURN NoLink END;
    ocr.getSelection(selStart, selEnd);
    RETURN Link{ name := name,
                 file := path,
                 origin := dir.origin,
                 selStart := selStart,
                 selEnd := selEnd };
  END NewLink;

PROCEDURE SelectStartsWith(t: T; new: TEXT): INTEGER =
    (* Return element number of first element beginning with "new", or -1. *)
    (* LL = VBT.mu *)
  VAR
    links := t.links[t.class];
  BEGIN
    FOR i := 0 TO LAST(links^) DO
      IF BeginsWith(links[i].name, new) THEN
        RETURN i;
      END;
    END;
    RETURN -1;
  END SelectStartsWith;

PROCEDURE Select(t: T; index: INTEGER) =
    (* Make the selected item be "index".  If this changes the
       selection, make it visible. *)
    (* LL = VBT.mu *)
  VAR
    selected: INTEGER;
  BEGIN
    IF t.browser.getFirstSelected(selected) AND selected = index THEN
      (* nothing changed, so do nothing *)
    ELSE
      t.browser.selectOnly(index);
      IF index >= 0 THEN t.browser.scrollToShow(index) END;
    END;
  END Select;

PROCEDURE DefaultLinkName(READONLY link: Link): TEXT =
  VAR
    imageName, wordName: TEXT;
  BEGIN
    IF link = NoLink THEN
      RETURN ""
    ELSE
      WITH p = link.selStart.page DO
        IF p > 0 THEN
          imageName := ", page " & Fmt.Int(p);
        ELSE
          imageName := ", image " & Fmt.Int(p+link.origin+1);
        END;
      END;
      WITH w = link.selStart.word DO
        IF w < 0 OR link.selStart = link.selEnd THEN
          wordName := "";
        ELSE
          wordName := ", word " & Fmt.Int(w+1);
        END;
      END;
      RETURN Pathname.Last(link.file) & imageName & wordName;
    END;
  END DefaultLinkName;

PROCEDURE Fixup(t: T) =
    (* Adjust the UI to match the data.  Browser already has correct entries.
       If "LinkName" is empty, leave selection alone, otherwise select first
       match.  Ensure appropriate link value is displayed, and appropriate
        value is in "LinkRename". *)
    (* LL = VBT.mu *)
  VAR
    selected: INTEGER;
    links := t.links[t.class];
  PROCEDURE ShowLink(link: Link) =
    BEGIN
      QuietPutText(t.fv, "Value", DefaultLinkName(link));
    END ShowLink;
  BEGIN
    WITH
      linkName = FormsVBT.GetText(t.fv, "LinkName") DO
      IF NOT Text.Equal(linkName, "") THEN
        Select(t, SelectStartsWith(t, linkName));
      END;
    END;
    IF NOT t.browser.getFirstSelected(selected) THEN selected := -1 END;
    CASE t.variant OF
    | Variant.Define =>
        ShowLink(NewLink(t.path, t.dir, t.ocr));
    | Variant.Edit, Variant.Jump =>
        IF selected >= 0 THEN
          QuietPutText(t.fv, "LinkRename", links[selected].name);
          ShowLink(links[selected]);
        ELSE
          QuietPutText(t.fv, "LinkRename", "");
          ShowLink(NoLink);
        END;
    END;
  END Fixup;

PROCEDURE RespondToTypein(t: T) =
    (* Adjust state to respond to new text in the type-in fields.  Uses
       "LinkRename" to modify name of selected link and calls Fixup. *)
    (* LL = VBT.mu *)
  VAR
    selected: INTEGER;
    links := t.links[t.class];
  BEGIN
    IF t.variant = Variant.Edit THEN
      WITH new = FormsVBT.GetText(t.fv, "LinkRename") DO
        IF t.browser.getFirstSelected(selected) THEN
          IF NOT Text.Equal(new, links[selected].name) THEN
            links[selected].name := new;
            t.browser.setValue(selected, new);
            FormsVBT.PutText(t.fv, "LinkName", "");
            t.modified[t.class] := TRUE;
          END;
        END;
      END;
    END;
    Fixup(t);
  END RespondToTypein;

PROCEDURE RespondToSelection(t: T) =
    (* Adjust state to respond to a change of the selected link.  Modifies
        "LinkName" to be consistent with the selection and calls Fixup. *)
    (* LL = VBT.mu *)
  BEGIN
    FormsVBT.PutText(t.fv, "LinkName", "");
    Fixup(t);
  END RespondToSelection;

PROCEDURE ReportUpdate(t: T; class: Class) =
    (* Call client with a copy of our list *)
    (* LL = VBT.mu *)
  VAR
    links := t.links[class];
    copy := NEW(LinkList, NUMBER(links^));
  BEGIN
    copy^ := links^;
    t.update(class, copy);
  END ReportUpdate;

PROCEDURE LoadBrowser(t: T; class: Class) =
    (* Ensure the browser contains the links of t.class *)
    (* LL = VBT.mu *)
  VAR
    oldCount := t.browser.count();
    links := t.links[class];
    newCount := NUMBER(links^);
    className: TEXT;
  BEGIN
    CASE class OF
    | Class.Bookmark => className := "ClassBookmark";
    | Class.Diary => className := "ClassDiary";
    | Class.Outline => className := "ClassOutline";
    | Class.Hyper => className := "ClassHyper";
    END;
    t.class := class;
    FormsVBT.MakeSelected(t.fv, className);
    IF newCount > oldCount THEN
      t.browser.insertCells(0, newCount-oldCount);
    ELSIF newCount < oldCount THEN
      t.browser.removeCells(0, oldCount-newCount);
    END;
    FOR i := 0 TO newCount-1 DO
      t.browser.setValue(i, links[i].name);
    END;
    t.browser.selectOnly(0);
    Fixup(t);
  END LoadBrowser;

PROCEDURE Remove(t: T; index: INTEGER; dest: Class): Link =
    (* Remove and return given entry; doesn't call Fixup. *)
    (* LL = VBT.mu *)
  VAR
    old := t.links[dest];
    oldCount := NUMBER(old^);
    result := NoLink;
  BEGIN
    IF index >= 0 AND index < oldCount THEN
      result := old[index];
      t.links[dest] := NEW(REF ARRAY OF Link, oldCount-1);
      SUBARRAY(t.links[dest]^, 0, index) := SUBARRAY(old^, 0, index);
      IF index < oldCount-1 THEN
        SUBARRAY(t.links[dest]^, index, oldCount-index-1) :=
            SUBARRAY(old^, index+1, oldCount-index-1);
      END;
      IF dest = t.class THEN
        t.browser.removeCells(index, 1);
      END;
      t.modified[dest] := TRUE;
    END;
    RETURN result;
  END Remove;

PROCEDURE Add(t: T; link: Link; dest: Class) =
    (* Add given link at end of given class and call Fixup *)
  VAR
    old := t.links[dest];
    oldCount := NUMBER(old^);
    index := NUMBER(old^);
    victim := -1;
  BEGIN
    index := MAX(0, MIN(index, oldCount));
    t.links[dest] := NEW(REF ARRAY OF Link, oldCount+1);
    SUBARRAY(t.links[dest]^, 0, index) := SUBARRAY(old^, 0, index);
    t.links[dest][index] := link;
    IF index < oldCount THEN
      SUBARRAY(t.links[dest]^, index+1, oldCount-index) :=
          SUBARRAY(old^, index, oldCount-index);
    END;
    IF dest = t.class THEN
      t.browser.insertCells(index, 1);
      t.browser.setValue(index, link.name);
      Select(t, index);
    END;
    (* Remove a victim if too many bookmarks, or identical to existing name *)
    IF dest = Class.Bookmark AND oldCount >= MaxBookmarks THEN victim := 0 END;
    FOR i := 0 TO oldCount-1 DO
      IF Text.Equal(old[i].name, link.name) THEN victim := i; EXIT END;
    END;
    IF victim >= 0 THEN EVAL Remove(t, victim, dest) END;
    IF dest = t.class THEN RespondToSelection(t) END;
    t.modified[dest] := TRUE;
  END Add;

PROCEDURE Move(t: T; towardsZero: BOOLEAN) =
    (* Move selected entry up or down *)
    (* Could use Add(Remove(..)..), but this is cheaper, and the other
       seems to provoke a repainting bug in ListVBT, that I don't choose to
       fix right now. *)
    (* LL = VBT.mu *)
  VAR
    links := t.links[t.class];
    selected, n: INTEGER;
    nLink: Link;
  BEGIN
    IF t.browser.getFirstSelected(selected) THEN
      IF towardsZero AND selected = 0 THEN RETURN END;
      IF NOT towardsZero AND selected = LAST(links^) THEN RETURN END;
      n := selected - ORD(towardsZero);
      nLink := links[n];
      links[n] := links[n+1];
      links[n+1] := nLink;
      t.browser.setValue(n, links[n].name);
      t.browser.setValue(n+1, links[n+1].name);
      t.modified[t.class] := TRUE;
      Select(t, n + 1 - ORD(towardsZero));
      RespondToSelection(t);
    END;
  END Move;

PROCEDURE Convert(t: T; dest: Class) =
    (* Convert selected entry to given class *)
    (* LL = VBT.mu *)
  VAR
    selected: INTEGER;
  BEGIN
    IF t.browser.getFirstSelected(selected) THEN
      Add(t, Remove(t, selected, t.class), dest);
      IF dest # t.class THEN
        IF selected <= LAST(t.links[t.class]^) THEN
          t.browser.selectOnly(selected);
        ELSE
          t.browser.selectOnly(selected-1);
        END;
        RespondToSelection(t);
      END;
    END;
  END Convert;

PROCEDURE Delete(t: T) =
    (* Delete selected entry *)
    (* LL = VBT.mu *)
  VAR
    selected: INTEGER;
  BEGIN
    IF t.browser.getFirstSelected(selected) THEN
      EVAL Remove(t, selected, t.class);
      IF selected <= LAST(t.links[t.class]^) THEN
        t.browser.selectOnly(selected);
      ELSE
        t.browser.selectOnly(selected-1);
      END;
      RespondToSelection(t);
    END;
  END Delete;


(* *)
(* Watcher thread *)
(* *)

TYPE Watcher = Thread.Closure OBJECT
    t: T;
  OVERRIDES
    apply := WatcherApply;
  END;

PROCEDURE WatcherApply(self: Watcher): REFANY =
  BEGIN
    LOOP
      Thread.Pause(10.0d0);
      LOCK VBT.mu DO
        LOCK self.t DO
          ReadFile(self.t, Class.Diary);
          ReadFile(self.t, Class.Bookmark);
        END;
      END;
    END;
  END WatcherApply;


(* *)
(* Events handled internally *)
(* *)

TYPE Op = {
    ClassBookmark,
    ClassDiary,
    ClassOutline,
    ClassHyper,
    MoveUp,
    MoveDown,
    ToBookmark,
    ToDiary,
    ToOutline,
    ToHyper,
    Delete
  };

TYPE FVClosure = FormsVBT.Closure OBJECT
  t: T;
  op: Op;
  OVERRIDES
    apply := FVApply;
  END;

PROCEDURE FVApply(self: FVClosure;
                  <*UNUSED*>fv: FormsVBT.T;
                  <*UNUSED*>name: TEXT;
                  <*UNUSED*>time: VBT.TimeStamp) =
  (* Perform an action *)
  (* LL = VBT.mu *)
  BEGIN
    CASE self.op OF
    | Op.ClassBookmark => LoadBrowser(self.t, Class.Bookmark);
    | Op.ClassDiary => LoadBrowser(self.t, Class.Diary);
    | Op.ClassOutline => LoadBrowser(self.t, Class.Outline);
    | Op.ClassHyper => LoadBrowser(self.t, Class.Hyper);
    | Op.MoveUp => Move(self.t, TRUE);
    | Op.MoveDown => Move(self.t, FALSE);
    | Op.ToBookmark => Convert(self.t, Class.Bookmark);
    | Op.ToDiary => Convert(self.t, Class.Diary);
    | Op.ToOutline => Convert(self.t, Class.Outline);
    | Op.ToHyper => Convert(self.t, Class.Hyper);
    | Op.Delete => Delete(self.t);
    END;
  END FVApply;

PROCEDURE Attach(t: T; name: TEXT; op: Op) =
    (* Attach an event to event "name", invoking "op" *)
  BEGIN
    FormsVBT.Attach(t.fv, name, NEW(FVClosure, t := t, op := op))
  END Attach;


(* *)
(* Exported methods *)
(* *)

PROCEDURE Init(t: T; fv: FormsVBT.T; bookmarkImage, diaryImage: Rd.T): T =
  BEGIN
    LOCK t DO
      t.fv := fv;
      t.images[Class.Bookmark] := bookmarkImage;
      t.images[Class.Diary] := diaryImage;
      t.variant := Variant.Jump; (* benign *)
      t.browser := FormsVBT.GetVBT(t.fv, "Browser");
      FOR c := FIRST(Class) TO LAST(Class) DO
        t.links[c] := NEW(REF ARRAY OF Link, 0);
      END;
      t.class := Class.Diary;
      t.modified := ARRAY Class OF BOOLEAN{ FALSE, .. };
      Attach(t, "ClassBookmark", Op.ClassBookmark);
      Attach(t, "ClassDiary", Op.ClassDiary);
      Attach(t, "ClassOutline", Op.ClassOutline);
      Attach(t, "ClassHyper", Op.ClassHyper);
      Attach(t, "MoveUp", Op.MoveUp);
      Attach(t, "MoveDown", Op.MoveDown);
      Attach(t, "ToBookmark", Op.ToBookmark);
      Attach(t, "ToDiary", Op.ToDiary);
      Attach(t, "ToOutline", Op.ToOutline);
      Attach(t, "ToHyper", Op.ToHyper);
      Attach(t, "Delete", Op.Delete);
      (* Establish empty diary and bookmark, in case file read fails *)
      ReportUpdate(t, Class.Diary);
      ReportUpdate(t, Class.Bookmark);
      ReadFile(t, Class.Diary);
      ReadFile(t, Class.Bookmark);
    END;
    EVAL Thread.Fork(NEW(Watcher, t := t));
    RETURN t;
  END Init;

PROCEDURE Update(<*UNUSED*>t: T;
                 <*UNUSED*>class: Class;
                 <*UNUSED*>list: LinkList) =
  (* Default does nothing *)
  (* LL = VBT.mu *)
  BEGIN
  END Update;

PROCEDURE Popup(t: T;
                variant: Variant;
                class: Class;
                viewer,  path: TEXT;
                READONLY dir: LecternDoc.Dir;
                outline: LinkList;
                ocr: LecternOCR.T;
                <*UNUSED*>time: VBT.TimeStamp): FormsVBT.T RAISES { Error } =
  BEGIN
    LOCK t DO
      IF t.viewer # NIL THEN
        RAISE Error("You are already manipulating links in the viewer \"" &
                    t.viewer & "\"");
      END;
      CASE variant OF
      | Variant.Define, Variant.Edit =>
        IF path # NIL THEN
          IF NOT Pathname.Absolute(path) THEN
            <* ASSERT FALSE *> (* should have been done by caller *)
          END;
          TRY
            WITH
              absPathArcs = Pathname.Decompose(path),
              TempPath = "tmp_mnt" (* SRC-specific bad path names *) DO
              IF Text.Equal(absPathArcs.get(1), TempPath) THEN
                RAISE Error("Document's pathname must not start with \"" &
                            TempPath & "\"");
              END;
            END;
          EXCEPT Pathname.Invalid => <*ASSERT FALSE*> (* caller's job *)
          END;
        END;
      | Variant.Jump =>
        (* any sort of pathname is OK for jumping *)
      END;
      t.viewer := viewer;
      t.variant := variant;
      t.path := path;
      t.dir := dir;
      t.ocr := ocr;
    END; (*LOCK t *)
    LOCK VBT.mu DO
      t.links[Class.Outline] := outline;
      IF t.class # class OR t.class = Class.Outline OR
                            t.class = Class.Hyper THEN
        LoadBrowser(t, class);
      END;
      CASE variant OF
      | Variant.Define =>
          FormsVBT.PutInteger(t.fv, "TSplit1", 0);
          FormsVBT.PutInteger(t.fv, "TSplit2", 0);
          FormsVBT.PutInteger(t.fv, "TSplit3", 0);
          FormsVBT.MakeVanish(t.fv, "ClassHyper");
          t.showHyper := FALSE;
      | Variant.Edit =>
          FormsVBT.PutInteger(t.fv, "TSplit1", 1);
          FormsVBT.PutInteger(t.fv, "TSplit2", 1);
          FormsVBT.PutInteger(t.fv, "TSplit3", 1);
          IF t.path = NIL THEN
            FormsVBT.MakeDormant(t.fv, "ToOutline");
          ELSE
            FormsVBT.MakeActive(t.fv, "ToOutline");
          END;
(*TEMP*)  FormsVBT.MakeVanish(t.fv, "ClassHyper");
(*TEMP*)  t.showHyper := FALSE; (* TRUE iff selection is suitable *)
(*TEMP*)  FormsVBT.MakeVanish(t.fv, "ToHyper");
      | Variant.Jump =>
          FormsVBT.PutInteger(t.fv, "TSplit1", 2);
          FormsVBT.PutInteger(t.fv, "TSplit2", 2);
          FormsVBT.PutInteger(t.fv, "TSplit3", 2);
          FormsVBT.MakeVanish(t.fv, "ClassHyper");
          t.showHyper := FALSE;
      END;
      IF t.path = NIL THEN
        FormsVBT.MakeDormant(t.fv, "ClassOutline");
      ELSE
        FormsVBT.MakeActive(t.fv, "ClassOutline");
      END;
      WITH
        from = NARROW(FormsVBT.GetVBT(t.fv, "LinkName"), TypeinVBT.T),
        to = NARROW(FormsVBT.GetVBT(t.fv, "LinkRename"), TypeinVBT.T) DO
        IF variant = Variant.Edit THEN
          from.tabNext := to;
        ELSE
          from.tabNext := from;
        END;
        to.tabNext := from;
      END;
      Fixup(t);
    END;
    RETURN t.fv;
  END Popup;

PROCEDURE BrowserHit(t: T): BOOLEAN =
    (* LL = VBT.mu *)
  VAR
    event := FormsVBT.GetTheEvent(t.fv);
  BEGIN
    RespondToSelection(t);
    RETURN t.variant = Variant.Jump AND
           ISTYPE(event, AnyEvent.Mouse) AND
           NARROW(event, AnyEvent.Mouse).mouse.clickCount = 3;
  END BrowserHit;

PROCEDURE Key(t: T; v: FVTypes.FVTypeIn; READONLY cd: VBT.KeyRec) =
  BEGIN
    IF cd.wentDown THEN
      CASE cd.whatChanged OF
        | KeyboardKey.Left =>
          CASE t.class OF
          | Class.Bookmark =>
            IF t.showHyper THEN
              LoadBrowser(t, Class.Hyper);
            ELSIF t.path # NIL THEN
              LoadBrowser(t, Class.Outline);
            ELSE
              LoadBrowser(t, Class.Diary);
            END;
          | Class.Diary =>
            LoadBrowser(t, Class.Bookmark);
          | Class.Outline =>
            LoadBrowser(t, Class.Diary);
          | Class.Hyper =>
            LoadBrowser(t, Class.Outline);
          END;
        | KeyboardKey.Right =>
          CASE t.class OF
          | Class.Bookmark =>
            LoadBrowser(t, Class.Diary);
          | Class.Diary =>
            IF t.path # NIL THEN
              LoadBrowser(t, Class.Outline);
            ELSE
              LoadBrowser(t, Class.Bookmark);
            END;
          | Class.Outline =>
            IF t.showHyper THEN
              LoadBrowser(t, Class.Hyper);
            ELSE
              LoadBrowser(t, Class.Bookmark);
            END;
          | Class.Hyper =>
            LoadBrowser(t, Class.Bookmark);
          END;
        | KeyboardKey.Down =>
          VAR
            selected: INTEGER;
          BEGIN
            IF t.browser.getFirstSelected(selected) THEN
              IF selected < LAST(t.links[t.class]^) THEN
                Select(t, selected+1);
              END;
            ELSE
              Select(t, 0);
            END;
            RespondToSelection(t);
          END;
        | KeyboardKey.Up =>
          VAR
            selected: INTEGER;
          BEGIN
            IF t.browser.getFirstSelected(selected) THEN
              IF selected > 0 THEN Select(t, selected-1) END;
            ELSE
              Select(t, t.browser.count()-1);
            END;
            RespondToSelection(t);
          END;
      ELSE
        FVTypes.FVTypeIn.key(v, cd); RespondToTypein(t);
      END;
    ELSE
      FVTypes.FVTypeIn.key(v, cd); RespondToTypein(t);
    END;
  END Key;

PROCEDURE PopDown(t: T; yes: BOOLEAN): Link RAISES { Error } =
  VAR
    result := NoLink;
  BEGIN
    LOCK VBT.mu DO
      LOCK t DO
        IF yes THEN
          CASE t.variant OF
          | Variant.Define =>
              Add(t, NewLink(t.path, t.dir, t.ocr,
                             FormsVBT.GetText(t.fv, "LinkName")), t.class);
          | Variant.Edit =>
          | Variant.Jump =>
              VAR
                selected: INTEGER;
              BEGIN
                IF t.browser.getFirstSelected(selected) THEN
                  result := t.links[t.class][selected];
                  IF t.path # NIL AND Text.Equal(result.file, t.path) THEN
                    result.file := NIL;
                  END;
                END;
              END;
          END;
        END;
      END;
      t.viewer := NIL;
      FOR c := FIRST(Class) TO LAST(Class) DO
        IF t.modified[c] THEN ReportUpdate(t, c) END;
      END;
      IF t.modified[Class.Outline] OR t.modified[Class.Hyper] THEN
        t.modified[Class.Outline] := FALSE;
        t.modified[Class.Hyper] := FALSE;
      END;
      IF t.modified[Class.Diary] THEN
        PutLinksFile(t, Class.Diary);
      END;
      IF t.modified[Class.Bookmark] THEN
        PutLinksFile(t, Class.Bookmark);
      END;
    END;
    RETURN result;
  END PopDown;

PROCEDURE LinkToHere(path: TEXT;
                     READONLY dir: LecternDoc.Dir;
                     ocr: LecternOCR.T;
                     page: INTEGER): Link =
    (* LL = VBT.mu *)
  VAR
    link: Link;
  BEGIN
    IF ocr = NIL OR path = NIL THEN RETURN NoLink END;
    link := NewLink(path, dir, ocr, "");
    IF link.selStart = LecternOCR.NoSelPos OR link.selStart.page # page THEN
      link.selStart.page := page;
      link.selStart.word := 0;
      link.selEnd := link.selStart;
    END;
    link.name := DefaultLinkName(link);
    RETURN link;
  END LinkToHere;

PROCEDURE AppendLink(t: T;
                     class: Class;
                     path: TEXT;
                     READONLY dir: LecternDoc.Dir;
                     ocr: LecternOCR.T;
                     page: INTEGER) =
  VAR
    link: Link;
  BEGIN
    IF path # NIL THEN
      TRY
        LOCK VBT.mu DO
          link := LinkToHere(path, dir, ocr, page);
          Add(t, link, class);
          ReportUpdate(t, class);
          IF class = Class.Diary OR class = Class.Bookmark THEN
            PutLinksFile(t, class);
          END;
        END;
      EXCEPT Error =>
      END;
    END;
  END AppendLink;

BEGIN

END Links.
