(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:41 PDT 1996 by mhb       *)

MODULE DeckVBT;

IMPORT DocVBT, FormsVBT, MyBundle, Rsrc, Split, TSplit, VBT;
IMPORT AnyEvent, FreeDocVBT, WorkspaceVBT, WSObjectVBT, Fmt;
IMPORT TextureVBT, Pixmap, Text, FVTypes, SourceVBT, ListVBT;
IMPORT ColorName, Color, Random, RefList;

<* FATAL ANY *>

REVEAL
  T = WSObjectVBT.T BRANDED OBJECT
    permanent := FALSE;
    numDocs := 0;
    title: TEXT;
    freeDocs: FreeDocVBT.T := NIL;
  OVERRIDES
    realize := Realize;
    addDoc := WSObjectAddDoc;
    remDoc := RemDoc;
    replaceDoc := ReplaceDoc;
  END;

TYPE 
  Source = FVTypes.FVSource OBJECT
    deck: T;
  OVERRIDES
    hit := Hit;
  END;

PROCEDURE New(title: TEXT; permanent := FALSE) : T =
VAR
  s := NEW(T);
  path := Rsrc.BuildPath("$DeckScapePATH", MyBundle.Get());
  merge := NEW(FormsVBT.Closure, apply := Merge);
  open := NEW(FormsVBT.Closure, apply := Open);
  home := NEW(FormsVBT.Closure, apply := Home);
  popRename := NEW(FormsVBT.Closure, apply := PopRename);
  rename := NEW(FormsVBT.Closure, apply := Rename);
  popTitles := NEW(FormsVBT.Closure, apply := PopTitles);
  titles := NEW(FormsVBT.Closure, apply := Titles);
  selectTitle := NEW(FormsVBT.Closure, apply := SelectTitle);
  gather := NEW(FormsVBT.Closure, apply := Gather);
  delete := NEW(FormsVBT.Closure, apply := Delete);
  bottom := NEW(FormsVBT.Closure, apply := Bottom);
  prev := NEW(FormsVBT.Closure, apply := Prev);
  next := NEW(FormsVBT.Closure, apply := Next);
  top := NEW(FormsVBT.Closure, apply := Top);
  duplicate := NEW(FormsVBT.Closure, apply := Duplicate);
  expand := NEW(FormsVBT.Closure, apply := Expand);
  reload := NEW(FormsVBT.Closure, apply := Reload);
  iconize := NEW(FormsVBT.Closure, apply := IconizeCB);
  gray := TextureVBT.New(txt := Pixmap.Gray);
  tSplit: TSplit.T;
  color := colorPicker.pick();
  target: VBT.T;
BEGIN
  s.title := title;
  EVAL FormsVBT.T.initFromRsrc(s, "Deck.fv", path, TRUE);
  tSplit := FormsVBT.GetVBT(s, "tSplit");
  Split.Insert(tSplit, NIL, gray);
  TSplit.SetCurrent(tSplit, gray);
  FormsVBT.PutText(s, "deckName", title);
  FormsVBT.Attach(s, "urlTypein", open);
  FormsVBT.Attach(s, "urlButton", open);
  FormsVBT.Attach(s, "urlHomeButton", home);
  FormsVBT.Attach(s, "popRenameButton", popRename);
  FormsVBT.Attach(s, "deleteButton", delete);
  FormsVBT.Attach(s, "mergeSource", merge);
  FormsVBT.Attach(s, "popTitlesButton", popTitles);
  FormsVBT.Attach(s, "titlesButton", titles);
  FormsVBT.Attach(s, "titlesBrowser", selectTitle);
  FormsVBT.Attach(s, "titlesNumeric", selectTitle);
  FormsVBT.Attach(s, "renameTypein", rename);
  FormsVBT.Attach(s, "renameButton", rename);
  FormsVBT.Attach(s, "gatherButton", gather);
  FormsVBT.Attach(s, "bottomButton", bottom);
  FormsVBT.Attach(s, "prevButton", prev);
  FormsVBT.Attach(s, "nextButton", next);
  FormsVBT.Attach(s, "topButton", top);
  FormsVBT.Attach(s, "iconize", iconize);
  FormsVBT.MakeDormant(s, "bottomButton");
  FormsVBT.MakeDormant(s, "prevButton");
  FormsVBT.MakeDormant(s, "nextButton");
  FormsVBT.MakeDormant(s, "topButton");
  FormsVBT.Attach(s, "duplicateDocButton", duplicate);
  FormsVBT.Attach(s, "expandDocButton", expand);
  FormsVBT.Attach(s, "reloadDocButton", reload);
  FormsVBT.PutColorProperty(s, "deckName", "BgColor", color);
  FormsVBT.PutColorProperty(s, "titlesDlgBanner", "BgColor", color);
  FormsVBT.PutColorProperty(s, "urlDlgBanner", "BgColor", color);
  FormsVBT.PutColorProperty(s, "renameDlgBanner", "BgColor", color);
  target := FormsVBT.GetVBT(s, "target");
  SourceVBT.BeTarget (target, SourceVBT.NewSwapTarget());
  FormsVBT.PutText(s, "urlTypein", DocVBT.DefaultHomeURL);
  SetPermanent(s, permanent);
  UpdateLocation(s);
  RETURN s;
END New;

PROCEDURE Realize (deck: T; type, name: TEXT) : VBT.T
  RAISES {FormsVBT.Error} =
  BEGIN
    IF Text.Equal (name, "target") THEN 
      RETURN NEW (Target, deck := deck)
    ELSIF Text.Equal (name, "mergeSource") THEN
      RETURN NEW (Source, deck := deck) 
    ELSE
      RETURN FormsVBT.T.realize (deck, type, name)
    END;
  END Realize;

PROCEDURE GetTarget (deck: T): Target =VAR
  BEGIN
    RETURN FormsVBT.GetVBT(deck, "target")
  END GetTarget;

PROCEDURE GetTitle(s: T): TEXT =
BEGIN
  RETURN s.title;
END GetTitle;

PROCEDURE GetBannerColor(s: T): Color.T =
BEGIN
  RETURN FormsVBT.GetColorProperty(s, "deckName", "BgColor")
END GetBannerColor;

PROCEDURE SetTitle(s: T; title: TEXT) =
BEGIN
  s.title := title;
  FormsVBT.PutText(s, "deckName", title);
  (* if this deck is in a workspace, tell the workspace *)
  WITH ws = s.getWorkspace() DO
    IF ws # NIL THEN WorkspaceVBT.RenamedDeck (ws, s) END
  END;
  (* look at all the free docs, and change their titles: *)
  VAR freeDoc := s.freeDocs; BEGIN
    WHILE freeDoc # NIL DO
      (* if this freedoc is in a workspace, tell the workspace *)
      WITH ws = freeDoc.getWorkspace() DO
        IF ws # NIL THEN WorkspaceVBT.RenamedFreeDoc (ws, freeDoc) END
      END; 
      FreeDocVBT.SetTitle(freeDoc, title & " [DOC]");
      freeDoc := freeDoc.next;
    END;
  END;
END SetTitle;

PROCEDURE SetPermanent (deck: T; permanent: BOOLEAN) =
  BEGIN
    deck.permanent := permanent;
    IF permanent THEN
      FormsVBT.MakeDormant(deck, "popRenameButton");
      FormsVBT.MakeDormant(deck, "deleteButton");
      FormsVBT.MakeDormant(deck, "mergeSource");
    ELSE
      FormsVBT.MakeActive(deck, "popRenameButton");
      FormsVBT.MakeActive(deck, "deleteButton");
      FormsVBT.MakeActive(deck, "mergeSource");
    END
  END SetPermanent;

PROCEDURE GetPermanent (deck: T): BOOLEAN =
  BEGIN
    RETURN deck.permanent;
  END GetPermanent;

PROCEDURE IndexOfDoc(s: T; doc: DocVBT.T): INTEGER =
VAR
  tSplit: TSplit.T := FormsVBT.GetVBT(s, "tSplit");
BEGIN
  RETURN Split.Index (tSplit, doc)
END IndexOfDoc;

PROCEDURE GetTopDoc(s: T) : DocVBT.T =
VAR
  tSplit: TSplit.T := FormsVBT.GetVBT(s, "tSplit");
BEGIN
  IF s.numDocs > 0 THEN RETURN TSplit.GetCurrent(tSplit);
  ELSE RETURN NIL; END;
END GetTopDoc;

PROCEDURE SetTopDoc(s: T; docNum: INTEGER) =
VAR
  tSplit: TSplit.T := FormsVBT.GetVBT(s, "tSplit");
  doc: DocVBT.T := Split.Nth(tSplit, docNum);
BEGIN
  TSplit.SetCurrent(tSplit, doc);
  UpdateLocation(s);
END SetTopDoc;

PROCEDURE GetFreeDocs(deck: T): WSObjectVBT.T =
BEGIN
  RETURN deck.freeDocs;
END GetFreeDocs;

PROCEDURE UpdateLocation(s: T) =
VAR
  tSplit: TSplit.T;
  doc: DocVBT.T;
  index: INTEGER;
  text: TEXT;
BEGIN
  IF s.numDocs # 0 THEN
    tSplit := FormsVBT.GetVBT(s, "tSplit");
    doc := GetTopDoc(s);
    index := Split.Index(tSplit, doc);
    text := Fmt.Int(index + 1) & "/" & Fmt.Int(s.numDocs);
  ELSE
    index := 0;
    text := "0/0";
  END;
  FormsVBT.PutText(s, "locText", text);
  IF index = 0 THEN
    FormsVBT.MakeDormant(s, "bottomButton");
  ELSE
    FormsVBT.MakeActive(s, "bottomButton");
  END;
  IF index >= s.numDocs - 1 THEN
    FormsVBT.MakeDormant(s, "topButton");
  ELSE
    FormsVBT.MakeActive(s, "topButton");
  END;
END UpdateLocation;

PROCEDURE WSObjectAddDoc(s: T; doc: DocVBT.T) =
BEGIN
  AddDoc(s, doc);
END WSObjectAddDoc;

PROCEDURE AddDoc(s: T; doc: DocVBT.T; makeCurrent: BOOLEAN := TRUE) =
VAR
  tSplit: TSplit.T := FormsVBT.GetVBT(s, "tSplit");
  top: DocVBT.T;
BEGIN
  IF (s.numDocs = 0) THEN
    FormsVBT.MakeActive(s, "popTitlesButton");
    FormsVBT.MakeActive(s, "reloadDocButton");
    FormsVBT.MakeActive(s, "duplicateDocButton");
    FormsVBT.MakeActive(s, "expandDocButton");
    Split.Delete(tSplit, Split.Succ(tSplit, NIL));  (* Delete gray child. *)
    makeCurrent := TRUE;
  END;
  IF makeCurrent THEN
    top := GetTopDoc(s);
    Split.Insert(tSplit, top, doc);
    TSplit.SetCurrent(tSplit, doc);
  ELSE
    Split.Insert(tSplit, Split.Pred(tSplit, NIL), doc);
  END;
  DocVBT.SetOwner(doc, s);
  INC(s.numDocs);
  UpdateLocation(s);
END AddDoc;


PROCEDURE RemDoc(s: T; doc: DocVBT.T) =
VAR
  tSplit: TSplit.T := FormsVBT.GetVBT(s, "tSplit");
  pred := Split.Pred(tSplit, doc);
BEGIN
  (* Use successor if there is no predecessor. *)
  IF pred = NIL THEN pred := Split.Succ(tSplit, doc) END;
  TSplit.SetCurrent(tSplit, pred);
  Split.Delete(tSplit, doc);
  DEC(s.numDocs);
  UpdateLocation(s);
  IF (s.numDocs = 0) THEN
    FormsVBT.MakeDormant(s, "popTitlesButton");
    FormsVBT.MakeDormant(s, "reloadDocButton");
    FormsVBT.MakeDormant(s, "duplicateDocButton");
    FormsVBT.MakeDormant(s, "expandDocButton");
    VAR gray := TextureVBT.New(txt := Pixmap.Gray);
    BEGIN
      Split.Insert(tSplit, NIL, gray);
      TSplit.SetCurrent(tSplit, gray);
    END;
  END;
END RemDoc;

PROCEDURE ReplaceDoc(s: T; old, new: DocVBT.T) =
BEGIN
  RemDoc(s, old);
  AddDoc(s, new);
END ReplaceDoc;


PROCEDURE Hit (s: Source; target: VBT.T;
  <* UNUSED *> READONLY cd: VBT.PositionRec): BOOLEAN =
BEGIN
  RETURN ISTYPE(target, Target) AND target # GetTarget(s.deck) 
END Hit;

PROCEDURE Merge(cl:FormsVBT.Closure; fv: FormsVBT.T;
               name: TEXT; time: VBT.TimeStamp) =
VAR
  fromDeck := NARROW (fv, T);
  fromTSplit: TSplit.T := FormsVBT.GetVBT (fromDeck, "tSplit");
  source := NARROW (FormsVBT.GetVBT (fv, "mergeSource"), Source);
  target := NARROW (SourceVBT.GetTarget (source), Target);
  toDeck := target.deck;
  toTSplit: TSplit.T := FormsVBT.GetVBT (toDeck, "tSplit");
  topDoc: DocVBT.T;
BEGIN
  Gather(cl, fv, name, time);   (* May want to change freedocs' home decks
                                   without gathering them..... *)
  topDoc := GetTopDoc(fromDeck);
  WHILE fromDeck.numDocs > 0 DO
    WITH doc = Split.Succ (fromTSplit, NIL) DO
      RemDoc (fromDeck, doc);
      AddDoc (toDeck, doc);
    END
  END;
  IF topDoc # NIL THEN
    TSplit.SetCurrent (toTSplit, topDoc);
  END;
  UpdateLocation (toDeck);
  WorkspaceVBT.RemDeck(fromDeck.getWorkspace(), fromDeck);
END Merge;

PROCEDURE AddFreeDoc(s: T; freeDoc: WSObjectVBT.T) =
VAR fDoc: FreeDocVBT.T := freeDoc;
BEGIN
  fDoc.next := s.freeDocs;
  IF s.freeDocs # NIL THEN s.freeDocs.prev := fDoc END;
  fDoc.prev := NIL;
  s.freeDocs := fDoc;
  FreeDocVBT.SetDeck(freeDoc, s);
END AddFreeDoc;

PROCEDURE RemFreeDoc(s: T; freeDoc: WSObjectVBT.T) =
VAR fDoc: FreeDocVBT.T := freeDoc;
BEGIN
  IF fDoc.next # NIL THEN fDoc.next.prev := fDoc.prev END;
  IF fDoc.prev # NIL
    THEN fDoc.prev.next := fDoc.next
    ELSE s.freeDocs := fDoc.next;
  END;
  fDoc.next := NIL;
  fDoc.prev := NIL;
END RemFreeDoc;



PROCEDURE Home(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
                   <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
BEGIN
  FormsVBT.PutText(fv, "urlTypein", DocVBT.DefaultHomeURL)
END Home;

PROCEDURE Open(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
                   <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  url := FormsVBT.GetText(fv, "urlTypein");
  doc := DocVBT.NewFromURL(url);
BEGIN
  AddDoc(fv, doc);
  FormsVBT.PopDown (fv, "urlDialog");
END Open;


PROCEDURE PopTitles(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                    <*UNUSED*> name: TEXT; time: VBT.TimeStamp) =
VAR s: T := fv;
    list: RefList.T;
    doc: DocVBT.T; 
    ct, active: INTEGER;
    title: TEXT;
    docList := DocList (s, FALSE);
    browser: ListVBT.T := FormsVBT.GetVBT (s, "titlesBrowser");
BEGIN
  browser.removeCells (0, LAST(CARDINAL));
  ct := 0;
  list := docList;
  WHILE list # NIL DO 
    INC(ct); 
    list := list.tail 
  END;
  browser.insertCells (0, ct);
  active := 0;
  ct := 0;
  list := docList;
  WHILE list # NIL DO
    doc := list.head;
    IF GetTopDoc (s) = doc THEN active := ct+1 END;
    title := Fmt.Int(ct+1) & ". " & DocVBT.GetTitle (doc);
    browser.setValue (ct, title);
    INC(ct);
    list := list.tail
  END;
  browser.selectOnly (active-1);
  FormsVBT.PutInteger (s, "titlesNumeric", active);
  FormsVBT.PutIntegerProperty (s, "titlesNumeric", "Max", ct);
  FormsVBT.PopUp (s,  "titlesDialog", TRUE, time);
END PopTitles;

PROCEDURE Titles(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                    <*UNUSED*> name: TEXT; <* UNUSED *> time: VBT.TimeStamp) =
VAR s: T := fv;
    browser: ListVBT.T := FormsVBT.GetVBT (s, "titlesBrowser");
    active: INTEGER;
BEGIN
  (* snarf the selection from the browser and shuffle the deck *)
  IF NOT browser.getFirstSelected(active) THEN active := 0 END;
  SetTopDoc (s, active);
  FormsVBT.PopDown (s, "titlesDialog");
END Titles;

PROCEDURE SelectTitle(cl: FormsVBT.Closure; fv: FormsVBT.T;
                       name: TEXT; time: VBT.TimeStamp) =
VAR 
  s: T := fv;
  browser: ListVBT.T := FormsVBT.GetVBT (s, "titlesBrowser");
  event := FormsVBT.GetTheEvent(fv);
  mouse: VBT.MouseRec;
  active: INTEGER;
BEGIN
  (* Four ways to get here:
       1) in browser, single click => update numeric
       2) in browser, double click => shuffle the deck
       3) in numeric, + or - => update selection in browser
       4) in numeric, CR => update selection in browser AND shuffle
   *)
   IF Text.Equal (name, "titlesBrowser") THEN
     mouse := NARROW (event, AnyEvent.Mouse).mouse;
     IF mouse.clickCount = 1 THEN
       (* Case 1: in browser, single click => update numeric *)
       IF NOT browser.getFirstSelected(active) THEN active := 0 END;
       FormsVBT.PutInteger (s, "titlesNumeric", active+1);
     ELSE
       (* Case 2: in browser, double click => shuffle the deck *)
       Titles (cl, fv, name, time);
     END;
  ELSE
    active := FormsVBT.GetInteger (s, "titlesNumeric") - 1;
    browser.selectOnly (active);
    TYPECASE event OF 
    | AnyEvent.Mouse =>
      (* Case 3: in numeric, + or - => update selection in browser *)
    | AnyEvent.Key =>
      (* Case 4: in numeric, CR => update selection in browser AND shuffle *)
      Titles (cl, fv, name, time);
    ELSE <* ASSERT FALSE *>
    END;
  END;
END SelectTitle;


PROCEDURE PopRename(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                    <*UNUSED*> name: TEXT; time: VBT.TimeStamp) =
VAR s: T := fv;
BEGIN
  FormsVBT.PutText(s, "renameTypein", s.title);
  FormsVBT.PopUp (s,  "renameDialog", TRUE, time);
END PopRename;

PROCEDURE Rename(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                 <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR s: T := fv;
BEGIN
  SetTitle (s, FormsVBT.GetText(s, "renameTypein"));
  FormsVBT.PopDown (s, "renameDialog");
END Rename;

PROCEDURE IconizeCB(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                    <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
BEGIN
  Iconize (fv);
END IconizeCB;

PROCEDURE Iconize (deck: T) =
  VAR freeDoc: FreeDocVBT.T := GetFreeDocs(deck);
  BEGIN
    WHILE freeDoc # NIL DO
      WorkspaceVBT.Iconize (freeDoc);
      freeDoc := freeDoc.next;
    END;
    WorkspaceVBT.Iconize (deck)
  END Iconize;

PROCEDURE Deiconize (deck: T) =
  VAR freeDoc: FreeDocVBT.T := GetFreeDocs(deck);
  BEGIN
    WHILE freeDoc # NIL DO
      WorkspaceVBT.Deiconize (freeDoc);
      freeDoc := freeDoc.next;
    END;
    WorkspaceVBT.Deiconize (deck)
  END Deiconize;

PROCEDURE Duplicate(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                    <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  deck: T := fv;
  doc := GetTopDoc (deck);
BEGIN
  AddDoc (deck, DocVBT.Copy(doc))
END Duplicate;

PROCEDURE Reload(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                 <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  deck: T := fv;
  doc := GetTopDoc (deck);
BEGIN
  ReplaceDoc (deck, doc, DocVBT.Reload (doc))
END Reload;

PROCEDURE Expand (<*UNUSED*> cl  : FormsVBT.Closure;
                             fv  : FormsVBT.T;
                  <*UNUSED*> name: TEXT;
                  <*UNUSED*> time: VBT.TimeStamp     ) =
  VAR
    deck   : T := fv;
    doc        := GetTopDoc(deck);
    newDeck: T := DocVBT.Expand(doc);
  BEGIN
    WorkspaceVBT.AddDeck(deck.getWorkspace(), newDeck)
  END Expand;


PROCEDURE Gather(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
                 <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
  freeDoc := s.freeDocs;
  temp: FreeDocVBT.T;
BEGIN
  WHILE freeDoc # NIL DO
    temp := freeDoc.next;
    FreeDocVBT.GoBack(freeDoc);
    freeDoc := temp;
  END;
END Gather;

PROCEDURE Delete(cl:FormsVBT.Closure; fv: FormsVBT.T;
                 name: TEXT; time: VBT.TimeStamp) =
VAR
  s: T := fv;
BEGIN
  Gather(cl, fv, name, time);
  WorkspaceVBT.RemDeck(s.getWorkspace(), s);
END Delete;



PROCEDURE Bottom(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
                 <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
  tSplit: TSplit.T := FormsVBT.GetVBT(s, "tSplit");
BEGIN
  TSplit.SetCurrent(tSplit, Split.Succ(tSplit, NIL));
  UpdateLocation(s);
END Bottom;

PROCEDURE Prev(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
               <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
BEGIN
  UpdateLocation(s);
END Prev;

PROCEDURE Next(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
               <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
BEGIN
  UpdateLocation(s);
END Next;

PROCEDURE Top(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
              <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
  tSplit: TSplit.T := FormsVBT.GetVBT(s, "tSplit");
BEGIN
  TSplit.SetCurrent(tSplit, Split.Pred(tSplit, NIL));
  UpdateLocation(s);
END Top;

PROCEDURE DocList (deck: T; includeFreeDocs := TRUE): RefList.T =
  VAR
    tSplit: TSplit.T  := FormsVBT.GetVBT(deck, "tSplit");
    ch    : VBT.T     := NIL;
    list  : RefList.T := NIL;
  BEGIN
    FOR i := 1 TO Split.NumChildren(tSplit) DO
      ch := Split.Succ(tSplit, ch);
      IF ISTYPE(ch, DocVBT.T) THEN
        list := RefList.Cons(ch, list)
      END
    END;
    IF includeFreeDocs THEN
      VAR freeDoc: FreeDocVBT.T := GetFreeDocs(deck); BEGIN
        WHILE freeDoc # NIL DO
          list := RefList.Cons(FreeDocVBT.GetDoc(freeDoc), list);
          freeDoc := freeDoc.next;
        END
      END
    END;
    RETURN RefList.ReverseD(list)
  END DocList;


TYPE ColorPicker = MUTEX OBJECT
  seed: INTEGER;
METHODS
  pick(): Color.T := Pick;
END;

VAR colorPicker: ColorPicker;
CONST baseColors = ARRAY OF TEXT {"azure", "chocolate", "coral", "cyan",
                                  "forestgreen", "goldenrod", "indianred",
                                  "lavender", "magenta", "royalblue",
                                  "mintcream", "orangered", "olivegreen"};
CONST variations = ARRAY OF TEXT {"", "pale", "verypale", "bright",
                                  "verydrab", "drab", "dim", "reddish",
                                  "greenish", "bluish", "yellowish"};

PROCEDURE Pick(s: ColorPicker): Color.T =
BEGIN
  LOCK s DO
    INC(s.seed);
    RETURN ColorName.ToRGB(variations[s.seed MOD 11] &
                           baseColors[s.seed MOD 13]);
  END;
END Pick;


BEGIN
  colorPicker := NEW(ColorPicker,
                     seed := NEW(Random.Default).init().integer(0, 142));
END DeckVBT.



