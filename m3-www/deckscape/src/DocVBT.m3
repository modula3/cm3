(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:45 PDT 1996 by mhb       *)

<* PRAGMA LL *>

MODULE DocVBT;

IMPORT Env, Fmt, FormsVBT, Rsrc, MyBundle, WSObjectVBT, VBT, Web, WebVBT,
       Thread, DeckVBT, WorkspaceVBT, FVTypes, SourceVBT, Text, TextList,
       FreeDocVBT, AnyEvent, Point, Rect, Options;

<* FATAL ANY *>

TYPE 
  MyWebVBT = WebVBT.T OBJECT 
    doc: T;
    fork: BOOLEAN;
    mu: MUTEX;
    cond: Thread.Condition;
    done: BOOLEAN;
  OVERRIDES
    hotlink := HotLink;
    ready := Ready;
  END;

REVEAL T = FormsVBT.T BRANDED OBJECT
    owner: WSObjectVBT.T := NIL;
    webvbt: MyWebVBT;
  OVERRIDES
    realize := Realize;
  END;

TYPE 
  Source = FVTypes.FVSource OBJECT
    doc: T;
  OVERRIDES
    hit := Hit;
  END;


PROCEDURE Realize (doc: T; type, name: TEXT) : VBT.T
  RAISES {FormsVBT.Error} =
  BEGIN
    IF Text.Equal (name, "source") THEN 
      RETURN NEW (Source, doc := doc)
    ELSE
      RETURN FormsVBT.T.realize (doc, type, name)
    END;
  END Realize;

PROCEDURE NewDoc (): T =
  VAR
    doc      := NEW(T);
    path     := Rsrc.BuildPath("$DeckScapePATH", MyBundle.Get());
    delete   := NEW(FormsVBT.Closure, apply := Delete);
    hotlist  := NEW(FormsVBT.Closure, apply := Hotlist);
    reparent := NEW(FormsVBT.Closure, apply := Reparent);
  BEGIN
    EVAL FormsVBT.T.initFromRsrc(doc, "Doc.fv", path, TRUE);
    FormsVBT.Attach(doc, "killButton", delete);
    FormsVBT.Attach(doc, "hotlistButton", hotlist);
    FormsVBT.Attach(doc, "source", reparent);
    doc.webvbt := NEW(MyWebVBT, doc:=doc).init();
    FormsVBT.PutGeneric(doc, "gen", doc.webvbt);
    RETURN doc;
  END NewDoc;

PROCEDURE NewFromPage (page: Web.Page; base: TEXT): T =
  VAR
    doc    := NewDoc();
    webvbt := doc.webvbt;
  BEGIN
    FormsVBT.PutText(doc, "docName", base);
    webvbt.fork := TRUE;
    webvbt.fromText(
      style := GetCurrentStyle(), zippers := GetCurrentZippers(),
      contents := page.contents, contentType := page.header.contentType,
      contentSubType := page.header.contentSubType, url := base);
    RETURN doc;
  END NewFromPage;

PROCEDURE NewFromURL (url: TEXT := NIL; reload := FALSE; fork := TRUE): T =
  VAR
    doc    := NewDoc();
    webvbt := doc.webvbt;
  BEGIN
    IF url = NIL THEN url := DefaultHomeURL END;
    FormsVBT.PutText(doc, "docName", url);
    webvbt.fork := fork;
    IF fork THEN
      webvbt.fetch(url, reload := reload, style := GetCurrentStyle(),
                   zippers := GetCurrentZippers());
    ELSE
      webvbt.mu := NEW(MUTEX);
      webvbt.cond := NEW(Thread.Condition);
      webvbt.done := FALSE;
      LOCK webvbt.mu DO
        webvbt.fetch(url, reload := reload, style := GetCurrentStyle(),
                     zippers := GetCurrentZippers());
        WHILE NOT webvbt.done DO Thread.Wait(webvbt.mu, webvbt.cond) END
      END
    END;
    RETURN doc;
  END NewFromURL;

(* Bug: GetCurrent... procedures need to lock VBT.mu; they cannot be NewFromURL is
   called sometime with VBT.mu lock (e.g., WorkspaceVBT.NewDeck) and
   sometime without (e.g., DoExpand). *)

PROCEDURE GetCurrentZippers (): BOOLEAN =
  BEGIN
    RETURN Options.zippers
  END GetCurrentZippers;

PROCEDURE GetCurrentStyle (): WebVBT.Style =
  BEGIN
    IF Options.fgImages THEN
      RETURN WebVBT.Style.Normal
    ELSE
      RETURN WebVBT.Style.Background
    END
  END GetCurrentStyle;

PROCEDURE Ready (w: MyWebVBT; ct: CARDINAL) =
  BEGIN
    IF NOT w.fork AND ct = 0 THEN
      LOCK w.mu DO w.done := TRUE; Thread.Broadcast(w.cond); END
    END
  END Ready;

PROCEDURE Copy (doc: T): T =
  BEGIN
    RETURN NewFromPage(doc.webvbt.page, doc.webvbt.url);
  END Copy;

PROCEDURE SetOwner (doc: T; owner: VBT.T) =
  BEGIN
    doc.owner := owner;
  END SetOwner;

PROCEDURE GetOwner (doc: T): VBT.T =
  BEGIN
    RETURN doc.owner
  END GetOwner;

PROCEDURE GetPage (doc: T): Web.Page =
  BEGIN
    RETURN doc.webvbt.page;
  END GetPage;

PROCEDURE GetTitle (doc: T): TEXT =
  BEGIN
    TYPECASE doc.webvbt.page OF
    | NULL => RETURN "fetching..."
    | WebVBT.HTMLPage (p) => RETURN p.html.title
    | WebVBT.ImagePage => RETURN "<image>"
    ELSE
      RETURN "<????>"
    END;
  END GetTitle;


PROCEDURE Hit (                      s     : Source;
                                     target: VBT.T;
               <* UNUSED *> READONLY cd    : VBT.PositionRec):
  BOOLEAN =
  VAR owner := s.doc.owner;
  BEGIN
    IF ISTYPE(owner, DeckVBT.T) THEN
      (* a DocVBT inside a DeckVBT can go into the Workspace
         and into a DeckVBT other than its owner *)
      IF ISTYPE(target, WorkspaceVBT.Target) THEN 
        RETURN TRUE
      ELSE
        RETURN target # DeckVBT.GetTarget (owner) 
      END
    ELSE
      (* a DocVBT inside a FreeDocVBT can go into any DeckVBT,
         but not into the Workspace *)
      RETURN ISTYPE (target, DeckVBT.Target)
    END
  END Hit;

PROCEDURE Reparent (<*UNUSED*> cl  : FormsVBT.Closure;
                               fv  : FormsVBT.T;
                    <*UNUSED*> name: TEXT;
                    <*UNUSED*> time: VBT.TimeStamp     ) =
  VAR
    doc    := NARROW(fv, T);
    source := NARROW(FormsVBT.GetVBT(fv, "source"), Source);
    target := SourceVBT.GetTarget(source);
    owner  := source.doc.owner;
    newDeck   : DeckVBT.T;
    newFreeDoc: FreeDocVBT.T;
  BEGIN
    TYPECASE owner OF
    | DeckVBT.T (deck) =>
        (* a DocVBT in a DeckVBT; the target is either a
           Workspace or a DeckVBT *)
        IF ISTYPE(target, WorkspaceVBT.Target) THEN
          DeckVBT.RemDoc(deck, doc);
          newFreeDoc :=
            FreeDocVBT.New(DeckVBT.GetTitle(deck) & " [DOC]");
          VAR dom := VBT.Domain(deck);
              nw := LocateMouse(fv);
              hor := Rect.HorSize(dom);
              ver := Rect.VerSize(dom);
              r := Rect.FromEdges (nw.h, nw.h+hor, nw.v, nw.v+ver);
          BEGIN
            WorkspaceVBT.AddFreeDoc(deck.getWorkspace(), newFreeDoc, r);
          END;
          DeckVBT.AddFreeDoc(deck, newFreeDoc);
          FreeDocVBT.AddDoc(newFreeDoc, doc);
        ELSE
          DeckVBT.RemDoc(deck, doc);
          newDeck := NARROW(target, DeckVBT.Target).deck;
          DeckVBT.AddDoc(newDeck, doc);
        END
    | FreeDocVBT.T (freeDoc) =>
        (* a DocVBT in a FreeDocVBT; the target must be a
           DeckVBT *)
        FreeDocVBT.RemDoc(freeDoc, doc);
        newDeck := NARROW(target, DeckVBT.Target).deck;
        DeckVBT.AddDoc(newDeck, doc);
    ELSE                         <* ASSERT FALSE *>
    END;
  END Reparent;

PROCEDURE LocateMouse (fv: FormsVBT.T): Point.T =
  BEGIN
    TYPECASE FormsVBT.GetTheEvent(fv) OF
    | AnyEvent.Mouse (m) =>
        WITH cp = m.mouse.cp DO
          IF NOT cp.offScreen THEN RETURN cp.pt END
        END
    ELSE
    END;
    RETURN Point.Origin
  END LocateMouse;


PROCEDURE Delete (<*UNUSED*> cl  : FormsVBT.Closure;
                             fv  : FormsVBT.T;
                  <*UNUSED*> name: TEXT;
                  <*UNUSED*> time: VBT.TimeStamp     ) =
  VAR doc: T := fv;
  BEGIN
    doc.owner.remDoc(doc);
  END Delete;

PROCEDURE Hotlist (<*UNUSED*> cl  : FormsVBT.Closure;
                              fv  : FormsVBT.T;
                   <*UNUSED*> name: TEXT;
                   <*UNUSED*> time: VBT.TimeStamp     ) =
  VAR doc: T := fv; 
       ws: WorkspaceVBT.T := doc.owner.getWorkspace();
  BEGIN
    DeckVBT.AddDoc(ws.hotlist, Copy(doc));
  END Hotlist;

PROCEDURE Search (doc: T; text: TEXT): BOOLEAN =
  BEGIN
    RETURN doc.webvbt.search(text)
  END Search;

PROCEDURE Reload (doc: T): T =
  BEGIN
    RETURN NewFromURL(doc.webvbt.url, reload := TRUE)
  END Reload;

TYPE 
  Link = REF RECORD
    label, url: TEXT;
    next: Link;
  END;

TYPE
  ExpandClosure = Thread.Closure OBJECT
                    (* READONLY by threads: *)
                    doc     : T;
                    deck    : DeckVBT.T;
                    numLinks: INTEGER;
                    (* protected by mu: *)
                    mu   : MUTEX;
                    links: TextList.T;  (* NIL'd once processed *)
                  OVERRIDES
                    apply := DoExpand;
                  END;

PROCEDURE Expand (doc: T): VBT.T =
  VAR
    links := doc.webvbt.getLinks();
    l     := links;
    ct    := 0;
    mu    := NEW(MUTEX);
  BEGIN
    WHILE l # NIL DO INC(ct); l := l.tail END;
    WITH deck = DeckVBT.New("Expanding " & Fmt.Int(ct) & " links") DO
      LOCK mu DO
        FOR th := 1 TO NumberOfExpansionThreads DO
          EVAL Thread.Fork(NEW(ExpandClosure, doc := doc, deck := deck,
                               numLinks := ct, mu := mu, links := links));
        END
      END;
      RETURN deck
    END
  END Expand;

PROCEDURE DoExpand (cl: ExpandClosure): REFANY =
  VAR
    doc   := cl.doc;
    deck  := cl.deck;
    links := cl.links;
  VAR
    l     : TextList.T;
    url   : TEXT;
    newURL: TEXT;
    newDoc: T;
  BEGIN
    LOOP
      LOCK cl.mu DO
        l := links;
        WHILE l # NIL AND l.head = NIL DO l := l.tail END;
        IF l = NIL THEN EXIT END;
        url := l.head;
        l.head := NIL;
      END;
      newURL := Web.AbsoluteURL(url, doc.webvbt.url);
      newDoc := NewFromURL(newURL, fork := FALSE);
      LOCK VBT.mu DO DeckVBT.AddDoc(deck, newDoc, FALSE) END
    END;
    LOCK VBT.mu DO DeckVBT.SetTitle(deck, "Expanded"); END;
    RETURN NIL;
  END DoExpand;

PROCEDURE HotLink (w: MyWebVBT; url: TEXT; READONLY cd: VBT.MouseRec) =
  VAR
    expandedURL    := Web.AbsoluteURL(url, w.url);
    newDoc     : T;
  BEGIN
    IF VBT.Modifier.Control IN cd.modifiers THEN
      VAR
        deck    := WhichDeck(w.doc);
        docList := DeckVBT.DocList(deck, includeFreeDocs := FALSE);
      BEGIN
        WHILE docList # NIL DO
          VAR doc: T := docList.head;
          BEGIN
            IF Text.Equal(expandedURL, doc.webvbt.url) THEN
              DeckVBT.SetTopDoc(deck, DeckVBT.IndexOfDoc(deck, doc));
              RETURN
            END
          END;
          docList := docList.tail
        END
      END
    END;
    newDoc := NewFromURL(expandedURL);
    w.doc.owner.addDoc(newDoc);
  END HotLink;

PROCEDURE WhichDeck (doc: T): DeckVBT.T =
  VAR owner := doc.owner;
  BEGIN
    IF ISTYPE (owner, DeckVBT.T) THEN 
      RETURN owner
    ELSIF ISTYPE (owner, FreeDocVBT.T) THEN 
      RETURN FreeDocVBT.GetDeck (owner)
    ELSE <* ASSERT FALSE *>
    END
  END WhichDeck;

BEGIN
  DefaultHomeURL := Env.Get("WWW_HOME");
  IF DefaultHomeURL = NIL OR Text.Empty(DefaultHomeURL) THEN
      DefaultHomeURL := "http://www.research.digital.com/"
  END;
END DocVBT.
