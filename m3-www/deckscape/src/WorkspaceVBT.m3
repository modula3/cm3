(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:14:04 PDT 1996 by mhb       *)

MODULE WorkspaceVBT;

IMPORT FormsVBT, MyBundle, Rsrc, Trestle, FreeDocVBT, DocVBT, VBT, DeckVBT,
       Point, Rect, Split, ZSplit, Fmt, Text, PaintOp, SourceVBT, RefList,
       WSObjectVBT, Pixmap, PixmapVBT, Image, Rd, VBTClass, VBTRep, WebVBT,
       RTCollector, RTutils, Thread, URLCache, Options;

<* FATAL ANY *>

REVEAL
  T = Public BRANDED OBJECT
    deckCt := 0;
  OVERRIDES
    realize := Realize;
  END;

TYPE 
  MButton = FormsVBT.T OBJECT
    deck: DeckVBT.T;
  END;

VAR 
  mappedPixmap: Pixmap.T;
  unmappedPixmap: Pixmap.T;
  path: Rsrc.Path;

CONST
 DeckMenuVBox = ARRAY BOOLEAN OF TEXT {"deckMenuVBox", "permanentDeckMenuVBox"};

PROCEDURE New (): T =
  VAR s := NEW(T);
  BEGIN
    EVAL FormsVBT.T.initFromRsrc(s, "Workspace.fv", path);
    FormsVBT.Attach(
      s, "newDeckButton", NEW(FormsVBT.Closure, apply := NewDeck));
    WITH searchSearch = NEW(FormsVBT.Closure, apply := SearchSearch) DO
      FormsVBT.Attach(s, "searchSearchButton", searchSearch);
      FormsVBT.Attach(s, "searchTypein", searchSearch);
    END;
    FormsVBT.Attach(
      s, "flushCache", NEW(FormsVBT.Closure, apply := FlushCache));
    FormsVBT.Attach(
      s, "caching", NEW(FormsVBT.Closure, apply := ToggleCaching));
    FormsVBT.Attach(
      s, "zippers", NEW(FormsVBT.Closure, apply := ToggleZippers));
    FormsVBT.Attach(
      s, "fgImages", NEW(FormsVBT.Closure, apply := ToggleFgImages));
    FormsVBT.Attach(s, "debug", NEW(FormsVBT.Closure, apply := Debug));
    FormsVBT.Attach(s, "quitButton", NEW(FormsVBT.Closure, apply := Quit));
    VAR
      target := FormsVBT.GetVBT(s, "target");
      bg     := FormsVBT.GetColorProperty(s, "target", "BgColor");
      fg     := FormsVBT.GetColorProperty(s, "target", "Color");
      bgOp := PaintOp.FromRGB(bg.r, bg.g, bg.b, PaintOp.Mode.Accurate,
                              -1.0, PaintOp.BW.UseBg);
      fgOp := PaintOp.FromRGB(fg.r, fg.g, fg.b, PaintOp.Mode.Accurate,
                              -1.0, PaintOp.BW.UseFg);
      swapOp := PaintOp.SwapPair(bgOp, fgOp);
      op     := PaintOp.Pair(PaintOp.Transparent, swapOp);
    BEGIN
      SourceVBT.BeTarget(target, SourceVBT.NewSwapTarget(op));
    END;
    SetCaching(s);
    SetZippers(s);
    SetFgImages(s);

    s.hotlist := DeckVBT.New(HotListDeck, permanent := TRUE);
    AddDeck(s, s.hotlist);
    Iconize(s.hotlist);

    WITH doc  = DocVBT.NewFromURL(),
         deck = DeckVBT.New(HomeDeck) DO
      DeckVBT.AddDoc(deck, doc);
      AddDeck(s, deck);
    END;

    WITH webvbt = NEW(WebVBT.T).init(),
         text   = Rsrc.Get("Help.html", path) DO
      webvbt.fromText(text);
      FormsVBT.PutGeneric(s, "helpText", webvbt)
    END;

    RETURN s;
  END New;

PROCEDURE Realize (ws: T; type, name: TEXT) : VBT.T
  RAISES {FormsVBT.Error} =
  BEGIN
    IF Text.Equal (name, "target") THEN 
      RETURN NEW (Target, ws := ws)
    ELSE
      RETURN FormsVBT.T.realize (ws, type, name)
    END;
  END Realize;

PROCEDURE GetTarget (ws: T): Target =
  BEGIN
    RETURN FormsVBT.GetVBT(ws, "target")
  END GetTarget;

PROCEDURE GetDecks (s: T): RefList.T =
  VAR list: RefList.T;
    p  := FormsVBT.GetVBT(s, "zSplit");
    ch := Split.Succ(p, NIL);
  BEGIN
    WHILE ch # NIL DO
      IF ISTYPE(ch, DeckVBT.T) THEN list := RefList.Cons(ch, list) END;
      ch := Split.Succ(p, ch)
    END;
    RETURN list
  END GetDecks;
 
PROCEDURE AddDeck (s: T; deck: DeckVBT.T; READONLY where := Rect.Empty) =
  VAR
    zSplit := FormsVBT.GetVBT(s, "zSplit");
    menuVBox := FormsVBT.GetVBT(
                  s, DeckMenuVBox[DeckVBT.GetPermanent(deck)]);
    mButton     := NEW(MButton, deck := deck);
    bannerColor := DeckVBT.GetBannerColor(deck);
  BEGIN
    INC(s.deckCt);
    ZSplit.Insert(zSplit, deck, PositionWindow(where));
    deck.setWorkspace(s);
    EVAL FormsVBT.T.initFromRsrc(mButton, "MButton.fv", path, TRUE);
    FormsVBT.PutText(mButton, "text", DeckVBT.GetTitle(deck));
    FormsVBT.PutColorProperty(mButton, "text", "BgColor", bannerColor);
    FormsVBT.PutColorProperty(mButton, "icon", "BgColor", bannerColor);
    FormsVBT.Attach(
      mButton, "button", NEW(FormsVBT.Closure, apply := Raise));
    Split.Insert(menuVBox, Split.Pred(menuVBox, NIL), mButton);
  END AddDeck;

PROCEDURE RemDeck (s: T; deck: DeckVBT.T) =
  BEGIN
    WITH mButton = GetMButton(s, deck) DO
      mButton.deck := NIL;
      Split.Delete(mButton.parent, mButton);
      VBT.Discard(mButton);
    END;
    Split.Delete(deck.parent, deck);
    (* need to fork the call to VBT.Discard, because RemDeck is
       called by a callback in a menu, located in the deck. *)
    EVAL Thread.Fork(NEW(DiscardVBTClosure, vbt := deck));
  END RemDeck;

TYPE
  DiscardVBTClosure = Thread.Closure OBJECT
                        vbt: VBT.T;
                      OVERRIDES
                        apply := DoDiscardVBT;
                      END;

PROCEDURE DoDiscardVBT (cl: DiscardVBTClosure): REFANY =
  BEGIN
    LOCK VBT.mu DO VBT.Discard(cl.vbt) END;
    RETURN NIL;
  END DoDiscardVBT;

PROCEDURE RenamedDeck (s: T; deck: DeckVBT.T) =
  VAR mButton := GetMButton(s, deck);
  BEGIN
    FormsVBT.PutText(mButton, "text", DeckVBT.GetTitle(deck))    
  END RenamedDeck;

PROCEDURE RenamedFreeDoc (<* UNUSED *> s      : T;
                          <* UNUSED *> freeDoc: FreeDocVBT.T) =
  BEGIN
  END RenamedFreeDoc;

PROCEDURE GetMButton (s: T; deck: DeckVBT.T): MButton =
  VAR
    menuVBox := FormsVBT.GetVBT(s, DeckMenuVBox[DeckVBT.GetPermanent(deck)]);
    ch: MButton := NIL;
  BEGIN
    ch := Split.Succ(menuVBox, NIL);
    WHILE ch # NIL DO
      IF ch.deck = deck THEN RETURN ch END;
      ch := Split.Succ(menuVBox, ch);
    END;
    <* ASSERT FALSE *>
  END GetMButton;

PROCEDURE AddFreeDoc (         s      : T;
                               freeDoc: FreeDocVBT.T;
                      READONLY where                   := Rect.Empty) =
  VAR zSplit := FormsVBT.GetVBT(s, "zSplit");
  BEGIN
    ZSplit.Insert(zSplit, freeDoc, PositionWindow(where));
    freeDoc.setWorkspace(s);
  END AddFreeDoc;


PROCEDURE RemFreeDoc (<* UNUSED *> s: T; freeDoc: FreeDocVBT.T) =
  BEGIN
    Split.Delete(VBT.Parent(freeDoc), freeDoc);
  END RemFreeDoc;


VAR
  mu := NEW(MUTEX);
  last: Point.T := Point.Origin;

CONST 
  DX = 30; DY = 60;
  MAXX = 165; MAXY = 330;

CONST
  DefaultWidth = 600;
  DefaultHeight = 500;

PROCEDURE PositionWindow (READONLY r: Rect.T): Rect.T =
  BEGIN
    IF Rect.IsEmpty(r) THEN 
      LOCK mu DO
        last.h := last.h + DX;
        IF last.h > MAXX THEN last.h := DX + (last.h MOD MAXX) END;
        last.v := last.v + DY;
        IF last.v > MAXY THEN last.v := DY + (last.v MOD MAXY) END;
        RETURN Rect.FromCorner(last, DefaultWidth, DefaultHeight)
      END;
    ELSE RETURN r END
  END PositionWindow;

PROCEDURE NewDeck(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
                  <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
  deck := DeckVBT.New("Deck " & Fmt.Int(s.deckCt));
  doc := DocVBT.NewFromURL();
BEGIN
  DeckVBT.AddDoc(deck, doc);
  AddDeck(s, deck);
END NewDeck;

PROCEDURE Iconize (ch: WSObjectVBT.T) =
  BEGIN
    IF ZSplit.IsMapped (ch) THEN 
      ZSplit.Unmap(ch);
      TYPECASE ch OF
      | DeckVBT.T (deck) =>
          VAR ws := deck.getWorkspace(); 
              mb := GetMButton (ws, deck); 
              v := FormsVBT.GetVBT (mb, "icon"); 
          BEGIN
            PixmapVBT.Put (v, unmappedPixmap);
          END
      ELSE
      END      
    END
  END Iconize;

PROCEDURE Deiconize (ch: WSObjectVBT.T) =
  BEGIN
    IF NOT ZSplit.IsMapped (ch) THEN
      ZSplit.Map (ch);
      TYPECASE ch OF
      | DeckVBT.T (deck) =>
          VAR ws := deck.getWorkspace(); 
              mb := GetMButton (ws, deck); 
              v := FormsVBT.GetVBT (mb, "icon"); 
          BEGIN
            PixmapVBT.Put (v, mappedPixmap);
          END
      ELSE
      END      
    END
  END Deiconize;

PROCEDURE SearchSearch(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
                       <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
  term := FormsVBT.GetText(s, "searchTypein");
  newDeck := DeckVBT.New("Search Results");
  deckList, docList: RefList.T;
  doc: DocVBT.T;
BEGIN
  FormsVBT.PopDown(s, "searchDialog");
  deckList := GetDecks (s);
  AddDeck(s, newDeck);
  WHILE deckList # NIL DO
    docList := DeckVBT.DocList (deckList.head, includeFreeDocs := TRUE);
    WHILE docList # NIL DO
      doc := docList.head;
      IF DocVBT.Search (doc, term) THEN  
       DeckVBT.AddDoc(newDeck, DocVBT.Copy(doc));
      END;
      docList := docList.tail;
    END;
    deckList := deckList.tail;
  END;
END SearchSearch;

PROCEDURE FlushCache (<*UNUSED*> cl  : FormsVBT.Closure;
                      <*UNUSED*> fv  : FormsVBT.T;
                      <*UNUSED*> name: TEXT;
                      <*UNUSED*> time: VBT.TimeStamp     ) =
  BEGIN
    URLCache.Flush();
    URLCache.FlushHTML();
  END FlushCache;


CONST
  DisableCachingText = "Disable Caching";
  EnableCachingText = "Enable Caching";

PROCEDURE ToggleCaching (<*UNUSED*> cl  : FormsVBT.Closure;
                              fv  : FormsVBT.T;
                   <*UNUSED*> name: TEXT;
                   <*UNUSED*> time: VBT.TimeStamp     ) =
  BEGIN
    Options.caching := NOT Options.caching;
    SetCaching(fv);
  END ToggleCaching;

PROCEDURE SetCaching (s: T) =
  BEGIN
    IF Options.caching THEN
      FormsVBT.PutText(s, "cachingText", DisableCachingText);
      URLCache.Enable();
      URLCache.EnableHTML();
    ELSE
      FormsVBT.PutText(s, "cachingText", EnableCachingText);
      URLCache.Disable();
      URLCache.DisableHTML();
    END
  END SetCaching;
 
 

CONST
  DisableZippersText = "Disable Zippers";
  EnableZippersText  = "Enable Zippers";

PROCEDURE ToggleZippers (<*UNUSED*> cl  : FormsVBT.Closure;
                              fv  : FormsVBT.T;
                   <*UNUSED*> name: TEXT;
                   <*UNUSED*> time: VBT.TimeStamp     ) =
  BEGIN
    Options.zippers := NOT Options.zippers;
    SetZippers(fv);
  END ToggleZippers;

PROCEDURE SetZippers (s: T) =
  BEGIN
    IF Options.zippers THEN
      FormsVBT.PutText(s, "zippersText", DisableZippersText);
    ELSE
      FormsVBT.PutText(s, "zippersText", EnableZippersText);
    END
  END SetZippers;
 
 

CONST
  DisableFgImagesText = "Enable background images";
  EnableFgImagesText  = "Disable background images";

PROCEDURE ToggleFgImages (<*UNUSED*> cl  : FormsVBT.Closure;
                               fv  : FormsVBT.T;
                    <*UNUSED*> name: TEXT;
                    <*UNUSED*> time: VBT.TimeStamp     ) =
  BEGIN
    Options.fgImages := NOT Options.fgImages;
    SetFgImages(fv)
  END ToggleFgImages;

PROCEDURE SetFgImages (s: T) =
  BEGIN
    IF Options.fgImages THEN
      FormsVBT.PutText(s, "fgImagesText", DisableFgImagesText)
    ELSE
      FormsVBT.PutText(s, "fgImagesText", EnableFgImagesText)
    END
  END SetFgImages;
 

PROCEDURE Debug (<*UNUSED*> cl  : FormsVBT.Closure;
                 <*UNUSED*> fv  : FormsVBT.T;
                 <*UNUSED*> name: TEXT;
                 <*UNUSED*> time: VBT.TimeStamp     ) =
  BEGIN
    RTCollector.Collect();
    RTutils.Heap(
      TRUE, presentation := RTutils.HeapPresentation.ByByteCount);
  END Debug;

PROCEDURE Quit (<*UNUSED*> cl  : FormsVBT.Closure;
                           fv  : FormsVBT.T;
                <*UNUSED*> name: TEXT;
                <*UNUSED*> time: VBT.TimeStamp     ) =
  BEGIN
    Trestle.Delete(fv);
  END Quit;


PROCEDURE Raise(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: MButton := fv;
  deck := s.deck;
BEGIN
  DeckVBT.Deiconize (deck);
  ZSplit.Lift(deck);
END Raise;


PROCEDURE GetPixmap (name: TEXT): Pixmap.T =
  VAR
    rd := Rsrc.Open(name, path);
    image := Image.FromRd(rd);
  BEGIN
    Rd.Close(rd);
    RETURN Image.Scaled(image)
  END GetPixmap;

BEGIN 
  path := Rsrc.BuildPath("$DeckScapePATH", MyBundle.Get());
  unmappedPixmap := GetPixmap ("unmapped.pbm");
  mappedPixmap := GetPixmap ("mapped.pbm");
END WorkspaceVBT.








