(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:55 PDT 1996 by mhb       *)

MODULE Storage;

IMPORT Atom, DeckVBT, DocVBT, Env, FileRd, FileWr, FreeDocVBT, OSError, Rd,
       Rect, RefList, Stdio, Sx, Text, WorkspaceVBT, Wr;

<*FATAL ANY*>

PROCEDURE Restore (): WorkspaceVBT.T =
  VAR
    w        := WorkspaceVBT.New();
    home     := Env.Get("HOME");
    filename : TEXT;
  VAR rd: Rd.T;
  BEGIN
    IF home = NIL THEN RETURN w; END;
    filename := home & "/.deckscape";
    TRY rd := FileRd.Open(filename) EXCEPT OSError.E => RETURN w END;
    Wr.PutText(
      Stdio.stderr, "Restoring workspace from " & filename & ".\n");
    TRY
      LOOP
        TRY
          TYPECASE Sx.Read(rd) OF
          | NULL =>
          | RefList.T (list) =>
              TYPECASE list.head OF
              | NULL =>
              | Atom.T (sym) =>
                  IF sym = Atom.FromText("Deck") THEN
                    WITH deck = RestoreDeck(list.tail) DO
                      IF deck # NIL THEN
                        WITH title = DeckVBT.GetTitle(deck) DO
                          WITH f = FindDeckByTitle(w, title) DO
                            IF f # NIL THEN WorkspaceVBT.RemDeck(w, f) END
                          END;
                          IF Text.Equal(title, WorkspaceVBT.HotListDeck) THEN
                            DeckVBT.SetPermanent(deck, TRUE);
                            w.hotlist := deck
                          END
                        END;
                        WorkspaceVBT.AddDeck(w, deck, Rect.Empty);
                      END
                    END
                  END
              ELSE
              END;
          ELSE
          END;
        EXCEPT
          Sx.ReadError =>
        END;
      END
    EXCEPT
    | Rd.EndOfFile =>
    END;
    Rd.Close(rd);
    RETURN w
  END Restore;

PROCEDURE FindDeckByTitle (w: WorkspaceVBT.T; title: TEXT): DeckVBT.T =
  VAR deckList := WorkspaceVBT.GetDecks(w);
  BEGIN
    WHILE deckList # NIL DO
      WITH deck = deckList.head DO 
        IF Text.Equal(title, DeckVBT.GetTitle(deck)) THEN RETURN deck END
      END;
      deckList := deckList.tail
    END;
    RETURN NIL
  END FindDeckByTitle;
    
VAR 
 titleAtom := Atom.FromText("Title");
 currentAtom := Atom.FromText("Current");
 pagesAtom := Atom.FromText("Pages");

PROCEDURE RestoreDeck (list: RefList.T): DeckVBT.T =
  VAR
    deckVBT: DeckVBT.T := DeckVBT.New("");
    title  : TEXT      := NIL;
    current: INTEGER   := -1;
  BEGIN
    WHILE list # NIL DO
      TYPECASE list.head OF
      | NULL =>
      | RefList.T (property) =>
          TYPECASE property.head OF
          | NULL =>
          | Atom.T (sym) =>
              IF sym = titleAtom THEN
                title := NARROW(property.tail.head, TEXT);
              ELSIF sym = currentAtom THEN
                current := NARROW(property.tail.head, REF INTEGER)^;
              ELSIF sym = pagesAtom THEN
                VAR
                  urlList       := NARROW(property.tail, RefList.T);
                  url    : TEXT;
                BEGIN
                  WHILE urlList # NIL DO
                    url := urlList.head;
                    Wr.PutText(Stdio.stderr, "Deck " & title & ": url = "
                                               & urlList.head & "\n");
                    WITH docVBT = DocVBT.NewFromURL(url) DO 
                      DeckVBT.AddDoc(deckVBT, docVBT)
                    END;
                    urlList := urlList.tail
                  END
                END
              ELSE
              END;
          ELSE
          END;
      ELSE
      END;
      list := list.tail
    END;
    IF title # NIL THEN DeckVBT.SetTitle(deckVBT, title) END;
    IF current # -1 THEN DeckVBT.SetTopDoc(deckVBT, current) END;
    RETURN deckVBT
  END RestoreDeck;


PROCEDURE Save (v: WorkspaceVBT.T) =
  VAR
    deckList := WorkspaceVBT.GetDecks(v);
    home     := Env.Get("HOME");
  BEGIN
    IF home # NIL THEN
      VAR wr := FileWr.Open(home & "/.deckscape"); BEGIN
        WHILE deckList # NIL DO
          SaveDeck(wr, deckList.head);
          deckList := deckList.tail;
        END;
        Wr.Close(wr);
      END;
    END;
  END Save;

PROCEDURE SaveDeck (wr: Wr.T; deck: DeckVBT.T) =
  BEGIN
    Wr.PutText(wr, "(Deck\n");

    Wr.PutText(wr, "  (Title ");
    Sx.Print(wr, DeckVBT.GetTitle(deck));
    Wr.PutText(wr, ")\n");

    (* print position VBT.Domain(deck); *)

    Wr.PutText(wr, "  (Current ");
    WITH curr = DeckVBT.GetTopDoc(deck) DO
      IF curr = NIL THEN
        Sx.Print(wr, Sx.FromInt(-1))
      ELSE
        Sx.Print(wr, Sx.FromInt(DeckVBT.IndexOfDoc(deck, curr)))
      END
    END;
    Wr.PutText(wr, ")\n");

    (* docs in deck *)
    Wr.PutText(wr, "  (Pages\n    ");
    VAR d := DeckVBT.DocList(deck, FALSE);
    BEGIN
      WHILE d # NIL DO SaveDoc(wr, d.head); d := d.tail END
    END;
    (* freedocs associated with deck are stored just as docs *)
    VAR fd: FreeDocVBT.T := DeckVBT.GetFreeDocs(deck);
    BEGIN
      WHILE fd # NIL DO SaveFreeDoc(wr, fd); fd := fd.next END
    END;
    Wr.PutText(wr, ")\n");

    Wr.PutText(wr, ")\n");
  END SaveDeck;

PROCEDURE SaveFreeDoc (wr: Wr.T; freeDoc: FreeDocVBT.T) =
  BEGIN
    WITH
      (* title = FreeDocVBT.GetTitle(freeDoc), *)
      (* position = VBT.Domain(freeDoc), *)
         doc = FreeDocVBT.GetDoc(freeDoc) 
    DO
      SaveDoc(wr, doc)
    END
  END SaveFreeDoc;

PROCEDURE SaveDoc (wr: Wr.T; doc: DocVBT.T) =
  BEGIN
    WITH page = DocVBT.GetPage(doc) DO
      IF page # NIL THEN
        Sx.Print(wr, page.header.location);
        Wr.PutText(wr, "\n    ");
      END
    END;
  END SaveDoc;


BEGIN 
END Storage.
