(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:49 PDT 1996 by mhb       *)

MODULE FreeDocVBT;

IMPORT MyBundle, DocVBT, FormsVBT, DeckVBT, Rsrc, VBT, WorkspaceVBT;

<* FATAL ANY *>

REVEAL T = Public BRANDED OBJECT
    deck: DeckVBT.T := NIL;
    doc: DocVBT.T := NIL;
  OVERRIDES
    addDoc := AddDoc;
    remDoc := RemDoc;
    replaceDoc := ReplaceDoc;
  END;

PROCEDURE New(title: TEXT) : T =
VAR
  s := NEW(T);
  path := Rsrc.BuildPath("$DeckScapePATH", MyBundle.Get());
  back := NEW(FormsVBT.Closure, apply := Back);
  newDeck := NEW(FormsVBT.Closure, apply := NewDeck);
  duplicate := NEW(FormsVBT.Closure, apply := Duplicate);
  expand := NEW(FormsVBT.Closure, apply := Expand);
  reload := NEW(FormsVBT.Closure, apply := Reload);
  iconize := NEW(FormsVBT.Closure, apply := Iconize);
BEGIN
  EVAL FormsVBT.T.initFromRsrc(s, "FreeDoc.fv", path, TRUE);
  SetTitle (s, title);
  FormsVBT.Attach(s, "backButton", back);
  FormsVBT.Attach(s, "newButton", newDeck);
  FormsVBT.Attach(s, "duplicateDocButton", duplicate);
  FormsVBT.Attach(s, "expandDocButton", expand);
  FormsVBT.Attach(s, "reloadDocButton", reload);
  FormsVBT.Attach(s, "iconize", iconize);
  RETURN s;
END New;

PROCEDURE SetTitle (s: T; title: TEXT) =
  BEGIN
    FormsVBT.PutText(s, "freeDocName", title);
  END SetTitle;

PROCEDURE GetTitle (s: T): TEXT =
  BEGIN
    RETURN FormsVBT.GetText(s, "freeDocName")
  END GetTitle;

PROCEDURE SetDeck(s: T; deck: DeckVBT.T) =
VAR color := DeckVBT.GetBannerColor (deck);
BEGIN
  s.deck := deck;
  FormsVBT.PutColorProperty(s, "freeDocName", "BgColor", color);
  FormsVBT.PutColorProperty(s, "freeDocPreName", "BgColor", color);
  FormsVBT.PutColorProperty(s, "freeDocPostName", "BgColor", color);
END SetDeck;

PROCEDURE GetDeck(s: T): DeckVBT.T =
BEGIN
  RETURN s.deck
END GetDeck;

PROCEDURE GetDoc(freeDoc: T): DocVBT.T =
BEGIN
  RETURN freeDoc.doc;
END GetDoc;

(* AddDoc adds a doc.  If a second doc is added, it gets put on the
   home deck of the freeDoc. *)
PROCEDURE AddDoc(s: T; doc: DocVBT.T) =
BEGIN
  IF s.doc = NIL THEN
    (* First child of the FreeDoc. *)
    FormsVBT.PutGeneric(s, "gen", doc);
    s.doc := doc;
    DocVBT.SetOwner(doc, s);
  ELSE
    (* A second or later child -- send to the deck. *)
    DeckVBT.AddDoc(s.deck, doc);
  END; 
END AddDoc;


(* RemDoc removes the (only) doc, thus deleting the freeDoc. *)
PROCEDURE RemDoc(s: T; <*UNUSED*> doc: DocVBT.T) =
BEGIN
  DeckVBT.RemFreeDoc(s.deck, s);
  WorkspaceVBT.RemFreeDoc(s.getWorkspace(), s);
  FormsVBT.PutGeneric(s, "gen", NIL);
END RemDoc;

PROCEDURE ReplaceDoc(s: T; <*UNUSED*>old: DocVBT.T; new: DocVBT.T) =
BEGIN
  s.doc := NIL;   (* Make AddDoc think that s contains no doc. *)
  AddDoc(s, new);
END ReplaceDoc;


PROCEDURE GoBack(s: T) =
VAR
  doc := s.doc;
BEGIN
  RemDoc(s, doc);        (* This does a DeckVBT.RemFreeDoc(s) and
                           a WorkspaceVBT.RemFreeDoc(s). *)
  DeckVBT.AddDoc(s.deck, doc);
END GoBack;

PROCEDURE Back(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
               <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
BEGIN
  GoBack(s);
END Back;

PROCEDURE NewDeck(<*UNUSED*> cl:FormsVBT.Closure; fv: FormsVBT.T;
                  <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
  doc := s.doc;
  deck : DeckVBT.T;
  dom := VBT.Domain(s);
BEGIN
  RemDoc(s, doc);  (* This pretty much destroys the freeDoc. *)
  deck := DeckVBT.New("Default New Deck Name");
  DeckVBT.AddDoc(deck, doc);
  WorkspaceVBT.AddDeck(s.getWorkspace(), deck, dom);
END NewDeck;

PROCEDURE Duplicate(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                    <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
BEGIN
  AddDoc (s, DocVBT.Copy(s.doc))
END Duplicate;

PROCEDURE Reload(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                 <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR
  s: T := fv;
BEGIN
  ReplaceDoc (s, NIL, DocVBT.Reload (s.doc))
END Reload;

PROCEDURE Expand (<*UNUSED*> cl  : FormsVBT.Closure;
                             fv  : FormsVBT.T;
                  <*UNUSED*> name: TEXT;
                  <*UNUSED*> time: VBT.TimeStamp     ) =
  VAR
    s   : T         := fv;
    deck: DeckVBT.T := DocVBT.Expand(s.doc);
  BEGIN
    WorkspaceVBT.AddDeck(s.getWorkspace(), deck)
  END Expand;
 
PROCEDURE Iconize(<*UNUSED*> cl: FormsVBT.Closure; fv: FormsVBT.T;
                  <*UNUSED*> name: TEXT; <*UNUSED*> time: VBT.TimeStamp) =
VAR s: T := fv; 
BEGIN
  DeckVBT.Iconize (s.deck);
END Iconize;


BEGIN END FreeDocVBT.
