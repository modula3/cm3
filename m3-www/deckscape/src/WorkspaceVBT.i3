(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:36 PDT 1996 by mhb       *)

INTERFACE WorkspaceVBT;

(* A WorkspaceVBT.T is the main VBT of the application; only one is ever
   created.  WorkspaceVBT.fv describes the structure of a WorkspaceVBT.  It
   has two main components: a menu bar for global commands, and a ZSplit
   for holding workspace objects.  There is a Target component that
   surrounds the ZSplit.  This is used as the object of drag-and-drop
   operation.  The AddDeck and RemDeck procedures manage decks in the
   workspace, and AddFreeDoc and RemFreeDoc manage free documents. *)

IMPORT FormsVBT, FreeDocVBT, FVTypes, DeckVBT, Rect, RefList, WSObjectVBT;

CONST 
  HotListDeck = "Hot List";
  HomeDeck = "Home Deck";

TYPE
  T <: Public;
  Public = FormsVBT.T OBJECT
    hotlist: DeckVBT.T;
  END;

PROCEDURE New(): T;

PROCEDURE AddDeck(ws: T; deck: DeckVBT.T; READONLY where := Rect.Empty);
PROCEDURE RemDeck (ws: T; deck: DeckVBT.T);
PROCEDURE RenamedDeck (ws: T;  deck: DeckVBT.T);

PROCEDURE GetDecks (ws: T): RefList.T; (* of DeckVBT.T's *)

PROCEDURE AddFreeDoc(ws: T; freeDoc: FreeDocVBT.T; READONLY where := Rect.Empty);
PROCEDURE RemFreeDoc(ws: T; freeDoc: FreeDocVBT.T);
PROCEDURE RenamedFreeDoc (ws: T; freeDoc: FreeDocVBT.T);

PROCEDURE Iconize (ch: WSObjectVBT.T);
PROCEDURE Deiconize (ch: WSObjectVBT.T);

TYPE 
  Target = FVTypes.FVTarget OBJECT
    ws: T;
  END;

PROCEDURE GetTarget (ws: T): Target;

END WorkspaceVBT.



