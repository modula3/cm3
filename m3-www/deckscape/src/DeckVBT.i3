(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:09 PDT 1996 by mhb       *)

INTERFACE DeckVBT;

(* A DeckVBT.T represents a deck in the workspace.  A deck contains a
   collection of DocVBTs and FreeDocVBTs.  There is a Target component that
   surrounds the deck.  This is used as the object of drag-and-drop
   operation. *)

IMPORT Color, DocVBT, FVTypes, RefList, WSObjectVBT;

TYPE
  T <: WSObjectVBT.T;

PROCEDURE New(title: TEXT; permanent := FALSE) : T;

PROCEDURE SetPermanent(deck: T; permanet: BOOLEAN);
PROCEDURE GetPermanent(deck: T): BOOLEAN;
PROCEDURE SetTitle(deck: T; title: TEXT);
PROCEDURE GetTitle(deck: T): TEXT;
PROCEDURE GetBannerColor(deck: T): Color.T;

PROCEDURE DocList (deck: T; includeFreeDocs := TRUE): RefList.T;
(* Returns the list of docs in increasing order.  If
   "includeFreeDocs" is "TRUE", the free-docs are at the end of
   the list. *)

PROCEDURE Iconize (deck: T); 
(* Iconify "deck" and all the deck's free docs *)
PROCEDURE Deiconize (deck: T);
(* Bring back the "deck" and the deck's free docs *)

PROCEDURE GetTopDoc(deck: T): DocVBT.T;
PROCEDURE SetTopDoc(deck: T; docNum: INTEGER);

PROCEDURE IndexOfDoc(deck: T; doc: DocVBT.T): INTEGER;

PROCEDURE AddDoc(deck: T; doc: DocVBT.T; makeCurrent: BOOLEAN := TRUE);
PROCEDURE RemDoc(deck: T; doc: DocVBT.T);

PROCEDURE AddFreeDoc(deck: T; freeDoc: WSObjectVBT.T);
PROCEDURE RemFreeDoc(deck: T; freeDoc: WSObjectVBT.T);
PROCEDURE GetFreeDocs(deck: T): WSObjectVBT.T;

TYPE 
  Target = FVTypes.FVTarget OBJECT deck: T;  END;

PROCEDURE GetTarget (deck: T): Target;

END DeckVBT.


