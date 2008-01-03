(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:21 PDT 1996 by mhb       *)

INTERFACE FreeDocVBT;

(* A FreeDocVBT.T is a subtype of WSObjectVBT.T which represents a document
   which is currently away from its home deck.  Its init method takes a
   pointer to the workspace (owner) and to the home deck (deck).  The
   goBack method sends the FreeDoc back to its home deck. *)

IMPORT DocVBT, DeckVBT, WSObjectVBT;

TYPE
  T <: Public;
  Public = WSObjectVBT.T OBJECT
    next, prev : T; (* Next and prev FreeDocVBTs in the deck. *)
  END;

PROCEDURE New(title: TEXT) : T;
PROCEDURE SetTitle(s: T; title: TEXT);
PROCEDURE GetTitle(s: T): TEXT;
PROCEDURE SetDeck(s: T; deck: DeckVBT.T);  (* DeckVBT calls! *)
PROCEDURE GetDeck(s: T): DeckVBT.T;
PROCEDURE GetDoc(freeDoc: T): DocVBT.T;
PROCEDURE AddDoc(s: T; doc: DocVBT.T);
PROCEDURE RemDoc(s: T; doc: DocVBT.T);
PROCEDURE GoBack(s: T);

END FreeDocVBT.
