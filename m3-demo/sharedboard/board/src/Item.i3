(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "Item" interface defines the entities stored in a shared board.
*)

INTERFACE Item;

IMPORT Word, RectR;

TYPE T <: Public;
     Public = OBJECT
       id: ID;
       box: RectR.T; 
     END;
     
     ID = Word.T;

(* An "Item.T",  or an {\em item}, is the smallest entity that
   can be stored in a board and displayed at the clients. 

   Each item in a board has a unique ID: "id". 
   (Further, when an item is deleted, its ID is not reused.)
   A client creates an item, except for the ID, and sends it to the
   board server, which then assigns the item a fresh ID.

   The "box" attribute gives the bounding box of the item.
   It is used by the server to determine the clients that should be
   notified of changes to the item. (Notifications to the clients are
   explained later.)

   A client window uses the interface "ItemClass" that reveals
   useful painting methods of "Item.T". Typically, the clients
   use subtypes of "Item.T" for specific kinds of items.
   (For instance, see "TextItem" and "RuleItem".) Unfortunately,
   "ItemClass" and subtypes of "Item.T" are required to be included in
   the server package; they are imported by the client package.

*)

TYPE TArray = REF ARRAY OF T;
     IDArray = REF ARRAY OF ID;

PROCEDURE Equal (i1, i2: T): BOOLEAN;
(* Two items are equal if they have the same ID. *)

END Item.

