(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "Callback" interface is a means for the board server
   to notify a client of changes made to items by other clients.
*)

INTERFACE Callback;

IMPORT NetObj, Thread,
       Item;

TYPE T = NetObj.T OBJECT 
    METHODS
      itemsCreated (it: Item.TArray)
          RAISES {NetObj.Error, Thread.Alerted};
      itemsModified (it: Item.TArray; additive: BOOLEAN) 
          RAISES {NetObj.Error, Thread.Alerted};
      itemsDeleted (id: Item.IDArray) 
          RAISES {NetObj.Error, Thread.Alerted};
    END;

(* A "Callback.T" is a network object owned by a client that it
   registers with the board server. The server invokes the methods of
   this object to notify the client of changes made to items by other
   clients. 

   The call "itemsCreated (its)" urges the client to create "its".
   It is used for items that have been recently created by another
   client, as well as items that have recently become visible in the
   client's scope (see the documentation in the client package).

   The call "itemsModified (its, additive)" notifies the client of the
   modified value "its" of items. 
   If "additive" is true, the modification is {\em additive} in that the new
   image of the items can be simply painted over the old one.
   For example, "additive" is true  when characters are added to a
   text item, or when its color changes, but false when its font changes.

   The call "itemsDeleted (ids)" gives the IDs of items deleted by
   another client.
*)

END Callback.
