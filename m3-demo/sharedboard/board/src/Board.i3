(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* The "Board" interface to an on-line board object allows clients to
   edit items.  
*)

INTERFACE Board;

IMPORT NetObj, Thread,
       Callback, Item, ClientInfo, RectR;

TYPE T = NetObj.T OBJECT 
  METHODS
    register    (cb: Callback.T): ClientInfo.T 
        RAISES {NetObj.Error, Thread.Alerted};
    setScope    (ci: ClientInfo.T; scope: RectR.T) 
        RAISES {NetObj.Error, Thread.Alerted};
    createItems  (ci: ClientInfo.T; its: Item.TArray): Item.IDArray
        RAISES {NetObj.Error, Thread.Alerted};
    modifyItems  (ci: ClientInfo.T; its: Item.TArray; additive: BOOLEAN)
       RAISES {NetObj.Error, Thread.Alerted};
    deleteItems  (ci: ClientInfo.T; ids: Item.IDArray)
       RAISES {NetObj.Error, Thread.Alerted};
    unregister  (ci: ClientInfo.T)
       RAISES {NetObj.Error, Thread.Alerted};
  END;

(* A "Board.T" is a network object owned by the board server. 
   A client opens a window and connects it to one such board.

   The client must initially call the "register" method to
   provide the board with a callback object (a "Callback.T"). 
   The board creates a  local object (a "ClientInfo.T") to cache the
   information provided by the client and
   returns a reference to this object to the client.
   The cached client information includes the callback object and the
   {\em scope} of the client:   
   a rectangular subset of the full area of the board that the client is
   interested in. The scope is initialized to be empty.
   In all subsequent calls, the client passes the "ClientInfo.T" as
   its own identification. 

   The method "setScope" is a directive to the server to change the
   client's scope. 
   The board may save work by not notifying the client of items not
   visible in the scope.
   Also, the board must notify the client of items visible in
   the new scope that were not visible in the previous scope.
   WARNING: The current implementation disregards the previous scope. 
   This notification happens in the background through the callback object.
   

   The method "createItems" installs a new items into the board.
   The board allocates fresh IDs to the items and returns them to the client.

   The client invokes "modifyItems" to provide the modified value of items.
   The client might have modified any or all of the attributes of an
   item. 
   If "additive" is true, the modifications are {\em additive} in that the new
   image of the items can be simply painted over the old one.
   The board passes this information in notifications to other clients.
   Concurrent modifications to the same item by different clients are
   handled as they arrive at the board: the modifications are applied 
   one after another.
   The new value prescribed by the last call will prevail.

   The method "deleteItems" results in the deletion of identified items.
   Subsequent calls of "modifyItems" on the same items are ignored. 

   A client is expected to take leave by calling "unregister" which
   uncaches the information being maintained for that client. 

   If any of the callbacks to a client fails, the server removes
   information about the client without further ado. If the client is
   alive and makes a subsequent call, the client must be
   reinitialized. (In the current implementation the server does not
   check whether a client is registered before executing a call.)
*)

END Board.

