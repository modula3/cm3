(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "NotifyRec" interface defines the structure of the record
   logged by the board to capture information provided in a client call,
   which the board subsequently uses to notify other clients.
*)

INTERFACE NotifyRec;

IMPORT RectR,
       Item, ClientInfo;

TYPE T = REF RECORD
    code: Code;
    doer: ClientInfo.T;
    ids: Item.IDArray := NIL;
    its: Item.TArray := NIL;
    newScope := RectR.Empty;
    additive: BOOLEAN;
  END;

  Code = {Scope, Create, Modify, Delete};

(* The board logs a record with different "code"s for different
   methods invoked by the client. The identity of the client making
   the call is  stored as the "doer".

   A "setScope" call results in a record with code "Scope". The
   only other useful attribute is "newScope": the new scope of the
   client window. (In the implementation, the old scope of the window
   is not overwritten until the record is used.)

   A "createItems" call results in a record with code "Create". 
   A "modifyItem" call results in a record with code "Modify".   
   The field "additive" records whether the modification is additive
   (see the "Board" interface.) The values of items are recorded in "its".

   A "deleteItems" call results in a record with code "Delete".   
   The "ids" field records the IDs of the items deleted.
*)

PROCEDURE Equal (nr1, nr2: T): BOOLEAN;
(* TO satisfy generic types. *)

END NotifyRec.
