(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* The "ClientInfo" interface is for the board server to store and
   access information about a client. 
*)

INTERFACE ClientInfo;

IMPORT RectR, NetObj, Thread,
       Callback;

TYPE  T = NetObj.T OBJECT 
  METHODS
    getScope (): RectR.T RAISES {NetObj.Error, Thread.Alerted};
    setScope (scope: RectR.T) RAISES {NetObj.Error, Thread.Alerted};
    getCallback (): Callback.T RAISES {NetObj.Error, Thread.Alerted};
  END;

(* A "ClientInfo.T" is a network object owned by the board server to
   store information about a client. Also, as described below, the
   object serves as an identifier for the client (hence the need for a
   network object).

   When a  client registers with a board server, the server
   creates a "ClientInfo.T" object for it, and returns a reference to
   it to the client. The client passes the reference in subsequent
   calls, which allows the server to identify the client.

   The methods "setScope" and "getScope" access the cached information
   about the scope of the client. The method "getCallback" retrieves
   the client's callback object.
*)

PROCEDURE Equal (ci1, ci2: T): BOOLEAN;

END ClientInfo.
