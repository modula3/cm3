(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* TCPNetObj.i3 *)
(* Last modified on Tue Jun  9 16:24:50 PDT 1992 by wobber *)

INTERFACE TCPNetObj;
  
IMPORT IP, NetObj;

EXCEPTION Failed;

PROCEDURE Locate (ep: IP.Endpoint) : NetObj.Address;
   (* ep.address = IP.NullAddress implies the local host *)
   (* port = IP.NullPort means use the default agent port *)
   
PROCEDURE Listen (port: IP.Port) : NetObj.Address RAISES {Failed};
   (* port = IP.NullPort means use the default agent port *)
   (* starts a agent listener at the specified address *)
   
END TCPNetObj.



