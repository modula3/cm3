(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* TransportRegistry.i3 *)
(* Last modified on Fri Sep 11 14:07:02 PDT 1992 by evers  *)
(*      modified on Tue Jul 28 03:19:20 1992 by wobber *)

(* The "TransportRegistry" interface provides the network objects runtime
   with access to the registry of transport  classes known within the
   current runtime environment. *)
   
INTERFACE TransportRegistry;
   
IMPORT NetObj, Transport;
      
PROCEDURE LocationFromAdr(
  where: NetObj.Address) : Transport.Location;

(* "LocationFromAdr" calls "tr.fromEndpoint" for each member 
   of "where" for each transport "tr" known to the runtime.  If any 
   such call returns a non-"NIL" result, it is returned.  Otherwise, 
   "LocationFromAdr" returns "NIL". *)
  
PROCEDURE LocalAdr() : NetObj.Address;

(* "LocalAdr" calls "tr.toEndpoint" for each transport "tr" known
    to the runtime, and returns the list as a "NetObj.Address"s. *)
     
PROCEDURE Iterate() : Iterator;

(* "Iterate" returns an "Iterator" over the known transports. *)

TYPE
  Iterator <: IteratorPublic;
  IteratorPublic = OBJECT METHODS
    next (VAR (*OUT*) tr: Transport.T): BOOLEAN;
  END;

(* If "it" is the result of the call "Iterate()", then the call
   "it.next(tr)" selects a "Transport.T" that has not already
   been returned by "it", sets "tr" to that "Transport.T", and
   returns "TRUE".  If no transports remain, the call returns "FALSE"
   without setting "tr".  It is a checked runtime error to call
   "next" after it has returned "FALSE". *)

END TransportRegistry.



