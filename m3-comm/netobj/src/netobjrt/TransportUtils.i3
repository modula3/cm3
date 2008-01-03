(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* TransportUtils.i3 *)
(* Last modified on Thu Jun  3 12:48:46 PDT 1993 by wobber *)
(*      modified on Mon Sep 14 09:56:14 PDT 1992 by evers  *)

(* The "TransportUtils" interface reveals methods of "Transport.T"s
   and "Transport.Location"s which are useful for management and
   debugging tools. *)

INTERFACE TransportUtils;

IMPORT Transport;

REVEAL
  Transport.T <: Public;
  Transport.Location <: LocationP;

TYPE
  Public = Transport.Public OBJECT METHODS
    enumerateLocs (p: EnumProc; cl: REFANY := NIL);
  END;

  EnumProc = PROCEDURE (loc: Transport.Location; cl: REFANY): BOOLEAN;

  LocationP = Transport.LocationP OBJECT METHODS
    getInfo (): TEXT;
    getEp (): Transport.Endpoint;
  END;

END TransportUtils.

(* The call "tr.enumerateLocs (p, cl)" should invoke "p (loc, cl)" for
each live "loc: Location" owned by "tr".  The enumeration should
terminate after the first invocation of "p" which returns "TRUE".

The method "loc.getInfo" should return a "TEXT" suitable for
presentation to a human which identifies the location amongst those
managed by the transport.

The method "loc.getEp" should return a "Transport.Endpoint" "e"
such that "loc = tr.fromEndpoint (e)".
*)
