(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjMonInit.i3 *)
(* Last modified on Thu Apr 15 11:34:35 PDT 1993 by wobber *)
(*      modified on Thu Aug 27 09:24:00 PDT 1992 by evers  *)

(* The "NetObjMonInit" interface is integral to network object runtime
   initialization.  It is carefully crafted to avoid initialization
   circularities.  Beware of changing it. *)

INTERFACE NetObjMonInit;


PROCEDURE InitMonitorStubs();

(* "InitMonitorStubs" registers stubs for the type "NetObjMon.T". *)

PROCEDURE InitRegistryStubs();

(* "InitRegistryStubs" registers stubs for the type "NetObjMon.Registry". *)

END NetObjMonInit.
