(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjInit.i3 *)
(* Last modified on Thu Apr 15 12:20:28 PDT 1993 by wobber *)
(*      modified on Thu Aug 27 09:24:00 PDT 1992 by evers  *)

(* The "NetObjInit" interface is integral to network object runtime
   initialization.  It is carefully crafted to avoid initialization
   circularities.  Beware of changing it. *)

INTERFACE NetObjInit;

PROCEDURE InitAgentStubs();

(* "InitAgentStubs" registers stubs for the type "SpecialObj.T". *)

PROCEDURE InitVoucherStubs();

(* "InitVoucherStubs" registers stubs for the type "Voucher.T". *)

END NetObjInit.
