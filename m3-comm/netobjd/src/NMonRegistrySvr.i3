(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NMonRegistrySvr.i3 *)
(* Last modified on Mon Sep 14 11:46:21 PDT 1992 by evers  *)

(* An implementation of the "NetObjMon.Registry" service. *)

INTERFACE NMonRegistrySvr;

IMPORT NetObjMon;

PROCEDURE New (): NetObjMon.Registry;

END NMonRegistrySvr.
