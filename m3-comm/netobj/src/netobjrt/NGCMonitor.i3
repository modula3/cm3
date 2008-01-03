(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NGCMonitor.i3 *)
(* Last modified on Tue Jul 20 10:26:51 PDT 1993 by wobber *)
(*      modified on Tue Sep 15 10:12:15 PDT 1992 by evers  *)

(* The interface "NGCMonitor" provides a way of inspecting the state
   of the network object runtime at a program. *)

INTERFACE NGCMonitor;

IMPORT WireRep, DirtyElem, Fingerprint, Transport, RefList;

TYPE
  (* A value of type "Dump" is a snapshot of the state of the network
     object garbage collector in some program. *)

  Dump = REF RECORD
    concs: RefList.T (* of CDump *) := NIL;
    srgts: RefList.T (* of SDump *) := NIL;
    locs:  RefList.T (* of LDump *) := NIL;
  END;

  ODump = OBJECT
    obj: WireRep.T;
    fp: Fingerprint.T;
    typeName: TEXT;
  END;

  CDump = ODump OBJECT
    pinCount: CARDINAL;
  END;

  SDump = ODump OBJECT
    owner: Transport.Endpoint;
  END;

  LDump = REF RECORD
    info: TEXT;
    ep: Transport.Endpoint;
    exports: RefList.T (* of DDump *) := NIL;
  END;

  DDump = REF RECORD
    wrep: WireRep.T;
    de: DirtyElem.T;
  END;

  NDump = REF RECORD
    name: TEXT;    (* in export table *)
    obj: WireRep.T;
  END;

PROCEDURE MonitorDump() : Dump;
  (* return snapshot of local network object state *)

PROCEDURE MonitorDumpNames() : RefList.T (*of NDump*);
  (* return snapshot of local agent table *)

END NGCMonitor.
