(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjMon.i3 *)
(* Last modified on Tue Jul 20 10:14:45 PDT 1993 by wobber *)
(*      modified on Tue Sep 15 10:12:15 PDT 1992 by evers  *)

(* The interface "NetObjMon" provides a way of inspecting the state
   of the network object runtime at a program. *)

INTERFACE NetObjMon;

IMPORT NetObj, RefList, Thread;

TYPE
  (* a "NetObjMon.T" is used to query the state of the network object
     runtime, and to simulate the death of surrogate objects and remote
     program instances *)

  T = NetObj.T OBJECT METHODS
    dump (): REFANY  (* NGCMonitor.Dump *)
      RAISES {NetObj.Error, Thread.Alerted};
    dumpNames (): RefList.T (* of NGCMonitor.NDump *)
      RAISES {NetObj.Error, Thread.Alerted};
  END;

(* Binding to "NetObjMon.T"s is performed via a "NetObjMon.Registry". *)

TYPE
  Registry = NetObj.T OBJECT METHODS
    register (t: T; id: TEXT)
      RAISES {NetObj.Error, Thread.Alerted};
    list (): REF ARRAY OF TEXT
      RAISES {NetObj.Error, Thread.Alerted};
      (* "list" would return an array of "(id: TEXT, t: T)" pairs,
          but for the fact that if any of the owners of the "t"s
          are dead, it won't be possible to unmarshal {\em any} of
          the "t"s.  So we give a list of "id"s, which can be passed
          as needed to "get", below. *)
    get (id: TEXT): T
      RAISES {NetObj.Error, Thread.Alerted};
  END;

CONST
  RegistryServiceName = "NetObjMon.Registry";

PROCEDURE Register ();   (* create and register a local monitor *)

END NetObjMon.
