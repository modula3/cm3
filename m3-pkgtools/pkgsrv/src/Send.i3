(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Send.i3 *)
(* Last modified on Tue May  4 17:44:56 PDT 1993 by wobber  *)
(*      modified on Wed Jul  1 15:13:25 GMT+2:00 1992 by prusker *)

INTERFACE Send;

IMPORT LockOps, PackageObj, PkgErr;

FROM LockOps IMPORT SiteName, Version;

PROCEDURE Enqueue(
    pn: PackageObj.PN;
    version: Version;
    manager: SiteName;
    forSite: SiteName;
    urgent: BOOLEAN := FALSE)
    RAISES {PkgErr.E};
    (* forSite=NIL means all send queues *)

PROCEDURE Dequeue(
    pn: PackageObj.PN;
    forSite: SiteName;
    interruptSend: BOOLEAN) : BOOLEAN
    RAISES {PkgErr.E};
    (* removes package from send queue regardless of instance *)
    (* if forSite is NIL, removes package from all send queues *)
    (* result indicates whether a dequeue actually occured *)

PROCEDURE Showqueues(): TEXT;
    (* NIL result means all queues empty *)

END Send.
