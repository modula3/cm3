(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Siphon.i3 *)
(* Last modified on Thu May  6 16:00:31 PDT 1993 by wobber  *)
(*      modified on Fri Jul  3  9:12:58 GMT+2:00 1992 by prusker *)

INTERFACE Siphon;

IMPORT TextList, NetObj, PackageObj, PkgErr, LockOps, Thread;

FROM PackageObj IMPORT PN, Source;
FROM LockOps IMPORT Version, SiteName;

CONST SiphonExportName = "Siphon";

TYPE
  Sites = REF ARRAY OF SiteName;
  SynchKind =
    {CheckOnly,                (* does nothing *)
     UpdateSelf,               (* update local site from network *)
     UpdateAll                 (* push updates from local site to world *)
     };

TYPE
  SiteList = TextList.T (* of SiteName *);

TYPE T = NetObj.T OBJECT METHODS
    ship(
        package: PN; 
        source: Source;
        version: Version;
        caller: SiteName; (* used for stats *)
        manager: SiteName;
        fwdSites: SiteList)
        RAISES {PkgErr.E, NetObj.Error, Thread.Alerted};
        (* ship a package across the siphon *)
        (* target should forward this package to "fwdRoute" *)
    enqueue(
        package: PN; 
        version: Version;
        manager: SiteName; 
        forSite: SiteName;
        urgent: BOOLEAN := FALSE)
        RAISES {PkgErr.E, NetObj.Error, Thread.Alerted};
        (* enqueue the named package for transmission *)
        (* if forSite=NIL,  send to all sites in siphon.config *)
        (* manager used only if the package has to be created *)
        (* if urgent, package is put at the beginning of the send queue *)
    dequeue(
        package: PN; forSite: SiteName := NIL;
        interruptSend: BOOLEAN := FALSE) : BOOLEAN
        RAISES {PkgErr.E, NetObj.Error, Thread.Alerted};
        (* removes package from send queue regardless of instance *)
        (* if forSite is NIL, removes package from all send queues *)
        (* result indicates whether a dequeue actually occured *)
    synch(kind: SynchKind; package: PN): TEXT 
        RAISES {NetObj.Error, Thread.Alerted};
        (* initiate skulker synchronization *)
        (* synchronize all packages if package = NIL*)
        (* result text tells what happened *)
        (* if kind=checkOnly, result contains what should be done *)
    lockserver() : LockOps.T RAISES {PkgErr.E, NetObj.Error, Thread.Alerted};
    status() : TEXT RAISES {NetObj.Error, Thread.Alerted};
  END;

PROCEDURE New(site: SiteName): T
   RAISES {PkgErr.E, NetObj.Error, Thread.Alerted};
   (* NIL means local site *)

PROCEDURE SetServerT(t: T; site: SiteName);
   (* register local server object *)

END Siphon.


