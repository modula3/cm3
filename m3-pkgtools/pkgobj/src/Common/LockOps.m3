(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* LockOps.m3 *)
(* Last modified on Fri Jun  4 16:59:13 PDT 1993 by wobber *)
(*      modified on Fri Jan  4 15:59:46 GMT+1:00 1991 by prusker *)

MODULE LockOps;
(*
   This is the package locking interface which the local lock server
   presents to its packagetool clients.  This interface does not
   depend on the existence of packagetool siphons, but it does
   provide consistent semantics if one or more siphons do exist.
   Locks may either be managed by the local site, in which case the
   actual lock key is held in the local database, or managed remotely,
   in which case the local database contains a pointer to the managing
   site.
*)

IMPORT LockMethods, Site, Text, Thread;
IMPORT Fingerprint, NetObj, IP, TCPNetObj, PkgProt, PkgErr;
FROM PkgErr IMPORT Aborted, LockServerDown;

VAR
  default: T := NIL;
  
PROCEDURE New (): T RAISES {PkgErr.E} =
  VAR siteT: Site.T;
  VAR obj: NetObj.T;
      addr: IP.Address;
  BEGIN
    TRY
      IF default # NIL THEN RETURN default; END;
      siteT := Site.Get();
      IF siteT.ipPort = IP.NullPort THEN
        obj := NetObj.Import(
          PkgProt.LockExportName, NetObj.Locate(siteT.lockserver));
      ELSE
        IF NOT IP.GetHostByName(siteT.lockserver, addr) THEN
          PkgErr.Raise(LockServerDown);
        END;
        obj := NetObj.Import(
          PkgProt.LockExportName,
          TCPNetObj.Locate(IP.Endpoint{addr, siteT.ipPort}));
      END;
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | IP.Error(ipErr) =>
        PkgErr.Raise(LockServerDown, ipErr);
    | NetObj.Invalid =>
        PkgErr.Raise(LockServerDown);
    | NetObj.Error(ec) =>
        PkgErr.Raise(LockServerDown, ec);
    END;
    IF (obj = NIL) OR NOT ISTYPE(obj, T) THEN
      PkgErr.Raise(LockServerDown);
    END;
    RETURN NARROW(obj, T);
  END New;

PROCEDURE SetServerT(t: T) =
  BEGIN
    default := t;
  END SetServerT;
  
PROCEDURE Create
  (auth: Auth; pn: PN; initialKey: Text.T;
   version: Version := Version{0,InitialVN};
   remoteCheck: BOOLEAN := TRUE; t: T := NIL) RAISES {PkgErr.E} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.create (auth, pn, initialKey, version, remoteCheck);
    EXCEPT
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    | Thread.Alerted => PkgErr.Raise(Aborted);
    END;
  END Create;

PROCEDURE Remove
  (auth: Auth; pn: PN; key: Text.T; reship: BOOLEAN := TRUE;
   t: T := NIL) RAISES {PkgErr.E, CommitFailed, LockConflict} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.remove(auth, pn, key, reship);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END Remove;

PROCEDURE Lock
  (auth: Auth; pn: PN; version: Version; key: Text.T;
   keySite: SiteName := NIL; t: T := NIL): Version
   RAISES {PkgErr.E, LockConflict} =
  VAR v: Version;
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
       v := t.lock (auth, pn, version, key, keySite);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
    RETURN v;
  END Lock;

PROCEDURE Unlock
  (auth: Auth; pn: PN; version: Version; key: Text.T;
   keySite: SiteName := NIL; breakRights: BreakRights := BreakRights.OwnerOnly;
   forceVersion: BOOLEAN := FALSE; t: T := NIL)
   RAISES {PkgErr.E, CommitFailed, LockConflict, SynchVersions} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.unlock (auth, pn, version,
                key, keySite, breakRights, forceVersion);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END Unlock;

PROCEDURE AssignVersion
  (auth: Auth; pn: PN; key: Text.T; keySite: SiteName := NIL;
   t: T := NIL): Version RAISES {PkgErr.E, LockConflict} =
  VAR v: Version;
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      v := t.assign (auth, pn, key, keySite);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
    RETURN v;
  END AssignVersion;

PROCEDURE Commit
  (auth: Auth; pn: PN; version: Version;
   VAR ships: ShipArray; reship: BOOLEAN := TRUE; t: T := NIL): CommitFailures
   RAISES {CommitFailed, PkgErr.E} =
  VAR res: CommitFailures;
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      res := t.commit (auth, pn, version, ships, reship);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
    RETURN res;
  END Commit;

(* lock enumeration *)

PROCEDURE Enumerate
  (dir: Dir; site: SiteName := NIL; locksOnly: BOOLEAN := TRUE;
   localOnly: BOOLEAN := TRUE; pendingOnly: BOOLEAN := FALSE;
   t: T := NIL) : EnumList RAISES {PkgErr.E} =
  VAR res: EnumList;
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      res := t.enumerate (dir, site, locksOnly, localOnly, pendingOnly);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
    RETURN res;
  END Enumerate;

PROCEDURE GetEntry
  (pn: PN; goRemote: BOOLEAN := FALSE; t: T := NIL) : RefEntry
   RAISES {PkgErr.E} =
  VAR e: RefEntry;
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      e := t.getEntry (pn, goRemote);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
    RETURN e;
 END GetEntry;

(* the following two procedures are administrative tools *)
(* they apply only to the database at the target machine *)

PROCEDURE SetEntry (auth: Auth; pn: PN; entry: RefEntry; t: T := NIL)
   RAISES {PkgErr.E} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.setEntry (auth, pn, entry);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END SetEntry;

PROCEDURE SetFingerprint
  (auth: Auth; pn: PN; version: Version; fp: Fingerprint.T; t: T := NIL)
   RAISES {PkgErr.E} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.setFingerprint (auth, pn, version, fp);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END SetFingerprint;

(* the following two procedures are called only through siphon ops *)
(* they apply only to the database at the target machine *)

PROCEDURE CreateCheck(pn: PN; t: T := NIL) RAISES {PkgErr.E} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.createCheck (pn);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END CreateCheck;

PROCEDURE CreateForeign
  (auth: Auth; pn: PN; owningSite: SiteName; instance: Instance;
   t: T := NIL) RAISES {PkgErr.E} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.createForeign (auth, pn, owningSite, instance);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END CreateForeign;


(*************************)
(* new public procedures *)
(*************************)

PROCEDURE CreateDir
  (auth: Auth; dir: Dir; t: T := NIL) RAISES {PkgErr.E} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.createDir (auth, dir);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END CreateDir;

PROCEDURE RemoveDir
  (auth: Auth; dir: Dir; t: T := NIL) RAISES {PkgErr.E} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.removeDir (auth, dir);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END RemoveDir;

PROCEDURE CheckDir (name: Dir; t: T := NIL) RAISES {PkgErr.E} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.checkDir (name);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END CheckDir;

PROCEDURE EnumerateDirs
  (site: SiteName := NIL; t: T := NIL): DirList RAISES {PkgErr.E} =
  VAR res: DirList;
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      res := t.enumerateDirs (site);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
    RETURN res;
  END EnumerateDirs;

PROCEDURE RemoveForeign
  (auth: Auth; pn: PN; t: T := NIL) RAISES {PkgErr.E, CommitFailed} =
  BEGIN
    IF t = NIL THEN t := New ();  END;
    TRY
      t.removeForeign (auth, pn);
    EXCEPT
    | Thread.Alerted => PkgErr.Raise(Aborted);
    | NetObj.Error(ec) => PkgErr.Raise(LockServerDown, ec);
    END;
  END RemoveForeign;

BEGIN
END LockOps.
