(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* LockMethods.i3 *)
(* Last modified on Tue May  4 17:24:21 PDT 1993 by wobber *)
(*      modified on Fri Jan  4 15:59:46 GMT+1:00 1991 by prusker *)

INTERFACE LockMethods;

IMPORT LockOps, Fingerprint, PkgErr, NetObj, Thread;

FROM LockOps IMPORT Auth, Dir, PN, BreakRights, CommitFailures, DirList,
                    EnumList, Instance, RefEntry, ShipArray, SiteName, Version;
  
REVEAL
  LockOps.T = NetObj.T BRANDED "LockOps.T" OBJECT METHODS
    create(
        auth: Auth; pn: PN; initialKey: TEXT;
        version: Version; remoteCheck: BOOLEAN)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    remove(auth: Auth; package: PN; key: TEXT; reship: BOOLEAN)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted,
                LockOps.CommitFailed, LockOps.LockConflict};
    lock(
        auth: Auth; pn: PN;
        version: Version; key: TEXT; keySite: SiteName) : Version
        RAISES {NetObj.Error, PkgErr.E, LockOps.LockConflict,
                Thread.Alerted};
    unlock(
        auth: Auth; pn: PN;
        version: Version; key: TEXT; keySite: SiteName;
        breakRights: BreakRights; forceVersion: BOOLEAN)
        RAISES {NetObj.Error, PkgErr.E, LockOps.CommitFailed,
                         LockOps.LockConflict, LockOps.SynchVersions,
                         Thread.Alerted};
    assign(
        auth: Auth; pn: PN;
        key: TEXT; keySite: SiteName) : Version
        RAISES {NetObj.Error, PkgErr.E, LockOps.LockConflict,
                Thread.Alerted};
    commit(
        auth: Auth; pn: PN; version: Version;
        VAR ships: ShipArray; reship: BOOLEAN) : CommitFailures
        RAISES {NetObj.Error, LockOps.CommitFailed, PkgErr.E,
                Thread.Alerted};
    enumerate(
        dir: Dir; site: SiteName;
        locksOnly, localOnly, pendingOnly: BOOLEAN): EnumList
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    getEntry(pn: PN; goRemote: BOOLEAN) : RefEntry
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    setEntry(auth: Auth; pn: PN; entry: RefEntry)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    setFingerprint(
        auth: Auth; pn: PN; version: Version; fp: Fingerprint.T)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    createCheck(pn: PN)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    createForeign (
        auth: Auth; pn: PN; owningSite: SiteName; instance: Instance)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    createDir (auth: Auth; dir: Dir)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    removeDir(auth: Auth; dir: Dir)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    checkDir(dir: Dir)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    enumerateDirs(site: SiteName): DirList
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    removeForeign(auth: Auth; pn: PN)
        RAISES {NetObj.Error, PkgErr.E, LockOps.CommitFailed,
                Thread.Alerted};
  END;
  
END LockMethods.
