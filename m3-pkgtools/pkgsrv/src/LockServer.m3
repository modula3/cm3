(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* LockServer.m3 *)
(* Last modified on Fri Apr  7 16:04:48 PDT 1995 by wobber *)
(*      modified on Thu Feb  2 09:17:46 PST 1995 by kalsow *)
(*      modified on Wed Nov 13 11:43:03 GMT+1:00 1991 by prusker *)

MODULE LockServer;

IMPORT Fmt;
IMPORT Atom, AtomList, Fingerprint, Text, Thread;
IMPORT LockMethods, LockOps, PackageDB, PackageLib, PSLib, Site;
IMPORT NetObj, PkgErr, PackageObj, Siphon, Subrepo;

TYPE
  SpecialShip = {InitIt, DeleteIt};
  Auth = LockOps.Auth;
  PN = LockOps.PN;
  SiteName = LockOps.SiteName;
  ShipArray = LockOps.ShipArray;
  Instance = LockOps.Instance;
  Version = LockOps.Version;
  Entry = LockOps.Entry;
  RefEntry = LockOps.RefEntry;
  BreakRights = LockOps.BreakRights;
  EnumList = LockOps.EnumList;
  Dir = LockOps.Dir;
  DirList = LockOps.DirList;
  CommitFailures = LockOps.CommitFailures;

CONST
  InitialVN = LockOps.InitialVN;
  DeletedVN = LockOps.DeletedVN;

CONST
  Indent = "              ";

TYPE
  T = LockOps.T BRANDED OBJECT
  OVERRIDES
    create := Create;
    remove := Remove;
    lock := Lock;
    unlock := Unlock;
    assign := Assign;
    commit := Commit;
    enumerate := Enumerate;
    getEntry := GetEntry;
    setEntry := SetEntry;
    setFingerprint := SetFingerprint;
    createCheck := CreateCheck;
    createForeign := CreateForeign;
    createDir := CreateDir;
    removeDir := RemoveDir;
    checkDir := CheckDir;
    enumerateDirs := EnumerateDirs;
    removeForeign := RemoveForeign;
  END;
  
PROCEDURE New() : LockOps.T =
  BEGIN
    RETURN NEW (T);
  END New;

(* methods for "me" *)

VAR NoOldVersion: Atom.T;

PROCEDURE Create(
    <*UNUSED*> t: T; auth: Auth; package: PN;
    initialKey: Text.T; version: Version; remoteCheck: BOOLEAN)
  RAISES {PkgErr.E, Thread.Alerted} =
  PROCEDURE CreateWork(package: PN) RAISES {PkgErr.E} =
    VAR remotes: REF ARRAY OF Site.Remote;
    BEGIN
      IF NOT remoteCheck THEN RETURN; END;
      TRY
        remotes := Site.Get().foreignSites;
        IF remotes = NIL THEN RETURN; END;
        FOR i := 0 TO LAST (remotes^) DO
          IF Subrepo.Has(remotes[i].name, package) # Subrepo.R.No THEN
            LockOps.CreateCheck (package, RemoteLockT(remotes[i].name));
          END;
        END;
      EXCEPT
      | Thread.Alerted => PkgErr.Raise(PkgErr.Aborted);
      | NetObj.Error(ec) =>
          PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
      | PkgErr.E(ec) =>
          PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
      END;
    END CreateWork;
  BEGIN
    CheckUser (auth);
    PackageDB.CreateLocal (package, initialKey, version, CreateWork);
    IF version.t = 0 THEN
      TRY
            (* now locally commit the first real version *)
            (* this is to propagate the initial version number *)
        ShipSpecial (auth, package, SpecialShip.InitIt);
      EXCEPT
      | LockOps.CommitFailed, PkgErr.E =>
      END;
    ELSE
      SiphonEnqueue (package, version, PSLib.localSite, TRUE);
    END;
    IF initialKey = NIL THEN initialKey := ""; END;
    PSLib.LogFmt(
       "PL.Create %s, user=%s, key=%s, v=%s.%s\n",
       PSLib.TA{PSLib.PkgText(package), auth, initialKey,
                Fmt.Int(version.t), Fmt.Int(version.vn)});
  END Create;

PROCEDURE Remove
  (<*UNUSED*> t: T; auth: Auth; package: PN; key: Text.T; reship: BOOLEAN)
   RAISES {PkgErr.E, LockOps.CommitFailed, LockOps.LockConflict,
           Thread.Alerted} =
  VAR v: Version;
  BEGIN
        (* add empty check here *)
    CheckUser (auth);
    TRY
      v := PackageDB.AssignVersion (package, TRUE, key);
      ShipSpecial (auth, package, SpecialShip.DeleteIt, reship);
    EXCEPT
      | PackageDB.NotManager =>
          PkgErr.Raise (PkgErr.PackageManagedRemotely);
    END;
    PSLib.LogFmt("PL.Remove %s, user=%s\n",
           PSLib.TA{PSLib.PkgText(package), auth});
  END Remove;

PROCEDURE Lock
  (<*UNUSED*> t: T; auth: Auth; package: PN; version: Version; key: Text.T;
   keySite: SiteName): Version RAISES {PkgErr.E, LockOps.LockConflict,
                                       Thread.Alerted} =
  VAR v: Version; e: PackageDB.Entry;
  BEGIN
    CheckUser (auth);
    TRY
      IF keySite = NIL THEN version := PackageDB.NullVersion ();  END;
      v := PackageDB.Lock (package, version, key, keySite);
    EXCEPT
      | PackageDB.NotManager(site) =>
          IF keySite # NIL THEN
            PkgErr.Raise (PkgErr.PackageNotManaged);
          ELSE
            PackageDB.GetEntry (package, e);
                      (* call the remote siphon here *)
            v.t := e.instance;
            v.vn := e.curVN;
            TRY
              v := LockOps.Lock (PSLib.SystemAuth, package, v,
                                 key, PSLib.localSite,
                                 RemoteLockT(site));
            EXCEPT
            | NetObj.Error(ec) =>
                  PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
            | PkgErr.E(ec) =>
                  PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
            END;
          END;
    END;
    PSLib.LogFmt(
       "PL.Lock %s, user=%s\n%skey=%s, site=%s, v=%s.%s\n",
       PSLib.TA{PSLib.PkgText(package), auth, Indent,
                key, keySite, Fmt.Int(v.t), Fmt.Int(v.vn)});
    RETURN v;
  END Lock;

PROCEDURE Unlock
  (<*UNUSED*> t: T; auth: Auth; package: PN; version: Version;
   key: Text.T; keySite: SiteName; breakRights: BreakRights;
   forceVersion: BOOLEAN)
   RAISES {PkgErr.E, LockOps.CommitFailed, LockOps.LockConflict,
           LockOps.SynchVersions, Thread.Alerted} =
  VAR
    v: Version;
    e: PackageDB.Entry;
  BEGIN
    CheckUser (auth);
    LOOP
      TRY
        TRY
          IF keySite = NIL THEN version := PackageDB.NullVersion ();  END;
          PackageDB.Unlock (package, version, key, keySite, breakRights,
                            forceVersion);
        EXCEPT
          | PackageDB.NotManager(site) =>
              IF keySite # NIL THEN
                PkgErr.Raise (PkgErr.PackageNotManaged);
              ELSE
                IF breakRights = BreakRights.AnySite THEN
                             (* not strictly necessary, a policy control *)
                  PkgErr.Raise (PkgErr.PackageManagedRemotely);
                END;
                PackageDB.GetEntry (package, e);
                              (* call the remote siphon here *)
                v.t := e.instance;
                v.vn := e.curVN;
                TRY
                  LockOps.Unlock (PSLib.SystemAuth, package, v,
                                  key, PSLib.localSite, breakRights,
                                  forceVersion, RemoteLockT(site));
                EXCEPT
                | NetObj.Error(ec) =>
                      PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
                | PkgErr.E(ec) =>
                      PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
                END;
              END;
        END;
        EXIT;
      EXCEPT
        | LockOps.SynchVersions(sver) =>
            IF keySite = NIL THEN
              UpgradeVersion (auth, package, sver);
            ELSE
              RAISE LockOps.SynchVersions (sver);
            END;
      END;
    END;
    PSLib.LogFmt(
       "PL.Unlock %s, user=%s, site=%s, (%s, %s)\n",
       PSLib.TA{ PSLib.PkgText(package), auth, keySite,
                Fmt.Int(ORD(breakRights)), Fmt.Int(ORD(forceVersion))});
  END Unlock;

PROCEDURE Assign
  (<*UNUSED*> t: T; auth: Auth; package: PN; key: Text.T;
     keySite: SiteName) : Version
     RAISES {PkgErr.E, LockOps.LockConflict, Thread.Alerted} =
  VAR v: Version;
  BEGIN
    CheckUser (auth);
    TRY
      v := PackageDB.AssignVersion (package, FALSE, key, keySite);
    EXCEPT
      | PackageDB.NotManager(site) =>
          IF keySite # NIL THEN
            PkgErr.Raise (PkgErr.PackageNotManaged);
          ELSE
            TRY
              v := LockOps.AssignVersion (PSLib.SystemAuth,
                                          package, key, PSLib.localSite,
                                          RemoteLockT(site));
            EXCEPT
            | NetObj.Error(ec) =>
                  PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
            | PkgErr.E(ec) =>
                  PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
            END;
          END;
    END;
    PSLib.LogFmt(
       "PL.Assign %s, user=%s, site=%s, vn=%s\n",
       PSLib.TA{PSLib.PkgText(package), auth, keySite, Fmt.Int(v.vn)});
    RETURN v;
  END Assign;

PROCEDURE Commit
  (<*UNUSED*> t: T; auth: Auth; package: PN; version: Version;
      VAR ships: ShipArray; reship: BOOLEAN): CommitFailures
   RAISES {LockOps.CommitFailed, PkgErr.E} =
  VAR r: CommitFailures;
  BEGIN
    CheckUser (auth);
    IF NUMBER (ships) = 0 THEN
      PkgErr.Raise (PkgErr.BadParameter);
    END;
    r := DoCommit (auth, package, version, ships);
    IF reship THEN
      VAR e: Entry; BEGIN
        PackageDB.GetEntry (package, e);
        SiphonEnqueue (package, version, e.managedBy);
      END;
    END;
    RETURN r;
  END Commit;

PROCEDURE Enumerate
  (<*UNUSED*> t: T; dir: Dir; site: SiteName; locksOnly: BOOLEAN;
   localOnly: BOOLEAN; pendingOnly: BOOLEAN): EnumList
   RAISES {PkgErr.E, Thread.Alerted} =
  BEGIN
    IF (site = NIL) OR Text.Equal (site, PSLib.localSite) THEN
      RETURN
      PackageDB.Enumerate (dir, locksOnly, localOnly, pendingOnly);
    ELSE
      TRY
        RETURN
           LockOps.Enumerate (dir, NIL, locksOnly, localOnly,
                          pendingOnly, RemoteLockT(site));
      EXCEPT
      | NetObj.Error(ec) =>
          <*NOWARN*> PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
      | PkgErr.E(ec) =>
          <*NOWARN*> PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
      END;
    END;
  END Enumerate;

PROCEDURE GetEntry
  (<*UNUSED*> t: T; package: PN; goRemote: BOOLEAN) : RefEntry
   RAISES {PkgErr.E, Thread.Alerted} =
  VAR mgr: SiteName;
      res: RefEntry := NEW(RefEntry);
  BEGIN
    PackageDB.GetEntry (package, res^);
    mgr := res.managedBy;
    IF goRemote AND  NOT Text.Equal (PSLib.localSite, mgr) THEN
      TRY
        res := LockOps.GetEntry (package, goRemote, RemoteLockT(mgr));
        IF NOT Text.Equal (res.managedBy, mgr) THEN
          PkgErr.Raise (PkgErr.PackageNotManaged);
        END;
      EXCEPT
      | NetObj.Error(ec) =>
          PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
      | PkgErr.E(ec) =>
          PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
      END;
    END;
    RETURN res;
  END GetEntry;

PROCEDURE SetEntry
  (<*UNUSED*> t: T; auth: Auth; package: PN; entry: RefEntry)
   RAISES {PkgErr.E} =
  BEGIN
    CheckUser (auth);
    PackageDB.SetEntry (package, entry^);
  END SetEntry;

PROCEDURE SetFingerprint
  (<*UNUSED*> t: T; auth: Auth; package: PN; version: Version;
   fp: Fingerprint.T) RAISES {PkgErr.E} =
  BEGIN
    CheckUser (auth);
    PackageDB.SetFingerprint (package, version, fp);
  END SetFingerprint;

PROCEDURE CreateCheck(<*UNUSED*> t: T; package: PN)
  RAISES {PkgErr.E} =
  BEGIN
    PackageDB.CreateCheck (package);
  END CreateCheck;

PROCEDURE CreateForeign
  (<*UNUSED*> t: T; auth: Auth; package: PN; owningSite: SiteName;
   instance: Instance) RAISES {PkgErr.E} =
  BEGIN
    CheckUser (auth);
    PackageDB.CreateForeign (package, owningSite, instance);
  END CreateForeign;

(* package commit *)
TYPE
  CommitControl = Thread.Closure OBJECT
    next: CommitControl;
    i: CARDINAL;
    auth: Auth;
    error: PkgErr.TL;
    package: PN;
    version, prevVersion: Version;
    thread: Thread.T;
    versionOnly: BOOLEAN;
    name: Text.T;              (* only for commit version *)
    ship: PackageObj.Ship;   (* only for normal commit *)
  OVERRIDES
    apply := CommitFork;
  END;

  CommitClosure = PackageDB.CommitClosure OBJECT
    cc: CommitControl;
    failures: BOOLEAN := FALSE;
  OVERRIDES
    work := CommitWork;
  END;

PROCEDURE UpgradeVersion (auth: Auth; package: PN; version: Version)
    RAISES {PkgErr.E, LockOps.CommitFailed} =
  VAR
    e: Entry;
    rs: PSLib.ReplicaSet;
    cc, ccl: CommitControl;
  BEGIN
    PackageDB.GetEntry (package, e);
    <* ASSERT (version.t = e.instance) *>
    rs := PSLib.GetReplicas ();
    FOR i := 0 TO LAST (rs^) DO
      cc :=  NEW (CommitControl);
      cc.next := ccl;
      cc.auth := auth;
      cc.package := package;
      cc.version := version;
      cc.prevVersion.t := e.instance;
      cc.prevVersion.vn := e.curVN;
      cc.name := rs[i];
      cc.error := NIL;
      cc.versionOnly := TRUE;
      ccl := cc;
    END;
    EVAL CommitInternal (ccl);
    SiphonEnqueue (package, version, e.managedBy, TRUE);
  END UpgradeVersion;

PROCEDURE ShipSpecial
  (auth: Auth; package: PN; which: SpecialShip; reship: BOOLEAN := TRUE)
  RAISES {PkgErr.E, LockOps.CommitFailed, Thread.Alerted} =
  VAR
    e: Entry;
    version: Version;
    rs: PSLib.ReplicaSet;
    cc, ccl: CommitControl;
    options: PackageObj.ShipOptions;
    pkgT: PackageObj.T;
    <* FATAL PackageObj.SourceOutOfDate *>
  BEGIN
        (* for creating initial or deleted versions at replicas *)
    options.keepBackup := FALSE;
    options.purgeLinks := TRUE;
    options.forceDateMatch := FALSE;
    PackageDB.GetEntry (package, e);
    version.t := e.instance;
    CASE which OF
    | SpecialShip.InitIt =>
        version.vn := InitialVN;
    | SpecialShip.DeleteIt =>
        version.vn := DeletedVN;
    END;
    rs := PSLib.GetReplicas ();
    FOR i := 0 TO LAST (rs^) DO
      cc :=  NEW (CommitControl);
      cc.next := ccl;
      cc.auth := auth;
      cc.package := package;
      cc.version := version;
      cc.error := NIL;
      cc.versionOnly := FALSE;
      ccl := cc;
      TRY
        pkgT := PackageObj.New (rs[i]);
        cc.ship := pkgT.newShip (auth, package, options);
        cc.ship.prepare (PackageLib.EmptySource(), NIL, NIL);
      EXCEPT
      | NetObj.Error(ec) => cc.error := ec;
      | PkgErr.E(pmEC) => cc.error := pmEC;
      END;
    END;
    EVAL CommitInternal (ccl);
    IF reship THEN
      SiphonEnqueue (package, version, e.managedBy);
    END;
  END ShipSpecial;

PROCEDURE DoCommit
  (auth: Auth; package: PN; version: Version; VAR ships: ShipArray)
   : CommitFailures RAISES {LockOps.CommitFailed, PkgErr.E} =
  VAR cc, ccl: CommitControl;
  BEGIN
    ccl := NIL;
    FOR i := 0 TO LAST (ships) DO
      IF ships[i] # NIL THEN
        cc :=  NEW (CommitControl);
        cc.i := i;
        cc.next := ccl;
        cc.auth := auth;
        cc.package := package;
        cc.version := version;
        cc.error := NIL;
        cc.versionOnly := FALSE;
        cc.ship := ships[i];
        ccl := cc;
      END;
    END;
    RETURN CommitInternal (ccl);
  END DoCommit;

PROCEDURE CommitInternal (ccl: CommitControl): CommitFailures
    RAISES {PkgErr.E, LockOps.CommitFailed} =
  VAR
    out: TEXT;
    cc: CommitControl;
    res: CommitFailures;
    ok: BOOLEAN := FALSE;
    cl := NEW(CommitClosure, cc := ccl);
  BEGIN
    res := NIL;
    ok := PackageDB.Commit (ccl.package, ccl.version, cl);
    IF ccl.versionOnly THEN
      out := "PL.Upgrade " & PSLib.PkgText(ccl.package) & ", v="
                     & Fmt.Int (ccl.version.t) & "."
                     & Fmt.Int (ccl.version.vn) & ", ok=" & Bool (ok);
    ELSE
      out := "PL.Commit " & PSLib.PkgText(ccl.package) & ", v="
                     & Fmt.Int (ccl.version.t) & "."
                     & Fmt.Int (ccl.version.vn) & ", ok=" & Bool (ok);
    END;
    IF cl.failures THEN
      out := out & "\n" & Indent;
      res :=  NEW (CommitFailures, ccl.i + 1);
      cc := ccl;
      WHILE cc # NIL DO
        res[cc.i] := cc.error;
        IF cc.error # NIL THEN
          out := out & " " & Fmt.Int(cc.i) & ":" & PkgErr.Msg(cc.error);
        END;
        cc := cc.next;
      END;
    END;
    PSLib.LogIt(out);
    IF NOT ok THEN RAISE LockOps.CommitFailed (res);  END;
    RETURN res;
  END CommitInternal;

PROCEDURE CommitWork (cl: CommitClosure): BOOLEAN RAISES {} =
  VAR cc: CommitControl; ok: BOOLEAN; waste: REFANY;
  BEGIN
    ok := FALSE;
    cc := cl.cc;
    WHILE cc # NIL DO
      IF cc.error = NIL THEN
        cc.thread := Thread.Fork (cc);
      ELSE
        cl.failures := TRUE;
        cc.thread := NIL;
      END;
      cc := cc.next;
    END;
    cc := cl.cc;
    WHILE cc # NIL DO
      IF cc.thread # NIL THEN
        waste := Thread.Join (cc.thread);
        IF cc.error = NIL THEN
          ok := TRUE;
        ELSE
          cl.failures := TRUE;
        END;
      END;
      cc := cc.next;
    END;
    RETURN ok;
  END CommitWork;

PROCEDURE CommitFork (cc: CommitControl): REFANY =
  VAR ok: BOOLEAN;
      pkgT: PackageObj.T;
    <* FATAL Thread.Alerted *>
  BEGIN
    TRY
      IF cc.versionOnly THEN
        pkgT := PackageObj.New (cc.name);
        ok := pkgT.vcommit (
          cc.auth, cc.package, cc.version, cc.prevVersion);
        IF NOT ok THEN cc.error := AtomList.List1(NoOldVersion);  END;
      ELSIF cc.ship # NIL THEN
        cc.ship.commit(cc.version);
      END;
    EXCEPT
    | NetObj.Error(ec) => cc.error := ec;
    | PkgErr.E(pmEC) => cc.error := pmEC;
    END;
    RETURN NIL;
  END CommitFork;

(* private stuff *)

PROCEDURE Bool (b: BOOLEAN): Text.T =
  BEGIN
    IF b THEN RETURN "TRUE";  ELSE RETURN "FALSE";  END;
  END Bool;

PROCEDURE CheckUser (auth: Auth) RAISES {PkgErr.E} =
  BEGIN
    IF auth = NIL THEN
      PkgErr.Raise(PkgErr.InvalidCredentials);
    END;
        (*
        pswdOK := FALSE;
        (* allow the firefly password (hack), see OS.def/mod *)
        IF Text.Equal(user, PSLib.SystemUserName) THEN RETURN; END;
        TRY
          uT := User.LookupUserByName(user);
          IF uT # NIL THEN
            pswdOK := User.CheckPassword(uT, pwd, TRUE);
          END;
        EXCEPT
        | User.Fault:
        END;
        IF NOT pswdOK THEN
          RAISE PkgErr.E( PkgErr.InvalidCredentials);
        END;
        *)
  END CheckUser;

(*************************)
(* new public procedures *)
(*************************)

PROCEDURE CreateDir
  (<*UNUSED*> t: T; auth: Auth; dir: Dir)
  RAISES {PkgErr.E} =
  BEGIN
    CheckUser (auth);
    PackageDB.CreateDir (dir);
    PSLib.LogFmt("PL.CreateRep %s, user=%s\n",
                 PSLib.TA{PSLib.PathText(dir), auth});
  END CreateDir;

PROCEDURE RemoveDir
  (<*UNUSED*> t: T; auth: Auth; dir: Dir)
  RAISES {PkgErr.E} =
  BEGIN
    CheckUser (auth);
    PackageDB.RemoveDir (dir);
    PSLib.LogFmt("PL.RemoveRep %s, user=%s\n",
                          PSLib.TA{PSLib.PathText(dir), auth});
  END RemoveDir;

PROCEDURE CheckDir (<*UNUSED*> t: T; name: Dir)
    RAISES {PkgErr.E} =
  BEGIN
    PackageDB.CheckDir (name);
  END CheckDir;

PROCEDURE EnumerateDirs (<*UNUSED*> t: T; site: SiteName): DirList
    RAISES {PkgErr.E, Thread.Alerted} =
  BEGIN
    IF (site = NIL) OR Text.Equal (site, PSLib.localSite) THEN
      RETURN PackageDB.EnumerateDirs ();
    ELSE
      TRY
        RETURN LockOps.EnumerateDirs (NIL, RemoteLockT(site));
      EXCEPT
      | NetObj.Error(ec) =>
          <*NOWARN*> PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
      | PkgErr.E(ec) =>
          <*NOWARN*> PkgErr.Raise (PkgErr.RemoteLockProblem, ec);
      END;
    END;
  END EnumerateDirs;

PROCEDURE RemoveForeign
  (<*UNUSED*> t: T; auth: Auth; package: PN)
   RAISES {PkgErr.E, LockOps.CommitFailed, Thread.Alerted} =
  VAR e: Entry;
  BEGIN
    CheckUser (auth);
    PackageDB.GetEntry (package, e);
    IF Text.Equal (e.managedBy, PSLib.localSite) THEN
      PkgErr.Raise (PkgErr.BadParameter);
    END;
    ShipSpecial (auth, package, SpecialShip.DeleteIt, FALSE);
    PSLib.LogFmt("PL.RemoveForeign %s, user=%s\n",
         PSLib.TA{PSLib.PkgText(package), auth});
  END RemoveForeign;

PROCEDURE RemoteLockT(site: SiteName) : LockOps.T
    RAISES {PkgErr.E, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN Siphon.New(site).lockserver();
  END RemoteLockT;


(* enqueuing things for siphon *)

VAR
  siphonT: Siphon.T := NIL;
  siphonTLock := NEW(MUTEX);

TYPE
  EnqueueFork = Thread.Closure OBJECT
    package: PN;
    version: LockOps.Version;
    managedBy: LockOps.SiteName;
    urgent: BOOLEAN;
  OVERRIDES
    apply := SiphonEnqueueFork;
  END;

PROCEDURE SiphonEnqueue
  (package: PN; version: LockOps.Version;
   managedBy: LockOps.SiteName; urgent: BOOLEAN := FALSE) =
  BEGIN
    EVAL Thread.Fork(NEW(EnqueueFork, package := package, version := version,
                        managedBy := managedBy, urgent := urgent));
  END SiphonEnqueue;

PROCEDURE SiphonEnqueueFork(f: EnqueueFork) : REFANY =
  VAR sT: Siphon.T;
      retry := FALSE;
    <* FATAL Thread.Alerted *>
  BEGIN
    REPEAT
      TRY
        LOCK siphonTLock DO
          IF retry THEN siphonT := NIL; END;
          IF siphonT = NIL THEN
            siphonT := Siphon.New(NIL);
            retry := FALSE;
          ELSE
            retry := TRUE;
          END;
          sT := siphonT;
        END;
        sT.enqueue(f.package, f.version, f.managedBy, NIL, f.urgent);
        RETURN NIL;
      EXCEPT
      | PkgErr.E, NetObj.Error =>
      END;
    UNTIL NOT retry;
    RETURN NIL;
  END SiphonEnqueueFork;

BEGIN
  NoOldVersion := Atom.FromText("Commit version failed: NotHoldingOldVersion");
END LockServer.
