(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSRestart.m3 *)
(* Last modified on Tue Feb 22 14:10:09 PST 1994 by wobber *)
(*      modified on Tue Dec  3 19:05:00 GMT+1:00 1991 by prusker *)

(* this module ensures that the local package database is up to date *)

MODULE PSRestart;

IMPORT Fmt;
IMPORT LockOps, Pathname, PackageObj, PkgErr, PathMap, TextList,
       PackageEvents, PackageLib, PSLib, TextIntTbl, Site, Siphon;
IMPORT FileSys, NetObj, OSError, Text, Thread, Time;

VAR
  scanner: Thread.T;
  oneok: BOOLEAN := FALSE;

  <* FATAL Thread.Alerted *>

TYPE
  ScanClosure = Thread.SizedClosure OBJECT
  OVERRIDES
    apply := ScanFork;
  END;

PROCEDURE Recover(init: BOOLEAN) =
  VAR
    dirs := PathMap.GetReps();
  BEGIN
    IF init THEN
      IF dirs # NIL THEN
        FOR i := 0 TO LAST (dirs^) DO ScanScope(dirs[i]); END;
      END;
    END;
    RemoveTmp(dirs);
    IF scanner = NIL THEN
      scanner := Thread.Fork (NEW(ScanClosure,
        stackSize := 2 * Thread.GetDefaultStackSize()));
    END;
  END Recover;

PROCEDURE RemoveTmp (dirs: LockOps.DirList)  =
    (* remove all directories: sub-repository/tmp *)
  VAR vols: TextList.T;
      tmp: FileSys.FN;
  BEGIN
    PSLib.LogIt("RR: Removing temporary directories");
    IF dirs # NIL THEN
      FOR i := 0 TO LAST (dirs^) DO
        <* FATAL PkgErr.E *>
        BEGIN
          vols := PathMap.GetVols(dirs[i]);
        END;
        WHILE vols # NIL DO
          tmp := Pathname.Join(vols.head, "tmp", NIL);
          TRY
            FileSys.Remove(tmp, TRUE);
          EXCEPT
          | OSError.E(fsEC) =>
              IF FileSys.ClassifyError(fsEC) # FileSys.ErrorClass.Lookup THEN
                PSLib.LogIt("RR: FS error removing " & tmp);
              END;
          END;
          vols := vols.tail;
        END;
      END;
    END;
  END RemoveTmp;

PROCEDURE ScanFork (<*UNUSED*> cl: ScanClosure): REFANY =
  CONST sixHours = 6.0D1 * 3.6D3;
  BEGIN
    LOOP (* forever *)
      EVAL DoScan ();
      Thread.Pause (sixHours);
    END;
    <*NOWARN*> RETURN NIL;
  END ScanFork;

PROCEDURE DoScan (): (*completed*) BOOLEAN =
  VAR locks: LockOps.EnumList;
      dirs: LockOps.DirList;
  BEGIN
    PSLib.LogIt("RR: Starting scan");
    TRY
      dirs := LockOps.EnumerateDirs();
      IF dirs = NIL THEN RETURN TRUE; END;
      FOR i := 0 TO LAST(dirs^) DO
        locks := LockOps.Enumerate (dirs[i], NIL, FALSE, FALSE, FALSE);
        ScanLocks (dirs[i], locks);
            (* later, we should garbage collect packages here *)
            (* too risky to do that automatically now *)
      END;
      PSLib.LogIt("RR: Scan complete");
      oneok := TRUE;
      RETURN TRUE;
    EXCEPT
      | PkgErr.E =>
          PSLib.LogIt("RR: Error at lock server");
    END;
    RETURN FALSE;
  END DoScan;

PROCEDURE ScanLocks (dir: LockOps.Dir; locks: LockOps.EnumList) =
  VAR
    v, nv: LockOps.Version;
    snarf: BOOLEAN;
    pn: PackageObj.PN;
    pkgT: PackageObj.T;
    <* FATAL NetObj.Error, PkgErr.E *>
  BEGIN
    IF locks = NIL THEN RETURN; END;
    pkgT := PackageObj.New (NIL);
    FOR i := 0 TO LAST (locks^) DO
      nv.t := locks[i].e.instance;
      nv.vn := locks[i].e.curVN;
      pn := PackageObj.PN{dir, locks[i].arc};
      snarf := FALSE;
      TRY
        v := pkgT.version(pn);
        snarf := TRUE;
      EXCEPT
      | PkgErr.E(ec) =>
          IF ec.head = PkgErr.NoSuchPackage THEN
            snarf := TRUE;
            v.t := 0;
            v.vn := LockOps.NullVN;
          ELSIF ec.head = PkgErr.NoSuchDir THEN
            IF NOT oneok THEN
              PSLib.LogIt("RR: No repository for " & PSLib.PkgText(pn));
            END;
          END;
      END;
      IF snarf AND ((v.vn # nv.vn) OR (v.t # nv.t)) THEN
        SnarfPackage (pkgT, pn, v, nv);
      ELSE
                  (* don't hog the (Ultrix) uni-processor *)
        Thread.Pause (1.0D-2); (* 10 ms *)
      END;
    END;
  END ScanLocks;

EXCEPTION AbortIt;

PROCEDURE SnarfPackage(
    pkgT: PackageObj.T; pn: PackageObj.PN; v, nv: LockOps.Version) =
  VAR
    ship: PackageObj.Ship;
    source: PackageObj.Source;
    checkV: LockOps.Version;
    ts: Time.T;
    replica: TEXT;
    options: PackageObj.ShipOptions;
    name := PSLib.PkgText(pn);
    <* FATAL PackageObj.SourceOutOfDate *>
  BEGIN
    ship := NIL;
    options.purgeLinks := TRUE;
    options.forceDateMatch := TRUE;
    options.keepBackup := FALSE;
    source := PickAServer (pn, nv, TRUE, replica);
    IF (source = NIL) AND ((nv.vn # LockOps.InitialVN) OR (v.t = nv.t)) THEN
            (* don't snarf empty "InitialVN" iff same instance is present *)
      PSLib.LogIt("RR: can\'t find " & name & " (" & Fmt.Int (nv.t) & "."
                     & Fmt.Int (nv.vn) & ")");
    ELSE
      IF source # NIL THEN
        PSLib.LogIt("RR: snarfing " & name & " from " & replica & ", ("
                       & Fmt.Int (v.t) & "." & Fmt.Int (v.vn) & " -> "
                       & Fmt.Int (nv.t) & "." & Fmt.Int (nv.vn) & ")");
      ELSE
        PSLib.LogIt("RR: creating " & name & " (" & Fmt.Int (nv.t) & "."
                       & Fmt.Int (nv.vn) & ")");
        source := PackageLib.EmptySource();
      END;
      ts := GetPkgTimestamp (pn);
      TRY
        ship := pkgT.newShip (PSLib.SystemAuth, pn, options);
        ship.prepare (source, NIL, NEW(Mon));
            (* now do another GetVersion, don't smash a more recent version *)
        TRY
          checkV := pkgT.version (pn);
          IF (checkV.t # v.t) OR (checkV.vn # v.vn) THEN RAISE AbortIt;  END;
        EXCEPT
        | PkgErr.E =>
            IF (v.vn # LockOps.NullVN) THEN RAISE AbortIt;  END;
        END;
            (* be even more paranoid ... check the package dir timestamp *)
        IF ts # GetPkgTimestamp (pn) THEN RAISE AbortIt;  END;
        ship.commit(nv);
        PSLib.LogIt("RR: snarf of " & name & " complete");
      EXCEPT
      | NetObj.Error(ec) =>
          PSLib.LogIt("RR: snarf of " & name & " failed: " & PkgErr.Msg(ec));
      | PkgErr.E(ec) =>
          PSLib.LogIt("RR: snarf of " & name & " failed: " & PkgErr.Msg(ec));
      | AbortIt =>
          PSLib.LogIt("RR: snarf of " & name &
                   " aborted, local version changed");
      END;
      ship := NIL;
      source := NIL;
    END;
  END SnarfPackage;

PROCEDURE PickAServer
  (pn: PackageObj.PN; VAR (*IN/OUT*) v: LockOps.Version; exactmatch: BOOLEAN;
   VAR (*OUT*)    replica: Text.T): PackageObj.Source =
    (* returns always the correct instance *)
    (* if exactmatch, the exact vn must be found *)
  VAR
    this, prevReplica: Text.T;
    start, i: CARDINAL;
    testV, prevV: LockOps.Version;
    source, prevSource: PackageObj.Source;
    reps: PSLib.ReplicaSet;
    pkgT: PackageObj.T;
  BEGIN
    reps := PSLib.GetReplicas ();
    start := ROUND(Time.Now()) MOD NUMBER (reps^);
    i := start;
    prevSource := NIL;
    REPEAT
      INC (i);
      IF i = NUMBER (reps^) THEN i := 0;  END;
      this := reps[i];
      TRY
        pkgT := PackageObj.New (this);
        source := pkgT.newSource (PSLib.SystemAuth, pn, testV);
        IF (testV.t = v.t) AND (testV.vn = v.vn) THEN
          prevSource := NIL;
          replica := this;
          RETURN source;
        ELSE
          IF exactmatch OR (testV.t # v.t)
            OR ((prevSource # NIL) AND (prevV.vn >= testV.vn)) THEN
            source := NIL;
          ELSE
            prevV := testV;
            prevSource := source;
            prevReplica := this;
          END;
        END;
      EXCEPT
      | NetObj.Error, PkgErr.E =>
      END;
    UNTIL (i = start);
    IF prevSource # NIL THEN v := prevV; replica := prevReplica;  END;
    RETURN prevSource;
  END PickAServer;

PROCEDURE GetPkgTimestamp (pn: PackageObj.PN): Time.T =
    <* FATAL PkgErr.E *>
  BEGIN
    TRY
      RETURN FileSys.GetInfo(PathMap.MapPkg(pn), TRUE).date;
    EXCEPT
      | OSError.E =>
    END;
    RETURN 0.0D0;
  END GetPkgTimestamp;

PROCEDURE ScanScope (scope: PackageObj.Dir) =
  VAR
    t: Text.T;
    pT: PackageObj.T;
    dirEnum: FileSys.Enumeration;
    renum := EnumRemoteDir(scope);
    <* FATAL NetObj.Error, PkgErr.E *>
  BEGIN
    pT := PackageObj.New(NIL);
    PSLib.LogIt("RR: Enumerating package scope " & PSLib.PathText(scope));
    TRY
      dirEnum := FileSys.Enumerate (PathMap.MapDir(scope));
    EXCEPT
    | OSError.E(ec) =>
        PSLib.LogIt("RR: Scope enum failed - " & PkgErr.Msg(ec));
        RETURN;
    END;
    WHILE dirEnum # NIL DO
      t := dirEnum.head;
      dirEnum := dirEnum.tail;
      IF NOT Text.Equal (t, PSLib.TemporaryDir) AND
            NOT Text.Equal (t, PSLib.BackupDir) THEN
        CheckLockEntry(pT, PackageObj.PN{scope, t}, renum);
      END;
    END;
  END ScanScope;

PROCEDURE CheckLockEntry(
    pT: PackageObj.T; pn: PackageObj.PN; renum: TextIntTbl.T) =
  VAR
    v: LockOps.Version;
    inst: INTEGER;
  BEGIN
    TRY
      EVAL LockOps.GetEntry(pn);
        (* this entry already exists ... LockRestart recovered it *)
    EXCEPT
    | PkgErr.E =>
            (* no entry ... must create one if there is no
               remote version or if the local version instance is
               more recent the remote one. *)
        TRY
          v := pT.version(pn);
          IF NOT renum.get(pn.arc, inst) OR v.t > inst THEN
            IF v.vn = LockOps.NullVN THEN
              v.t := TRUNC(Time.Now());
              v.vn := LockOps.InitialVN + 1;
              IF NOT pT.vcommit(
                   PSLib.SystemAuth, pn, v,
                   LockOps.Version{0, LockOps.NullVN}) THEN
                RAISE PkgErr.E(NIL);
              END;
            END;
            LockOps.Create(PSLib.SystemAuth, pn, NIL, v, FALSE);
            PSLib.LogIt("RR: CreateLockEntry: " &
                              PSLib.PkgText(pn) & ", v=" &
                              Fmt.Int(v.t) & "." & Fmt.Int(v.vn));
          END;
        EXCEPT
        | NetObj.Error, PkgErr.E =>
           PSLib.LogIt("RR: CreateLockEntry failed: " & PSLib.PkgText(pn));
        END;
    END;
  END CheckLockEntry;

PROCEDURE EnumRemoteDir(dir: LockOps.Dir) : TextIntTbl.T =
  VAR
    remotes := Site.Get().foreignSites;
    res := NEW(TextIntTbl.Default).init();
    arc: TEXT;
    en: LockOps.EnumList;
    inst: INTEGER;
  BEGIN
    (* here we make a table of all the packages thought to be managed
       remotely and of their version instance numbers *)
    IF remotes # NIL THEN
      FOR i := 0 TO LAST (remotes^) DO
        TRY
          (* enumerate remote - NOT lockOnly, NOT localOnly *)
          en := LockOps.Enumerate (dir, NIL, FALSE, FALSE, FALSE,
                                  Siphon.New(remotes[i].name).lockserver());
          IF en # NIL THEN
            FOR j := 0 TO LAST(en^) DO
              IF NOT Text.Equal(PSLib.localSite, en[j].e.managedBy) THEN
                arc := en[j].arc;
                IF res.get(arc, inst) THEN
                  inst := MAX(inst, en[j].e.instance);
                ELSE
                  inst := en[j].e.instance;
                END;
                EVAL res.put(arc, inst);
              END;
            END;
          END;
        EXCEPT
        | NetObj.Error, PkgErr.E =>
            PSLib.LogIt("RR: WARNING: RemoteEnumerate failed of " &
                                       PSLib.PathText(dir) & " at " &
                                       remotes[i].name);
        END;
      END;
    END;
    RETURN res;
  END EnumRemoteDir;

TYPE
  Mon = PackageObj.Monitor OBJECT
  OVERRIDES
    report := Report;
  END;

PROCEDURE Report(<*UNUSED*> mon: Mon; arg: REFANY) =
  VAR op: TEXT := NIL;
  BEGIN
    TYPECASE arg OF
    | PackageEvents.FileReport(up) =>
        CASE up.type OF
        | PackageEvents.FileET.New => op := "new";
        | PackageEvents.FileET.Updated => op := "updated";
        | PackageEvents.FileET.Removed => op := "removed";
        | PackageEvents.FileET.ChangeMode => op := "changeMode";
        END;
        op := "FileReport: " & op & " " & PSLib.PathText(up.path);
    | PackageEvents.LinkReport(up) =>
        CASE up.type OF
        | PackageEvents.LinkET.Installed => op := "export";
        | PackageEvents.LinkET.Removed => op := "removedExport";
        | PackageEvents.LinkET.NoDir => op := "noDir";
        | PackageEvents.LinkET.Denied => op := "denied";
        | PackageEvents.LinkET.Bad => op := "badFileName";
        END;
        op := "LinkReport: " & op & " " & PSLib.PathText(up.path);
    ELSE
    END;
    IF op # NIL THEN PSLib.LogIt(op); END;
  END Report;

BEGIN
END PSRestart.


(******************)
(* new procedures *)
(******************)

(*
)PROCEDURE Copy (sourcePkg, targetPkg: PackageObj.PN;
                 version: PackageObj.Version) : BOOLEAN =
    (* check no same package ??? *)
  VAR
    pkgT: PackageObj.T;
    source: PackageObj.Source;
    ship: PackageObj.Ship;
    servername: TEXT;
    fromLocal := TRUE;
    thisV: PackageObj.Version;
    options: PackageObj.ShipOptions;
  BEGIN
          (* should check not same package ??? *)
    TRY
      pkgT := PackageObj.New (NIL);
    EXCEPT
    | NetObj.Error, NetObj.Invalid, PkgErr.E => <* ASSERT FALSE *>
    END;
    ship := NIL;
    source := NIL;
    options.purgeLinks := TRUE;
    options.forceDateMatch := TRUE;
    options.keepBackup := FALSE;
    TRY
      ship := pkgT.newShip(PSLib.SystemAuth, targetPkg, options);
      TRY
        source := pkgT.newSource(PSLib.SystemAuth, sourcePkg, thisV);
        IF thisV.t # version.t THEN
          source := NIL;
        ELSE
          fromLocal := TRUE;
        END;
      EXCEPT
      | NetObj.Error, PkgErr.E =>
      END;
      IF source = NIL THEN
        thisV.vn := version.vn;
        thisV.t := version.t;
        source := PickAServer (sourcePkg, thisV, TRUE, servername);
                  (* PickAServer returns always the correct instance *)
        IF source = NIL THEN
          PSLib.LogIt("Copy of " & PSLib.PkgText(sourcePkg)
                         & " (" & Fmt.Int (version.t)
                         & "." & Fmt.Int (version.vn)
                         & " failed: version not found");
          RETURN FALSE;
        END;
      ELSE
        servername := "local server";
      END;
      PSLib.LogIt("Copying " & PSLib.PkgText(sourcePkg)
                     & " (" & Fmt.Int (version.t)
                     & "." & Fmt.Int (version.vn)
                     & ") from " & servername);
      ship.prepare (source, NIL, NEW(Mon));
      ship.commit(version);
      PSLib.LogIt("Copy to " & PSLib.PkgText((targetPkg) & " complete");
    EXCEPT
    | NetObj.Error(ec) =>
         PSLib.LogIt("Copy to " & PSLib.PkgText((targetPkg) &
              " failed: " & PkgErr.Msg(ec));
         RETURN FALSE;
    | PkgErr.E(ec) =>
         PSLib.LogIt("Copy to " & PSLib.PkgText((targetPkg) &
              " failed: " & PkgErr.Msg(ec));
         RETURN FALSE;
    END;
    ship := NIL;
    source := NIL;
    RETURN TRUE;
  END Copy;

*)
