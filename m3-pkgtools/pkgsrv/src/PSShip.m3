(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSShip.m3 *)
(* Last modified on Fri Apr  7 15:51:04 PDT 1995 by wobber *)
(*      modified on Thu Feb  2 08:55:52 PST 1995 by kalsow *)

MODULE PSShip;

IMPORT Fmt, OSError, AtomList;
IMPORT FileSys, NetPath, Pathname, Text, Thread, Time, Rd, Wr;
IMPORT LockOps, NetObj, PackageObj, PackageLib, PkgErr, PSGet,
        PackageEvents, PSExportLink, PSLib, WeakRef, RefList;

TYPE
  FN = FileSys.FN;
  PN = PackageObj.PN;
  ExportLinks = PackageObj.ExportLinks;
  FileType = PackageObj.FileType;
  Options = PackageObj.ShipOptions;
  Version = PackageObj.Version;

  State = {Idle, Preparing, Prepared, Committed};

REVEAL
  T = PackageObj.Ship BRANDED OBJECT
    mu: MUTEX;
    pn: PN;
    pkgPath: FN;
    realPackage: FN;
    tempDir: FN := NIL;
    opts: PackageObj.ShipOptions;
    ignoreContent: BOOLEAN;
    get: PSGet.T;
    state: State := State.Idle;
    doBackup: BOOLEAN := FALSE;
    en: PackageObj.Enum := PackageObj.Enum{0.0D0, NIL};
    links: ExportLinks := NIL;
    actions: PSExportLink.WriteActions := NIL;
    pulling: NetPath.T := NIL;
    pullWait: Thread.Condition;
  OVERRIDES
    (* ship methods *)
    prepare := Prepare;
    commit := Commit;
    pullFile := PullFile;
  END;

PROCEDURE New (pn: PN; pkgPath, realName: FN; get: PSGet.T;
   ignoreContent: BOOLEAN; options: Options): T =
  VAR t: T;
  BEGIN
    (* CheckUser(name, passwd); *)
    t := NEW (T,
        mu := NEW(MUTEX),
        pn := pn,
        pkgPath := pkgPath,
        realPackage := realName,
        get := get,
        opts := options,
        doBackup := options.keepBackup,
        ignoreContent := ignoreContent,
        pullWait := NEW(Thread.Condition));
    PSLib.StatIncr (PSLib.StatShipInProgress);
    EVAL WeakRef.FromRef(t, CleanupT);
    RETURN t;
  END New;

PROCEDURE CleanupT (<*UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
  VAR t: T := r;
  BEGIN
    LOCK t.mu DO
      PSLib.StatDecr (PSLib.StatShipInProgress);
      (* IF t.e # NIL THEN t.e.cleanup(); t.e := NIL; END; *)
      t.get := NIL;
      IF t.tempDir # NIL THEN
        TRY FileSys.Remove(t.tempDir, TRUE); EXCEPT OSError.E => END;
      END;
    END;
  END CleanupT;


CONST XferBufferBytes = 32768;

TYPE
  DiffClosure = PackageLib.DiffClosure OBJECT
    t: T;
    err: PkgErr.TL := NIL;
    res: PackageEvents.PrepareReport;
    changed := FALSE;
    source: PackageObj.Source;
    siblings: PackageObj.Siblings;
    mon: PackageObj.Monitor;
    buf: ARRAY [0..XferBufferBytes-1] OF CHAR;
    frpt: PackageEvents.FileReport;
    oldSourcePath: NetPath.T := NIL;
    monCount: INTEGER := -1;
    monThread: Thread.T;
  OVERRIDES
    report := ShipDiffReport;
  END;

PROCEDURE Prepare (
     t: T; source: PackageObj.Source;
     siblings: PackageObj.Siblings;
     mon: PackageObj.Monitor)
     RAISES {PackageObj.SourceOutOfDate, PkgErr.E, Thread.Alerted} =
  BEGIN
    PSLib.StatIncr(PSLib.StatPrepareInProgress);
    TRY
      TryPrepare(t, source, siblings, mon);
    FINALLY
      PSLib.StatDecr(PSLib.StatPrepareInProgress);
    END;
  END Prepare;

PROCEDURE TryPrepare (
     t: T; source: PackageObj.Source;
     siblings: PackageObj.Siblings;
     mon: PackageObj.Monitor)
     RAISES {PackageObj.SourceOutOfDate, PkgErr.E, Thread.Alerted} =
  VAR lclEnum: PackageObj.DirEnum := NIL;
      lclLinks: ExportLinks;
      cl := NEW(DiffClosure,
           t := t, mon := mon, source := source, siblings := siblings,
           res := NEW(PackageEvents.PrepareReport),
           frpt := NEW(PackageEvents.FileReport));
  BEGIN
    LOCK t.mu DO
      IF t.state # State.Idle OR source = NIL THEN
        PkgErr.Raise(PkgErr.BadParameter);
      ELSE
        t.state := State.Preparing;
      END;
    END;
    TRY
      IF t.get # NIL AND NOT t.ignoreContent THEN
        lclEnum := t.get.enum().dir;
        lclLinks := t.get.links();
      END;
      t.en := source.enum();
      t.links := source.links();
      PackageLib.Compare(t.en.dir, lclEnum, cl);
      IF cl.oldSourcePath # NIL THEN
        RAISE PackageObj.SourceOutOfDate(cl.oldSourcePath);
      END;
    EXCEPT
    | NetObj.Error(err) =>
        cl.err := AtomList.Cons(PkgErr.SourceFailed, err);
    | PkgErr.E(err) =>
        cl.err := AtomList.Cons(PkgErr.SourceFailed, err);
    END;
    IF cl.err # NIL THEN
      PSLib.LogIt("PM.Prepare " & PSLib.PkgText(t.pn) 
                                & ": "  & PkgErr.Msg(cl.err));
      RAISE PkgErr.E(cl.err);
    END;

    t.actions := PSExportLink.Prepare(
          t.pn, t.pkgPath, lclLinks,
          t.links, t.opts.purgeLinks, mon);
    t.doBackup := t.doBackup AND cl.changed;
    cl.res.keptBackup := t.doBackup;
    t.state := State.Prepared;
    PSLib.StatIncr (PSLib.StatPrepares);
    PSLib.LogIt(
      "PM.Prepare " & PSLib.PkgText(t.pn) & ", chgd="
                   & Fmt.Int (ORD (cl.changed)) & ", bk="
                   & Fmt.Int (ORD (cl.res.keptBackup)));
    IF mon # NIL THEN
      TRY
        mon.report(cl.res);
      EXCEPT
      | NetObj.Error =>
      END;
    END;
  END TryPrepare;

PROCEDURE ShipDiffReport(
    cl: DiffClosure;
    dir: FN;
    type: PackageLib.DiffType;
    elem: PackageObj.DirElem)
    RAISES {PackageLib.Stop, Thread.Alerted} =
  VAR t := cl.t;
      tempPath: FN;
      path := Pathname.Join(dir, elem.arc, NIL);
      <* FATAL NetPath.Invalid *>
  PROCEDURE Note(et: PackageEvents.FileET) RAISES {Thread.Alerted} =
    BEGIN
      IF cl.mon = NIL THEN RETURN; END;
      cl.frpt.type := et;
      cl.frpt.path := NetPath.FromRelFN(path);
      TRY
        cl.mon.report(cl.frpt);
      EXCEPT
      | NetObj.Error =>
      END;
    END Note;
  BEGIN
    TRY
      CASE type OF
      | PackageLib.DiffType.Same =>
      | PackageLib.DiffType.NoSrc =>
          cl.changed := TRUE;
          Note (PackageEvents.FileET.Removed);
          RETURN;
      | PackageLib.DiffType.SrcOlder =>
          IF NOT t.opts.forceDateMatch THEN
            cl.oldSourcePath := NetPath.FromRelFN(path);
            RAISE PackageLib.Stop;
          ELSE
            cl.changed := TRUE;
          END;
      ELSE
        cl.changed := TRUE;
      END;
      IF t.tempDir = NIL THEN
        t.tempDir :=
             PSLib.PrefixDirName (t.realPackage, PSLib.TemporaryDir, TRUE);
        FileSys.MakeDir(t.tempDir);
      END;
      tempPath := Pathname.Join(t.tempDir, path, NIL);
      CASE elem.info.type OF
      | PackageObj.FileType.Dir =>
          FileSys.MakeDir(tempPath);
      | PackageObj.FileType.SLink =>
          FileSys.SymLink(tempPath, elem.referent);
      | PackageObj.FileType.Normal =>
          IF type = PackageLib.DiffType.Same OR
                type = PackageLib.DiffType.ModesDiffer THEN
            VAR
              hardLinkOK: BOOLEAN := FALSE;
            PROCEDURE TryHardLink(dirx: FN) RAISES {Thread.Alerted} =
              BEGIN
                TRY
                  FileSys.HardLink(tempPath, Pathname.Join(dirx, path, NIL));
                  IF type = PackageLib.DiffType.ModesDiffer THEN
                    (* this changes the file mode in the potential backup!! *)
                    (* this isn't perfect ... but it avoids the copy *)
                     FileSys.SetMode(tempPath, elem.info.perm);
                     Note (PackageEvents.FileET.ChangeMode);
                  END;
                  hardLinkOK := TRUE;
                EXCEPT
                | OSError.E => (* fall through to update on failed link *)
                END;
              END TryHardLink;
            BEGIN
              t.get.doItLocked(TryHardLink);
              IF hardLinkOK THEN
                INC(cl.res.filesUnchanged);
                RETURN;
              END;
            END;
          END;
          FetchFile(cl, path, tempPath, elem.info);
          FileSys.SetMode (tempPath, elem.info.perm);
          FileSys.SetModifiedDate (tempPath, elem.info.date);
          IF type = PackageLib.DiffType.NoDest THEN
            Note (PackageEvents.FileET.New);
          ELSE
            Note (PackageEvents.FileET.Updated);
          END;
          INC(cl.res.filesPulled);
      | FileType.Other => <* ASSERT (FALSE) *>
      END;
      RETURN;
    EXCEPT
    | OSError.E(osErr) => cl.err := PkgErr.MapOSError(osErr);
    | NetObj.Error(nErr) =>
        cl.err := AtomList.Cons(PkgErr.SourceFailed, nErr);
    | PkgErr.E(pmErr) =>
        cl.err := AtomList.Cons(PkgErr.SourceFailed, pmErr);
    | Rd.Failure(ec) =>
        cl.err := AtomList.Cons(PkgErr.SourceFailed, PkgErr.MapOSError(ec));
    | Wr.Failure(ec) => cl.err := PkgErr.MapOSError(ec);
    END;
    RAISE PackageLib.Stop;
  END ShipDiffReport;

PROCEDURE FetchFile(cl: DiffClosure; path,tempPath: FN; info: FileSys.FileInfo)
    RAISES {PkgErr.E, NetObj.Error, OSError.E,
                         Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR rd: Rd.T;
      np: NetPath.T;
      <* FATAL NetPath.Invalid *>
  BEGIN
    np := NetPath.FromRelFN(path);
    IF cl.siblings # NIL THEN
      FOR i := 0 TO LAST(cl.siblings^) DO
        IF cl.siblings[i] # NIL AND cl.siblings[i] # cl.t THEN
          LOOP
            TRY
              rd := cl.siblings[i].pullFile(np, info.date, info.length);
              IF rd # NIL THEN
                TryFetchFile(cl, tempPath, rd);
                cl.frpt.fromSibling := TRUE;
                RETURN;
              END;
              EXIT;
            EXCEPT
            | NoProgress =>   (* go around loop *)
            | NetObj.Error, Rd.Failure => EXIT;
            END;
          END;
        END;
      END;
    END;
    LOCK cl.t.mu DO cl.t.pulling := np; END;
    TRY
      LOOP
        rd := cl.source.pullFile(np);
        TRY
          TryFetchFile(cl, tempPath, rd);
          EXIT;
        EXCEPT
        | NoProgress => (* go around loop *)
        END;
      END;
    FINALLY
      LOCK cl.t.mu DO
        cl.t.pulling := NIL;
        Thread.Broadcast(cl.t.pullWait);
      END;
    END;
    cl.frpt.fromSibling := FALSE;
  END FetchFile;

EXCEPTION NoProgress;

VAR
  monitorMu := NEW(MUTEX);
  monitorList: RefList.T := NIL;
  monitoring: BOOLEAN := FALSE;

PROCEDURE MonitorCl(cl: DiffClosure) =
  BEGIN
    LOCK monitorMu DO
      monitorList := RefList.Cons(cl, monitorList);
      cl.monCount := -1;
      cl.monThread := Thread.Self();
      IF NOT monitoring THEN
        EVAL Thread.Fork(NEW(Thread.Closure, apply := DoMonitorCl));
        monitoring := TRUE;
      END;
    END;
  END MonitorCl;

PROCEDURE UnmonitorCl(cl: DiffClosure) =
  BEGIN
    LOCK monitorMu DO
      VAR last: RefList.T := NIL; try := monitorList; BEGIN
        WHILE try # NIL DO
          IF cl = try.head THEN
            IF last = NIL THEN
              monitorList := try.tail;
            ELSE
              last.tail := try.tail;
            END;
          ELSE
            last := try;
          END;
          try := try.tail;
        END;
      END;
    END;
  END UnmonitorCl;

PROCEDURE CheckCl(cl: DiffClosure): BOOLEAN =
  BEGIN
    LOCK monitorMu DO
      RETURN (cl.monCount # cl.frpt.bytesTransferred);
    END;
  END CheckCl;

PROCEDURE DoMonitorCl(<*UNUSED*> x: Thread.Closure): REFANY =
  BEGIN
    LOOP
      Thread.Pause(6.0D1);
      LOCK monitorMu DO
        VAR l := monitorList; BEGIN
          WHILE l # NIL DO
            VAR cl: DiffClosure := l.head; BEGIN
              IF cl.monCount = cl.frpt.bytesTransferred THEN
                Thread.Alert(cl.monThread);
              ELSE
                cl.monCount := cl.frpt.bytesTransferred;
              END;
            END;
            l := l.tail;
          END;
        END;
      END;
    END;
    <*NOWARN*> RETURN NIL;
  END DoMonitorCl;

PROCEDURE TryFetchFile(cl: DiffClosure; tempPath: FN; rd: Rd.T)
      RAISES {NoProgress, OSError.E, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR
    start: Time.T;
    c: CARDINAL;
    wr: Wr.T := NIL;
  BEGIN
    cl.frpt.bytesTransferred := 0;
    start := Time.Now ();
    TRY
      MonitorCl(cl);
      wr := FileSys.OpenWrite(tempPath);
      REPEAT
        TRY
          c := Rd.GetSub (rd, cl.buf);
        EXCEPT
        | Thread.Alerted => 
            IF CheckCl(cl) THEN RAISE Thread.Alerted; END;
            PSLib.LogIt("PSShip: No progress on fetch.");
            RAISE NoProgress;
        END;
        IF c # 0 THEN
          Wr.PutString (wr, SUBARRAY(cl.buf, 0, c));
        END;
        INC (cl.frpt.bytesTransferred, c);
      UNTIL c # NUMBER (cl.buf);
    FINALLY
      UnmonitorCl(cl);
      IF wr # NIL THEN Wr.Close (wr); END;
      Rd.Close (rd);
    END;
    cl.frpt.elapsedMSec := ROUND((Time.Now() - start) * 1.0D3);
  END TryFetchFile;

PROCEDURE Commit (t: T; version: Version)
    RAISES {PkgErr.E, Thread.Alerted} =
  BEGIN
    LOCK t.mu DO
      IF t.state # State.Prepared THEN
        PkgErr.Raise(PkgErr.BadParameter);
      END;
      TRY
        IF version.vn = LockOps.NullVN THEN
          TRY
            version := PackageObj.New(NIL).version(t.pn);
          EXCEPT
            | NetObj.Error, PkgErr.E =>
          END;
        ELSIF version.vn = LockOps.DeletedVN THEN
          IF t.tempDir # NIL THEN
            PSLib.LogIt("PM.Error: Non-empty delete on " &
                          PSLib.PkgText(t.pn));
            PkgErr.Raise (PkgErr.BadParameter);
          END;
          t.doBackup := FALSE;
        ELSE
          IF t.tempDir = NIL THEN
            t.tempDir :=
                PSLib.PrefixDirName (t.realPackage, PSLib.TemporaryDir, TRUE);
            FileSys.MakeDir(t.tempDir);
          END;
        END;
        (* write the backup link file *)
        IF t.tempDir = NIL THEN
          (* if "wr" is NIL then there should be not export links *)
          PSExportLink.Write (t.actions, NIL);
        ELSE
          VAR wr := FileSys.OpenWrite(
                Pathname.Join(t.tempDir, PackageLib.ExportLinkFile, NIL));
          BEGIN
            TRY
              PSExportLink.Write (t.actions, wr);
              Wr.Close(wr); wr := NIL;
            FINALLY
              IF wr # NIL THEN TRY Wr.Close(wr) EXCEPT Wr.Failure => END; END;
            END;
          END;
        END;
        CommitContent (t, version);
        t.state := State.Committed;
      EXCEPT
      | Wr.Failure(err) =>
          PSLib.MapError (t.pn, "Commit", err);
      | OSError.E(err) =>
          PSLib.MapError (t.pn, "Commit", err);
      END;
      PSLib.StatIncr(PSLib.StatCommits);
      PSLib.LogIt("PM.Commit " & PSLib.PkgText(t.pn) & ", version="
           & Fmt.Int (version.t) & "." & Fmt.Int (version.vn));
    END;
  END Commit;

PROCEDURE PullFile(t: T; path: NetPath.T;
         <*UNUSED*> date: Time.T; length: CARDINAL) : Rd.T =
  VAR fn := NetPath.ToRelFN(path);
  BEGIN
    (* lookup in partial-ship cache when we implement it *)
    LOCK t.mu DO
      WHILE t.pulling # NIL AND NetPath.Equal(t.pulling, path) DO
        Thread.Wait(t.mu, t.pullWait);
      END;
      IF t.state = State.Committed OR t.tempDir = NIL THEN
        RETURN NIL;
      END;
      TRY
        VAR pn := Pathname.Join(t.tempDir, fn, NIL);
            info := FileSys.GetInfo(pn, FALSE);
        BEGIN
          IF info.type = FileType.Normal AND info.length = length THEN
            RETURN FileSys.OpenRead(pn);
          END;
        END;
      EXCEPT
      | OSError.E =>
      END;
    END;
    RETURN NIL;
  END PullFile;

PROCEDURE CommitVersion (get: PSGet.T; version: Version): BOOLEAN =
  VAR res: BOOLEAN := FALSE;
    <* FATAL Thread.Alerted *>
  PROCEDURE TryCommitVersion(dir: FN) RAISES {} =
    VAR date: Time.T; BEGIN
      TRY
        date := FileSys.GetInfo(dir, TRUE).date;
        WriteVersionFile (dir, version);
        res := TRUE;
        FileSys.SetModifiedDate (
               Pathname.Join(dir, PackageLib.VersionFile, NIL), date);
        FileSys.SetModifiedDate (dir, date);
      EXCEPT
      | OSError.E =>
      END;
    END TryCommitVersion;
  BEGIN
    get.doItLocked(TryCommitVersion);
    RETURN res;
  END CommitVersion;

PROCEDURE CommitContent (t: T; version: Version) RAISES {OSError.E} =
  VAR
    saveName, destName, backup, realBackup: FN;
  BEGIN
        (* do the double rename *)
        (* rename package to temporary (or maybe backup) *)
    IF t.tempDir # NIL THEN
      WriteVersionFile (t.tempDir, version);
      PackageLib.SetDirDates(t.tempDir, t.en.dir);
      TRY
        FileSys.SetModifiedDate(
            Pathname.Join(t.tempDir, PackageLib.VersionFile, NIL), t.en.ts);
        FileSys.SetModifiedDate(
            Pathname.Join(t.tempDir, PackageLib.ExportLinkFile, NIL), t.en.ts);
        FileSys.SetModifiedDate(t.tempDir, t.en.ts);
      EXCEPT
        | OSError.E =>
      END;
    END;
    IF t.get # NIL THEN
      destName := PSLib.PrefixDirName(t.realPackage, PSLib.TemporaryDir,FALSE);
      destName := PSLib.PrefixDirName(destName, PSLib.DiscardDir, TRUE);
            (* rename the backup directory if any *)
      IF t.doBackup THEN
        PSLib.StatIncr (PSLib.StatBackups);
        saveName := PSLib.PrefixDirName (
                         t.realPackage, PSLib.BackupDir, FALSE);
        TRY
          FileSys.Rename(saveName, destName);
          FileSys.Remove(destName, TRUE);
        EXCEPT
          | OSError.E =>
        END;
        destName := saveName;
      END;
    END;
        (* now rename to the real package *)
    PSGet.DoubleRename (t.pkgPath, t.realPackage, t.tempDir, destName,
           NOT t.doBackup);
    IF t.tempDir = NIL THEN
            (* this is a package deletion *)
            (* delete link to package if there was one *)
            (* delete backups if any *)
      TRY
        FileSys.Remove (t.pkgPath);
        backup := PSLib.PrefixDirName (t.pkgPath, PSLib.BackupDir, FALSE);
        realBackup := PSLib.RealPath(backup);
        FileSys.Remove (realBackup, TRUE);
            (* backupDir might have been a link ... remove it *)
        FileSys.Remove(backup);
      EXCEPT
      | OSError.E =>
      END;
    ELSE
      t.tempDir := NIL;
      MakeLinks(t, saveName);
    END;
  END CommitContent;

PROCEDURE MakeLinks(t: T; backupName: FN)
    RAISES {OSError.E} =
  VAR
    makeLink := FALSE;
    referent, oldReferent, backupReferent, backupLink: FN;
    info: FileSys.FileInfo;
  BEGIN
      (* post-processing .... make link to real package *)
      (*                 .... and create backup link *)
    TRY
      info := FileSys.GetInfo (t.pkgPath);
      makeLink := info.type # FileType.Dir;
      IF info.type = FileType.SLink THEN
        oldReferent := FileSys.ReadLink(t.pkgPath);
      END;
    EXCEPT
    | OSError.E(ec) =>
        makeLink := (FileSys.ClassifyError(ec) = FileSys.ErrorClass.Lookup);
    END;
    IF makeLink THEN
      referent := PSLib.RelativePath(
                     Pathname.Join(
                         PSLib.RealPath(Pathname.Prefix(t.pkgPath)),
                         t.pn.arc, NIL),
                     t.realPackage);
      IF oldReferent = NIL OR NOT Text.Equal(oldReferent, referent) THEN
        TRY FileSys.Remove (t.pkgPath, FALSE); EXCEPT | OSError.E => END;
        FileSys.SymLink(t.pkgPath, referent);
      END;
      (* create backup link *)
      IF t.doBackup THEN
        FileSys.SetModifiedDate(backupName, Time.Now ());
        backupLink := PSLib.PrefixDirName(
                  t.pkgPath, PSLib.BackupDir, FALSE);
        backupReferent := PSLib.PrefixDirName(
                  referent, PSLib.BackupDir, FALSE);
        TRY FileSys.Remove (backupLink, TRUE); EXCEPT | OSError.E => END;
        FileSys.SymLink(backupLink, backupReferent);
      END;
    END;
  END MakeLinks;

PROCEDURE WriteVersionFile (dir: FN; v: Version) RAISES {OSError.E} =
  VAR wr: Wr.T;
    <* FATAL Thread.Alerted *>
  BEGIN
    wr := FileSys.OpenWrite(Pathname.Join(dir, PackageLib.VersionFile, NIL));
    TRY
      Wr.PutText (wr, Fmt.Int (v.t) & "." & Fmt.Int (v.vn) & "\n");
      Wr.Close (wr);
    EXCEPT
    | Wr.Failure(e) => RAISE OSError.E(e);
    END;
  END WriteVersionFile;

BEGIN
END PSShip.
