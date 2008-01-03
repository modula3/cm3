(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PackageTool.m3 *)
(* Last modified on Thu Feb  2 08:45:25 PST 1995 by kalsow *)
(*      modified on Thu Oct  7 11:13:49 PDT 1993 by wobber *)
(*      modified on Mon Dec  2 11:44:33 GMT+1:00 1991 by prusker *)

MODULE PackageTool EXPORTS Main;

IMPORT Env, RefList, RefListSort, OSError, Params, Fmt, FmtTime, Rd, Text,
       TextList, TextRefTbl, Thread, Time, Wr;
IMPORT FileSys, LockOps, NetObj, OpSys, Site, PackageLib,
       NetPath, Pathname, PackageObj, PkgErr, PackageEvents, Process;

FROM PackageObj IMPORT FileType, ShipOptions;
FROM Stdio IMPORT stdin, stdout, stderr;

EXCEPTION FatalError;   <* FATAL FatalError *>
                        <* FATAL Thread.Alerted *>

TYPE
  PN = NetPath.PN;
  FN = FileSys.FN;
  Dir = NetPath.Dir;

CONST
  XferBufferSize = 32 * 1024;

TYPE
  ErrorClass = {LocalOS, Lock, NetObj, PM};

  PackageOp = {Compare, Get, Ship, Lock, Unlock, WhoHas, Create, Remove,
               RemoveBackups};
  LockOp = {LockIt, UnlockIt, WhoseLock, GetVersion, CreateIt, RemoveIt,
            CheckLock, Commit};

  ExportLinks = PackageObj.ExportLinks;

  XferBuffer = REF ARRAY [0..XferBufferSize - 1] OF CHAR;

  ReplicaSet = REF ARRAY OF TEXT;
  
  Parameters = REF
                 RECORD
                   op: PackageOp;
                   pn: PN;
                   verbose: BOOLEAN;
                   site: Text.T;
                   wildOp: BOOLEAN; (* user specified replica string *)
                   force: BOOLEAN; (* force date match on ship *)
                   ignoreLock: BOOLEAN; (* ignore lock database *)
                   purgeLinks: BOOLEAN; (* actually delete extra links *)
                   exportLinks: ExportLinks; (* links parsed from command *)
                   breakRights: LockOps.BreakRights;
                   (* lock match params on unlock *)
                   forceVersion: BOOLEAN; (* version match on unlock *)
                   version: LockOps.Version; (* version for get/ship *)
                   goRemote: BOOLEAN; (* used for "whohas" *)
                   replicas: ReplicaSet;
                   (* replica set for get/compare *)
                   ships: REF LockOps.ShipArray;
                   backupWanted: BOOLEAN; (* keep backup on ship *)
                   backupHosts: ReplicaSet;
                   (* host names for ship w/backup and remove backup *)
                   hostStr: Text.T;
                   foreignSites: REF ARRAY OF Site.Remote;
                 END;

TYPE
  EnumClosure = PackageLib.EnumClosure OBJECT
  OVERRIDES
    acquire := EnumAcquire;
    release := EnumRelease;
  END;

TYPE
  ParamObj = OBJECT
    iParam: CARDINAL := 1;
    tokens: RefList.T := NIL;
  METHODS
    backup() := BackupParam;
    next(): TEXT := NextParam;
  END;

VAR
  userAuth, invoc: Text.T;
  printLock: Thread.Mutex;
  myWorkingDirName: Text.T;
  defaultRepository: Dir;


(* packagetool invocation:

   comparepackage [-r<replicas>] [package]
   getpackage [-ifv] [-r<replicas>] [package]
   shippackage [-ifvn] [-r<replicas>] [-b<replicas>] [package] [-l exports]
   createpackage [package]
   deletepackage [-b] [package]
   lockpackage [package]
   unlockpackage [-fk] [package]
   whohas [-va] [package]

      where "packagetool create" means "createpackage", etc.
*)

PROCEDURE ActOnPackage (paramObj: ParamObj) =
  VAR p: Parameters; ok: BOOLEAN;
  BEGIN
    ok := FALSE;
    p := ParseCommandLine (paramObj);
    CASE p.op OF
      | PackageOp.Lock => ok := DoLockOp (LockOp.LockIt, p);
      | PackageOp.Unlock => ok := DoLockOp (LockOp.UnlockIt, p);
      | PackageOp.WhoHas => ok := DoLockOp (LockOp.WhoseLock, p);
      | PackageOp.Create =>
          CheckWorkingDir ();
          ok := DoLockOp (LockOp.CreateIt, p);
          IF ok THEN CreateReadme(p); END;
      | PackageOp.Remove =>
          IF NOT p.force THEN ConfirmRemoval(p); END;
          ok := DoLockOp (LockOp.RemoveIt, p);
      | PackageOp.RemoveBackups =>
          Fail ("backup removal not implemented\n");
      | PackageOp.Compare =>
          IF NOT p.wildOp THEN
            IF NOT DoLockOp (LockOp.GetVersion, p) THEN Fail(); END;
          END;
          ok := ComparePackage (p);
      | PackageOp.Get =>
          CheckWorkingDir ();
          IF p.ignoreLock THEN
            IF NOT p.wildOp THEN
              IF NOT DoLockOp (LockOp.GetVersion, p) THEN Fail(); END;
            END;
          ELSE
            IF NOT DoLockOp (LockOp.LockIt, p) THEN Fail(); END;
          END;
          ok := GetPackage (p);
      | PackageOp.Ship =>
          AssertWorkingDirNonEmpty ();
          IF  NOT p.ignoreLock THEN
            IF NOT DoLockOp (LockOp.CheckLock, p) THEN Fail();  END;
          END;
          ok := ShipPackage (p);
    END;
    IF NOT ok THEN
      Fail();
    ELSE
      IF p.op # PackageOp.WhoHas AND p.op # PackageOp.Compare THEN
        PutStdout(invoc & " complete\n");
      END;
    END;
  END ActOnPackage;

PROCEDURE DoLockOp (op: LockOp; p: Parameters): BOOLEAN =
  VAR
    t: LockOps.T;
    key: Text.T;
    entry: LockOps.RefEntry;
    cf: LockOps.CommitFailures;
    version: LockOps.Version;
  BEGIN
    key := BuildKey ();
    TRY
      t := LockOps.New ();
      CASE op OF
        | LockOp.CreateIt =>
            IF NOT DoCreateTest(p) THEN RETURN FALSE; END;
            PutStdout("Creating lock entry for " & PkgText(p) & "\n");
            TRY
              LockOps.Create (userAuth, p.pn, key, 
                       LockOps.Version{0, LockOps.InitialVN},
                       TRUE, t);
            EXCEPT
            | PkgErr.E(ec) =>
               IF ec.head = PkgErr.RemoteLockProblem THEN
                 IF ec.tail # NIL AND
                      ec.tail.head = PkgErr.PackageNameInUse THEN
                   PutWarning("package name still exists at remote site\n");
                   PutStdout(
             "        (perhaps an earlier deletion has yet to propagate)\n");
                 ELSE
                   PutWarning(PkgErr.Msg(ec) & "\n");
                 END;
                 PutStdout("        package name not guaranteed unique\n");
                 LockOps.Create (userAuth, p.pn, key,
                             LockOps.Version{0, LockOps.InitialVN}, FALSE, t);
               ELSE
                 RAISE PkgErr.E(ec);
               END;
            END;
            PutStdout("Created and locked: " & key & "\n");
        | LockOp.RemoveIt =>
            PutStdout("Removing lock entry for " & PkgText(p) & "\n");
            LockOps.Remove (userAuth, p.pn, key, TRUE, t);
            PutStdout("Removed\n");
        | LockOp.LockIt =>
            version.t := 0;
            version.vn := LockOps.NullVN;
            PutStdout("Locking package " & PkgText(p) & "\n");
            p.version
              := LockOps.Lock (userAuth, p.pn, version, key, NIL, t);
            PutStdout("Locked: " & key & "\n");
        | LockOp.UnlockIt =>
            version.t := 0;
            version.vn := LockOps.NullVN;
            PutStdout("Unlocking " & PkgText(p) & "\n");
            LockOps.Unlock (userAuth, p.pn, version, key, NIL,
                            p.breakRights, p.forceVersion, t);
            PutStdout("Unlocked\n");
        | LockOp.CheckLock =>
            PutStdout("Checking lock entry for " & PkgText(p) & "\n");
            entry := LockOps.GetEntry (p.pn, TRUE, t);
            IF entry.owner.key = NIL OR
                 (NOT (Text.Equal (key, entry.owner.key)
                   AND Text.Equal (p.site, entry.owner.site))) THEN
              RAISE LockOps.LockConflict (entry.owner);
            END;
        | LockOp.GetVersion =>
            entry := LockOps.GetEntry (p.pn, FALSE, t);
            p.version.t := entry.instance;
            p.version.vn := entry.curVN;
        | LockOp.WhoseLock =>
            IF p.pn.dir # NIL THEN
              entry := LockOps.GetEntry (p.pn, TRUE, t);
              IF p.verbose THEN
                PrintEntry (entry);
              ELSE
                IF entry.owner.key # NIL THEN
                  PutStdout(entry.owner.key & " ("
                                     & entry.owner.site & ")\n");
                END;
              END;
            ELSE
              DoEnumerateLocks (p, t);
            END;
        | LockOp.Commit =>
            p.version := LockOps.AssignVersion (
                userAuth, p.pn, key, NIL, t);
            cf := LockOps.Commit (
                userAuth, p.pn, p.version, p.ships^, TRUE, t);
            PrintCommitFailures (cf, p);
            PutStdout("Commit successful\n");
      END;
    EXCEPT
      | LockOps.SynchVersions =>    <* ASSERT (FALSE) *>
      | LockOps.CommitFailed(cf) =>
          PrintCommitFailures (cf, p);
          PutStderr ("commit failed at all replicas\n");
          RETURN FALSE;
      | PkgErr.E(ec) =>
          PrintError ("lock server", ErrorClass.Lock, ec);
          RETURN FALSE;
      | LockOps.LockConflict(owner) =>
          IF owner.key # NIL THEN
            PutStderr ("package checked out to " & owner.key
                               & " (" & owner.site & ")\n");
          ELSE
            PutStderr ("package not checked out\n");
          END;
          RETURN FALSE;
    END;
    RETURN TRUE;
  END DoLockOp;

PROCEDURE DoEnumerateLocks (p: Parameters; t: LockOps.T)
    RAISES {PkgErr.E} =
  VAR
    dirs := LockOps.EnumerateDirs(NIL, t);
    locks: LockOps.EnumList;
    nSite: CARDINAL;
    ok: BOOLEAN;
  PROCEDURE PrintLocks (dir: Dir; el: LockOps.EnumList) =
    BEGIN
      IF el # NIL THEN
        FOR i := 0 TO LAST (el^) DO
          PutStdout(NetPath.PNToText(PN{dir, el[i].arc}) &
                         " -> " & el[i].e.owner.key & " (" &
                         el[i].e.owner.site & ")\n");
        END;
      END;
    END PrintLocks;
  BEGIN
    IF dirs = NIL THEN RETURN; END;
    IF p.goRemote THEN
      PutStdout("\nLocks at local site:\n");
    END;
    FOR i := 0 TO LAST(dirs^) DO
      locks := LockOps.Enumerate (dirs[i], NIL, TRUE, TRUE, FALSE, t);
      PrintLocks (dirs[i], locks);
    END;
    IF p.goRemote AND p.foreignSites # NIL THEN
      PutStdout("\nLocks at foreign sites:\n");
      FOR j := 0 TO LAST (p.foreignSites^) DO
        nSite := j;
        ok := TRUE;
        PutStdout("[" & p.foreignSites[nSite].name & "]\n");
        FOR i := 0 TO LAST(dirs^) DO
          TRY
            IF ok THEN
              locks := LockOps.Enumerate (
                dirs[i], p.foreignSites[nSite].name,
                TRUE, TRUE, FALSE, t);
              PrintLocks (dirs[i], locks);
            END;
          EXCEPT
          | PkgErr.E(ec) =>
             IF ec.head # PkgErr.NoSuchDir THEN
                PrintError (p.foreignSites[nSite].name, ErrorClass.Lock, ec);
                ok := FALSE;
             END;
          END;
        END;
      END;
    END;
  END DoEnumerateLocks;

(* get/compare package operations *)

TYPE
  Diff = PackageLib.DiffClosure OBJECT
    same: BOOLEAN := TRUE;
  OVERRIDES
    report := ReportCompareDiff;
  END;

PROCEDURE ComparePackage (p: Parameters): BOOLEAN =
  VAR
    source: PackageObj.Source;
    host: Text.T;
    res: BOOLEAN;
    d := NEW(Diff);
    dirEnum, remEnum: PackageObj.DirEnum;
  BEGIN
    TRY
      source := PickAServer (p, host);
      PutStdout("Comparing with " & PkgText(p) & " on " & host & "\n");
      dirEnum := PackageLib.Enumerate (NewEnum ());
      remEnum := source.enum().dir;
      PackageLib.Compare(dirEnum, remEnum, d);
      IF d.same THEN PutStdout("Packages are the same\n");  END;
      res := TRUE;
    EXCEPT
    | OSError.E(osErr) =>
        PrintError (NIL, ErrorClass.LocalOS, osErr);
        res := FALSE;
    | NetObj.Error(nErr) =>
        PrintError (host, ErrorClass.NetObj, nErr);
        res := FALSE;
    | PkgErr.E(pmErr) =>
        PrintError (host, ErrorClass.PM, pmErr);
        res := FALSE;
    END;
    RETURN res;
  END ComparePackage;

PROCEDURE ReportCompareDiff(
    d: Diff; dir: FN; type: PackageLib.DiffType; e: PackageObj.DirElem) =
  VAR
    x: TEXT;
  BEGIN
    CASE type OF
    | PackageLib.DiffType.Same => RETURN;
    | PackageLib.DiffType.NoSrc => x := "local file is missing";
    | PackageLib.DiffType.NoDest => x := "remote file is missing"
    | PackageLib.DiffType.SrcNewer => x := "local is newer"
    | PackageLib.DiffType.SrcOlder => x := "remote is newer"
    | PackageLib.DiffType.LinksDiffer => x := "links differ"
    | PackageLib.DiffType.LengthsDiffer => x := "lengths differ"
    | PackageLib.DiffType.TypesDiffer => x := "types differ"
    | PackageLib.DiffType.ModesDiffer => x := "modes differ"
    END;
    PutStdout(Pathname.Join(dir, e.arc, NIL) & ": " & x & "\n");
    d.same := FALSE;
  END ReportCompareDiff;


TYPE
  GetAction = {GetNewMode, GetNewDir, GetNewLink, GetNewFile, GetOldFile,
               DeletedFile, GetFailErr, LocalNewerWarn, MissingLocalWarn};

TYPE
  VerboseGetAction = [GetAction.GetNewMode..GetAction.GetNewLink];

TYPE
  GetDiff = PackageLib.DiffClosure OBJECT
    p: Parameters;
    host: TEXT := NIL;
    source: PackageObj.Source := NIL;
    error: BOOLEAN := FALSE;
    buf: XferBuffer := NIL;
  OVERRIDES
    report := ReportGetDiff;
  END;


PROCEDURE GetPackage (p: Parameters): BOOLEAN =
  VAR
    source: PackageObj.Source;
    remEnum, dirEnum: PackageObj.DirEnum;
    d: GetDiff := NEW(GetDiff, p := p);
  BEGIN
    TRY
      source := PickAServer (p, d.host);
      d.source := source;
      PutStdout("Getting with " & PkgText(p) & " on " & d.host & "\n");
      dirEnum := PackageLib.Enumerate (NewEnum ());
      remEnum := source.enum().dir;
      PackageLib.Compare(remEnum, dirEnum, d);
      IF d.error THEN RETURN FALSE; END;
      TRY
        PackageLib.SetDirDates("", remEnum);
      EXCEPT
      | OSError.E =>
      END;
      RETURN TRUE;
    EXCEPT
    | Thread.Alerted =>
    | OSError.E(osErr) =>
        PrintError (NIL, ErrorClass.LocalOS, osErr);
    | NetObj.Error(nErr) =>
        PrintError (d.host, ErrorClass.NetObj, nErr);
    | PkgErr.E(pmErr) =>
        PrintError (d.host, ErrorClass.PM, pmErr);
    END;
    RETURN FALSE;
  END GetPackage;

PROCEDURE ReportGetDiff(
    d: GetDiff; dir: FN; type: PackageLib.DiffType; e: PackageObj.DirElem)
    RAISES {PackageLib.Stop} =
  VAR fileExists: BOOLEAN := TRUE;
      p := d.p;
      rate: INTEGER;
      fn: FN;
  BEGIN
    IF type = PackageLib.DiffType.Same THEN RETURN; END;
    fn := Pathname.Join(dir, e.arc, NIL);
    TRY
      CASE type OF
      | PackageLib.DiffType.Same => RETURN;
      | PackageLib.DiffType.NoSrc =>
         IF p.force THEN
           NoteGetAction (p, GetAction.DeletedFile, fn);
           EnsureRemoved(fn, TRUE);
         ELSE
           NoteGetAction (p, GetAction.MissingLocalWarn, fn);
         END;
         RETURN;
      | PackageLib.DiffType.ModesDiffer =>
          FileSys.SetMode (fn, e.info.perm);
          NoteGetAction (p, GetAction.GetNewMode, fn);
          RETURN;
      | PackageLib.DiffType.SrcOlder =>
          IF NOT p.force THEN
            NoteGetAction (p, GetAction.LocalNewerWarn, fn);
            RETURN;
          END;
      | PackageLib.DiffType.NoDest =>
          fileExists := FALSE;
      ELSE
        (* fall through *)
      END;
      CASE e.info.type OF
      | FileType.Dir =>
          IF fileExists THEN EnsureRemoved(fn, FALSE);  END;
          FileSys.MakeDir (fn);
          NoteGetAction (p, GetAction.GetNewDir, fn);
      | FileType.SLink =>
          IF fileExists THEN EnsureRemoved(fn, TRUE);  END;
          FileSys.SymLink (fn, e.referent);
          NoteGetAction (p, GetAction.GetNewLink, fn);
      | FileType.Normal =>
          rate := RetrieveFile (d, fn, e);
          IF fileExists THEN
            NoteGetAction (p, GetAction.GetOldFile, fn, rate);
          ELSE
            NoteGetAction (p, GetAction.GetNewFile, fn, rate);
          END;
      | FileType.Other => <* ASSERT (FALSE) *>
      END;
      RETURN;
    EXCEPT
    | Thread.Alerted =>
    | OSError.E(osErr) =>
        PrintError (NIL, ErrorClass.LocalOS, osErr);
    | NetObj.Error(nErr) =>
        PrintError (d.host, ErrorClass.NetObj, nErr);
    | PkgErr.E(pmErr) =>
        PrintError (d.host, ErrorClass.PM, pmErr);
    END;
    NoteGetAction (p, GetAction.GetFailErr, fn);
    d.error := TRUE;
    RAISE PackageLib.Stop;
  END ReportGetDiff;

PROCEDURE EnsureRemoved(fn: FN; recurse: BOOLEAN) RAISES {OSError.E} =
  BEGIN
    TRY
      FileSys.Remove(fn, recurse);
    EXCEPT
    | OSError.E(ec) =>
        IF FileSys.ClassifyError(ec) # FileSys.ErrorClass.Lookup THEN
          RAISE OSError.E(ec);
        END;
    END;
  END EnsureRemoved;

PROCEDURE RetrieveFile (
   d: GetDiff; fn: TEXT; e: PackageObj.DirElem) : (*rate*) INTEGER
   RAISES {OSError.E, NetObj.Error, PkgErr.E, Thread.Alerted} =
  VAR
    rd: Rd.T;
    wr: Wr.T;
    retrFn: FN;
    transferComplete: BOOLEAN;
    rate: INTEGER;
    start, now: Time.T;
    c, xferBytes, diff: CARDINAL;
    <* FATAL NetPath.Invalid *>
  BEGIN
    IF d.buf = NIL THEN d.buf := NEW (XferBuffer); END;
    retrFn := TemporaryFn ();
    TRY
      wr := FileSys.OpenWrite(retrFn);
      TRY
        IF e.info.length = 0 THEN
          Wr.Close (wr);
          rate := 0;
        ELSE
          rd := d.source.pullFile (NetPath.FromRelFN(fn));
          xferBytes := 0;
          start := Time.Now ();
          REPEAT
            c := Rd.GetSub (rd, d.buf^);
            IF c # 0 THEN Wr.PutString (wr, SUBARRAY(d.buf^, 0, c));  END;
            INC (xferBytes, c);
          UNTIL c # NUMBER (d.buf^);
          now := Time.Now ();
          Wr.Close (wr);
          Rd.Close (rd);
          diff := ROUND((now - start) * 1.0D3);
          IF diff = 0 THEN INC (diff);  END;
          rate := (xferBytes * BITSIZE (CHAR)) DIV diff;
        END;
      EXCEPT
      (* be more specific if we can figure out how *)
      | Wr.Failure(ec) => RAISE OSError.E(ec);
      | Rd.Failure(ec) => RAISE OSError.E(ec);
      END;
      FileSys.SetMode (retrFn, e.info.perm);
      FileSys.SetModifiedDate (retrFn, e.info.date);
      TRY FileSys.Remove (fn, TRUE); EXCEPT | OSError.E => END;
      FileSys.Rename (retrFn, fn);
      transferComplete := TRUE;
    FINALLY
      IF NOT transferComplete THEN
        TRY FileSys.Remove (retrFn); EXCEPT | OSError.E => END;
      END;
    END;
    RETURN rate;
  END RetrieveFile;

PROCEDURE NoteGetAction
  (p: Parameters; kind: GetAction; arg: Text.T; rate: CARDINAL := 0) =
  PROCEDURE Rate() : Text.T =
    BEGIN
      IF p.verbose THEN
        RETURN " (" & Fmt.Int (rate) & " kb/s)";
      ELSE
        RETURN "";
      END;
    END Rate;
  BEGIN
    IF NOT p.verbose AND (kind <= LAST (VerboseGetAction))
      AND (kind >= FIRST (VerboseGetAction)) THEN
      RETURN ;
    END;
    LOCK printLock DO
      CASE kind OF
        | GetAction.GetNewMode =>
            PutStdout ("Changed mode of " & arg & "\n");
        | GetAction.GetNewDir =>
            PutStdout ("Made local directory " & arg & "\n");
        | GetAction.GetNewLink =>
            PutStdout ("Made local link " & arg & "\n");
        | GetAction.GetNewFile =>
            PutStdout ("Fetched to new file " & arg & Rate() & "\n");
        | GetAction.GetOldFile =>
            PutStdout ("Fetched to old file " & arg & Rate() & "\n");
        | GetAction.DeletedFile =>
            PutStdout ("Deleted " & arg & "\n");
        | GetAction.GetFailErr =>
            PutStderr ("failed getting " & arg & "\n");
        | GetAction.LocalNewerWarn =>
            PutWarning ("local file " & arg & " is newer\n");
        | GetAction.MissingLocalWarn =>
            PutWarning ("local file " & arg & " not in package\n");
      END;
    END;
  END NoteGetAction;

(* ship package *)

TYPE
  ShipForkIndex = [0..31];
  ShipForkMask = SET OF ShipForkIndex;
  ShipAction = REF ShipActionRec;
  ShipActionRec = RECORD
                       next: ShipAction := NIL;
                       arg: REFANY;
                       mask: ShipForkMask;
                       printed: BOOLEAN := FALSE;
                     END;

TYPE
  ShipFork = Thread.Closure OBJECT
    p: Parameters;
    host: Text.T;
    thread: Thread.T := NIL;
    st: PackageObj.Ship := NIL;
    source: PackageObj.Source;
    stab: ShipActionTable;
    mask: ShipForkMask;
    hasdir: BOOLEAN := TRUE;
    ok, didBackup: BOOLEAN := FALSE;
  OVERRIDES
    apply := ShipPackageFork;
  END;

  ShipForkList = REF ARRAY OF ShipFork;

  ShipActionTable = REF RECORD
    t: TextRefTbl.T := NIL;
    okMask: ShipForkMask := ShipForkMask {};
  END;

  ShipMonitor = PackageObj.Monitor OBJECT
    sf: ShipFork;
  OVERRIDES
    report := ReportShipAction;
  END;

  LocalSource = PackageObj.Source OBJECT
    dirEnum: PackageObj.DirEnum;
    explinks: ExportLinks;
  OVERRIDES
    enum := LocalEnum;
    pullFile := LocalPullFile;
    links := LocalReadLinks;
  END;

PROCEDURE LocalEnum(s: LocalSource) : PackageObj.Enum =
  BEGIN
    RETURN PackageObj.Enum{Time.Now(), s.dirEnum};
  END LocalEnum;

PROCEDURE LocalPullFile(<*UNUSED*> s: LocalSource; path: NetPath.T) : Rd.T
    RAISES {PkgErr.E} =
  BEGIN
    TRY
      RETURN FileSys.OpenRead(NetPath.ToRelFN(path));
    EXCEPT
    | OSError.E(ec) =>
        PrintError (NIL, ErrorClass.LocalOS, ec);
        RAISE PkgErr.E(ec);
    END;
  END LocalPullFile;

PROCEDURE LocalReadLinks(s: LocalSource) : ExportLinks =
  BEGIN
    RETURN s.explinks;
  END LocalReadLinks;

PROCEDURE ShipPackage (p: Parameters): BOOLEAN =
  VAR
    sfl: ShipForkList;
    sf: ShipFork;
    ok: BOOLEAN := FALSE;
    str, failedStr, backupStr, noDirStr: TEXT := NIL;
    source: PackageObj.Source;
    stab := NEW(ShipActionTable);
  BEGIN
    TRY
      source := NEW(LocalSource,
          dirEnum := PackageLib.Enumerate(NewEnum()),
          explinks := p.exportLinks);
    EXCEPT
    | OSError.E(osErr) =>
        PrintError (NIL, ErrorClass.LocalOS, osErr);
        RETURN FALSE;
    END;
    sfl :=  NEW (ShipForkList, NUMBER (p.replicas^));
    PutStdout("Shipping to " & PkgText(p) & " on "
                       & TextFromList (p.replicas) & "\n");
    IF p.backupWanted THEN
      PutStdout("Backup to be kept on " & TextFromList (p.backupHosts) & "\n");
    END;
    FOR i := 0 TO LAST (sfl^) DO
      stab.okMask := stab.okMask + ShipForkMask{i};
    END;
    FOR i := 0 TO LAST (sfl^) DO
      sf :=  NEW (ShipFork,
                  p := p,
                  source := source,
                  stab := stab,
                  host := p.replicas[i],
                  mask := ShipForkMask{i});
      sfl[i] := sf;
      sf.thread := Thread.Fork (sf);
    END;
    p.ships :=  NEW (REF LockOps.ShipArray, NUMBER(sfl^));
    FOR i := 0 TO LAST (sfl^) DO
      sf := sfl[i];
      EVAL Thread.Join (sf.thread);
      IF sf.ok THEN
        p.ships[i] := sf.st;
        IF BackupRequested (p, sf.host) THEN
          IF sf.didBackup THEN
            IF backupStr = NIL THEN
              backupStr := sf.host;
            ELSE
              backupStr := backupStr & "+" & sf.host;
            END;
          ELSE
            PutStdout("Backup unnecessary on " & sf.host & "\n");
          END;
        END;
        ok := TRUE;
      ELSE
        p.ships[i] := NIL;
        IF sf.hasdir THEN
          IF failedStr = NIL THEN
            failedStr := sf.host;
          ELSE
            failedStr := failedStr & "+" & sf.host;
          END;
        ELSE
          IF noDirStr = NIL THEN
            noDirStr := sf.host;
          ELSE
            noDirStr := noDirStr & "+" & sf.host;
          END;
        END;
      END;
    END;
    IF NOT ok THEN
      PutStderr ("ship failed on all replicas\n");
    ELSE
      PrintShipTable (stab, sfl);
      str := "Ship of " & PkgText(p) & " done";
      IF backupStr # NIL THEN
        str := str & ", backup on " & backupStr;
      END;
      IF failedStr # NIL THEN
        str := str & ", (failures: " & failedStr & ")";
      END;
      IF noDirStr # NIL THEN
        str := str & ", (no sub-repository: " & noDirStr & ")";
      END;
      PutStdout(str & "\n");
      PutStdout("Committing changes at replicas\n");
      IF p.ignoreLock THEN
        ok := DoWildCommit (p);
      ELSE
        ok := DoLockOp (LockOp.Commit, p);
      END;
    END;
    RETURN ok;
  END ShipPackage;

PROCEDURE ShipPackageFork (sf: ShipFork) : REFANY =
  VAR
    p: Parameters;
    options: ShipOptions;
    pkgT: PackageObj.T;
    mon := NEW(ShipMonitor, sf := sf);
  BEGIN
    p := sf.p;
    options.keepBackup := BackupRequested (p, sf.host);
    options.purgeLinks := p.purgeLinks;
    options.forceDateMatch := p.force;
    TRY
      pkgT := PackageObj.New (sf.host);
      sf.st := pkgT.newShip(userAuth, p.pn, options);
      sf.st.prepare (sf.source, NIL, mon);
      sf.ok := TRUE;
    EXCEPT
    | Thread.Alerted =>
    | NetObj.Error(nErr) =>
        PrintError (sf.host, ErrorClass.NetObj, nErr);
    | PkgErr.E(pmErr) =>
        IF pmErr.head = PkgErr.NoSuchDir THEN
          sf.hasdir := FALSE;
        ELSE
          PrintError (sf.host, ErrorClass.PM, pmErr);
        END;
    | PackageObj.SourceOutOfDate(path) =>
        PrintSourceError(sf.host, path);
    END;
    IF NOT sf.ok THEN
      LOCK printLock DO sf.stab.okMask := sf.stab.okMask - sf.mask; END;
    END;
    RETURN NIL;
  END ShipPackageFork;

PROCEDURE DoWildCommit (p: Parameters): BOOLEAN =
  VAR ok: BOOLEAN;
  BEGIN
    ok := TRUE;
    FOR i := 0 TO LAST (p.ships^) DO
      TRY
        p.ships[i].commit(p.version);
        PutStdout("Wild commit succeeded at " & p.replicas[i] & "\n");
      EXCEPT
      | NetObj.Error(ec) =>
          PrintError (p.replicas[i], ErrorClass.NetObj, ec);
          ok := FALSE;
      | PkgErr.E(ec) =>
          PrintError (p.replicas[i], ErrorClass.PM, ec);
          ok := FALSE;
      END;
    END;
    RETURN ok;
  END DoWildCommit;

PROCEDURE ReportShipAction(mon: ShipMonitor; arg: REFANY) =
  VAR sa: ShipAction;
      ref: REFANY;
      sf := mon.sf;
      fn: FN;
  BEGIN
    TYPECASE arg OF
    | PackageEvents.PrepareReport(pr) =>
        sf.didBackup := pr.keptBackup;
        RETURN;
    | PackageEvents.LinkReport(lr) =>
        fn := NetPath.ToText(lr.path);
    | PackageEvents.FileReport(fr) =>
        fn := NetPath.ToRelFN(fr.path);
        IF sf.p.verbose AND
              ((fr.type = PackageEvents.FileET.New) OR
               (fr.type = PackageEvents.FileET.Updated)) THEN
          LOCK printLock DO
            IF fr.elapsedMSec = 0 THEN
              PrintShipAction (fr, "(" & sf.host & ")");
            ELSE 
              PrintShipAction (fr, 
                 " (" & Fmt.Int (fr.bytesTransferred DIV fr.elapsedMSec) &
                 " kbit/s)  (" & sf.host & ")");
            END;
          END;
          RETURN;
        END;
    ELSE
      RETURN;
    END;
    LOCK printLock DO
      ref := NIL;
      IF sf.stab.t # NIL THEN
        IF sf.stab.t.get(fn, ref) THEN
          sa := ref;
          WHILE (sa # NIL) AND NOT SameAction(arg, sa.arg) DO
            sa := sa.next;
          END;
        ELSE
          sa := NIL;
        END;
      END;
      IF sa # NIL THEN
        sa.mask := sa.mask + sf.mask;
        IF sa.mask = sf.stab.okMask THEN
          PrintShipAction (sa.arg, "");
          sa.printed := TRUE;
        END;
      ELSIF sf.mask = sf.stab.okMask THEN
        PrintShipAction (arg, "");
      ELSE
        sa :=  NEW (ShipAction, next := ref, arg := arg, mask := sf.mask);
        IF sf.stab.t = NIL THEN
          sf.stab.t := NEW(TextRefTbl.Default).init(); END;
        EVAL sf.stab.t.put(fn, sa);
      END;
    END;
  END ReportShipAction;

PROCEDURE SameAction(arg1, arg2: REFANY) : BOOLEAN =
  BEGIN
    TYPECASE arg1 OF
    | PackageEvents.FileReport(rpt1) =>
        TYPECASE arg2 OF
        | PackageEvents.FileReport(rpt2) => RETURN rpt1.type = rpt2.type;
        ELSE RETURN FALSE;
        END;
    | PackageEvents.LinkReport(rpt1) =>
        TYPECASE arg2 OF
        | PackageEvents.LinkReport(rpt2) => RETURN rpt1.type = rpt2.type;
        ELSE RETURN FALSE;
        END;
    ELSE RAISE FatalError;
    END;
  END SameAction;

PROCEDURE PrintShipTable (stab: ShipActionTable; sfl: ShipForkList) =
  VAR
    list: RefList.T := NIL;
    first: BOOLEAN;
    sa: ShipAction;
    key, extra: Text.T;
    it: TextRefTbl.Iterator;
    val: REFANY;
  BEGIN
    IF stab.t = NIL THEN RETURN; END;
    it := stab.t.iterate();
    WHILE it.next(key, val) DO
      list := RefList.Cons(val, list);
    END;
    list := RefListSort.SortD (list, CompareShipAction);
    WHILE list # NIL DO
      sa := list.head;
      list := list.tail;
      WHILE sa # NIL DO
        IF NOT sa.printed THEN
          IF (stab.okMask - sa.mask) # ShipForkMask {} THEN
            extra := " (only on ";
            first := TRUE;
            FOR i := 0 TO LAST (sfl^) DO
              IF (i IN sa.mask) AND (i IN stab.okMask) THEN
                IF NOT first THEN extra := extra & "+"; END;
                extra := extra & sfl[i].host;
                first := FALSE;
              END;
            END;
            extra := extra & ")";
          ELSE
            extra := "";
          END;
          PrintShipAction (sa.arg, extra);
        END;
        sa := sa.next;
      END;
    END;
  END PrintShipTable;

PROCEDURE CompareShipAction(sa1, sa2: REFANY) : [-1..1] =
  VAR np1, np2: NetPath.T;
  BEGIN
    TYPECASE sa1 OF
    | PackageEvents.FileReport(rpt) => np1 := rpt.path;
    | PackageEvents.LinkReport(rpt) => np1 := rpt.path;
    ELSE
      np1 := NIL;
    END;
    TYPECASE sa2 OF
    | PackageEvents.FileReport(rpt) => np2 := rpt.path;
    | PackageEvents.LinkReport(rpt) => np2 := rpt.path;
    ELSE
      np2 := NIL;
    END;
    RETURN NetPath.Compare(np1, np2);
  END CompareShipAction;

PROCEDURE PrintShipAction (arg: REFANY; arg1: TEXT) =
  VAR x, fn: FN;
     warn:= FALSE;
  BEGIN
    TYPECASE arg OF
    | PackageEvents.FileReport(rpt) =>
        fn := NetPath.ToRelFN(rpt.path);
        CASE rpt.type OF
        | PackageEvents.FileET.New => x := "Stored to new file ";
        | PackageEvents.FileET.Updated => x := "Stored to old file ";
        | PackageEvents.FileET.Removed => x := "Removed ";
        | PackageEvents.FileET.ChangeMode => x := "Changed mode of ";
        END;
    | PackageEvents.LinkReport(rpt) =>
        fn := NetPath.ToText(rpt.path);
        CASE rpt.type OF
        | PackageEvents.LinkET.Installed => x := "Exported link ";
        | PackageEvents.LinkET.Removed => x := "Removed export link ";
        | PackageEvents.LinkET.NoDir =>
           warn := TRUE; x := "No directory for link ";
        | PackageEvents.LinkET.Denied =>
           warn := TRUE;  x := "Couldn\'t export link ";
        | PackageEvents.LinkET.Bad =>
           warn := TRUE;  x := "Bad link specification ";
        END;
    ELSE RAISE FatalError;
    END;
    x := x & fn & arg1 & "\n";
    IF warn THEN PutWarning(x); ELSE PutStdout(x); END;
  END PrintShipAction;


(* private utility stuff - command line parsing *)

PROCEDURE ParseCommandLine (paramObj: ParamObj): Parameters =
  VAR arg: Text.T; p: Parameters;
      packageArg: TEXT := NIL;
  BEGIN
    p :=  NEW (Parameters);
            (* set defaults *)
    p.site := NIL;
    p.force := FALSE;
    p.wildOp := FALSE;
    p.ignoreLock := FALSE;
    p.purgeLinks := TRUE;
    p.exportLinks := NIL;
    p.breakRights := LockOps.BreakRights.OwnerOnly;
    p.forceVersion := TRUE;
      (* NB, default is rights:ownerOnly, force:TRUE *)
    p.goRemote := FALSE;
    p.replicas := NIL;
    p.foreignSites := NIL;
    p.backupWanted := FALSE;
    p.backupHosts := NIL;
    p.hostStr := NIL;
    p.version.t := 0;
    p.version.vn := LockOps.NullVN;
    p.verbose := FALSE;

    IF Text.Equal (invoc, "comparepackage") THEN
      p.op := PackageOp.Compare;
    ELSIF Text.Equal (invoc, "getpackage") THEN
      p.op := PackageOp.Get;
    ELSIF Text.Equal (invoc, "shippackage") THEN
      p.op := PackageOp.Ship;
    ELSIF Text.Equal (invoc, "lockpackage") THEN
      p.op := PackageOp.Lock;
    ELSIF Text.Equal (invoc, "unlockpackage") THEN
      p.op := PackageOp.Unlock;
    ELSIF Text.Equal (invoc, "whohas") THEN
      p.op := PackageOp.WhoHas;
    ELSIF Text.Equal (invoc, "createpackage") THEN
      p.op := PackageOp.Create;
    ELSIF Text.Equal (invoc, "deletepackage") THEN
      p.op := PackageOp.Remove;
    ELSE
      arg := paramObj.next();
      IF arg = NIL THEN UsageError(); END;
      IF Text.Equal (arg, "compare") THEN
        p.op := PackageOp.Compare;
      ELSIF Text.Equal (arg, "get") THEN
        p.op := PackageOp.Get;
      ELSIF Text.Equal (arg, "ship") THEN
        p.op := PackageOp.Ship;
      ELSIF Text.Equal (arg, "lock") THEN
        p.op := PackageOp.Lock;
      ELSIF Text.Equal (arg, "unlock") THEN
        p.op := PackageOp.Unlock;
      ELSIF Text.Equal (arg, "whohas") THEN
        p.op := PackageOp.WhoHas;
      ELSIF Text.Equal (arg, "create") THEN
        p.op := PackageOp.Create;
      ELSIF Text.Equal (arg, "delete") THEN
        p.op := PackageOp.Remove;
      ELSE
        UsageError();
      END;
      invoc := invoc & " " & arg;
    END;
    LOOP
      arg := paramObj.next();
      IF arg = NIL THEN EXIT; END;
      IF Text.GetChar (arg, 0) # '-' THEN
        packageArg := arg;
        EXIT;
      ELSIF Text.Equal(arg, "-l") AND p.op = PackageOp.Ship THEN
        paramObj.backup();
        EXIT;
      ELSE
        ParseSwitches (Text.Sub (arg, 1, LAST(CARDINAL)), p);
      END;
    END;
    ParseHosts (p.hostStr, p);
    IF p.backupWanted AND (p.backupHosts = NIL) THEN
      Fail ("no backup hosts specified\n");
    END;
    IF packageArg = NIL THEN
      IF (p.op # PackageOp.WhoHas) THEN
        p.pn.dir := DefaultRepository(myWorkingDirName);
        p.pn.arc := Pathname.Last(myWorkingDirName);
      END;
    ELSE
      TRY
        p.pn := NetPath.PNFromText(packageArg);
        IF p.pn.dir = NIL THEN
          p.pn.dir := DefaultRepository(myWorkingDirName);
        END;
      EXCEPT
      | NetPath.Invalid =>
          Fail ("invalid package name\n");
      END;
      IF p.op = PackageOp.Ship THEN
        p.exportLinks := ParseExportLinks (paramObj);
      END;
    END;
    RETURN p;
  END ParseCommandLine;

PROCEDURE ParseHosts (str: Text.T; p: Parameters) =
  VAR site: Site.T;
  BEGIN
        (* If non-NIL, treat host string as a string
           of names separated by "+".  *)
    IF str # NIL THEN
      (* we failed trying to use "str" as a site ... use it
         as a string of replicas instead *)
      p.replicas := ParseHostList (str);
      IF ((p.replicas = NIL) OR (NUMBER (p.replicas^) = 0)) THEN
        Fail ("bad replica spec\n");
      END;
      p.wildOp := TRUE;
      IF NOT (p.ignoreLock OR (p.op = PackageOp.Compare)) THEN
        Fail ("replica spec requires \"-i\" switch\n");
      END;
    ELSE
      TRY
        site := Site.Init();
        p.replicas := site.replicas;
        p.foreignSites := site.foreignSites;
      EXCEPT
      | Site.Error(ec) =>
          Fail(Site.ErrMsg(ec) & "\n");
      END;
      p.site := site.name;
      defaultRepository := site.defaultRepository;
      IF p.backupHosts = NIL THEN p.backupHosts := site.backupHosts; END;
    END;
  END ParseHosts;

PROCEDURE ParseHostList (t: Text.T): ReplicaSet =
  VAR
    rs: ReplicaSet;
    i, len: CARDINAL;
    end, pos: INTEGER;
    arr: ARRAY [0..9] OF Text.T;
  BEGIN
    i := 0;
    pos := 0;
    len := Text.Length (t);
    IF len = 0 THEN RETURN NIL;  END;
    LOOP
      <* ASSERT (i < NUMBER (arr)) *>
      end := Text.FindChar (t, '+', pos);
      IF end < 0 THEN
        arr[i] := Text.Sub (t, pos, len-pos);
        INC (i);
        EXIT;
      ELSE
        IF (end = pos) OR (end + 1 = len) THEN
          Fail ("bad host name spec\n");
        END;
        arr[i] := Text.Sub (t, pos, end-pos);
        INC (i);
        pos := end + 1;
      END;
    END;
    rs :=  NEW (ReplicaSet, i);
    REPEAT DEC (i); rs[i] := arr[i];  UNTIL (i = 0);
    RETURN rs;
  END ParseHostList;

PROCEDURE ParseSwitches (t: Text.T; p: Parameters) =
  VAR len: CARDINAL;
  VAR ok: BOOLEAN;
  VAR c: CHAR;
  BEGIN
    len := Text.Length (t);
    FOR i := 0 TO len - 1 DO
      c := Text.GetChar (t, i);
      ok := FALSE;
      CASE c OF
        | 'b' =>
            IF p.op = PackageOp.Remove THEN
              p.op := PackageOp.RemoveBackups;
              ok := TRUE;
            ELSIF (p.op = PackageOp.Ship) AND ( NOT p.backupWanted) THEN
              p.backupWanted := TRUE;
              p.backupHosts :=
                ParseHostList (Text.Sub (t, i+1, LAST(CARDINAL)));
              RETURN ;
            END;
        | 'r' => p.hostStr := Text.Sub(t, i+1, LAST(CARDINAL)); RETURN ;
        | 'a' =>
            IF (p.op = PackageOp.WhoHas) THEN
              p.goRemote := TRUE;
              ok := TRUE;
            END;
        | 'v' => p.verbose := TRUE; ok := TRUE;
        | 'i' =>
            IF (p.op = PackageOp.Ship) OR (p.op = PackageOp.Get) THEN
              p.ignoreLock := TRUE;
              ok := TRUE;
            END;
        | 'n' =>
            IF (p.op = PackageOp.Ship) THEN
              p.purgeLinks := FALSE;
              ok := TRUE;
            END;
        | 'f' =>
            IF (p.op = PackageOp.Ship) OR (p.op = PackageOp.Get) OR
               (p.op = PackageOp.Remove) THEN
              p.force := TRUE;
              ok := TRUE;
            ELSIF p.op = PackageOp.Unlock THEN
              p.breakRights := LockOps.BreakRights.SiteOnly;
              p.forceVersion := FALSE;
              ok := TRUE;
            END;
        | 'k' =>
            IF p.op = PackageOp.Unlock THEN
              p.breakRights := LockOps.BreakRights.AnySite;
              ok := TRUE;
            END;
        ELSE ok := FALSE;
      END;
      IF NOT ok THEN
        Fail("invalid switch\n");
      END;
    END;
  END ParseSwitches;

PROCEDURE ParseExportLinks (paramObj: ParamObj): ExportLinks =
  TYPE ExportLinkRef = REF PackageObj.ExportLink;
  VAR
    seenSwitch: BOOLEAN;
    arg, prefix, target: Text.T;
    list: RefList.T;
    ex: ExportLinks;
    ll: ExportLinkRef;
    i: CARDINAL;
  PROCEDURE AddExport (referent: Text.T) =
    VAR
      linkName: Text.T;
      j: INTEGER;
      l: ExportLinkRef;
    BEGIN
            (* split the name here, if aliasing is to be done *)
      j := Text.FindChar (referent, '=');
      IF (j > 0) AND (j < Text.Length (referent)) THEN
        linkName := Text.Sub (referent, 0, j);
        referent := Text.Sub (referent, j+1, LAST(CARDINAL));
      ELSE
        linkName := Pathname.Last(referent);
      END;
      linkName := Pathname.Join(target, linkName, NIL);
      referent := Pathname.Join(prefix, referent, NIL);
      TRY
        l :=  NEW (ExportLinkRef,
                  link := NetPath.FromText(linkName),
                  referent := NetPath.FromRelFN(referent));
                (* check for non-existent referent *)
        EVAL FileSys.GetInfo (referent, FALSE);
        list := RefList.Cons (l, list);
      EXCEPT
      | NetPath.Invalid =>
          Fail("invalid export link specification\n");
      | OSError.E =>
          PutWarning ("can\'t export link to non-existent file "
             & referent & "\n");
      END;
    END AddExport;
  BEGIN (* ParseExportLinks *)
    list := NIL;
    seenSwitch := FALSE;
    seenSwitch := FALSE;
    LOOP
      arg := paramObj.next();
      IF arg = NIL THEN EXIT; END;
      IF Text.Equal (arg, "-l") THEN
        target := NIL;
        prefix := "";
        seenSwitch := TRUE;
      ELSE
        IF NOT seenSwitch THEN
          Fail ("bad export link spec\n");
        END;
        IF Text.GetChar (arg, 0) = '+' THEN
          prefix := Text.Sub (arg, 1, LAST(CARDINAL));
        ELSE
          IF target = NIL THEN
            target := arg;
            prefix := NIL;
          ELSE
            AddExport (arg);
          END;
        END;
      END;
    END;
    i := RefList.Length (list);
    IF i = 0 THEN RETURN NIL; END;
    ex :=  NEW (ExportLinks, i);
    WHILE i # 0 DO
      DEC (i);
      ll := list.head;
      list := list.tail;
      ex[i] := ll^;
    END;
    RETURN ex;
  END ParseExportLinks;

PROCEDURE PrintEntry (e: LockOps.RefEntry) =
  BEGIN
    IF e.owner.key # NIL THEN
      PutStdout("  Locked: " & e.owner.key & " (" & e.owner.site
                         & ")\n");
    END;
    PutStdout("  Managed by: " & e.managedBy & "\n");
    PutStdout("  Instance=" & Fmt.Int (e.instance) & "; lv="
                       & Fmt.Int (e.lastVN) & "; cv=" & Fmt.Int (e.curVN)
                       & "; pv=" & Fmt.Int (e.pendVN) & "\n");
    PutStdout("  Last modified: " &
                 FmtTime.Long(e.lastModified) & "\n");
  END PrintEntry;

PROCEDURE PrintCommitFailures (cf: LockOps.CommitFailures; p: Parameters) =
  BEGIN
    IF (cf = NIL) OR (NUMBER (cf^) = 0) THEN RETURN; END;
    LOCK printLock DO
      FOR i := 0 TO LAST (cf^) DO
        IF cf[i] # NIL THEN
          PutStderr ("commit failed at " & p.replicas[i] & ": " &
                    PkgErr.Msg(cf[i]) & "\n");
        END;
      END;
    END;
  END PrintCommitFailures;

PROCEDURE PrintError (hostname: Text.T; ecl: ErrorClass; a: PkgErr.TL) =
  BEGIN
    LOCK printLock DO
      CASE ecl OF
      | ErrorClass.LocalOS =>
          PutStderr("unexpected local error: " & PkgErr.Msg(a) & "\n");
      ELSE
          PutStderr (PkgErr.Msg(a) & " (" & hostname & ")\n");
      END;
    END;
  END PrintError;

PROCEDURE PrintSourceError (hostname: Text.T; path: NetPath.T) =
  BEGIN
    LOCK printLock DO
      PutStderr("Local source out of date: " &
                      NetPath.ToRelFN(path) & " (" & hostname & ")\n");
    END;
  END PrintSourceError;

PROCEDURE TemporaryFn (): FN =
  BEGIN
    RETURN ".pget";
  END TemporaryFn;

PROCEDURE BuildKey (): Text.T =
  VAR myHostName: Text.T;
      i, lastSlash: INTEGER := 0;
  BEGIN
    (* hack to look for automounter-style naming *)
    LOOP
      i := Text.FindChar(myWorkingDirName, '/', lastSlash+1);
      IF i < 0 THEN EXIT; END;
      IF Text.Equal("/r/", Text.Sub(myWorkingDirName, i, 3)) THEN
        RETURN Text.Sub(myWorkingDirName, lastSlash, i-lastSlash) &
               Text.Sub(myWorkingDirName, i+2, Text.Length(myWorkingDirName));
      ELSE
        lastSlash := i;
      END;
    END;
    TRY
      myHostName := OpSys.GetHostName () & ":";
    EXCEPT
    | OpSys.Error =>
        Fail ("local host name unknown.\n");
    END;
    RETURN Text.Cat(myHostName, myWorkingDirName);
  END BuildKey;

PROCEDURE BackupRequested (p: Parameters; host: Text.T): BOOLEAN =
  BEGIN
    IF p.backupWanted THEN
      FOR i := 0 TO LAST (p.backupHosts^) DO
        IF Text.Equal (host, p.backupHosts[i]) THEN RETURN TRUE;  END;
      END;
    END;
    RETURN FALSE;
  END BackupRequested;

PROCEDURE TextFromList (l: REF ARRAY OF TEXT): Text.T =
  VAR out: TEXT;
  BEGIN
    IF (l = NIL) OR (NUMBER (l^) = 0) THEN RETURN NIL;  END;
    out :="";
    FOR i := 0 TO LAST (l^) DO
      IF i # 0 THEN out := out & "+"; END;
      out := out & l[i];
    END;
    RETURN out;
  END TextFromList;

PROCEDURE AssertWorkingDirNonEmpty () =
  VAR e: FileSys.Enumeration;
  BEGIN
    TRY
      e := FileSys.Enumerate (NIL);
      IF e # NIL THEN RETURN ;  END;
    EXCEPT
      | OSError.E =>
    END;
    Fail ("working directory is empty or bad?\n");
  END AssertWorkingDirNonEmpty;

PROCEDURE CheckWorkingDir () =
  VAR home := Env.Get("HOME");
      homePath: Text.T;
  BEGIN
        (* check that this directory is not the user's home directory *)
        (* check that this directory is writeable *)
    IF home # NIL AND NOT Text.Empty (home) THEN
      TRY
        homePath := FileSys.GetPath (home);
        IF Text.Equal (myWorkingDirName, homePath) THEN
          Fail ("current directory is $HOME?\n");
        END;
      EXCEPT
      | OSError.E =>
      END;
    END;
    TRY
      IF FileSys.CheckAccess (NIL, TRUE) THEN RETURN ;  END; (* success *)
    EXCEPT
    | OSError.E =>
    END;
    Fail ("working directory not writeable?\n");
  END CheckWorkingDir;

PROCEDURE DoCreateTest (p: Parameters): BOOLEAN =
  VAR initial: CARDINAL; repl: Text.T;
  VAR i: CARDINAL;
  BEGIN
        (* compute a "random" initial array index, and then try a
           PackageObj.TestCreate on the first up replica *)
    initial := ROUND(Time.Now()) MOD NUMBER (p.replicas^);
    i := initial;
    REPEAT
      TRY
        repl := p.replicas[i];
        PackageObj.New(repl).checkDir(p.pn.dir, p.pn.arc);
        RETURN TRUE;
      EXCEPT
      | NetObj.Error(ec) =>
          PrintError (repl, ErrorClass.NetObj, ec);
      | PkgErr.E(ec) =>
          PrintError (repl, ErrorClass.PM, ec);
      END;
      i := (i + 1) MOD NUMBER (p.replicas^);
    UNTIL i = initial;
    IF p.force THEN
      PutWarning ("create test failed at all replicas!!\n");
      RETURN TRUE;
    END;
    PutStderr ("create test failed at all replicas!!\n");
    RETURN FALSE;
  END DoCreateTest;

(* pick a server for get/compare *)

PROCEDURE PickAServer
  (p: Parameters; VAR (*OUT*) hostName: Text.T): PackageObj.Source =
  VAR v: LockOps.Version; t: PackageObj.T; source: PackageObj.Source;
  BEGIN
        (*
           get a binding/handle to one of the servers in the host list
           return NIL if none are available.
           if p^.version is NullVN, then pick the first version found,
           otherwise find exactly p^.version.  if the package is found,
           but not p^.version, then recompute p^.version and loop if it
           has changed.
        *)
    LOOP
      t := PickAServerInternal (p, hostName);
      IF t = NIL THEN
        v := p.version;
        IF NOT p.wildOp THEN
          IF NOT DoLockOp (LockOp.GetVersion, p) THEN Fail();  END;
        END;
        IF (v.t = p.version.t) AND (v.vn = p.version.vn) THEN
          Fail ("current package version not available!!\n");
        END;
      ELSE
        TRY
          source := t.newSource(userAuth, p.pn, v);
          IF (p.version.vn = LockOps.NullVN) THEN EXIT;  END;
          IF (v.t = p.version.t) AND (v.vn = p.version.vn) THEN EXIT;  END;
        EXCEPT
          | NetObj.Error, PkgErr.E => (* go around loop *)
        END;
      END;
    END;
    RETURN source;
  END PickAServer;

TYPE
  PickAServerClosure = Thread.Closure OBJECT
    p: Parameters;
    pickersGo: BOOLEAN := FALSE;
    pickersOut: CARDINAL := 0;
    pickersIn: CARDINAL := 0;
    foundT: PackageObj.T := NIL;
    foundHost: TEXT := NIL;
    foundServer: BOOLEAN := FALSE;
    foundPackage: BOOLEAN := FALSE;
    mu: MUTEX;
    wait: Thread.Condition;
  OVERRIDES
    apply := PickAServerFork;
  END;
  
PROCEDURE PickAServerInternal (
    p: Parameters; VAR (*OUT*) host: TEXT): PackageObj.T =
  VAR cl := NEW(PickAServerClosure,
           p := p, mu := NEW(MUTEX),
           wait := NEW(Thread.Condition));
  BEGIN
        (* returns NIL if package ofund but not correct version *)
    FOR i := 0 TO LAST (p.replicas^) DO
      EVAL Thread.Fork (cl);
    END;
    LOCK cl.mu DO
      cl.pickersGo := TRUE;
      Thread.Broadcast (cl.wait);
      WHILE (cl.foundT = NIL) AND (cl.pickersIn # NUMBER (p.replicas^)) DO
        Thread.Wait (cl.mu, cl.wait);
      END;
    END;
    IF NOT cl.foundServer THEN
      Fail ("no servers are available!!\n");
    ELSIF NOT cl.foundPackage THEN
      Fail ("no such package at replicas!!\n");
    END;
    host := cl.foundHost;
    RETURN cl.foundT;
  END PickAServerInternal;

PROCEDURE PickAServerFork (cl: PickAServerClosure): REFANY =
  VAR t: PackageObj.T;
  VAR p: Parameters;
  VAR v, wantV: LockOps.Version;
  VAR rep: TEXT;
  BEGIN
    p := cl.p;
    wantV := p.version;
        (* the following line just synchronizes with the forker *)
    LOCK cl.mu DO
      rep := p.replicas[cl.pickersOut];
      INC(cl.pickersOut);
      WHILE NOT cl.pickersGo DO Thread.Wait (cl.mu, cl.wait); END;
    END;
    TRY
      t := PackageObj.New (rep);
      cl.foundServer := TRUE;
      v := t.version(p.pn);
      cl.foundPackage := TRUE;
    EXCEPT
      | NetObj.Error, PkgErr.E => t := NIL;
    END;
    LOCK cl.mu DO
      IF (t # NIL) AND (cl.foundT = NIL) THEN
        IF (wantV.vn = LockOps.NullVN)
          OR ((v.t = wantV.t) AND (v.vn = wantV.vn)) THEN
          cl.foundT := t;
          cl.foundHost := rep;
        END;
      END;
      INC (cl.pickersIn);
      Thread.Signal (cl.wait);
    END;
    RETURN NIL;
  END PickAServerFork;

PROCEDURE NewEnum (): PackageLib.EnumClosure =
  VAR e := NEW(EnumClosure);
  BEGIN
    RETURN e;
  END NewEnum;

PROCEDURE EnumAcquire (<*UNUSED*> e: PackageLib.EnumClosure): Text.T RAISES {} =
  BEGIN
    RETURN myWorkingDirName;
  END EnumAcquire;

PROCEDURE EnumRelease (<*UNUSED*> e: PackageLib.EnumClosure) RAISES {} =
  BEGIN
        (* no-op *)
  END EnumRelease;


(* main program *)

PROCEDURE PutStdout(t: TEXT) =
  BEGIN
    PutText(stdout, t);
  END PutStdout;
  
PROCEDURE PutStderr(t: TEXT) =
  BEGIN
    PutText(stderr, "ERROR [" & invoc & "]: " & t);
  END PutStderr;

PROCEDURE PutWarning(t: TEXT) =
  BEGIN
    PutText(stderr, "WARNING: " & t);
  END PutWarning;

PROCEDURE UsageError() =
  BEGIN
    PutText(stderr, "Invalid packagetool option\n");
    Process.Exit(1);
  END UsageError;
  
PROCEDURE Fail(err: TEXT := NIL) =
  BEGIN
    IF err # NIL THEN PutStderr(err); END;
    PutText(stderr, invoc & " failed\n");
    Process.Exit(1);
  END Fail;

PROCEDURE PutText(wr: Wr.T; t: TEXT) =
  BEGIN
    TRY
      Wr.PutText(wr, t);
      Wr.Flush(wr);
    EXCEPT
    | Wr.Failure =>
        Process.Exit(1);
    END;
  END PutText;

PROCEDURE PkgText(p: Parameters) : TEXT =
  BEGIN
    RETURN NetPath.PNToText(p.pn);
  END PkgText;

PROCEDURE BackupParam(p: ParamObj) =
  BEGIN
    IF p.iParam > 1 THEN DEC(p.iParam); END;
  END BackupParam;

PROCEDURE NextParam (p: ParamObj): TEXT =
  VAR t, res: Text.T;
      fn: TEXT;
      rd: Rd.T;
  BEGIN
    IF p.tokens # NIL THEN
      res := p.tokens.head;
      p.tokens := p.tokens.tail;
      RETURN res;
    END;
    REPEAT
      IF p.iParam >= Params.Count THEN RETURN NIL; END;
      t := Params.Get(p.iParam);
      INC (p.iParam);
    UNTIL (t # NIL) AND NOT Text.Empty(t);
    IF Text.Equal(t, "-file") THEN
      fn := Params.Get(p.iParam);
      INC (p.iParam);
      IF fn = NIL THEN
        Fail("file name expected after \"-file\"\n");
      END;
      TRY
        rd := FileSys.OpenRead(fn);
        p.tokens := ParseReader(rd);
        Rd.Close(rd);
      EXCEPT
      | Rd.Failure, OSError.E => 
          Fail("problem reading command file \"" & fn & "\"\n");
      END;
      t := NextParam(p);
    END;
    RETURN t;
  END NextParam;

PROCEDURE ParseReader(rd: Rd.T) : RefList.T RAISES {Rd.Failure} =
  VAR res: RefList.T := NIL;
      i: CARDINAL;
      ch: CHAR;
      tmp: ARRAY [0..99] OF CHAR;
  BEGIN
    TRY
      LOOP
        i := 0;
        REPEAT
          ch := Rd.GetChar(rd);
        UNTIL NOT IsWhiteSpace(ch);
        TRY
          REPEAT
            IF i < NUMBER(tmp) THEN
              tmp[i] := ch;
              INC(i);
            END;
            ch := Rd.GetChar(rd);
          UNTIL IsWhiteSpace(ch);
        FINALLY
          IF i # 0 THEN
            res := RefList.Cons(Text.FromChars(SUBARRAY(tmp, 0, i)), res);
          END;
        END;
      END;
    EXCEPT
    | Rd.EndOfFile =>
    END;
    RETURN RefList.ReverseD(res);
  END ParseReader;

PROCEDURE IsWhiteSpace(ch: CHAR) : BOOLEAN =
  BEGIN
    RETURN ch = ' ' OR ch = '\n' OR ch = '\t';
  END IsWhiteSpace;

PROCEDURE ConfirmRemoval(p: Parameters) =
  VAR t: TEXT;
  BEGIN
    LOOP
      PutStdout("Do you really want to delete " & PkgText(p) & "? (y/n) ");
      TRY t := Rd.GetLine(stdin);
      EXCEPT 
      | Rd.EndOfFile =>
         PutStdout("Reported EOF\n");
         Process.Exit(1);
      | Rd.Failure(e) => 
         PutStdout(PkgErr.Msg(e) & "\n");
         Process.Exit(1);
      END;
      IF Text.Equal(t, "y") OR Text.Equal(t, "Y") THEN EXIT; END;
      IF Text.Equal(t, "n") OR Text.Equal(t, "N") THEN
        PutStdout(invoc & " aborted\n");
        Process.Exit(0);
      END;
    END;
  END ConfirmRemoval;

PROCEDURE CreateReadme(p: Parameters) =
  VAR wr: Wr.T;
  BEGIN
    TRY
      FileSys.Rename("README", "existing_README");
      PutStdout("Renamed README to existing_README\n");
    EXCEPT
    | OSError.E =>
    END;
    TRY
      wr := FileSys.OpenWrite("README");
      Wr.PutText(wr, PkgText(p) & " -- created " &
                           FmtTime.Long(Time.Now()) & " by " &
                           userAuth & "\n");
      Wr.PutText(wr, "current owner " & userAuth & "\n");
      Wr.Close(wr);
      PutStdout("Created new README file\n");
    EXCEPT
    | Wr.Failure, OSError.E =>
    END;
  END CreateReadme;

PROCEDURE DefaultRepository(wd: TEXT): NetPath.T =
  (* Interpret the penultimate arc of the argument pathname
     as a single element NetPath.T.  If a repository by
     that name exists, use it as the default.  Otherwise
     use the default repository. *)
  BEGIN
    TRY
      VAR arcs := Pathname.Decompose(wd);
          n := arcs.size();
          dir: TextList.T;
      BEGIN
        IF n > 1 THEN
          dir := TextList.List1(arcs.get(n-2));
          LockOps.CheckDir(dir);
          RETURN dir;
        END;
      END;
    EXCEPT
    | Pathname.Invalid, PkgErr.E =>
    END;
    RETURN defaultRepository;
  END DefaultRepository;

 BEGIN
  invoc := Pathname.Last(Params.Get(0));
  printLock := NEW(MUTEX);
  TRY
    myWorkingDirName := Process.GetWorkingDirectory();
  EXCEPT
  | OSError.E =>
      PutStderr ("can\'t find working directory\n");
      Process.Exit(1);
  END;
  TRY
    userAuth := OpSys.GetUser ();
  EXCEPT
  | OpSys.Error =>
      PutStderr ("can\'t discover user name\n");
      Process.Exit(1);
  END;
  ActOnPackage (NEW(ParamObj));
END PackageTool.

