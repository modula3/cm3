(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PackageDB.m3 - stable storage for package name and lock DB *)

MODULE PackageDB;
(*
 * Last modified on Thu Feb  2 09:00:02 PST 1995 by kalsow
 *      modified on Fri Apr 22 12:35:47 PDT 1994 by wobber
 *      modified on Thu Dec 27 12:33:13 GMT+1:00 1990 by prusker
 *)

IMPORT Fingerprint, Fmt;
IMPORT Atom, AtomList, LockOps, PSLib, SmallDB, Pathname, NetPath,
       PkgErr, OSError, FileSys, Date, FmtTime, Process,
       Thread, Text, Time, SNetPathRefTbl, SortedTextRefTbl,
       StablePkl, TextRefTbl, Wr;

FROM StablePkl IMPORT DB, DirEntry, PkgEntry, LogElem;

(* Types for log entries *)

TYPE
  HistoryEvent = {CreateEvent, LockEvent, UnlockEvent,
                  CommitEvent, SetEvent, FPEvent};

  DirHistoryEvent = {DirCreateEvent, DirRemoveEvent};


CONST
  LogSizeLimit = 100; (* Maximum number of log entries before a snapshot *)
  LocksDir = "locks";
  HistoryFile = "history";

VAR
  mutex := NEW(MUTEX);
  firstTime: BOOLEAN := TRUE;
  ourDB: DB; (* The in-memory database itself *)
  logsize: CARDINAL; (* Size of stable storage log *)
  stabledir: SmallDB.T; (* Stable storage rep of dir *)
  instanceCtr := ROUND(Time.Now());
  historyFilePath: FileSys.FN;


PROCEDURE Lock
  (package: PN; version: Version; key: Text.T; keySite: SiteName := NIL)
   : Version RAISES {NotManager, PkgErr.E, LockOps.LockConflict} =
  VAR pe: PkgEntry;
  VAR comp: [-1..1];
  BEGIN
    IF (key = NIL) OR Text.Empty (key) THEN
      PkgErr.Raise (PkgErr.BadParameter);
    END;
    LOCK mutex DO
      IF (keySite = NIL) OR Text.Empty (keySite) THEN
        keySite := ourDB.localSite;
      END;
      pe := LookupEntry (ourDB, package, TRUE);
      IF version.vn = NullVN THEN version := CurVersion (pe); END;
      comp := CompareVersions (version, LastVersion (pe));
      IF (pe.owner.key # NIL)
             AND NOT (Text.Equal (key, pe.owner.key)
                      AND Text.Equal (keySite, pe.owner.site)) THEN
        RAISE LockOps.LockConflict (pe.owner);
      END;
      CASE comp OF
        | 0 =>
        | 1 => PkgErr.Raise (PkgErr.BadParameter);
        | -1 => PkgErr.Raise (PkgErr.OldLocalVersion);
      END;
      pe.owner.key := key;
      pe.owner.site := keySite;
      WriteLogEntry (package, pe);
    END;
    LogHistory (HistoryEvent.LockEvent, package, pe);
    RETURN version;
  END Lock;

PROCEDURE Unlock
  (package: PN; version: Version; key: Text.T; keySite: SiteName := NIL;
   breakRights: BreakRights := BreakRights.OwnerOnly;
   forceVersion: BOOLEAN := FALSE)
   RAISES {NotManager, PkgErr.E, LockOps.LockConflict,
           LockOps.SynchVersions} =
  VAR pe: PkgEntry;
  VAR comp: [-1..1];
  BEGIN
    IF (key = NIL) OR Text.Empty (key) THEN
      PkgErr.Raise (PkgErr.BadParameter);
    END;
    LOCK mutex DO
      IF (keySite = NIL) OR Text.Empty (keySite) THEN
        keySite := ourDB.localSite;
      END;
      pe := LookupEntry (ourDB, package, TRUE);
      IF version.vn = NullVN THEN
        version := CurVersion (pe);
      ELSE
        IF breakRights = BreakRights.AnySite THEN
          PkgErr.Raise (PkgErr.BadParameter);
        END;
      END;
      comp := CompareVersions (version, LastVersion (pe));
      CASE breakRights OF
        | BreakRights.OwnerOnly =>
            IF (pe.owner.key = NIL)
                  OR NOT (Text.Equal (key, pe.owner.key)
                      AND Text.Equal (keySite, pe.owner.site)) THEN
              RAISE LockOps.LockConflict (pe.owner);
            END;
        | BreakRights.SiteOnly =>
            IF (pe.owner.key # NIL)
                   AND NOT Text.Equal (keySite, pe.owner.site) THEN
              RAISE LockOps.LockConflict (pe.owner);
            END;
        | BreakRights.AnySite =>
      END;
      CASE comp OF
        | 0 =>
        | 1 => PkgErr.Raise (PkgErr.BadParameter);
        | -1 =>
            IF NOT forceVersion THEN
              PkgErr.Raise (PkgErr.OutstandingVersion);
            END;
                    (* we now unconditionally bump the last version number here *)
                    (* this gets around the possibility that the current last
                       version was shipped to some replica, but not committed *)
            INC (pe.lastVN);
            WriteLogEntry (package, pe);
            RAISE LockOps.SynchVersions (LastVersion (pe));
      END;
      pe.owner.key := NIL;
      WriteLogEntry (package, pe);
    END;
    LogHistory (HistoryEvent.UnlockEvent, package, pe);
  END Unlock;

PROCEDURE AssignVersion
  (package: PN; delete: BOOLEAN; key: Text.T; keySite: SiteName := NIL)
   : Version RAISES {NotManager, PkgErr.E, LockOps.LockConflict} =
  VAR pe: PkgEntry;
  VAR v: Version;
  BEGIN
    IF (key = NIL) OR Text.Empty (key) THEN
      PkgErr.Raise (PkgErr.BadParameter);
    END;
    LOCK mutex DO
      IF (keySite = NIL) OR Text.Empty (keySite) THEN
        keySite := ourDB.localSite;
      END;
      pe := LookupEntry (ourDB, package, TRUE);
      IF (pe.owner.key = NIL)
        OR NOT (Text.Equal (key, pe.owner.key)
                AND Text.Equal (keySite, pe.owner.site)) THEN
        RAISE LockOps.LockConflict (pe.owner);
      END;
      IF delete THEN
        IF NOT Text.Equal (keySite, ourDB.localSite) THEN
          PkgErr.Raise (PkgErr.BadParameter);
        END;
        v.t := pe.instance;
        v.vn := DeletedVN;
        RETURN v;
      END;
      INC (pe.lastVN);
      WriteLogEntry (package, pe);
      RETURN LastVersion (pe);
    END;
  END AssignVersion;

PROCEDURE Commit
  (package: PN; version: Version; closure: CommitClosure) : BOOLEAN
    RAISES {PkgErr.E} =
  VAR comp: [-1..1];
  VAR pe: PkgEntry;
  VAR ok: BOOLEAN;
    <*FATAL NotManager*>
  BEGIN
    LOCK mutex DO
      pe := LookupEntry (ourDB, package, FALSE);
      comp := CompareVersions (version, CurVersion (pe));
      IF version.vn = InitialVN THEN
        IF pe.curVN # InitialVN THEN
          PkgErr.Raise (PkgErr.StaleVersion);
        END;
      ELSIF version.vn # DeletedVN THEN
        IF Text.Equal (ourDB.localSite, pe.managedBy)
              AND (version.vn > pe.lastVN) THEN
          PkgErr.Raise (PkgErr.BadParameter);
        END;
        IF (comp <= 0) THEN
          PkgErr.Raise (PkgErr.StaleVersion);
        END;
      END;
      EVAL AcquireLock (package, TRUE);
      pe.pendVN := version.vn;
      WriteLogEntry (package, pe);
    END;
    IF closure # NIL THEN ok := closure.work(); END;
    LOCK mutex DO
      IF ok THEN pe.curVN := pe.pendVN; pe.fp := Fingerprint.Zero;  END;
      pe.pendVN := NullVN;
      WriteLogEntry (package, pe);
      ReleaseLock (package);
    END;
    IF ok THEN LogHistory (HistoryEvent.CommitEvent, package, pe);  END;
    RETURN ok;
  END Commit;

(*
  package creation and deletion
*)

PROCEDURE CreateLocal(package: PN; key: Text.T; version: Version;
   createWork: CreateProc) RAISES {PkgErr.E} =
  VAR e: Entry;
  VAR pe: PkgEntry;
  BEGIN
    e.owner.key := key;
    e.owner.site := ourDB.localSite;
    e.managedBy := ourDB.localSite;
    e.fp := Fingerprint.Zero;
    IF version.t = 0 THEN
      LOCK mutex DO
        e.instance := instanceCtr;
        INC(instanceCtr);
      END;
    ELSE
      e.instance := version.t;
    END;
    e.lastVN := version.vn;
    e.curVN := version.vn;
    e.pendVN := NullVN;
    LOCK mutex DO
      CheckDirAndLock(package);
    END;
    TRY
      IF createWork # NIL THEN createWork (package);  END;
      LOCK mutex DO
        pe := AddEntry (ourDB, package, e);
        WriteLogEntry (package, pe);
      END;
    FINALLY
      ReleaseLock (package);
    END;
    LogHistory (HistoryEvent.CreateEvent, package, pe);
    IF key # NIL THEN LogHistory (HistoryEvent.LockEvent, package, pe); END;
  END CreateLocal;

PROCEDURE CreateCheck (package: PN) RAISES {PkgErr.E} =
  BEGIN
    CheckDirAndLock(package);
    ReleaseLock (package);
  END CreateCheck;

PROCEDURE CreateForeign
  (package: PN; owningSite: SiteName; instance: LockOps.Instance)
   RAISES {PkgErr.E} =
  VAR e: Entry;
  VAR pe: PkgEntry;
  BEGIN
    IF (owningSite = NIL) OR Text.Empty (owningSite) THEN
      PkgErr.Raise (PkgErr.BadParameter);
    END;
    e.owner.key := NIL;
    e.owner.site := NIL;
    e.managedBy := owningSite;
    e.instance := instance;
    e.fp := Fingerprint.Zero;
    e.lastVN := InitialVN;
    e.curVN := InitialVN;
    e.pendVN := NullVN;
    LOCK mutex DO
      CheckDirAndLock (package);
      TRY
        pe := AddEntry (ourDB, package, e);
        WriteLogEntry (package, pe);
      FINALLY
        ReleaseLock (package);
      END;
    END;
  END CreateForeign;

PROCEDURE CheckDirAndLock(pkg: PN) RAISES {PkgErr.E} =
  VAR pe: PkgEntry;
    <*FATAL NotManager*>
  BEGIN
    TRY
      pe := LookupEntry (ourDB, pkg, FALSE);
      PkgErr.Raise(PkgErr.PackageNameInUse);
    EXCEPT
      | PkgErr.E(ec) =>
          IF ec.head # PkgErr.NoSuchPackage THEN RAISE PkgErr.E(ec); END;
    END;
    IF NOT AcquireLock(pkg, FALSE) THEN
      PkgErr.Raise(PkgErr.PackageNameInUse);
    END;
  END CheckDirAndLock;

PROCEDURE Enumerate(dir: Dir; locksOnly, localOnly, pendingOnly: BOOLEAN)
   : EnumList RAISES {PkgErr.E} =
  VAR de: DirEntry; en: EnumList; pn: TEXT; pe: PkgEntry;
      it: SortedTextRefTbl.Iterator; r: REFANY;
      i: CARDINAL := 0;
  BEGIN
    LOCK mutex DO
      de := LookupDir(ourDB, dir);
      it := de.iterateOrdered();
      WHILE it.next(pn, r) DO
        pe := r;
        IF (pe.curVN # DeletedVN)
              AND ( NOT locksOnly OR (pe.owner.key # NIL))
              AND ( NOT localOnly OR Text.Equal (ourDB.localSite,pe.managedBy))
              AND ( NOT pendingOnly OR (pe.pendVN # NullVN)) THEN
          INC(i);
        END;
      END;
      IF i = 0 THEN RETURN NIL; END;
      en := NEW(EnumList, i);
      i := 0;
      it := de.iterateOrdered();
      WHILE it.next(pn, r) DO
        pe := r;
        IF (pe.curVN # DeletedVN)
              AND ( NOT locksOnly OR (pe.owner.key # NIL))
              AND ( NOT localOnly OR Text.Equal (ourDB.localSite,pe.managedBy))
              AND ( NOT pendingOnly OR (pe.pendVN # NullVN)) THEN
          en[i] := LockOps.EnumEntry{pe^, pn};
          INC(i);
        END;
      END;
    END;
    RETURN en;
  END Enumerate;

PROCEDURE GetEntry
  (package: PN; VAR (*OUT*) entry: Entry) RAISES {PkgErr.E} =
  VAR pe: PkgEntry;
    <*FATAL NotManager*>
  BEGIN
    LOCK mutex DO pe := LookupEntry (ourDB, package, FALSE); entry := pe^; END;
  END GetEntry;

PROCEDURE SetEntry(package: PN; VAR (*IN*) entry: Entry) RAISES {PkgErr.E} =
  VAR pe: PkgEntry;
  BEGIN
    LOCK mutex DO
      pe := AddEntry (ourDB, package, entry);
      WriteLogEntry(package, pe);
    END;
    LogHistory (HistoryEvent.SetEvent, package, pe);
  END SetEntry;

PROCEDURE SetFingerprint(package: PN; version: Version; fp: Fingerprint.T)
   RAISES {PkgErr.E} =
  VAR pe: PkgEntry;
    <*FATAL NotManager*>
  BEGIN
    LOCK mutex DO
      pe := LookupEntry (ourDB, package, FALSE);
      IF fp = pe.fp OR (CompareVersions (version, CurVersion (pe)) # 0) THEN
        RETURN;
      END;
      pe.fp := fp;
      WriteLogEntry (package, pe, FALSE);
    END;
    LogHistory (HistoryEvent.FPEvent, package, pe);
  END SetFingerprint;

TYPE
  MyClosure = StablePkl.Closure OBJECT
    thisSite: TEXT;
    create: BOOLEAN;
  OVERRIDES
    new := NewDB;
    apply := ApplyLogEntry;
  END;

PROCEDURE Init (VAR (*in/out*) site: SiteName; create: BOOLEAN := FALSE)
   : BOOLEAN =
  BEGIN
      (* initialize database, use existing DB if present *)
      (* returns local "site" parameter *)
      (* iff "create", then go ahead and create a DB if none present.
         in this case, use the supplied "site" value *)
      (* state from stable storage *)
    IF firstTime THEN
      historyFilePath := Pathname.Join(LocksDir, HistoryFile, NIL);
      firstTime := FALSE;
    END;
    PSLib.LogIt("Recovering stable storage");
    TRY
      stabledir := SmallDB.New("locks",
            NEW(MyClosure, thisSite := site, create := create));
      ourDB := stabledir.recover();
      WriteSnapshot ("Restart");
      site := ourDB.localSite;
    EXCEPT
      | OSError.E(errorArg) =>
          NoteStableError (PkgErr.Msg(errorArg));
          RETURN FALSE;
      | SmallDB.Failed(errorArg) =>
          NoteStableError(PkgErr.Msg(errorArg));
          RETURN FALSE;
    END;
    snapDaemon := Thread.Fork (NEW(Thread.Closure, apply := SnapshotDaemon));
    RETURN TRUE;
  END Init;

PROCEDURE NewDB(cl: MyClosure): REFANY RAISES {SmallDB.Failed} =
  BEGIN
    IF NOT cl.create THEN
      RAISE SmallDB.Failed(AtomList.List1(Atom.FromText("No database")));
    END;
    RETURN NEW(DB,
      localSite := cl.thisSite,
      dirs :=  NEW(SNetPathRefTbl.Default).init());
  END NewDB;

(*
   History logging
*)

PROCEDURE LogHistory (event: HistoryEvent; pn: PN; pe: PkgEntry) =
  VAR wr: Wr.T;
      pkg := PSLib.PkgText(pn);
    <* FATAL Thread.Alerted *>
  PROCEDURE Version() : TEXT =
    BEGIN
      RETURN " (" & Fmt.Int (pe.instance) & "." & Fmt.Int (pe.curVN) & ")";
    END Version;
  BEGIN
    TRY
      wr := FileSys.OpenAppend(historyFilePath);
      Wr.PutText(wr, FmtTime.Long (Time.Now ()));
      CASE event OF
        | HistoryEvent.CreateEvent =>
            Wr.PutText (wr, " N: " & pkg & Version() & "\n");
        | HistoryEvent.LockEvent =>
            Wr.PutText (wr, " L: " & pkg & " " & pe.owner.key & " ("
                           & pe.owner.site & ")\n");
        | HistoryEvent.UnlockEvent =>
            Wr.PutText (wr, " U: " & pkg & "\n");
        | HistoryEvent.CommitEvent =>
            Wr.PutText (wr, " C: " & pkg & Version() & "\n");
        | HistoryEvent.SetEvent =>
            Wr.PutText (wr, " S: " & pkg & Version() & "\n");
        | HistoryEvent.FPEvent =>
            Wr.PutText (wr, " F: " & pkg & Version() & "\n");
      END;
      Wr.Close (wr);
    EXCEPT
      | Wr.Failure, OSError.E =>
    END;
  END LogHistory;

PROCEDURE LogDirHistory (event: DirHistoryEvent; dir: Dir) =
  VAR wr: Wr.T;
      text := PSLib.PathText(dir);
    <* FATAL Thread.Alerted *>
  BEGIN
    TRY
      wr := FileSys.OpenAppend(historyFilePath);
      Wr.PutText(wr, FmtTime.Long (Time.Now ()));
      CASE event OF
        | DirHistoryEvent.DirCreateEvent =>
            Wr.PutText (wr, " D: " & text & " (create)\n");
        | DirHistoryEvent.DirRemoveEvent =>
            Wr.PutText (wr, " D: " & text & " (remove)\n");
      END;
      Wr.Close (wr);
    EXCEPT
      | Wr.Failure, OSError.E =>
    END;
  END LogDirHistory;


(*
   Version number utilities
*)

PROCEDURE CompareVersions
  (a, b: Version): [-1..1] RAISES {PkgErr.E} =
  BEGIN
    IF a.t # b.t THEN PkgErr.Raise (PkgErr.BadVersionStamp); END;
    IF a.vn = b.vn THEN
      RETURN 0;
    ELSIF a.vn > b.vn THEN
      RETURN 1;
    ELSE
      RETURN -1;
    END;
  END CompareVersions;

PROCEDURE NullVersion (): Version RAISES {} =
  VAR v: Version;
  BEGIN
    v.t := 0;
    v.vn := NullVN;
    RETURN v;
  END NullVersion;

PROCEDURE LastVersion (pe: PkgEntry): Version =
  VAR v: Version;
  BEGIN
    v.t := pe.instance;
    v.vn := pe.lastVN;
    RETURN v;
  END LastVersion;

PROCEDURE CurVersion (pe: PkgEntry): Version =
  VAR v: Version;
  BEGIN
    v.t := pe.instance;
    v.vn := pe.curVN;
    RETURN v;
  END CurVersion;

(*
 * Private procedures
 *)

(* Database manipulators *)

PROCEDURE AddEntry (db: DB; pn: PN; VAR e: Entry): PkgEntry RAISES {PkgErr.E} =
  VAR de: DirEntry; pe: PkgEntry;
      r: REFANY;
  BEGIN
    de := LookupDir(db, pn.dir);
    IF de.get(pn.arc, r) THEN
      pe := r;
      pe^ := e;
    ELSE
      pe := NEW(PkgEntry);
      pe^ := e;
      EVAL de.put(pn.arc, pe);
    END;
    RETURN pe;
  END AddEntry;

PROCEDURE LookupEntry (db: DB; package: PN; mustBeLocal: BOOLEAN): PkgEntry
    RAISES {PkgErr.E, NotManager} =
  VAR de: DirEntry; pe: PkgEntry;
      r: REFANY;
  BEGIN
    de := LookupDir(db, package.dir);
    IF de.get(package.arc, r) THEN
      pe := r;
      IF pe.curVN # DeletedVN THEN
        IF mustBeLocal AND NOT Text.Equal (db.localSite, pe.managedBy) THEN
          RAISE NotManager (pe.managedBy);
        END;
        RETURN pe;
      END;
    END;
    <*NOWARN*> PkgErr.Raise (PkgErr.NoSuchPackage);
  END LookupEntry;

PROCEDURE Purge () =
  VAR pe: PkgEntry; de: DirEntry;
      di: SNetPathRefTbl.Iterator;
      pi: TextRefTbl.Iterator;
      wdir: Dir; w: TEXT; r: REFANY;
      deletions: BOOLEAN;
      dt := ourDB.dirs;
  BEGIN
    di := dt.iterate();
    WHILE di.next(wdir, r) DO
      de := r;
      REPEAT
        deletions := FALSE;
        pi := de.iterate();
        WHILE NOT deletions AND pi.next(w, r) DO
          pe := r;
          IF (pe.curVN = DeletedVN) THEN
            EVAL de.delete(w, r);
            deletions := TRUE;
          END;
        END;
      UNTIL NOT deletions;
    END;
  END Purge;


(* volatile lock state *)
TYPE
  VolatileLock = REF VLRecord;
  VLRecord = RECORD next: VolatileLock; package: PN; END;


VAR
  volatileLockQ: VolatileLock; (* list of transaction in progress *)
  volatileLockWait := NEW(Thread.Condition);


PROCEDURE AcquireLock(pkg: PN; wait: BOOLEAN)  : (* gotIt *) BOOLEAN =
  VAR l: VolatileLock;
  BEGIN
    LOOP
      l := volatileLockQ;
      WHILE (l # NIL) AND ( NOT NetPath.EqualPN (pkg, l.package)) DO
        l := l.next;
      END;
      IF l = NIL THEN EXIT;  END;
      IF wait THEN
        RETURN FALSE;
      ELSE
        Thread.Wait (mutex, volatileLockWait);
      END;
    END;
    l :=  NEW (VolatileLock);
    l.package := pkg;
    l.next := volatileLockQ;
    volatileLockQ := l;
    RETURN TRUE;
  END AcquireLock;

PROCEDURE ReleaseLock (pkg: PN) =
  VAR try, prev: VolatileLock;
  BEGIN
    prev := NIL;
    try := volatileLockQ;
    WHILE (try # NIL) AND  NOT NetPath.EqualPN (pkg, try.package) DO
      prev := try;
      try := try.next;
    END;
    IF try # NIL THEN
      IF prev = NIL THEN
        volatileLockQ := try.next;
      ELSE
        prev.next := try.next;
      END;
      Thread.Broadcast (volatileLockWait);
    END;
  END ReleaseLock;

(* Stable storage operations *)

PROCEDURE WriteSnapshot (why: Text.T) =
  BEGIN
        (* called with mutex held *)
    IF logsize # 0 THEN
      PSLib.LogIt(why & " snapshot with log size " & Fmt.Int (logsize));
      Purge();
      TRY
        stabledir.snapshot(ourDB);
      EXCEPT
      | OSError.E(errorArg) =>
          NoteStableError (PkgErr.Msg(errorArg));
          Process.Exit (1);
      END;
      logsize := 0; (* No entries left in log either *)
    END;
  END WriteSnapshot;

VAR
  snapDaemon: Thread.T;

PROCEDURE SnapshotDaemon (<*UNUSED*>cl: Thread.Closure): REFANY =
  VAR cal: Date.T;
  BEGIN
    LOOP
      Thread.Pause (3.6D3);
      LOCK mutex DO
        IF logsize > (LogSizeLimit * 9) DIV 10 THEN
                  (* do it now to avoid a synchronous snapshot *)
          WriteSnapshot ("Background");
        ELSIF logsize > LogSizeLimit DIV 10 THEN
          cal := Date.FromTime(Time.Now());
          IF cal.hour <= 6 THEN
                      (* tidy up in the early hours of the morning *)
            WriteSnapshot ("Overnight");
          END;
        END;
      END; (*LOCK*)
    END; (*LOOP*)
    <*NOWARN*> RETURN NIL
  END SnapshotDaemon;

PROCEDURE CheckLogSize () =
  BEGIN
    IF logsize >= LogSizeLimit THEN WriteSnapshot ("Synchronous");  END;
  END CheckLogSize;

PROCEDURE WriteLogEntry (pn: PN; pe: PkgEntry; setTs: BOOLEAN := TRUE) =
  BEGIN
    IF setTs THEN pe.lastModified := Time.Now();  END;
    TRY
      stabledir.update(NEW(LogElem, pn := pn, e := pe));
    EXCEPT
      | OSError.E(errorArg) =>
          NoteStableError (PkgErr.Msg(errorArg));
          Process.Exit (1);
    END;
    INC (logsize);
    CheckLogSize ();
  END WriteLogEntry;

PROCEDURE WriteDirLogEntry (dir: Dir; add: BOOLEAN) =
  BEGIN
    TRY
      stabledir.update(NEW(LogElem, pn := PN{dir, NIL}, add := add));
    EXCEPT
      | OSError.E(errorArg) =>
          NoteStableError (PkgErr.Msg(errorArg));
          Process.Exit (1);
    END;
    INC (logsize);
    CheckLogSize ();
  END WriteDirLogEntry;

PROCEDURE ApplyLogEntry (<*UNUSED*> cl: StablePkl.Closure;
                                    le: LogElem; st: DB): DB =
  BEGIN
    INC (logsize);
    TRY
      IF le.pn.arc = NIL THEN
        AddDirEntry(st, le.pn.dir, le.add);
      ELSE
        EVAL AddEntry (st, le.pn, le.e^);
      END;
    EXCEPT
    | PkgErr.E =>
    END;
    RETURN st;
  END ApplyLogEntry;

PROCEDURE NoteStableError (t: TEXT) =
  BEGIN
    PSLib.LogIt("Stable storage error: " & t);
  END NoteStableError;


(*************************)
(* new public procedures *)
(*************************)

PROCEDURE CreateDir (dir: Dir) RAISES {PkgErr.E} =
  VAR cur := dir;
      first: BOOLEAN := TRUE;
      dt := ourDB.dirs;
      r: REFANY;
  BEGIN
    LOCK mutex DO
      WHILE cur # NIL DO
        IF dt.get(cur, r) THEN
          IF first THEN
            PkgErr.Raise(PkgErr.DirNameInUse);
          ELSE
            PkgErr.Raise(PkgErr.ParentDirExists);
          END;
        END;
        cur := NetPath.Parent(cur);
        first := FALSE;
      END;
      AddDirEntry(ourDB, dir, TRUE);
      WriteDirLogEntry (dir, TRUE);
    END;
    LogDirHistory (DirHistoryEvent.DirCreateEvent, dir);
  END CreateDir;

PROCEDURE RemoveDir (dir: Dir) RAISES {PkgErr.E} =
  VAR
    de: DirEntry;
    cur: VolatileLock;
  BEGIN
    LOCK mutex DO
      de := LookupDir (ourDB, dir);
              (* no packages in database *)
      IF de.size() # 0 THEN
        PkgErr.Raise(PkgErr.DirNotEmpty);
      END;
      (* no packages in lock queue *)
      cur := volatileLockQ;
      WHILE cur # NIL DO
        IF NetPath.Equal (dir, cur.package.dir) THEN
          PkgErr.Raise(PkgErr.DirNotEmpty);
        END;
        cur := cur.next;
      END;
      AddDirEntry(ourDB, dir, FALSE);
      WriteDirLogEntry(dir, FALSE);
    END;
    LogDirHistory (DirHistoryEvent.DirRemoveEvent, dir);
  END RemoveDir;

PROCEDURE CheckDir (dir: Dir) RAISES {PkgErr.E} =
  BEGIN
    EVAL LookupDir (ourDB, dir);
  END CheckDir;

PROCEDURE EnumerateDirs (): DirList RAISES {} =
  VAR it: SNetPathRefTbl.Iterator;
      en: DirList; r: REFANY;
      dt := ourDB.dirs;
  BEGIN
    LOCK mutex DO
      en := NEW(DirList, dt.size());
      it := dt.iterateOrdered();
      FOR i := 0 TO LAST(en^) DO EVAL it.next(en[i], r) END;
    END;
    RETURN en;
  END EnumerateDirs;

PROCEDURE LookupDir (db: DB; dir: Dir): DirEntry RAISES {PkgErr.E}  =
  VAR dt := db.dirs;
      r: REFANY;
  BEGIN
    IF NOT dt.get(dir, r) THEN PkgErr.Raise(PkgErr.NoSuchDir); END;
    RETURN r;
  END LookupDir;

PROCEDURE AddDirEntry (db: DB; dir: Dir; add: BOOLEAN) =
  VAR r: REFANY;
      dt := db.dirs;
  BEGIN
    IF add THEN
      EVAL dt.put(dir,  NEW(SortedTextRefTbl.Default).init());
    ELSE
      EVAL dt.delete(dir, r);
    END;
  END AddDirEntry;

(* conversion from old pickle format *)
(*

TYPE
  OldTable = REF ARRAY OF OldPkgEntry;
  OldPkgEntry = REF OldPkgEntryRecord;
  OldPkgEntryRecord = RECORD next: OldPkgEntry; e: OldEntry;  END;
  Seconds = INTEGER;
  FP = RECORD a, b: INTEGER;  END;
  FN = REF ARRAY OF TEXT;

TYPE
  OldDB = REF RECORD
           magic: INTEGER;
           version: INTEGER;
           localSite: Text.T;
           entries: INTEGER;
           packages: OldTable;
         END;
    (* Types for log entries *)

  OldEntry = RECORD
            package: FN;
            lastModified: Seconds;
            owner: Owner; (* who owns the lock for this package *)
                          (* package is not lock iff owner.key = NIL *)
            managedBy: SiteName;
                          (* the siphon site managing this package lock *)
            fp: FP; (* for fast package content comparison *)
                                        (* set this through Siphon.def *)
            fpValid: BOOLEAN; (* fingerprint is valid for curVN *)
            shared: BOOLEAN;
            (* TRUE => in more than one siphon's name space *)
            instance: Seconds; (* package create instance timestamp *)
            curVN: PackageObj.VN; (* package version number at local site *)
            lastVN: PackageObj.VN; (* most recent version number (unique) *)
            pendVN: PackageObj.VN; (* non-zero if Commit in progress *)
                (* note that the following fields are significant only at the
                       managing site: owner, lastVN *)
          END;

  OldLogEntry = REF OldEntry;

PROCEDURE ConvertDB(old: OldDB) =
    <* FATAL PkgErr.E *>
  VAR
    he: OldPkgEntry;
    e: Entry;
    db: DB;
  BEGIN
    db := NEW(DB,
             localSite := old.localSite,
             dirs := NEW(SNetPathRefTbl.Default).init());
    FOR i := 0 TO LAST(old.packages^) DO
      he := old.packages[i];
      WHILE he # NIL DO
        IF he.e.managedBy = NIL AND he.e.curVN # DeletedVN THEN
          AddDirEntry(db, FNToDir(he.e.package^), TRUE);
        END;
        he := he.next;
      END;
    END;
    FOR i := 0 TO LAST(old.packages^) DO
      he := old.packages[i];
      WHILE he # NIL DO
        IF he.e.managedBy # NIL AND he.e.curVN # DeletedVN THEN
          ConvertEntry(he.e, e);
          EVAL AddEntry(db, FNToPackage(he.e.package), e);
        END;
        he := he.next;
      END;
    END;
    ourDB := db;
  END ConvertDB;

PROCEDURE ApplyOldLogEntry (<*UNUSED*> cl: SmallDB.UpdateClosure;           
                              value: REFANY) =
  VAR e: Entry;
  BEGIN
    INC (logsize);
    TRY
      TYPECASE value OF
      | OldLogEntry(l) =>
          IF l.managedBy = NIL THEN
            AddDirEntry(FNToDir(l.package^), l.curVN # DeletedVN);
          ELSE
            ConvertEntry(l^, e);
            EVAL AddEntry(db, FNToPackage(l.package), e);
          END;
      ELSE
        <* ASSERT FALSE *>
      END;
    EXCEPT
    | PkgErr.E, NotManager =>
    END;
  END ApplyOldLogEntry;

PROCEDURE FNToDir(READONLY fn: ARRAY OF TEXT) : Dir =
  VAR tl := TextList.FromArray(fn);
  BEGIN
    IF Text.Equal(tl.head, "proj") THEN tl := tl.tail; END;
    IF TextList.Length(tl) >= 2 AND Text.Equal(TextList.Nth(tl, 1), "pkg") THEN
      tl.tail := tl.tail.tail;
    END;
    RETURN tl;
  END FNToDir;

PROCEDURE FNToPackage(fn: FN) : PN =
  BEGIN
    <* ASSERT(fn # NIL AND NUMBER(fn^) > 0) *>
    RETURN PN{FNToDir(SUBARRAY(fn^, 0, NUMBER(fn^)-1)), fn[LAST(fn^)]};
  END FNToPackage;

PROCEDURE ConvertEntry(VAR l: OldEntry; VAR e: Entry) =
  BEGIN
    e.lastModified := FLOAT(l.lastModified, LONGREAL);
    e.owner := l.owner;
    e.managedBy := l.managedBy;
    e.fp := LOOPHOLE(l.fp, Fingerprint.T);
    IF NOT l.fpValid THEN e.fp := Fingerprint.Zero; END;
    e.instance := l.instance;
    e.curVN := l.curVN;
    e.lastVN := l.lastVN;
    e.pendVN := l.pendVN;
  END ConvertEntry;
*)

BEGIN
  logsize := 0; (* Zero log entries read thus far *)
  volatileLockQ := NIL;
END PackageDB.

