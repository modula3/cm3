(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSGet.m3 *)
(* Last modified on Thu Feb  2 08:55:16 PST 1995 by kalsow *)
(*      modified on Tue Mar  8 13:39:18 PST 1994 by wobber *)

MODULE PSGet;

IMPORT OSError, FileSys, PackageLib, PackageObj, Pathname, PkgErr,
       PSExportLink, PSLib, NetPath, Rd, Text, Time, WeakRef, Thread,
       Lex, FloatMode, LockOps;

TYPE
  ExportLinks = PackageObj.ExportLinks;
  Version = PackageObj.Version;

TYPE
  TT = T OBJECT
    mu: MUTEX;
    pn: PN;
    e: Enum;
    en: PackageObj.Enum;
  OVERRIDES
    (* get methods *)
    enum := Enumerate;
    pullFile := PullFile;
    links := ReadLinks;
    (* T methods *)
    doItLocked := DoItLocked;
  END;

EXCEPTION BadVersion;

PROCEDURE New (pn: PN; fn: FN; ship: BOOLEAN): T RAISES {PkgErr.E} =
  VAR t := NEW(TT, version := Version{0, LockOps.NullVN},
                  mu := NEW(MUTEX), pn := pn, e := NewEnum(fn),
                  en := PackageObj.Enum{Time.Now(), NIL});
    rd: Rd.T;
    dir: FN;
    int1, int2: INTEGER;
    <* FATAL Thread.Alerted *>
  BEGIN
    TRY
      TRY
        dir := t.e.acquire();
        IF FileSys.GetInfo (dir, TRUE).type # FileSys.FileType.Dir THEN
          PkgErr.Raise (PkgErr.NoSuchPackage);
        END;
      EXCEPT
      | OSError.E(err) =>
          IF FileSys.ClassifyError(err) = FileSys.ErrorClass.Lookup THEN
            IF ship THEN RETURN NIL; END;
            DiagnoseLookupError(dir);
          END;
          PSLib.MapError (pn, "OpenPkg", err);
      END;
      TRY
        rd := FileSys.OpenRead(Pathname.Join(dir,PackageLib.VersionFile,NIL));
        TRY
          int1 := Lex.Int(rd);
          IF Rd.GetChar(rd) # '.' THEN RAISE BadVersion; END;
          int2 := Lex.Int(rd);
          IF (int1 < 0) OR (int2 < 0) THEN RAISE BadVersion; END;
          t.version.t := int1;
          t.version.vn := int2;
        FINALLY
          Rd.Close(rd);
        END;
      EXCEPT
        | BadVersion, Lex.Error, FloatMode.Trap, Rd.Failure,
          Rd.EndOfFile, OSError.E =>
            PSLib.LogIt("PM.Error: BadVersionFile " & PSLib.PkgText(pn));
      END;
    FINALLY
      t.e.release();
    END;
    PSLib.StatIncr (PSLib.StatGetInProgress);
    EVAL WeakRef.FromRef(t, CleanupT);
    RETURN t;
  END New;

PROCEDURE DiagnoseLookupError(dir: TEXT) RAISES {PkgErr.E} =
  BEGIN
    TRY
      EVAL FileSys.GetInfo(Pathname.Prefix(dir), TRUE);
      PkgErr.Raise (PkgErr.NoSuchPackage);
    EXCEPT
    | OSError.E(err) =>
        IF FileSys.ClassifyError(err) = FileSys.ErrorClass.Lookup THEN
          PkgErr.Raise (PkgErr.NoSuchDir);
        END;
    END;
  END DiagnoseLookupError;

PROCEDURE CleanupT (<*UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
  VAR t: TT := r;
  BEGIN
    PSLib.StatDecr (PSLib.StatGetInProgress);
    CleanupEnum(t.e);
  END CleanupT;

PROCEDURE Enumerate(t: TT): PackageObj.Enum
    RAISES {PkgErr.E} =
  BEGIN
    LOCK t.mu DO
      TRY
        IF t.en.dir = NIL AND t.e # NIL THEN
          t.en.dir := PackageLib.Enumerate(t.e);
          TRY
            t.en.ts := FileSys.GetInfo(t.e.acquire(), TRUE).date;
          FINALLY
            t.e.release();
          END;
        END;
      EXCEPT
      | OSError.E(ec) =>
          PSLib.MapError (t.pn, "Enumerate", ec);
      END;
    END;
    RETURN t.en;
  END Enumerate;

PROCEDURE PullFile (t: TT; path: NetPath.T): Rd.T
    RAISES {PkgErr.E} =
  VAR dir: Text.T; rd: Rd.T;
      fn := NetPath.ToRelFN(path);
  BEGIN
    LOCK t.mu DO
      TRY
        dir := t.e.acquire();
        TRY
          rd := FileSys.OpenRead(Pathname.Join(dir, fn, NIL));
        FINALLY
          t.e.release();
        END;
      EXCEPT
      | OSError.E(ec) =>
          PSLib.MapError (t.pn, "PullFile", ec);
      END;
    END;
    RETURN rd;
  END PullFile;

PROCEDURE ReadLinks (t: TT): ExportLinks
    RAISES {Thread.Alerted} =
  VAR dir: TEXT;
      rd: Rd.T := NIL;
  BEGIN
    LOCK t.mu DO
      IF t.e = NIL THEN RETURN NIL; END;
      TRY
        dir := t.e.acquire();
        TRY
          rd := FileSys.OpenRead(
                    Pathname.Join(dir, PackageLib.ExportLinkFile, NIL));
          RETURN PSExportLink.Read (rd);
        FINALLY
          t.e.release();
          IF rd # NIL THEN
            TRY Rd.Close(rd) EXCEPT Rd.Failure => END;
          END;
        END;
      EXCEPT
      | Rd.Failure, OSError.E => RETURN NIL;
      END;
    END;
  END ReadLinks;

PROCEDURE DoItLocked(t: TT; proc: DoItProc) RAISES {Thread.Alerted} =
  BEGIN
    proc(t.e.acquire());
    t.e.release();
  END DoItLocked;


(* atomic update handling *)

TYPE
  Enum = PackageLib.EnumClosure OBJECT
    next: Enum;
    path: FN; (* the initial pathname for the bits *)
    currentPath: FN;
    (* current real directory for the original bits *)
    waitCount: CARDINAL := 0;
    lockCount: INTEGER := 0;
    useCount: INTEGER := 0;
    wait: Thread.Condition;
    deletePending: BOOLEAN := FALSE;
  OVERRIDES
    acquire := Acquire;
    release := Release;
  END;

VAR
  mutex: MUTEX := NEW(MUTEX);
  enumList: Enum := NIL;

PROCEDURE NewEnum (path: FN; lock: BOOLEAN := FALSE): Enum RAISES {} =
  VAR e: Enum;
  BEGIN
    (* NewInternal(fn,TRUE) returns with lock count = -1 => exclusive *)
    <* ASSERT (path # NIL) AND NOT Text.Empty (path) *>
    LOCK mutex DO
      e := enumList;
      WHILE e # NIL DO
        IF Text.Equal (path, e.path) AND (e.path = e.currentPath) THEN
          IF e.lockCount # 0 THEN
            INC (e.waitCount);
            Thread.Wait (mutex, e.wait);
            DEC (e.waitCount);
            e := enumList; (* start at beginning of list next time *)
          ELSE
            EXIT;
          END;
        ELSE
          e := e.next;
        END;
      END;
      IF e = NIL THEN
        e := NEW (Enum,
               next := enumList,
               path := path,
               currentPath := path,
               wait := NEW(Thread.Condition));
        enumList := e;
      END;
      IF lock THEN e.lockCount := -1; END;
      INC(e.useCount);
    END;
    RETURN e;
  END NewEnum;

PROCEDURE Acquire (e: Enum): FN RAISES {} =
  BEGIN
    LOCK mutex DO
      WHILE e.lockCount < 0 DO
        INC (e.waitCount);
        Thread.Wait (mutex, e.wait);
        DEC (e.waitCount);
      END;
      INC (e.lockCount);
      <* ASSERT e.currentPath # NIL *>
      RETURN e.currentPath;
    END;
  END Acquire;

PROCEDURE Release(e: Enum) RAISES {} =
  BEGIN
    LOCK mutex DO
      IF e.lockCount > 0 THEN
        DEC (e.lockCount);
      ELSIF e.lockCount =  -1 THEN
        e.lockCount := 0;
      ELSE
        <* ASSERT (FALSE) *>
      END;
      IF e.waitCount # 0 THEN Thread.Signal (e.wait);  END;
      IF e.useCount = 0 THEN CleanupInner(e); END;
    END;
  END Release;

PROCEDURE DoubleRename
  (fn: FN; targetPath: FN; (* should be FileSys.GetPath of "path" *)
   srcPath, destPath: FN; doDiscard: BOOLEAN)
   RAISES {OSError.E} =
  VAR e: Enum;
  BEGIN
    e := NewEnum (fn, TRUE);
        (* now we have the write lock for this pathname *)
        (* we are also sure that no Gets on the T are outstanding *)
    TRY
      IF destPath = NIL THEN
        e.currentPath := NIL;
      ELSE
        FileSys.Rename (targetPath, destPath);
        e.currentPath := destPath;
      END;
            (* now rename to the real package *)
      IF srcPath # NIL THEN
        FileSys.Rename (srcPath, targetPath);
      END;
            (* finally cleanup atomic T *)
      IF doDiscard THEN e.deletePending := TRUE;  END;
    FINALLY
      LOCK mutex DO DEC(e.useCount); END;
      Release(e);
    END;
  END DoubleRename;

PROCEDURE CleanupEnum (e: Enum) RAISES {} =
  BEGIN
    LOCK mutex DO
      DEC(e.useCount);
      IF e.useCount = 0 THEN CleanupInner(e); END;
    END;
  END CleanupEnum;

PROCEDURE CleanupInner (e: Enum) RAISES {} =
  VAR try, prev: Enum;
  BEGIN
    prev := NIL;
    try := enumList;
    WHILE (try # NIL) AND (e # try) DO
      prev := try;
      try := try.next;
    END;
    IF try # NIL THEN
      IF prev = NIL THEN
        enumList := e.next;
      ELSE
        prev.next := e.next;
      END;
    END;
    IF e.deletePending AND e.currentPath # NIL THEN
      TRY
        FileSys.Remove (e.currentPath, TRUE);
      EXCEPT
        | OSError.E =>
      END;
    END;
  END CleanupInner;

BEGIN
END PSGet.

