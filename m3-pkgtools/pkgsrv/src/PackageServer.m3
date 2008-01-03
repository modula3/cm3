(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PackageServer.m3 *)
(* Last modified on Thu Feb  2 09:09:54 PST 1995 by kalsow *)
(*      modified on Fri Jun  4 16:39:07 PDT 1993 by wobber *)
(*      modified on Mon Nov 18 14:40:17 GMT+1:00 1991 by prusker *)
(*      modified on Mon Jan 14 19:29:40 1991 by hisgen *)

MODULE PackageServer;

IMPORT Fmt;
IMPORT FileSys, Pathname, LockOps, PkgErr, PackageObj, OSError,
       PathMap, PSGet, PSLib,  PSShip, ServerLog;

TYPE
  Auth = PackageObj.Auth;
  PN = PackageObj.PN;
  FN = FileSys.FN;
  Source = PackageObj.Source;
  Ship = PackageObj.Ship;
  ShipOptions = PackageObj.ShipOptions;
  Version = PackageObj.Version;

TYPE
  T = PackageObj.T BRANDED OBJECT
  OVERRIDES
    version := GetVersion;
    newSource := NewSource;
    newShip := NewShip;
    vcommit := CommitVersion;
    removeBackup := RemoveBackup;
    checkDir := CheckDir;
    status := Status;
  END;

PROCEDURE New (): PackageObj.T =
  BEGIN
    RETURN NEW (T);
  END New;

(* get ops *)

PROCEDURE GetVersion
  (<*UNUSED*> t: T; pn: PN): Version
    RAISES {PkgErr.E} =
  VAR fn := PathMap.MapPkg(pn);
  BEGIN
    RETURN PSGet.New(pn, fn, FALSE).version;
  END GetVersion;

PROCEDURE NewSource (<*UNUSED*> t: T; auth: Auth; pn: PN;
    VAR OUTversion: Version): Source
    RAISES {PkgErr.E} =
  VAR s: PSGet.T;
      fn := PathMap.MapPkg(pn);
  BEGIN
    PSLib.StatIncr (PSLib.StatGets);
    CheckUser (auth);
    s := PSGet.New (pn, fn, FALSE);
    OUTversion := s.version;
    PSLib.LogIt(
      "PM.Get " & PSLib.PkgText(pn) & ", user=" & auth & ", ver="
           & Fmt.Int (OUTversion.t) & "." & Fmt.Int (OUTversion.vn));
    RETURN s;
  END NewSource;

PROCEDURE NewShip (<*UNUSED*> t: T; auth: Auth; pn: PN;
   options: ShipOptions): Ship
   RAISES {PkgErr.E} =
  VAR
    ship: Ship;
    realPackage, vol: FN;
    ignoreContent: BOOLEAN;
    fn := PathMap.MapPkg(pn);
    get: PSGet.T;
  BEGIN
    ignoreContent := FALSE;
    PSLib.StatIncr (PSLib.StatShips);
    CheckUser (auth);
    get := PSGet.New(pn, fn, TRUE);
    TRY
      IF get = NIL THEN
        vol := PathMap.GetBestVol(pn.dir);
        IF vol = NIL THEN PkgErr.Raise(PkgErr.NoSuchDir); END;
        IF NOT CheckWriteAccess (vol) THEN
          PkgErr.Raise(PkgErr.AccessViolation);
        END;
        realPackage := Pathname.Join(PSLib.RealPath(vol), pn.arc, NIL);
      ELSE
        realPackage := PSLib.RealPath(fn);
        IF  NOT (CheckWriteAccess (realPackage)
                 AND CheckWriteAccess (Pathname.Prefix(realPackage))) THEN
          PkgErr.Raise(PkgErr.AccessViolation);
        END;
                (* ignore all previous content iff corrupt *)
                (* corrupt is indicated by NullVN and non-null instance *)
        IF (get.version.t # 0) AND (get.version.vn = LockOps.NullVN) THEN
          ignoreContent := TRUE;
        END;
      END;
    EXCEPT
    | OSError.E(err) => PSLib.MapError (pn, "Ship", err);
    END;
    ship := PSShip.New (pn, fn, realPackage, get, ignoreContent, options);
    PSLib.LogIt ("PM.Ship " & PSLib.PkgText(pn) & ", user=" & auth);
    RETURN ship;
  END NewShip;

PROCEDURE CommitVersion
  (<*UNUSED*> t: T; auth: Auth; pn: PN; version: Version;
    previousVersion: Version): BOOLEAN
   RAISES {PkgErr.E} =
  VAR get: PSGet.T;
      fn := PathMap.MapPkg(pn);
  BEGIN
    get := PSGet.New (pn, fn, FALSE);
    IF previousVersion.vn # LockOps.NullVN THEN
      IF (get.version.t # previousVersion.t)
        OR (get.version.vn < previousVersion.vn) THEN
        RETURN FALSE;
      END;
    END;
    IF NOT PSShip.CommitVersion (get, version) THEN RETURN FALSE;  END;
    PSLib.LogIt(
      "PM.CommitVersion " & PSLib.PkgText(pn) & ", version="
             & Fmt.Int (version.t) & "." & Fmt.Int (version.vn)
             & ", user=" & auth);
    RETURN TRUE;
  END CommitVersion;

PROCEDURE RemoveBackup(<*UNUSED*> t: T; auth: Auth; pn: PN)
    RAISES {PkgErr.E} =
  VAR backup: FN;
      pkgPath := PathMap.MapPkg(pn);
  BEGIN
        (* delete backup package and link if any *)
    CheckUser (auth);
    TRY
      backup := PSLib.PrefixDirName (pkgPath, PSLib.BackupDir, FALSE);
      FileSys.Remove(PSLib.RealPath(backup), TRUE);
            (* backupDir might have been a link ... remove it *)
      FileSys.Remove (backup);
    EXCEPT
      | OSError.E =>
    END;
  END RemoveBackup;

PROCEDURE CheckDir (<*UNUSED*> t: T; dir: PackageObj.Dir; arc: TEXT)
    RAISES {PkgErr.E} =
  VAR foundDir: BOOLEAN := FALSE;
      path := PathMap.MapDir(dir);
  BEGIN
    TRY
      IF NOT CheckWriteAccess (path) THEN
        PkgErr.Raise(PkgErr.AccessViolation);
      END;
      foundDir := TRUE;
      IF arc # NIL AND
               NOT CheckWriteAccess (Pathname.Join(path, arc, NIL)) THEN
        PkgErr.Raise(PkgErr.AccessViolation);
      END;
    EXCEPT
    | OSError.E(ec) =>
        IF FileSys.ClassifyError(ec) = FileSys.ErrorClass.Lookup THEN
          IF NOT foundDir THEN PkgErr.Raise(PkgErr.NoSuchDir); END;
        ELSE
          PSLib.MapError (PN{dir, arc}, "CheckDir", ec);
        END;
    END;
  END CheckDir;

PROCEDURE Status (<*UNUSED*> t: T) : TEXT =
  BEGIN
    RETURN ServerLog.DumpStats(PSLib.log);
  END Status;

(* private procs *)

PROCEDURE CheckUser (auth: TEXT) RAISES {PkgErr.E} =
    (* VAR uT: User.T; pswdOK: BOOLEAN; *)
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
          RAISE (PkgErr.E, PkgErr.InvalidCredentials);
        END;
        *)
  END CheckUser;

PROCEDURE CheckWriteAccess (path: FN): BOOLEAN RAISES {OSError.E} =
  BEGIN
    IF FileSys.GetInfo (path, TRUE).type # FileSys.FileType.Dir THEN
      RETURN FALSE;
    END;
    RETURN FileSys.CheckAccess (path, TRUE, FALSE);
  END CheckWriteAccess;

BEGIN
END PackageServer.
