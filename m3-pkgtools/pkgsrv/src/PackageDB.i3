(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PackageDB.i3 -- package lock database *)
(* Last modified on Tue May  4 17:38:07 PDT 1993 by wobber *)
(*      modified on Wed Dec  5 17:04:34 GMT+1:00 1990 by prusker *)

INTERFACE PackageDB;

IMPORT LockOps, Fingerprint, PkgErr;

TYPE
  BreakRights = LockOps.BreakRights; (* OwnerOnly, SiteOnly, AnySite *)
  Entry = LockOps.Entry;
  RefEntry = LockOps.RefEntry;
  EnumList = LockOps.EnumList;
  PN = LockOps.PN;

  Owner = LockOps.Owner; (* RECORD key: TEXT; site: Site; *)
  SiteName = LockOps.SiteName; (* TEXT *)
  Instance = LockOps.Instance;
  Version = LockOps.Version;
  VN = LockOps.Version;

TYPE
  Dir = LockOps.Dir;
  DirList = LockOps.DirList;


EXCEPTION
  NotManager (SiteName);
      (* package not managed by this site, the correct managing
         site is given as arg *)


CONST
  NullVN = LockOps.NullVN;
  InitialVN = LockOps.InitialVN;
  DeletedVN = LockOps.DeletedVN;

PROCEDURE Lock
  (package: PN; version: Version; key: TEXT; keySite: SiteName := NIL)
   : Version RAISES {NotManager, PkgErr.E, LockOps.LockConflict};
  (* Possible ECs: NoSuchPackage, OldLocalVersion, BadParameter *)
  (* acquires the lock for this package if possible *)
  (* "version.vn" will default to curVN for local callers,
        such callers must pass version := NullVersion() *)
  (* "key" is used to set the current check-out state *)
  (* "keySite" is the site checking out the package, NIL => local *)
  (* pidgin M2+:
      e := LookupLocalEntry();   (* RAISES NoSuchPackage, NotManager *)
      IF e.owner.key # NIL AND
          ((key # e.owner.key) OR (keySite # e.owner.site)) RAISE LockConflict
      IF version.vn > e.lastVN RAISE BadParameter
      IF version.vn # e.lastVN RAISE OldLocalVersion
      e.owner.key := key
      e.owner.site := keySite
  *)

PROCEDURE Unlock
  (package: PN; version: Version; key: TEXT; keySite: SiteName := NIL;
   breakRights: BreakRights := BreakRights.OwnerOnly;
   forceVersion: BOOLEAN := FALSE)
   RAISES {NotManager, PkgErr.E, LockOps.LockConflict,
           LockOps.SynchVersions};
  (* Possible ECs: NoSuchPackage, OutstandingVersion *)
  (* unlocks the named package after checking lock ownership *)
  (* "version.vn" will default to curVN for local callers,
        such callers must pass version := NullVersion() *)
  (* SynchVersions error implies that client has break permissions *)
  (* pidgin M2+:
      e := LookupLocalEntry();   (* RAISES NoSuchPackage, NotManager *)
      CASE breakRights OF
      | OwnerOnly:
         IF (key # e.owner.key) OR (keySite # e.owner.site) RAISE LockConflict
      | SiteOnly:
         IF keySite # e.owner.site RAISE LockConflict
      | AnySite:
      END
      IF (version.vn > e.lastVN) RAISE BadParameter
      IF (version.vn # e.lastVN) THEN
         IF NOT forceVersion RAISE OutstandingVersion
         IF NOT Text.Equal(e.owner.site, keySite) INC(e.lastVN);
         RAISE SynchVersions(e.lastVN);
         (* caller must upgrade his curVN via Commit *)
      END;
      e.owner.key := NIL;
  *)

PROCEDURE AssignVersion
  (package: PN; delete: BOOLEAN; key: TEXT; keySite: SiteName := NIL)
   : Version RAISES {NotManager, PkgErr.E, LockOps.LockConflict};
  (* Possible ECs: NoSuchPackage *)

  (* validate a key/keySite pair for the named package *)
  (* "keySite" is the client's site, NIL => local *)
  (* assign a new version number *)
  (* if "delete" return DeletedVN *)
  (* pidgin M2+:
      e := LookupLocalEntry();   (* RAISES NoSuchPackage, NotManager *)
      IF (key # e.owner.key) OR (keySite # e.owner.site) RAISE LockConflict
      INC(e.lastVN)
      RETURN e.lastVN
  *)
(*
   Several of the following procedures obtain mutual exclusivity over
   some name in the name space outside of the scope of a single call.
   This mechanism is used internally during all Commit/Create calls.
*)
(*
   The following procedure is provided for changing the local
   current version number.
*)

TYPE
  CommitClosure = OBJECT METHODS 
    work() : BOOLEAN;
               (* true if commit should proceed *)
  END;

PROCEDURE Commit
  (package: PN; version: Version;
   closure: CommitClosure): (* ok *) BOOLEAN
   RAISES {PkgErr.E};
  (* Possible ECs: NoSuchPackage, PackageIsBusy, StaleVersion, BogusVersion *)
  (* RAISES Error(PackageIsBusy) if package doesn't become available over
     the course of some timeout (probably 20 secs). *)
  (* if version.vn = DeletedVN, then package is deleted after Commit *)
  (* pidgin M2+:
      e := LookupEntry(package);
      IF version.vn > e.lastVN RAISE BogusVersion
      IF version.vn <= e.curVN RAISE StaleVersion
      AcquireLock();
      e.pendVN := version.vn
      (* write log entry *)
      ok := commitWork()
      IF ok THEN
        e.fpValid := FALSE
        e.curVN := e.pendVN;
      ELSE
        e.pendVN := nullVN;
      END;
      (* write log entry *)
      ReleaseLock()
      RETURN ok
  *)
(*
  package creation and deletion
  Creation is in two steps so that the name can be reserved locally
  while calls are made to the remote site to establish foreign
  entries there.  When all foreign entries are established, the local
  entry can be written to stable storage.
  Deletion occurs when the client Commits with version.vn = DeletedVN
*)

TYPE
  CreateProc = PROCEDURE (pkg: PN) RAISES {PkgErr.E};
  (* raise Error to abort Create *)

PROCEDURE CreateLocal(package: PN; key: TEXT; version: Version;
   createWork: CreateProc) RAISES {PkgErr.E};
  (* Possible ECs: PackageNameInUse *)
  (* create a new package, managed by the local site *)
  (* "key" will be used to set the initial checkout state *)
  (* nothing written to stable storage until "createWork" returns *)
  (* IF version.instance = 0, then fill it in *)

PROCEDURE CreateCheck (package: PN) RAISES {PkgErr.E};
  (* Possible ECs: PackageNameInUse *)
  (* Checks if a package entry can be created under the given name. *)
  (* "localOK" implies that a localOnly package doesn't clash *)
  (* RAISES Error(PackageNameInUse) if a package entry already exists
     or if another Create operation is in progress. *)

PROCEDURE CreateForeign(
   package: PN; owningSite: SiteName; instance: Instance)
   RAISES {PkgErr.E};
  (* Possible ECs: PackageNameInUse *)
  (* create a pointer to a remote package, managed by "owningSite" *)
(*
   Version number utilities
*)

PROCEDURE CompareVersions (a, b: Version): [-1..1] RAISES {PkgErr.E};
             (* the obvious comparison:
                  res > 0 => a > b
                  res = 0 => a = b
                  res < 0 => a < b            *)
             (* raises Error(BadVersionStamp) *)

PROCEDURE NullVersion (): Version RAISES {};
             (* returns a version number of (0,0) *)
(*
   Miscellaneous operations
*)

PROCEDURE Enumerate(dir: Dir; locksOnly, localOnly, pendingOnly: BOOLEAN)
   : EnumList RAISES {PkgErr.E};
  (* entries are returned in alphanumeric sort order *)
  (* pending only enumerates entries which have pending transactions *)
  (* if dir # NIL  then only entries for which  dir = entry.dir are returned *)
  
PROCEDURE GetEntry(package: PN; VAR (*out*) e: Entry)
    RAISES {PkgErr.E};
  (* Possible ECs: NoSuchPackage *)
  (* fetches a consistent single entry from the database *)

PROCEDURE SetEntry(package: PN; VAR (*in*) e: Entry)
    RAISES {PkgErr.E};
  (* blasts specified entry into database ... overrides everything *)
  (* for administrative use only *)
  (* package or directory ... *)

PROCEDURE SetFingerprint
  (package: PN; version: Version; fp: Fingerprint.T)
    RAISES {PkgErr.E};
  (* Possible ECs: NoSuchPackage *)
  (* Note that that "fp" is the fingerprint for package/version. *)

PROCEDURE Init (VAR (*in/out*) site: SiteName; create: BOOLEAN := FALSE)
   : BOOLEAN;
  (* initialize database, use existing DB if present *)
  (* returns local "site" parameter *)
  (* iff "create", then go ahead and create a DB if none present.
     in this case, use the supplied "site" value *)
  (* returns TRUE if database is successfully inited *)


(* new procedures *)

PROCEDURE CreateDir (name: Dir) RAISES {PkgErr.E};
  (* DirNameInUse, ParentDirExists *)
  (* raises ParentDirExists if any parent directory exists *)

PROCEDURE RemoveDir (name: Dir) RAISES {PkgErr.E};
  (* NoSuchDir, DirNotEmpty *)
  (* raises DirNotEmpty if directory contains package entries *)

PROCEDURE CheckDir (name: Dir) RAISES {PkgErr.E};
  (* NoSuchDir *)

PROCEDURE EnumerateDirs (): DirList;

END PackageDB.
