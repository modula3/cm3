(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* LockOps.def *)
(* Last modified on Mon Apr  4 16:09:16 PDT 1994 by wobber *)
(*      modified on Fri Jan  4 15:59:46 GMT+1:00 1991 by prusker *)

INTERFACE LockOps;
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

IMPORT Fingerprint, PackageObj, PkgErr, Time;

TYPE
  T <: ROOT; (* lock server instance, NIL means the local one *)

  Owner = RECORD key: TEXT; site: SiteName; END;
  ShipArray = ARRAY OF PackageObj.Ship;   (* NIL elements are ignored *)
  SiteName = TEXT;

  Auth = PackageObj.Auth;
  Dir = PackageObj.Dir;
  Instance = PackageObj.Instance;
  PN = PackageObj.PN;
  VN = PackageObj.VN;
  Version = PackageObj.Version;
    (*
       There is one database entry for each package at each site.  Only
       one lock server manages the locks for any given package.  This
       server's site is stored as "managedBy".  If the package is locked, the
       owner's key and site are stored in "owner".
       Packages are managed by a version number scheme.  Each instance
       of a package ship acquires a new version number.  Version numbers
       are monotonically increasing and are stored with the actual package
       data at each replicas.  Three version numbers are stored in the lock
       database for each package.  The "curVN" describes the most
       recent version successfully shipped.  "lastVN" describes the
       last version number to have be issued to a ship operation.  Finally,
       a non-zero "pendVN" indicates that the replicas are in the process
       of rolling over to the indicated version.  The actual file bits are
       moved to be replicas prior to initiation of "rollover".  This serves
       to shrink the window during which the replicas are inconsistent to
       a single directory rename.  If the lock server crashes with
       pendVN # 0, then the lock server queries the replicas as to
       their most recent version and sets curVN appropriately.
       When a client checks out a package, he gets the version number of
       the most recent version.  Only replicas which hold that version
       (or possibly later versions) are eligible to act as a source for
       the package.
       Lock entries are removed from the database *very* infrequently.
       An entry is considered to be "deleted" when curVN is set to
       DeletedVN.  At that point it can be recycled (for another create).
       A fingerprint of the package content is stored as "fp".  The
       "fpValid" field indicates whether this fingerprint is valid for
       curVN.
    *)


TYPE
  DirList = REF ARRAY OF Dir;

  EnumList = REF ARRAY OF EnumEntry;
  EnumEntry = RECORD e: Entry; arc: TEXT; fill: TEXT := NIL; END;

  RefEntry = REF Entry;
  Entry = RECORD
            lastModified: Time.T;
            owner: Owner; (* who owns the lock for this package *)
                          (* package is not lock iff owner.key = NIL *)
            managedBy: SiteName;
                          (* the siphon site managing this package lock *)
            fill: TEXT := NIL;  (* for alignment *)
            fp: Fingerprint.T; (* for fast package content comparison *)
                               (* Fingerprint.Zero if invalid *)
            instance: Instance; (* package create instance timestamp *)
            curVN: VN; (* package version number at local site *)
            lastVN: VN; (* most recent version number (unique) *)
            pendVN: VN; (* non-zero if Commit in progress *)
                (* note that the following fields are significant only at the
                       managing site: owner, lastVN *)
          END;


TYPE
  BreakRights = {OwnerOnly, (* caller must be the lock owner *)SiteOnly,
                 (* caller must be at lock owning site *)
                 AnySite(* anyone may unlock *)};


TYPE
  CommitEC = PkgErr.TL;      (* iff non-NIL, an AtomList from PkgErr.i3 *)
  CommitFailures = REF ARRAY OF CommitEC;

CONST
  NullVN = 0; (* the first version of a package *)
  InitialVN = 1; (* the first version of a package *)
  DeletedVN = LAST (VN); (* the version number for deleted packages *)
        (* Note that only the first and last versions of a package
           are empty.   Any other Commit of null content is disallowed. *)

EXCEPTION
  LockConflict (Owner); (* says who the real lock owner is *)
  CommitFailed (CommitFailures);
  SynchVersions (Version);
      (* occurs on Unlock when (ver < lastVer) *)


(* *)
(* procedures *)
(* *)

PROCEDURE New (): T RAISES {PkgErr.E};

PROCEDURE SetServerT(t: T);
   (* register local server object *)

PROCEDURE Create (auth: Auth; pn: PN; initialKey: TEXT;
   version: Version := Version{0, InitialVN}; 
   remoteCheck: BOOLEAN := TRUE; t: T := NIL) RAISES {PkgErr.E};
  (* Possible ECs: PackageNameInUse, NoSuchDir *)
  (*
     Attempts to create a package lock entry for the named package
     managed by the local lock server.  The package is locked under
     the supplied "initialKey".  Pointers to the new package lock entry
     will be installed in all domains that share the repository implied
     by "package".   To maintain name consistency between siphon sites,
     package names are checked for remote existence prior to successful
     creation.  The remote check can be overriden by calling with
     "remoteCheck" = FALSE.  This can lead to database inconsistencies.
     Package creation at multiple sites is not an atomic transaction.
     The order of operations is as follows:
          -- the package is checked for uniqueness locally
          -- a local in-core lock is acquired on the package name
          -- all remote sites are checked that package doesn't exist
          -- the local entry is created and the lock released
     This all takes place synchronously.  The actual remote package
     entry is created the first time the package is shipped.  (There
     might be a ship for the initial null version.)
  *)
  (* raises NoSuchDir if parent directory doesn't exist *)

PROCEDURE Remove(auth: Auth; pn: PN; key: TEXT; reship: BOOLEAN := TRUE;
   t: T := NIL) RAISES {PkgErr.E, CommitFailed, LockConflict};
  (*
     This call removes the named package lock entry after checking
     lock ownership. If the lock is managed by a remote site,
     Error(PackageManagedRemotely) is raised.  This call also removes
     the package from all known replicas and a null ship (version.vn =
     DeletedVN) of the package is queued for transmission to all
     remote siphons.
     Package deletion involves a Commit.  If the deletion fails because
     the Commit failed.  Then CommitFailed can arise.
  *)

PROCEDURE Lock(auth: Auth; pn: PN; version: Version; key: TEXT;
   keySite: SiteName := NIL; t: T := NIL): Version
   RAISES {PkgErr.E, LockConflict};
  (* attempts to acquire the named lock *)
  (* if the lock is managed by a remote site, that site is
     contacted synchronously through the siphon as part of this call *)
  (* resultant version describes the most recent version of the package *)
  (* see comments for PackageDB.Lock *)
  (* version should be NullVersion() when called locally.  the siphon
     software will supply a non-null version and keySite.  A non-null
     keySite is sufficient to terminate recursion in the case that
     the callee doesn't manage the package *)

PROCEDURE Unlock(auth: Auth;  pn: PN; version: Version; key: TEXT;
   keySite: SiteName := NIL; breakRights: BreakRights := BreakRights.OwnerOnly;
   forceVersion: BOOLEAN := FALSE; t: T := NIL)
   RAISES {PkgErr.E, CommitFailed, LockConflict, SynchVersions};
  (* Unlocks the named package *)
  (* If the lock is managed by a remote site, then Unlock is
     called synchronously accross the siphon *)
  (* "breakRights" describes how aggressive the system should be
     in attempting to break existing locks. *)
  (* (breakRights = AnySite) can only be used at the managing site. *)
  (* the system asserts that the local (curVN,instance) is equal to
     (lastVN,instance) at the managing site.  this might force a "commit"
     of null content to take place,  if "forceVersion" is false,
     then Error(OutstandingVersion) is RAISED instead. *)
  (* See comments for PackageDB.Unlock. *)
  (* Unlock with forceVersion involves a Commit.  If this fails then
     CommitFailed can arise. *)
  (* version should be NullVersion() when called locally.  the siphon
     software will supply a non-null version and keySite.  A non-null
     keySite is sufficient to terminate recursion in the case that
     the callee doesn't manage the package *)
  (* SynchVersions only shows through if keySite # NIL *)

PROCEDURE AssignVersion(
   auth: Auth; pn: PN; key: TEXT; keySite: SiteName := NIL;
   t: T := NIL): Version RAISES {PkgErr.E, LockConflict};
  (* This procedure is called by the packagetool to initiate a package ship. *)
  (* Returns a new version number to identify the ship operation. *)
  (* If the caller holds the lock, then a unique version number is issued. *)
  (* For foreign packages, the remote siphon is called synchronously. *)
  (* A non-null keySite is sufficient to terminate recursion in the case that
     the callee doesn't manage the package *)

PROCEDURE Commit (auth: Auth; pn: PN; version: Version;
   VAR ships: ShipArray; reship: BOOLEAN := TRUE; t: T := NIL): CommitFailures
   RAISES {CommitFailed, PkgErr.E};
  (* Caller should have "prepared" ships (PackageObj.Ship.prepare) to all
     replicas in "ships".   First the lock server checks the validity
     of the supplied version number and iff valid, marks the entry as
     unstable.  Next the argument replicas are contacted and instructed to
     commit the ship in progress ... this must succeed for at least one
     of the named replicas.  If successful, the (curVN,instance for the
     package is set to "version". *)
  (* The definition of a "valid" version number is as follows:
         version.instance = entry.instance
         AND
         Managing site:  lastVN >= version > localCurVersion
         Foreign site:   version > localCurVersion
     "localCurVersion" is the value of curVN at the local site *)
  (* This procedure guarantees mutual exclusion over all ships of the
     argument package. *)
  (* This operation also makes an entry in the siphon queue if it is
     appropriate for this package. *)
  (* If this procedure returns NIL (the normal case) then all replicas
     have successfully committed the new version.  If it returns
     non-NIL, then the return value indicates which replica(s) (in the
     same order as the replicas argument) failed and for which reason.
     If all replicas fail, then the CommitResult is passed back as an
     argument to the CommitFailed error. *)
  (* IFF "reship" then package will be enqueued at all siphons *)


(* lock enumeration *)

PROCEDURE Enumerate
  (dir: Dir; site: SiteName := NIL; locksOnly: BOOLEAN := TRUE;
   localOnly: BOOLEAN := TRUE; pendingOnly: BOOLEAN := FALSE;
   t: T := NIL) : EnumList RAISES {PkgErr.E};
  (* Enumerate returns information about entries in the specified locking
     database.  "locksOnly" requests data about only those packages
     which are currently checked out. *)
  (* package entries are returned in alphanumeric sort order *)
  (* If site = NIL then the local locking database is enumerated. *)
  (* IF site # NIL then the specified remote site is enumerated. *)
  (* IFF localOnly, then only entries managed at "site" are returned *)
  (* IFF pendingOnly, then only entries with pendVN # NullVN are returned *)
  (* if dir # NIL then only entries for which dir = entry.dir are returned *)

PROCEDURE GetEntry(
   pn: PN; goRemote: BOOLEAN := FALSE; t: T := NIL) : RefEntry
   RAISES {PkgErr.E};
  (* Get just one entry from database.  "goRemote" requests that
     data be fetched directly from the managing site of each package.
     Otherwise, the local lock database is examined.
     If "goRemote" is requested and a remote server is inaccesible,
     then Error(RemoteLockServerDown) will be raised.
  *)

(* the following two procedures are administrative tools *)
(* they apply only to the database at the target machine *)

PROCEDURE SetEntry(auth: Auth; pn: PN; entry: RefEntry; t: T := NIL)
   RAISES {PkgErr.E};
  (* This overrides everything.  An administrative tool. *)

PROCEDURE SetFingerprint(
    auth: Auth; pn: PN; version: Version; fp: Fingerprint.T; t: T := NIL)
    RAISES {PkgErr.E};
  (* Sets the fingerprint for this package.  It also notes whether
     the supplied fingerprint is valid for the current version, (ie.
     fpValid := (version = curVN)). *)
(* the following two procedures are called only through siphon ops *)
(* they apply only to the database at the target machine *)

PROCEDURE CreateCheck(pn: PN; t: T := NIL) RAISES {PkgErr.E};
  (* Possible ECs: PackageNameInUse *)
  (* Checks if a package entry can be created under the given name. *)
  (* RAISES Error(PackageNameInUse) if a package entry already exists
     or if another Create operation is in progress. *)

PROCEDURE CreateForeign(
   auth: Auth; pn: PN; owningSite: SiteName;
   instance: Instance; t: T := NIL) RAISES {PkgErr.E};
  (* Possible ECs: PackageNameInUse, NoSuchDir *)
  (* create a pointer to a remote package, managed by "owningSite" *)
  (* raises NoSuchDir if parent directory doesn't exist *)

PROCEDURE RemoveForeign(auth: Auth; pn: PN; t: T := NIL)
     RAISES {PkgErr.E, CommitFailed};


(*************************)
(* directory operations  *)
(*************************)

PROCEDURE CreateDir (auth: Auth; dir: Dir; t: T := NIL) RAISES {PkgErr.E};
  (* DirNameInUse, ParentDirExists *)
  (* raises ParentDirExists if any parent directory exists *)

PROCEDURE RemoveDir (auth: Auth; dir: Dir; t: T := NIL) RAISES {PkgErr.E};
  (* NoSuchDir, DirNotEmpty *)
  (* raises DirNotEmpty if directory contains package entries *)

PROCEDURE CheckDir (name: Dir; t: T := NIL) RAISES {PkgErr.E};
  (* NoSuchDir *)

PROCEDURE EnumerateDirs
  (site: SiteName := NIL; t: T := NIL): DirList RAISES {PkgErr.E};

END LockOps.
