(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PackageObj.i3 *)
(* Last modified on Mon Mar  7 17:46:59 PST 1994 by wobber *)

INTERFACE PackageObj;

IMPORT FileSys, NetObj, NetPath, RefList, PkgErr, Rd, Time, Thread;

TYPE

  (* package names *)
  Path = NetPath.T;
  Dir = NetPath.Dir;          (* logical repository name *)
  PN = NetPath.PN;
  Referent = NetPath.Referent;

  (* misc other stuff *)
  Auth = TEXT;
  FileInfo = FileSys.FileInfo;
  FileType = FileSys.FileType;

  Int32 = BITS 32 FOR [-16_7FFFFFFF..16_7FFFFFFF];

  Instance = Int32;  (* an instantiation (create) timestamp *)
  VN = Int32; (* version number -- monotonically increasing*)
  Version = RECORD t: Instance; vn: VN; END;
  
  ExportLink = RECORD link, referent: Path; END;
  ExportLinks = REF ARRAY OF ExportLink;

  Enum = RECORD ts: Time.T; dir: DirEnum; fill: TEXT := NIL; END;

  DirEnum = RefList.T  (* of DirElem *);
  DirElem = REF RECORD
    info: FileInfo;
    arc: TEXT;
    referent: Referent;    (* for sym links only *)
    children: DirEnum;
    fill: TEXT := NIL;
  END;
    (* the enumeration starts with the first child of the directory *)

  ShipOptions = RECORD
    keepBackup: BOOLEAN;      (* keep backup package instead of deleting *)
    purgeLinks: BOOLEAN;      (* delete links no longer exported *)
    forceDateMatch: BOOLEAN;  (* all files with differing dates are updated,
                                       instead of just newer source files *)
  END;

TYPE
  T = NetObj.T OBJECT METHODS
    version(package: PN) : Version
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    newSource(auth: Auth; package: PN; VAR OUTversion: Version) : Source
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    newShip(auth: Auth; package: PN; options: ShipOptions) : Ship
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    vcommit(auth: Auth; package: PN; version, prevVersion: Version) : BOOLEAN
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    removeBackup(auth: Auth; package: PN)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    checkDir(dir: Dir; child: TEXT)
        RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    status() : TEXT RAISES {NetObj.Error, Thread.Alerted};
  END;
  (* vcommit: update the current package version number to "version",
       but only if the package previously held "previousVersion"; return
       result indicates whether this was successfully accomplished. *)
  (* copy: copies sourcePkg with the given version, at local replica
       or at another replica; if found, sourcePkg is copied to targetPkg,
       and the export links are set accordingly.  Replaces targetPkg if
       it exists.  Returns FALSE if the specified version cannot be
       found.  Returns TRUE otherwise (success). *)
  (* checkDir: checks if packages can be created at this path.
       If not, raises CantCreateAtPath.  If "childName" is non-NIL,
       this also checks if a directory of that name can be created
       at "pathName", and raises CantCreateAtPath if not. *)

TYPE
  Source = NetObj.T OBJECT METHODS
    enum() : Enum RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    pullFile(path: Path): Rd.T
              RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    links(): ExportLinks RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
  END;
    (* enum method:
         returns a "Enum" describing the package directory. *)
    (* pullFile method:
         get a reader on the named file in the package *)
    (* links method:
         reads the set of export links associated with this package *)

EXCEPTION SourceOutOfDate(Path);

TYPE
  Siblings = REF ARRAY OF Ship;    (* NIL elements are ignored *)
  
  Monitor = NetObj.T OBJECT METHODS
    report(arg: REFANY) RAISES {NetObj.Error, Thread.Alerted};
  END;

  Ship = NetObj.T OBJECT METHODS
    prepare(source: Source; siblings: Siblings; monitor: Monitor)
        RAISES {SourceOutOfDate, NetObj.Error, PkgErr.E, Thread.Alerted};
    commit(version: Version) RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
    pullFile(path: Path; date: Time.T; length: CARDINAL) : Rd.T
        RAISES {NetObj.Error, Thread.Alerted};
  END;
    (* prepare method:
         Prepare ship transaction for Commit.
         The callee updates itself from the argument source
         and its siblings.  Siblings are given priority
         for pulling files.  Progress reports are reported
         using the supplied Monitor object. Upon successful
         completion, the package is ready for commit. *)
    (* commit method
         Called by the lock server (or by client in "wild" ship).
         Does a double rename to swap temporary and real package.
         Saves a backup copy according to ship options.
         If version.vn = NullVN, the previous package version (if any)
         will be used.  If version.vn = DeletedVN, the package AND any
         backup are deleted *)
    (* pullFile method
         Called to fetch a file from a sibling ship.  Returns NIL
         if file is not available.  This call can block for a while
         if the sibling is currently fetching "path". *)

(* server instances *)

PROCEDURE New (replica: TEXT): T
   RAISES {NetObj.Error, PkgErr.E, Thread.Alerted};
   (* can pass (NIL), implies the local replica *)
   (* site implies configuration information within
            which "name" identifies a packageserver *)
        
PROCEDURE SetServerT(t: T; replica: TEXT);
   (* register local server object *)

END PackageObj.
