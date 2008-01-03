(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSLib.i3 *)
(* Last modified on Fri May 21 14:14:13 PDT 1993
 by wobber *)

INTERFACE PSLib;

IMPORT OSError, LockOps, NetPath, PackageObj,
       FileSys, PkgErr, ServerLog;

TYPE
  Dir = PackageObj.Dir;
  PN = PackageObj.PN;
  FN = FileSys.FN;
  SiteName = LockOps.SiteName;
  ReplicaSet = REF ARRAY OF TEXT;

CONST
    (* log statistic indices *)
  StatGets = 0;
  StatGetInProgress = 1;
  StatShips = 2;
  StatShipInProgress = 3;
  StatBackups = 4;
  StatPrepares = 5;
  StatPrepareInProgress = 6;
  StatCommits = 7;
  StatLast = StatCommits;

CONST
  TemporaryDir = "tmp";
  DiscardDir = "waste";   (* a subdirectory of "tmp" *)
  BackupDir = "backups";

CONST
  SystemAuth = "pkgsrvr";

VAR
  log: ServerLog.T;
  localSite: SiteName;

TYPE TA = ARRAY OF TEXT;

PROCEDURE DefineStats(tag: TEXT);

PROCEDURE LogIt(t: TEXT);
  (* writes to log -- appending <CR> *)
  
PROCEDURE LogFmt(fmt: TEXT; texts: TA);
  (* formatted writes to log -- no appended <CR> *)
  (* allows NIL elements, changes them to "" *)
 
(* procedures for the logging of pkgs, dirs and paths *)

PROCEDURE PkgText(pn: PN) : TEXT;
PROCEDURE DirText(dir: Dir) : TEXT;
PROCEDURE PathText(path: NetPath.T) : TEXT;

PROCEDURE MapError (pn: PN; op: TEXT; e: OSError.Code) RAISES {PkgErr.E};

PROCEDURE RelativePath(src: FN; tgt: FN) : FN;

PROCEDURE RealPath(p: FN) : FN RAISES {OSError.E};
PROCEDURE PrefixDirName (t: FN; prefix: TEXT; unique: BOOLEAN): FN;

PROCEDURE StatIncr(statNum: CARDINAL);
PROCEDURE StatDecr(statNum: CARDINAL);

PROCEDURE GetReplicas (): ReplicaSet;

END PSLib.
