(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSGet.def *)
(* Last modified on Fri May 28 15:23:35 PDT 1993 by wobber *)

INTERFACE PSGet;

IMPORT PackageObj, FileSys, PkgErr, OSError, Thread;

TYPE
  PN = PackageObj.PN;
  FN = FileSys.FN;
  DoItProc = PROCEDURE(dir: FN) RAISES {Thread.Alerted};

  T = PackageObj.Source OBJECT
    version: PackageObj.Version;
  METHODS
    doItLocked(proc: DoItProc) RAISES {Thread.Alerted};
  END;


PROCEDURE New (pn: PN; fn: FN; ship: BOOLEAN): T RAISES {PkgErr.E};

PROCEDURE DoubleRename (
     path: FN;
     targetPath: FN; (* should be real path of "path" *)
     srcPath, destPath: FN; doDiscard: BOOLEAN)
   RAISES {OSError.E};
  (* path must be non-empty *)
  (* This operation first guarantees mutual exclusion over other New
     and DoubleRename calls to "path".  It also acquires the lock
     associated with outstanding T's referencing the same real
     directory.  Hold all these locks, it then proceeds to rename
     targetPath to destPath, and then srcPath to destPath.  srcPath
     can be NIL, in which case the second rename is not performed.
     A directory need not exist at targetPath unless destPath is
     non-NIL.  Finally, all outstanding T's which pointed to path are
     modified to point to destPath.  If doDiscard is TRUE, the directory
     subtree at destPath is flagged for deletion to occur when all
     outstanding T's referencing it are gone. *)

END PSGet.
