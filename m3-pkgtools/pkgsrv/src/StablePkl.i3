(* Copyright 1993 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* SNetTblPkl.i3 - pickle special for lock database            *)
(* Last modified on Fri Apr 22 12:20:23 PDT 1994 by wobber *)

INTERFACE StablePkl;

IMPORT SmallDB, SNetPathRefTbl, SortedTextRefTbl, LockOps;

TYPE
  DB = REF RECORD
    localSite: TEXT;
    dirs: SNetPathRefTbl.T;          (* of DirEntry *)
  END;

  DirEntry = SortedTextRefTbl.T;     (* of PkgEntry *)
  PkgEntry = LockOps.RefEntry;

  LogElem = REF RECORD
    pn: LockOps.PN;            (* pn.arc = NIL if directory add/remove *)
    add: BOOLEAN := TRUE;      (* meaningful only for directory ops *)
    e: PkgEntry := NIL;        (* meaningful only for package ops *)
  END;
 
  Closure <: ClosurePublic;
  ClosurePublic = SmallDB.Closure OBJECT METHODS
    apply(le: LogElem; state: DB) : DB;
  END;

   (* the "new" method of Closure must be supplied by the client *)
   (* the "apply" method is used to apply log entries to an
      existing "DB".  "x" is guaranteed not "NIL". *)

END StablePkl.
