(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Subrepo.m3 *)
(* Last modified on Fri May 14 15:03:44 PDT 1993 by wobber  *)
(*      modified on Wed Jul  1 14:18:23 GMT+2:00 1992 by prusker *)

MODULE Subrepo;

IMPORT LockOps, PackageObj, PkgErr, NetPath, Time, TextRefTbl;

FROM LockOps IMPORT SiteName;

VAR
  table: RECORD
      mutex: MUTEX;
      t: TextRefTbl.T;
    END;
  
TYPE CacheElem = REF RECORD timeout: Time.T; dirs: LockOps.DirList; END;

CONST
  ShortTimeout = 60;         (* one minute *)
  LongTimeout = 30 * 60;     (* 15 minutes *)

PROCEDURE Query(site: SiteName) =
  BEGIN
    EVAL HasInner(site, NIL, TRUE);
  END Query;

PROCEDURE Has(site: SiteName; pkg: PackageObj.PN): R =
  BEGIN
    RETURN HasInner(site, pkg.dir, FALSE);
  END Has;

PROCEDURE HasDir(site: SiteName; dir: PackageObj.Dir): R =
  BEGIN
    RETURN HasInner(site, dir, FALSE);
  END HasDir;

PROCEDURE HasInner(site: SiteName; dir: PackageObj.Dir; force: BOOLEAN): R =
  VAR value: REFANY;
      dirs: LockOps.DirList;
      e: CacheElem := NIL;
  BEGIN
    LOCK table.mutex DO
      IF table.t = NIL THEN
        table.t := NEW(TextRefTbl.Default).init();
      END;
      IF table.t.get(site, value) THEN
        e := value;
        IF NOT force AND Time.Now() < e.timeout THEN
          RETURN InDirList(dir, e.dirs);
        END;
      END;
    END;
    TRY
      dirs := LockOps.EnumerateDirs(site);
      IF dirs = NIL THEN dirs := NEW(LockOps.DirList, 0); END;
      e := NEW(CacheElem,
              timeout := Time.Now() + FLOAT(LongTimeout, LONGREAL),
              dirs := dirs);
    EXCEPT
    | PkgErr.E =>
         IF e # NIL THEN RETURN InDirList(dir, e.dirs); END;
         e := NEW(CacheElem,
              timeout := Time.Now() + FLOAT(ShortTimeout, LONGREAL),
              dirs := NIL);
    END;
    LOCK table.mutex DO EVAL table.t.put(site, e); END;
    RETURN InDirList(dir, dirs);
  END HasInner;

PROCEDURE InDirList(dir: PackageObj.Dir; dirs: LockOps.DirList) : R =
  BEGIN
    IF dirs = NIL THEN RETURN R.Dontknow; END;
    FOR i := 0 TO LAST(dirs^) DO
      IF NetPath.Equal(dir, dirs[i]) THEN RETURN R.Yes; END;
    END;
    RETURN R.No;
  END InDirList;

BEGIN
  table.mutex := NEW(MUTEX);
  table.t := NIL; 
END Subrepo.

