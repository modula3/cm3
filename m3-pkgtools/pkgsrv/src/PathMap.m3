(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PathMap.m3 *)
(* Last modified on Fri May 21 14:14:13 PDT 1993
 by wobber *)

MODULE PathMap;

IMPORT Pathname, NetPath, LockOps, PkgErr, FileSys,
       NetPathRefTbl, OSError, SiteServer, TextList, Time;

PROCEDURE Init (siteT: SiteServer.T) =
  BEGIN
    ourSiteT := siteT;
  END Init;

PROCEDURE GetReps () : LockOps.DirList =
  VAR res: REF ARRAY OF NetPath.Dir;
      r := GetRepInfo();
      len: CARDINAL;
      x: REFANY;
      it: NetPathRefTbl.Iterator;
  BEGIN
    LOCK r.mu DO
      len := r.reps.size();
      IF len = 0 THEN RETURN NIL; END;
      res := NEW(REF ARRAY OF NetPath.Dir, len);
      it := r.reps.iterate();
      FOR i := 0 TO len-1 DO EVAL it.next(res[i], x); END;
    END;
    RETURN res;
  END GetReps;

PROCEDURE GetVols(dir: NetPath.Dir) : TextList.T RAISES {PkgErr.E} =
  VAR r := GetRepData(dir);
  BEGIN
    IF r.vols # NIL THEN RETURN r.vols; END;
    RETURN TextList.Cons(r.fn, NIL);
  END GetVols;

PROCEDURE GetBestVol(dir: NetPath.Dir) : FN RAISES {PkgErr.E} =
  VAR vols: TextList.T;
      best: FN := NIL;
      bestFree: CARDINAL := 0;
      free: CARDINAL;
  BEGIN
    vols := GetVols(dir);
    WHILE vols # NIL DO
      TRY
        free := FileSys.FreeDiskSpace(vols.head);
        IF best = NIL OR free > bestFree THEN
          best := vols.head;
          bestFree := free;
        END;
      EXCEPT
      | OSError.E =>
      END;
      vols := vols.tail;
    END;
    RETURN best;
  END GetBestVol;

PROCEDURE MapDir(dir: NetPath.Dir) : FN RAISES {PkgErr.E} =
  BEGIN
    IF NOT NetPath.Check(dir) THEN
      PkgErr.Raise(PkgErr.BadParameter);
    END;
    RETURN GetRepData(dir).fn;
  END MapDir;
   (* maps the repository "dir" into a file name. *)

PROCEDURE MapPkg(pn: NetPath.PN) : FN RAISES {PkgErr.E} =
   (* maps the package name  "pn" into a file name. *)
  BEGIN
    IF NOT NetPath.CheckArc(pn.arc) THEN
      PkgErr.Raise(PkgErr.BadParameter);
    END;
    RETURN Pathname.Join(GetRepData(pn.dir).fn, pn.arc, NIL);
  END MapPkg;

PROCEDURE MapExport(path: NetPath.T) : FN RAISES {PkgErr.E} =
  VAR p := path;
      r := GetRepInfo();
      ref: REFANY;
      len := TextList.Length(path);
      i := len;
      fn: FN;
  BEGIN
    IF NOT NetPath.Check(path) THEN
      PkgErr.Raise(PkgErr.BadParameter);
    END;
    LOCK r.mu DO
      LOOP
        DEC(i);
        p := NetPath.Parent(p);
        IF p = NIL THEN EXIT; END;
        IF r.exports.get(p, ref) THEN
          fn := ref;
          WHILE i # len DO
            fn := Pathname.Join(fn, TextList.Nth(path, i), NIL);
            INC(i);
          END;
          RETURN fn;
        END;
      END;
    END;
    <*NOWARN*> PkgErr.Raise(PkgErr.NoSuchDir);
  END MapExport;

PROCEDURE GetRepData(dir: NetPath.T) : SiteServer.RepData RAISES {PkgErr.E} =
  VAR r := GetRepInfo();
      ref: REFANY;
  BEGIN
    IF NOT NetPath.Check(dir) THEN
      PkgErr.Raise(PkgErr.BadParameter);
    END;
    LOCK r.mu DO
      IF r.reps.get(dir, ref) THEN RETURN ref; END;
    END;
    <*NOWARN*> PkgErr.Raise(PkgErr.NoSuchDir);
  END GetRepData;

VAR
  mu := NEW(MUTEX);
  ourSiteT: SiteServer.T;
  lastRepInfo: SiteServer.RepInfo := NIL;
  lastRepInfoTime: Time.T := 0.0D0;

CONST RepInfoTimeout = 3.0D2;       (* 5 minutes *)

PROCEDURE GetRepInfo() : SiteServer.RepInfo =
  BEGIN
    LOCK mu DO
      IF (Time.Now() - lastRepInfoTime) > RepInfoTimeout THEN
        lastRepInfo := ourSiteT.repInfo();
      END;
      RETURN lastRepInfo;
    END;
  END GetRepInfo;

BEGIN
END PathMap.
