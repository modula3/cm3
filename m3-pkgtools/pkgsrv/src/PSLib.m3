(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSLib.m3 *)
(* Last modified on Fri Apr 29 12:43:24 PDT 1994 by wobber *)

MODULE PSLib;

IMPORT Fmt, OSError;
IMPORT FileSys, PackageObj, PkgErr, NetPath,  Pathname,
       ServerLog, Text, TextSeq, Time, Site;

VAR stats: ServerLog.Stats := NIL;

PROCEDURE DefineStats(tag: TEXT) =
  VAR statNames := ARRAY [0..StatLast] OF TEXT {
                       "get", "currGet", "ship", "currShip",
                       "backup", "prepare", "currPrepare", "commit"};
  BEGIN
    stats := ServerLog.NewStats (log, tag, statNames);
  END DefineStats;
    
PROCEDURE LogIt(t: TEXT) =
  BEGIN
    ServerLog.WriteText(log, t & "\n");
  END LogIt;
  
PROCEDURE LogFmt(fmt: TEXT; <*NOWARN*> texts: TA) =
  BEGIN
    FOR i := 0 TO LAST(texts) DO
      IF texts[i] = NIL THEN texts[i] := ""; END;
    END;
    ServerLog.WriteText(log, Fmt.FN(fmt, texts));
  END LogFmt;
  
PROCEDURE PkgText(pn: PN) : TEXT =
  BEGIN
    RETURN "\"" & NetPath.PNToText(pn) & "\"";
  END PkgText;

PROCEDURE DirText(path: NetPath.T) : TEXT =
  BEGIN
    RETURN PathText(path);
  END DirText;

PROCEDURE PathText(path: NetPath.T) : TEXT =
  BEGIN
    RETURN "\"" & NetPath.ToText(path) & "\"";
  END PathText;

PROCEDURE MapError (pn: PackageObj.PN; op: TEXT; e: OSError.Code)
    RAISES {PkgErr.E} =
  BEGIN
    LogIt("PM.MapError " & PkgText(pn) & ", op=" & op & ", " &
                                                 PkgErr.Msg(e));
    RAISE PkgErr.E(PkgErr.MapOSError(e));
  END MapError;

PROCEDURE RelativePath(src, tgt: FN) : FN =
  VAR
    x: Pathname.T := NIL;
    s,t: TextSeq.T;
    i, j := 1;
    imax, jmax: CARDINAL;
    <* FATAL Pathname.Invalid *>
  BEGIN
    s := Pathname.Decompose(src);
    t := Pathname.Decompose(tgt);
    imax := s.size();
    jmax := t.size();
    WHILE i < imax AND j # jmax AND Text.Equal(s.get(i), t.get(j)) DO
      INC(i); INC(j);
    END;
    WHILE i < imax-1 DO
      x := Pathname.Join(x, Pathname.Parent, NIL);
      INC(i);
    END;
    WHILE j # jmax DO
      x := Pathname.Join(x, t.get(j), NIL);
      INC(j);
    END;
    RETURN x;
  END RelativePath;

PROCEDURE RealPath(p: FN) : FN RAISES {OSError.E} =
  BEGIN
    RETURN FileSys.GetPath(p);
  END RealPath;

PROCEDURE PrefixDirName (t: FN; prefix: TEXT; unique: BOOLEAN): FN =
  VAR
    suffix, r: FN;
  BEGIN
        (* insert prefix directory after parent directory name *)
        (* eg: /proj/packages/foo => /proj/packages/????/foo *)
    suffix := Pathname.Last(t);
    r := Pathname.Join(Pathname.Prefix(t), prefix, NIL);
        (* now make sure the new (internal) dir really exists.
           14-Jan-91 hisgen: changed this to do PGetInfo first,
           in order to be cheaper on Echo.  The MakeDir of
           /proj/packages/tmp (and /proj/packages/backups) was
           getting the WriteToken on the /proj/packages directory
           unnecessarily, since in most cases the "tmp" subdirectory
           already exists. *)
    TRY
      EVAL FileSys.GetInfo (r, (*follow*) TRUE);
    EXCEPT
      | OSError.E =>
          TRY FileSys.MakeDir (r);  EXCEPT | OSError.E  => END;
    END; (*try*)
        (* now complete the path name *)
    IF unique THEN
      suffix := suffix & "_" & Fmt.Int (ROUND(Time.Now()));
    END;
    RETURN Pathname.Join(r, suffix, NIL);
  END PrefixDirName;

PROCEDURE StatIncr(statNum: CARDINAL) =
  BEGIN
    stats.incr(statNum);
  END StatIncr;
  
PROCEDURE StatDecr(statNum: CARDINAL) =
  BEGIN
    stats.decr(statNum);
  END StatDecr;

PROCEDURE GetReplicas (): ReplicaSet =
    (* Site.GetLocal  succeeded once ... not supposed to fail thereafter *)
  BEGIN
    RETURN Site.Get().replicas;
  END GetReplicas;

BEGIN
END PSLib.
