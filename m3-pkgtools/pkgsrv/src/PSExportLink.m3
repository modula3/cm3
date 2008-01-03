(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSExportLink.m3 *)
(* Last modified on Thu Feb  2 08:59:43 PST 1995 by kalsow *)
(*      modified on Fri Jul 30 10:05:04 PDT 1993 by wobber *)

MODULE PSExportLink;

IMPORT FileSys, NetPath, Pathname, RefList, NetObj, PackageEvents,
       PathMap, OSError, PackageObj, PkgErr, PSLib, Rd, SNetPathRefTbl,
       Text, Thread, Wr;

TYPE
  ExportLink = PackageObj.ExportLink;
  PN = PackageObj.PN;
  FN = FileSys.FN;

  Kind = {Old, Same, New, Skip, Bad, Deleted};  (* ordering matters *)
  Action = REF RECORD
    kind: Kind; export: ExportLink; link, referent: FN;
    reportType: PackageEvents.LinkET := PackageEvents.LinkET.Installed;
  END;

REVEAL
  WriteActions = BRANDED REF RECORD
    pn: PN;
    purge: BOOLEAN;
    mu: MUTEX;  (* for table *)
    tbl: SNetPathRefTbl.T;
  END;

PROCEDURE Prepare(
    pn: PackageObj.PN; fn: FN; oldLinks, links: ExportLinks;
    purge: BOOLEAN; mon: Monitor) : WriteActions
    RAISES {Thread.Alerted} =
  VAR
    wa := NEW(WriteActions,
      pn := pn, purge := purge,
      mu := NEW(MUTEX), tbl := NEW(SNetPathRefTbl.Default).init());
  BEGIN
    IF oldLinks # NIL THEN
      FOR i := 0 TO LAST(oldLinks^) DO
        LOCK wa.mu DO
          TRY
            VAR p := PathMap.MapExport(oldLinks[i].link); BEGIN
              EVAL wa.tbl.put(oldLinks[i].link,
                NEW(Action, kind := Kind.Old,
                     export := oldLinks[i],
                     link := p,
                     referent := Pathname.Join(
                         PSLib.RelativePath(p, fn),
                         NetPath.ToRelFN(oldLinks[i].referent), NIL)));
            END;
          EXCEPT
          | PkgErr.E =>
          END;
        END;
      END;
    END;
    IF links # NIL THEN
      FOR i := 0 TO LAST (links^) DO
        VAR
          a := NEW(Action, kind := Kind.New, export := links[i]);
       BEGIN
          TRY
            a.link := PathMap.MapExport(links[i].link);
            a.referent := Pathname.Join(
                             PSLib.RelativePath(a.link, fn),
                             NetPath.ToRelFN(links[i].referent), NIL);
            CASE FileSys.GetInfo(a.link).type OF
            | FileSys.FileType.SLink =>
                IF Text.Equal (a.referent, FileSys.ReadLink(a.link)) THEN
                  a.kind := Kind.Same;
                END;
            | FileSys.FileType.Normal =>
            ELSE
              a.kind := Kind.Bad;
              a.reportType := PackageEvents.LinkET.Denied;
            END;
          EXCEPT
          | OSError.E =>
          | PkgErr.E =>
                  a.kind := Kind.Skip;
                  a.reportType := PackageEvents.LinkET.NoDir;
          END;
          LOCK wa.mu DO EVAL wa.tbl.put(links[i].link, a); END;
        END;
      END;
    END;
    ReportLinks(wa, mon);
    RETURN wa;
  END Prepare;

PROCEDURE ReportLinks(wa: WriteActions; mon: PackageObj.Monitor)
    RAISES {Thread.Alerted} =
  VAR it: SNetPathRefTbl.Iterator;
      key: NetPath.T; val: REFANY;
      a: Action;
  BEGIN
    LOCK wa.mu DO
      it := wa.tbl.iterate();
      WHILE it.next(key, val) DO
        a := val;
        CASE a.kind OF
        | Kind.Old =>
            IF wa.purge THEN
              (* be very careful *)
              (* check that this is a link and that the referents are equal *)
              TRY
                IF FileSys.GetInfo(a.link).type = FileSys.FileType.SLink AND
                     Text.Equal (a.referent, FileSys.ReadLink(a.link)) THEN
                  a.reportType := PackageEvents.LinkET.Removed;
                  a.kind := Kind.Deleted;
                END;
              EXCEPT
              | OSError.E =>
              END;
            END;
        | Kind.New =>
            TRY
              VAR
                parent := Pathname.Prefix(a.link);
              BEGIN
                MaybeCreateDir(parent);
                IF FileSys.CheckAccess(parent, TRUE) THEN
                  a.reportType := PackageEvents.LinkET.Installed;
                ELSE
                  a.kind := Kind.Bad;
                  a.reportType := PackageEvents.LinkET.Denied;
                END;
              END;
            EXCEPT
            | OSError.E(err) =>
                IF FileSys.ClassifyError(err) = FileSys.ErrorClass.Lookup THEN
                  a.kind := Kind.Bad;
                  a.reportType := PackageEvents.LinkET.NoDir;
                ELSE
                  a.kind := Kind.Bad;
                  a.reportType := PackageEvents.LinkET.Denied;
                END;
            END;
        ELSE
        END;
        IF mon # NIL AND a.kind >= Kind.New  THEN
          TRY
            mon.report(
                NEW(PackageEvents.LinkReport,
                     type := a.reportType, path := a.export.link));
          EXCEPT
            | NetObj.Error =>
          END;
        END;
      END;
    END;
  END ReportLinks;

PROCEDURE Write(wa: WriteActions; wr: Wr.T)
    RAISES {PkgErr.E, Wr.Failure, Thread.Alerted} =
  VAR it: SNetPathRefTbl.Iterator;
      key: NetPath.T; val: REFANY;
      a: Action;
  PROCEDURE WriteToFile(err: BOOLEAN) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      Wr.PutText (wr, NetPath.ToText(key) & " " &
                      NetPath.ToText(a.export.referent));
      IF err THEN Wr.PutText(wr, " XXX"); END;
      Wr.PutChar(wr, '\n');
    END WriteToFile;
  BEGIN
    LOCK wa.mu DO
      it := wa.tbl.iterate();
      WHILE it.next(key, val) DO
        a := val;
        TRY
          CASE a.kind OF
          | Kind.Deleted =>
              FileSys.Remove(a.link);
              PSLib.LogIt("PM.Unexport " & PSLib.PathText(key));
          | Kind.New =>
              TRY FileSys.Remove (a.link);
              EXCEPT OSError.E => END;
              FileSys.SymLink (a.link, a.referent);
              PSLib.LogIt("PM.Export " & PSLib.PathText(key));
              WriteToFile(FALSE);
          | Kind.Same, Kind.Skip => WriteToFile(FALSE);
          | Kind.Bad => WriteToFile(TRUE);
          ELSE
          END;
        EXCEPT
        | OSError.E(ec) =>
            PSLib.MapError(wa.pn, "Export", ec);
        END;
      END;
   END;
  END Write;

EXCEPTION BadText;

PROCEDURE Read (rd: Rd.T): ExportLinks RAISES {Rd.Failure, Thread.Alerted} =
  VAR suffix: Text.T;
      res: ExportLinks;
      list: RefList.T;
      entries: CARDINAL;
      buffer: ARRAY [0..2048] OF CHAR;
      refNP, linkNP: NetPath.T;
      bufLen: CARDINAL;
      i: CARDINAL;
  PROCEDURE NextStr() : TEXT RAISES {BadText} =
    VAR j := i;
        res: TEXT;
    BEGIN
      WHILE j < bufLen AND buffer[j] # ' ' AND buffer[j] # '\n' DO INC(j); END;
      IF i = j THEN RAISE BadText; END;
      res := Text.FromChars(SUBARRAY(buffer, i, j-i));
      i := j + 1;
      RETURN res;
    END NextStr;
  BEGIN
    list := NIL;
    suffix := NIL;
    LOOP
      bufLen := Rd.GetSubLine(rd, buffer);
      IF bufLen = 0 THEN EXIT; END;
      TRY
        (* first string is the link *)
        i := 0;
        linkNP := NetPath.FromText(NextStr());
        refNP := NetPath.FromText(NextStr());
        list := RefList.Cons (linkNP, list);
        list := RefList.Cons (refNP, list);
      EXCEPT
      | BadText, NetPath.Invalid =>
      END;
    END;
    entries := RefList.Length (list) DIV 2;
    IF entries = 0 THEN RETURN NIL;  END;
    res :=  NEW (ExportLinks, entries);
    REPEAT
      DEC (entries);
      res[entries].referent := list.head;
      list := list.tail;
      res[entries].link := list.head;
      list := list.tail;
    UNTIL entries = 0;
    RETURN res;
  END Read;

PROCEDURE MaybeCreateDir(path: FN) =
  PROCEDURE MaybeCreateDirInner(p: FN) : BOOLEAN =
    VAR sawDir := FALSE;
    BEGIN
      IF p = NIL OR Text.Empty(p) THEN RETURN FALSE; END;
      TRY
        IF FileSys.GetInfo(p, TRUE).type # FileSys.FileType.Dir THEN
          RETURN FALSE;
        END;
        sawDir := TRUE;
      EXCEPT
      | OSError.E(ec) =>
          IF FileSys.ClassifyError(ec) # FileSys.ErrorClass.Lookup THEN
            RETURN FALSE;
          END;
          IF MaybeCreateDirInner(Pathname.Prefix(p)) THEN
            TRY
              IF NOT sawDir THEN FileSys.MakeDir(p); END;
            EXCEPT
            | OSError.E => RETURN FALSE;
            END;
          ELSE
            RETURN FALSE;
          END;
      END;
      RETURN TRUE;
    END MaybeCreateDirInner;
  BEGIN
    EVAL MaybeCreateDirInner(path);
  END MaybeCreateDir;

BEGIN
END PSExportLink.

