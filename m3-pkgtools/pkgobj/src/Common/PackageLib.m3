(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PackageLib.m3 *)
(* Last modified on Fri Jun  4 13:44:14 PDT 1993 by wobber *)

MODULE PackageLib;

IMPORT FileSys, OSError, PackageObj, Pathname, NetPath, PkgErr,
       RefList, TextList, TextListSort, Rd, Text, Time, Thread;

EXCEPTION FatalError; <* FATAL FatalError *>

PROCEDURE Enumerate (e: EnumClosure): PackageObj.DirEnum
    RAISES {OSError.E} =
  CONST StatsPerWait = 40;
  VAR waitCtr: CARDINAL := StatsPerWait;
  PROCEDURE Build (path: FileSys.FN): PackageObj.DirEnum
      RAISES {OSError.E} =
    VAR t: TEXT;
        edir, cpath, spath: FileSys.FN;
        eList: FileSys.Enumeration;
        res: PackageObj.DirEnum := NIL;
        elem: PackageObj.DirElem;
    BEGIN
      TRY
        edir := e.acquire();
        IF path # NIL THEN
          edir := Pathname.Join(edir, path, NIL);
        END;
        eList := FileSys.Enumerate (edir);
      FINALLY
        e.release();
      END;
      (* hack for broken List generic *)
      (* IF eList = NIL THEN RETURN NIL; END; *)
      eList := TextList.ReverseD(TextListSort.SortD(eList));
      WHILE eList # NIL DO
        t := eList.head;
        IF NOT SpecialFile(t) THEN
          (* gross hackery for lack of multithreaded OS *)
          DEC(waitCtr);
          IF waitCtr = 0 THEN
            Thread.Pause(5.0D-2);
            waitCtr := StatsPerWait;
          END;
          (* end of hackery *)
          elem := NEW (PackageObj.DirElem);
          elem.children := NIL;
          elem.arc := t;
          cpath := Pathname.Join(path, t, NIL);
          TRY
            spath := Pathname.Join(e.acquire(), cpath, NIL);
            elem.info := FileSys.GetInfo(spath, FALSE);
            IF elem.info.type = FileSys.FileType.SLink THEN
              elem.referent := FileSys.ReadLink(spath);
            ELSE
              elem.referent := NIL;
            END;
          FINALLY
            e.release();
          END;
          CASE elem.info.type OF
          | FileSys.FileType.Other =>
          | FileSys.FileType.Dir =>
              elem.children := Build(cpath);
              res := RefList.Cons(elem, res);
          ELSE
              res := RefList.Cons(elem, res);
          END;
        END;
        eList := eList.tail;
      END;
      RETURN res;
    END Build;
  BEGIN
    RETURN Build(NIL);
  END Enumerate;

PROCEDURE Compare(src, dest: PackageObj.DirEnum; cl: DiffClosure)
    RAISES {Thread.Alerted} =
  VAR
     dirName: FileSys.FN := NIL;
  PROCEDURE CompareInner(
      arc: TEXT; src, dest: PackageObj.DirEnum; cl: DiffClosure)
      RAISES {Stop, Thread.Alerted} =
    VAR e1,e2: PackageObj.DirElem;
        comp: [-1..1];
        saveDir := dirName;
    BEGIN
      IF src = NIL AND dest = NIL THEN RETURN; END;
      IF arc # NIL THEN dirName := Pathname.Join(dirName, arc, NIL); END;
      WHILE src # NIL OR dest # NIL DO
        IF dest = NIL THEN
          e1 := src.head;
          comp := -1;
        ELSIF src = NIL THEN
          e2 := dest.head;
          comp := 1;
        ELSE
          e1 := src.head;
          e2 := dest.head;
          comp := TextCompare(e1.arc, e2.arc);
        END;
        CASE comp OF
        | 1 =>
            dest := dest.tail;
            cl.report(dirName, DiffType.NoSrc, e2);
            CompareInner(e2.arc, NIL, e2.children, cl);
        | 0 =>
            src := src.tail;
            dest := dest.tail;
            cl.report(dirName, CompareInfo(e1,e2), e1);
            CompareInner(e1.arc, e1.children, e2.children, cl);
        | -1 =>
            src := src.tail;
            cl.report(dirName, DiffType.NoDest, e1);
            CompareInner(e1.arc, e1.children, NIL, cl);
        END;
      END;
      dirName := saveDir;
    END CompareInner;
  BEGIN
    TRY
      CompareInner(NIL, src, dest, cl);
    EXCEPT Stop =>
    END;
  END Compare;

PROCEDURE TextCompare(t1, t2: TEXT) : [-1..1] =
  BEGIN
    IF t1 = NIL THEN
      IF t2 = NIL THEN RETURN 0; END;
      RETURN 1;
    ELSIF t2 = NIL THEN
      RETURN -1;
    ELSE
      RETURN Text.Compare(t1, t2);
    END;
  END TextCompare;

PROCEDURE CompareInfo(e1, e2: PackageObj.DirElem): DiffType =
  BEGIN
    IF e1.info.type # e2.info.type THEN RETURN DiffType.TypesDiffer; END;
    CASE e1.info.type OF
    | FileSys.FileType.Dir => RETURN DiffType.Same;
    | FileSys.FileType.Normal =>
       IF e1.info.date = e2.info.date THEN
         IF e1.info.length # e2.info.length THEN
           RETURN DiffType.LengthsDiffer;
         END;
         IF e1.info.perm # e2.info.perm THEN
           RETURN DiffType.ModesDiffer;
         END;
       ELSIF e1.info.date > e2.info.date THEN
         RETURN DiffType.SrcNewer;
       ELSE
         RETURN DiffType.SrcOlder;
       END;
    | FileSys.FileType.SLink =>
       IF NOT Text.Equal(e1.referent, e2.referent) THEN
         RETURN DiffType.LinksDiffer;
       END;
    | FileSys.FileType.Other => RAISE FatalError;
    END;
    RETURN DiffType.Same;
  END CompareInfo;

PROCEDURE SpecialFile (t: Text.T): BOOLEAN =
  BEGIN
    IF Text.GetChar (t, 0) # '.' THEN RETURN FALSE;  END;
    RETURN Text.Equal (t, VersionFile) OR Text.Equal (t, ExportLinkFile);
  END SpecialFile;

PROCEDURE SetDirDates(
    <*UNUSED*>path: FileSys.FN; <*UNUSED*>enum: PackageObj.DirEnum) =
  BEGIN
  END SetDirDates;

TYPE
  SpecialSource = PackageObj.Source OBJECT
  OVERRIDES
    enum := SpecialEnumerate;
    pullFile := NullPullFile;
    links := NullExportLinks;
  END;

PROCEDURE SpecialEnumerate(<*UNUSED*> s: SpecialSource) : PackageObj.Enum
    RAISES {} =
  BEGIN
    RETURN PackageObj.Enum{Time.Now(), NIL};
  END SpecialEnumerate;

PROCEDURE NullPullFile(<*UNUSED*>s: SpecialSource;
                       <*UNUSED*>path: NetPath.T) : Rd.T
    RAISES {PkgErr.E} =
  BEGIN
    PkgErr.Raise(PkgErr.NoSuchFile);
    RETURN NIL;
  END NullPullFile;

PROCEDURE NullExportLinks(<*UNUSED*> s: SpecialSource) : PackageObj.ExportLinks
    RAISES {} =
  BEGIN
    RETURN NIL;
  END NullExportLinks;

PROCEDURE EmptySource() : PackageObj.Source =
  BEGIN
    RETURN NEW(SpecialSource);
  END EmptySource;

BEGIN
END PackageLib.
