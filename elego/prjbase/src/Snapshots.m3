(*---------------------------------------------------------------------------*)
MODULE Snapshots;

IMPORT Text, TextSeq, TextTextTbl, Rd, Wr, OSError, Fmt, Thread, Pathname,
       Time, MxConfig;
IMPORT PkgVC, PkgBase, PrjDesc, FilePool, FSUtils, TextReadingUtils, 
       ChangeSet, SortedTextPrjDescTbl, SortedTextChangeSetTbl,
       SortedTimePrjDescTbl, SortedTimeChangeSetTbl;
IMPORT APN AS APN;
IMPORT (* FSFixed AS *) FS;

(*---------------------------------------------------------------------------*)
CONST
  SnapshotIndexFileName = "snaps.idx";

(*---------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "Snapshots Type 0.0"  OBJECT
    path          : Pathname.T;
    fp            : FilePool.T;
    snapshotIndex : TextTextTbl.T;
    releaseIndex  : TextTextTbl.T;
    changeSetIndex: TextTextTbl.T;
    cfg           : PkgBase.T;
    pkgvc         : PkgVC.T;
  METHODS
    addRoot(fn : TEXT) : TEXT := AddRoot;
    apn(fn : TEXT) : APN.T := Apn;
  OVERRIDES
    init := Init;
    buildIndex := BuildIndex;
    readIndex := ReadIndex;
    writeIndex := WriteIndex;
    snapshotDefined := SnapshotDefined;
    releaseDefined := ReleaseDefined;
    changeSetDefined := ChangeSetDefined;
    getSnapshot := GetSnapshot;
    getRelease := GetRelease;
    getChangeSet := GetChangeSet;
    putSnapshot := PutSnapshot;
    putRelease := PutRelease;
    putChangeSet := PutChangeSet;
    listSnapshots := ListSnapshots;
    listReleases := ListReleases;
    listChangeSets := ListChangeSets;
    snapshotsByName := SnapshotsByName;
    snapshotsByDate := SnapshotsByDate;
    releasesByName := ReleasesByName;
    releasesByDate := ReleasesByDate;
    changeSetsByName := ChangeSetsByName;
    changeSetsByDate := ChangeSetsByDate;
    everythingUnderVersionControl := EverythingUnderVersionControl;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE AddRoot(self : T; fn : TEXT) : TEXT =
  BEGIN
    RETURN Pathname.Join(self.path, fn, NIL);
  END AddRoot;

(*---------------------------------------------------------------------------*)
PROCEDURE Apn(self : T; fn : TEXT) : APN.T =
  BEGIN
    RETURN APN.New(Pathname.Join(self.path, fn, NIL));
  END Apn;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; dir : TEXT; cfg : PkgBase.T;
               pkgvc : PkgVC.T := NIL) : T RAISES {Error} =
  BEGIN
    self.cfg := cfg;
    self.path := dir;
    self.pkgvc := pkgvc;
    IF NOT FSUtils.IsDir(dir) THEN
      TRY
        FS.CreateDirectory(dir);
      EXCEPT
        OSError.E => RAISE Error("cannot create directory " & dir);
      END;
    END;
    TRY
      self.fp := NEW(FilePool.T).init(dir)
    EXCEPT
      FilePool.Error(e) => 
      RAISE Error("cannot initialize snapshot cache: " & e);
    END;
    IF FSUtils.IsFile(self.addRoot(SnapshotIndexFileName)) THEN
      ReadIndex(self);
    ELSE
      BuildIndex(self);
      WriteIndex(self);
    END;
    IF self.pkgvc # NIL THEN
      WITH f = APN.New(dir) DO
        TRY
          IF NOT self.pkgvc.known(f) THEN
            EVAL pkgvc.add(f);
          END;
        EXCEPT
          PkgVC.E(m) => RAISE Error("version control backend failed: " & m);
        END;
      END;
    END;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
TYPE
  GrepClosure = FilePool.ProcClosure OBJECT
    snapshots : T;
  METHODS
  OVERRIDES
    proc := ExtractSnapshotNames;
  END;
      
(*---------------------------------------------------------------------------*)
PROCEDURE ExtractSnapshotNames(self : GrepClosure; fn : TEXT) RAISES {Error}=
  VAR
    rd   : Rd.T;
    key  : TEXT;
    name : TEXT;
  BEGIN
    IF Text.Equal(fn, SnapshotIndexFileName) THEN RETURN END;
    TRY
      rd := self.snapshots.fp.getReader(fn);
    EXCEPT
      FilePool.Error(e) => RAISE Error("cannot open snapshot index: " & e);
    END;
    TRY
      TRY
        WHILE NOT Rd.EOF(rd) DO
          key  := TextReadingUtils.GetTokenOrString(rd);
          IF Text.Equal(key, "snapshot") THEN
            name := TextReadingUtils.GetTokenOrString(rd);
            EVAL self.snapshots.snapshotIndex.put(name, fn);
          ELSIF Text.Equal(key, "release") THEN
            name := TextReadingUtils.GetTokenOrString(rd);
            EVAL self.snapshots.releaseIndex.put(name, fn);
          ELSIF Text.Equal(key, "changeset") THEN
            name := TextReadingUtils.GetTokenOrString(rd);
            EVAL self.snapshots.changeSetIndex.put(name, fn);
            RAISE Rd.EndOfFile; (* stop reading *)
          END;
        END;
      EXCEPT
        Rd.Failure => RAISE Error("cannot read snapshot " & fn);
      | Thread.Alerted => RAISE Error("interrupted reading snapshot " & fn);
      | Rd.EndOfFile => (* skip *)
      END;
    FINALLY
      TRY Rd.Close(rd) EXCEPT ELSE END;
    END;
  END ExtractSnapshotNames;

(*---------------------------------------------------------------------------*)
PROCEDURE BuildIndex(self : T) RAISES {Error} =
  VAR
    cl := NEW(GrepClosure, snapshots := self);
  BEGIN
    self.snapshotIndex := NEW(TextTextTbl.Default).init();
    self.releaseIndex := NEW(TextTextTbl.Default).init();
    self.changeSetIndex := NEW(TextTextTbl.Default).init();
    self.fp.apply(cl, ordinaryOnly := TRUE); <* NOWARN *>
  END BuildIndex;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadIndex(self : T) RAISES {Error} =
  VAR
    rd   : Rd.T;
    key  : TEXT;
    name : TEXT;
    fn   : TEXT;
  BEGIN
    self.snapshotIndex := NEW(TextTextTbl.Default).init();
    self.releaseIndex := NEW(TextTextTbl.Default).init();
    self.changeSetIndex := NEW(TextTextTbl.Default).init();
    TRY
      rd := self.fp.getReader(SnapshotIndexFileName);
    EXCEPT
      FilePool.Error(e) => RAISE Error("cannot open snapshot index: " & e);
    END;
    TRY
      TRY
        WHILE NOT Rd.EOF(rd) DO
          key  := TextReadingUtils.GetTokenOrString(rd);
          name := TextReadingUtils.GetTokenOrString(rd);
          fn   := TextReadingUtils.GetTokenOrString(rd);
          IF Text.Equal(key, "snapshot") THEN
            EVAL self.snapshotIndex.put(name, fn);
          ELSIF Text.Equal(key, "release") THEN
            EVAL self.releaseIndex.put(name, fn);
          ELSIF Text.Equal(key, "changeset") THEN
            EVAL self.changeSetIndex.put(name, fn);
          ELSE
            RAISE Error("corrupted snapshot index file");
          END;
        END;
      EXCEPT
        Rd.Failure => RAISE Error("cannot read snapshot index");
      | Thread.Alerted => RAISE Error("interrupted reading snapshot index");
      | Rd.EndOfFile => (* skip *)
      END;
    FINALLY
      TRY Rd.Close(rd) EXCEPT ELSE END;
    END;
  END ReadIndex;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteIndex(self : T) RAISES {Error} =
  VAR
    wr   : Wr.T;
    iter : TextTextTbl.Iterator;
    name : TEXT;
    fn   : TEXT;
    line : TEXT;
  BEGIN
    TRY
      wr := self.fp.getWriter(SnapshotIndexFileName);
    EXCEPT
      FilePool.Error(e) => RAISE Error("cannot open snapshot index: " & e);
    END;
    TRY
      iter := self.snapshotIndex.iterate();
      WHILE iter.next(name, fn) DO
        line := Fmt.F("snapshot \"%s\" %s" & NL, name, fn);
        Wr.PutText(wr, line);
      END;
      iter := self.releaseIndex.iterate();
      WHILE iter.next(name, fn) DO
        line := Fmt.F("release \"%s\" %s" & NL, name, fn);
        Wr.PutText(wr, line);
      END;
      iter := self.changeSetIndex.iterate();
      WHILE iter.next(name, fn) DO
        line := Fmt.F("changeset \"%s\" %s" & NL, name, fn);
        Wr.PutText(wr, line);
      END;
      Wr.Close(wr);
      IF self.pkgvc # NIL THEN
        TRY
          WITH f = APN.New(self.path) DO
            IF NOT self.pkgvc.known(f) THEN
              EVAL self.pkgvc.add(f);
            END;
          END;
          WITH f = self.apn(SnapshotIndexFileName) DO
            IF NOT self.pkgvc.known(f) THEN
              EVAL self.pkgvc.add(f);
            END;
          END;
        EXCEPT
          PkgVC.E(m) => RAISE Error("version control backend failed: " & m);
        END;
      END;
    EXCEPT
      Wr.Failure => RAISE Error("cannot write snapshot index");
    | Thread.Alerted => RAISE Error("interrupted writing snapshot index");
    END;
  END WriteIndex;

(*---------------------------------------------------------------------------*)
PROCEDURE SnapshotDefined(self : T; name : TEXT) : BOOLEAN =
  VAR fn : TEXT;
  BEGIN
    RETURN self.snapshotIndex.get(name, fn);
  END SnapshotDefined;

(*---------------------------------------------------------------------------*)
PROCEDURE ReleaseDefined(self : T; name : TEXT) : BOOLEAN =
  VAR fn : TEXT;
  BEGIN
    RETURN self.releaseIndex.get(name, fn);
  END ReleaseDefined;

(*---------------------------------------------------------------------------*)
PROCEDURE ChangeSetDefined(self : T; name : TEXT) : BOOLEAN =
  VAR fn : TEXT;
  BEGIN
    RETURN self.changeSetIndex.get(name, fn);
  END ChangeSetDefined; 

(*---------------------------------------------------------------------------*)
PROCEDURE GetSnapshot(self : T; name : TEXT) : PrjDesc.T RAISES {Error} =
  VAR
    prj : PrjDesc.T;
    fn  : TEXT;
  BEGIN
    IF NOT self.snapshotIndex.get(name, fn) THEN
      RAISE Error("no snapshot " & name);
    END;
    TRY
      prj := NEW(PrjDesc.T).init(self.addRoot(fn), self.cfg,
                                 check := FALSE, useCache := FALSE);
    EXCEPT
      PrjDesc.Error(e) => 
      RAISE Error("cannot create project description for snapshot: " & e);
    END;
    RETURN prj;
  END GetSnapshot;

(*---------------------------------------------------------------------------*)
PROCEDURE GetRelease(self : T; name : TEXT) : PrjDesc.T RAISES {Error} =
  VAR
    prj : PrjDesc.T;
    fn  : TEXT;
  BEGIN
    IF NOT self.releaseIndex.get(name, fn) THEN
      RAISE Error("no release " & name);
    END;
    TRY
      prj := NEW(PrjDesc.T).init(self.addRoot(fn), self.cfg,
                                 check := FALSE, useCache := FALSE);
    EXCEPT
      PrjDesc.Error(e) => 
      RAISE Error("cannot create project description for release: " & e);
    END;
    RETURN prj;
  END GetRelease;

(*---------------------------------------------------------------------------*)
PROCEDURE GetChangeSet(self : T; name : TEXT) : ChangeSet.T RAISES {Error} =
  VAR
    cs : ChangeSet.T;
    fn : TEXT;
  BEGIN
    IF NOT self.changeSetIndex.get(name, fn) THEN
      RAISE Error("no changeset " & name);
    END;
    cs := NEW(ChangeSet.T).init(name);
    TRY
      cs.read(self.addRoot(fn));
    EXCEPT
      ChangeSet.Error(e) => 
      RAISE Error("cannot read changeset: " & e);
    END;
    RETURN cs;
  END GetChangeSet; 

(*---------------------------------------------------------------------------*)
PROCEDURE PutSnapshot(self : T; name : TEXT; snap : PrjDesc.T; ovwr := FALSE)
  RAISES {Error} =
  VAR
    fn  : TEXT;
  BEGIN
    IF self.snapshotIndex.get(name, fn) THEN
      IF NOT ovwr THEN
        RAISE Error("snapshot " & name & " already exists");
      END;
    ELSE
      TRY
        fn := self.fp.createNewFile();
      EXCEPT
        FilePool.Error(e) => RAISE Error("cannot create snapshot file: " & e);
      END;
    END;
    TRY
      snap.writeSnapshot(self.addRoot(fn), name);
    EXCEPT
      PrjDesc.Error(e) =>
      RAISE Error("cannot write snapshot from project description: " & e);
    END;
    EVAL self.snapshotIndex.put(name, fn);
    IF self.pkgvc # NIL THEN
      WITH f = self.apn(fn) DO
        TRY
          IF NOT self.pkgvc.known(f) THEN
            EVAL self.pkgvc.add(f);
          END;
        EXCEPT
          PkgVC.E(m) => RAISE Error("version control backend failed: " & m);
        END;
      END;
    END;
    WriteIndex(self);
  END PutSnapshot;

(*---------------------------------------------------------------------------*)
PROCEDURE PutRelease(self : T; name : TEXT; snap : PrjDesc.T; ovwr := FALSE)
  RAISES {Error} =
  VAR
    fn  : TEXT;
  BEGIN
    IF self.releaseIndex.get(name, fn) THEN
      IF NOT ovwr THEN
        RAISE Error("snapshot " & name & " already exists");
      END;
    ELSE
      TRY
        fn := self.fp.createNewFile();
      EXCEPT
        FilePool.Error(e) => RAISE Error("cannot create snapshot file: " & e);
      END;
    END;
    TRY
      snap.writeRelease(self.addRoot(fn), name);
    EXCEPT
      PrjDesc.Error(e) =>
      RAISE Error("cannot write release from project description: " & e);
    END;
    EVAL self.releaseIndex.put(name, fn);
    IF self.pkgvc # NIL THEN
      WITH f = self.apn(fn) DO
        TRY
          IF NOT self.pkgvc.known(f) THEN
            EVAL self.pkgvc.add(f);
          END;
        EXCEPT
          PkgVC.E(m) => RAISE Error("version control backend failed: " & m);
        END;
      END;
    END;
    WriteIndex(self);
  END PutRelease;

(*---------------------------------------------------------------------------*)
PROCEDURE PutChangeSet(self : T; name : TEXT; cs : ChangeSet.T; ovwr := FALSE) 
  RAISES {Error} =
  VAR
    fn  : TEXT;
  BEGIN
    IF self.changeSetIndex.get(name, fn) THEN
      IF NOT ovwr THEN
        RAISE Error("changeset " & name & " already exists");
      END;
    ELSE
      TRY
        fn := self.fp.createNewFile();
      EXCEPT
        FilePool.Error(e) => RAISE Error("cannot create changeset file: " & e);
      END;
    END;
    cs.setName(name);
    TRY
      cs.save(self.addRoot(fn));
    EXCEPT
      ChangeSet.Error(e) =>
      RAISE Error("cannot write changeset: " & e);
    END;
    EVAL self.changeSetIndex.put(name, fn);
    IF self.pkgvc # NIL THEN
      WITH f = self.apn(fn) DO
        TRY
          IF NOT self.pkgvc.known(f) THEN
            EVAL self.pkgvc.add(f);
          END;
        EXCEPT
          PkgVC.E(m) => RAISE Error("version control backend failed: " & m);
        END;
      END;
    END;
    WriteIndex(self);
  END PutChangeSet; 

(*---------------------------------------------------------------------------*)
PROCEDURE ListSnapshots(self : T; so := Sort.None; up := TRUE) : TextSeq.T
  RAISES {Error} =
  VAR
    res  := NEW(TextSeq.T).init();
    iter :  TextTextTbl.Iterator;
    name , 
    fn   : TEXT;
    prjd : PrjDesc.T;
  BEGIN
    IF so = Sort.None THEN
      iter := self.snapshotIndex.iterate();
      WHILE iter.next(name, fn) DO
        res.addhi(name);
      END;
    ELSIF so = Sort.ByName THEN
      WITH snapsByName = self.snapshotsByName(),
           nameIter = snapsByName.iterateOrdered() DO
        WHILE nameIter.next(name, prjd) DO
          IF up THEN
            res.addhi(name);
          ELSE
            res.addlo(name);
          END;
        END;
      END;
    ELSE
      RAISE Error("impossible sort order")
    END;
    RETURN res;
  END ListSnapshots;

(*---------------------------------------------------------------------------*)
PROCEDURE ListReleases(self : T; so := Sort.None; up := TRUE) : TextSeq.T
  RAISES {Error} =
  VAR
    res  := NEW(TextSeq.T).init();
    iter :  TextTextTbl.Iterator;
    name , 
    fn   :  TEXT;
    prjd : PrjDesc.T;
  BEGIN
    IF so = Sort.None THEN
      iter := self.releaseIndex.iterate();
      WHILE iter.next(name, fn) DO
        res.addhi(name);
      END;
    ELSIF so = Sort.ByName THEN
      WITH relsByName = self.releasesByName(),
           nameIter = relsByName.iterateOrdered() DO
        WHILE nameIter.next(name, prjd) DO
          IF up THEN
            res.addhi(name);
          ELSE
            res.addlo(name);
          END;
        END;
      END;
    ELSE
      RAISE Error("impossible sort order")
    END;
    RETURN res;
  END ListReleases;

(*---------------------------------------------------------------------------*)
PROCEDURE ListChangeSets(self : T; so := Sort.None; up := TRUE) : TextSeq.T
  RAISES {Error} =
  VAR
    res  := NEW(TextSeq.T).init();
    iter :  TextTextTbl.Iterator;
    name ,
    fn   :  TEXT;
    date :  Time.T;
    cs   :  ChangeSet.T;
  BEGIN
    IF so = Sort.None THEN
      iter := self.changeSetIndex.iterate();
      WHILE iter.next(name, fn) DO
        res.addhi(name);
      END;
    ELSIF so = Sort.ByName THEN
      WITH csByName = self.changeSetsByName(),
           nameIter = csByName.iterateOrdered() DO
        WHILE nameIter.next(name, cs) DO
          IF up THEN
            res.addhi(name);
          ELSE
            res.addlo(name);
          END;
        END;
      END;
    ELSIF so = Sort.ByDate THEN
      WITH csByDate = self.changeSetsByDate(),
           dateIter = csByDate.iterateOrdered() DO
        WHILE dateIter.next(date, cs) DO
          IF up THEN
            res.addhi(cs.getName());
          ELSE
            res.addlo(cs.getName());
          END;
        END;
      END;
    ELSE
      RAISE Error("impossible sort order")
    END;
    RETURN res;
  END ListChangeSets; 

(*---------------------------------------------------------------------------*)
PROCEDURE SnapshotsByName(self : T) : SortedTextPrjDescTbl.T
  RAISES {Error} =
  VAR
    res  := NEW(SortedTextPrjDescTbl.Default).init();
    name :  TEXT;
    fn   :  TEXT;
    prjd :  PrjDesc.T;
    iter := self.snapshotIndex.iterate();
  BEGIN
    WHILE iter.next(name, fn) DO
      TRY
        prjd := NEW(PrjDesc.T).init(self.addRoot(fn), self.cfg,
                                    check := FALSE, useCache := FALSE);
      EXCEPT
        PrjDesc.Error(e) => 
        RAISE Error("cannot create project description for snapshot: " & e);
      END;
      EVAL res.put(name, prjd);
    END;
    RETURN res;
  END SnapshotsByName;

(*---------------------------------------------------------------------------*)
PROCEDURE SnapshotsByDate(self : T; sortByModificationDate := FALSE)
  : SortedTimePrjDescTbl.T RAISES {Error} =
  VAR
    res  := NEW(SortedTimePrjDescTbl.Default).init();
    name :  TEXT;
    fn   :  TEXT;
    prj  :  PrjDesc.T;
    iter := self.snapshotIndex.iterate();
    date :  Time.T;
    dd   :  Time.T := 0.1d0;
  BEGIN
    WHILE iter.next(name, fn) DO
      TRY
        prj := NEW(PrjDesc.T).init(self.addRoot(fn),
                                   self.cfg, useCache := FALSE);
      EXCEPT
        PrjDesc.Error(e) => 
        RAISE Error("cannot read project description: " & e);
      END;
      IF sortByModificationDate THEN
        date := prj.getModificationDate();
      ELSE
        date := prj.getCreationDate();
      END;
      IF date = 0.0d0 THEN
        date := dd;
        dd := dd + 0.1d0;
      END;
      EVAL res.put(date, prj);
    END;
    RETURN res;
  END SnapshotsByDate;

(*---------------------------------------------------------------------------*)
PROCEDURE ReleasesByName(self : T) : SortedTextPrjDescTbl.T
  RAISES {Error} =
  VAR
    res  := NEW(SortedTextPrjDescTbl.Default).init();
    name :  TEXT;
    fn   :  TEXT;
    prjd :  PrjDesc.T;
    iter := self.releaseIndex.iterate();
  BEGIN
    WHILE iter.next(name, fn) DO
      TRY
        prjd := NEW(PrjDesc.T).init(self.addRoot(fn), self.cfg,
                                    check := FALSE, useCache := FALSE);
      EXCEPT
        PrjDesc.Error(e) => 
        RAISE Error("cannot create project description for release: " & e);
      END;
      EVAL res.put(name, prjd);
    END;
    RETURN res;
  END ReleasesByName; 

(*---------------------------------------------------------------------------*)
PROCEDURE ReleasesByDate(self : T; sortByModificationDate := FALSE)
  : SortedTimePrjDescTbl.T RAISES {Error} =
  VAR
    res  := NEW(SortedTimePrjDescTbl.Default).init();
    name :  TEXT;
    fn   :  TEXT;
    prj  :  PrjDesc.T;
    iter := self.releaseIndex.iterate();
    date :  Time.T;
    dd   :  Time.T := 0.1d0;
  BEGIN
    WHILE iter.next(name, fn) DO
      TRY
        prj := NEW(PrjDesc.T).init(self.addRoot(fn),
                                   self.cfg, useCache := FALSE);
      EXCEPT
        PrjDesc.Error(e) => 
        RAISE Error("cannot read project description: " & e);
      END;
      IF sortByModificationDate THEN
        date := prj.getModificationDate();
      ELSE
        date := prj.getCreationDate();
      END;
      IF date = 0.0d0 THEN
        date := dd;
        dd := dd + 0.1d0;
      END;
      EVAL res.put(date, prj);
    END;
    RETURN res;
  END ReleasesByDate;

(*---------------------------------------------------------------------------*)
PROCEDURE ChangeSetsByName(self : T) : SortedTextChangeSetTbl.T
  RAISES {Error} =
  VAR
    res  := NEW(SortedTextChangeSetTbl.Default).init();
    name :  TEXT;
    fn   :  TEXT;
    cs   :  ChangeSet.T;
    iter := self.changeSetIndex.iterate();
  BEGIN
    WHILE iter.next(name, fn) DO
      cs := NEW(ChangeSet.T).init(name);
      TRY
        cs.read(self.addRoot(fn));
      EXCEPT
        ChangeSet.Error(e) => 
        RAISE Error("cannot read changeset: " & e);
      END;
      EVAL res.put(name, cs);
    END;
    RETURN res;
  END ChangeSetsByName; 

(*---------------------------------------------------------------------------*)
PROCEDURE ChangeSetsByDate(self : T) : SortedTimeChangeSetTbl.T
  RAISES {Error} =
  VAR
    res  := NEW(SortedTimeChangeSetTbl.Default).init();
    name :  TEXT;
    fn   :  TEXT;
    cs   :  ChangeSet.T;
    iter := self.changeSetIndex.iterate();
  BEGIN
    WHILE iter.next(name, fn) DO
      cs := NEW(ChangeSet.T).init(name);
      TRY
        cs.read(self.addRoot(fn));
      EXCEPT
        ChangeSet.Error(e) => 
        RAISE Error("cannot read changeset: " & e);
      END;
      EVAL res.put(cs.getDate(), cs);
    END;
    RETURN res;
  END ChangeSetsByDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE EverythingUnderVersionControl(self : T)  RAISES {Error} =
  VAR
    iter := self.snapshotIndex.iterate();
    name, fn : TEXT;
    errors := "";
  BEGIN
    IF self.pkgvc = NIL THEN
      RAISE Error("no version control backend present");
    END;
    self.pkgvc.flushCache();
    TRY
      WITH f = self.apn(SnapshotIndexFileName) DO
        IF NOT self.pkgvc.known(f) THEN
          IF NOT self.pkgvc.add(f) THEN
            errors := errors & NL & "  " & f.denotation();
          END;
        END;
      END;
      WHILE iter.next(name, fn) DO
        WITH f = self.apn(fn) DO
          IF NOT self.pkgvc.known(f) THEN
            IF NOT self.pkgvc.add(f) THEN
              errors := errors & NL & "  " & f.denotation();
            END;
          END;
        END;
      END;
      iter := self.releaseIndex.iterate();
      WHILE iter.next(name, fn) DO
        WITH f = self.apn(fn) DO
          IF NOT self.pkgvc.known(f) THEN
            IF NOT self.pkgvc.add(f) THEN
              errors := errors & NL & "  " & f.denotation();
            END;
          END;
        END;
      END;
      iter := self.changeSetIndex.iterate();
      WHILE iter.next(name, fn) DO
        WITH f = self.apn(fn) DO
          IF NOT self.pkgvc.known(f) THEN
            IF NOT self.pkgvc.add(f) THEN
              errors := errors & NL &"  " & f.denotation();
            END;
          END;
        END;
      END;
      IF NOT Text.Empty(errors) THEN
        RAISE Error("cannot put the following files under version control:" &
              errors);
      END;
    EXCEPT
      PkgVC.E(m) => RAISE Error("version control backend failed: " & m);
    END;
  END EverythingUnderVersionControl;

(*---------------------------------------------------------------------------*)
VAR NL := "\n";
BEGIN
  IF MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.WIN32 THEN
    NL := "\r\n";
  END;
END Snapshots.
