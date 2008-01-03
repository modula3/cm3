(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3DirFindFile;

IMPORT
  CITextRefTbl, TextRefTbl, Rd, RdExtras, FileRd, RefList, ASCII, 
  M3Directory, M3Extension, M3FindFile, M3PathElem, M3PathElemList,
  Pathname, OSError;

IMPORT Thread;
<*FATAL M3FindFile.Failed, Rd.Failure, Thread.Alerted*>

REVEAL Finder = FinderPublic BRANDED OBJECT 
    table: TextRefTbl.T;
    allDirs: M3PathElemList.T;
    extSet: M3Extension.TSet;
    extCount: CARDINAL;
    extToIndex: ARRAY M3Extension.T OF CARDINAL;
    indexToExt: IndexToExts;
  METHODS
    addDir(dir: M3PathElem.T) RAISES {OSError.E} := AddDir;
  OVERRIDES
    exts := Exts;
    init := Init;
    find := Find;
    dirOf := DirOf;
    dirs := Dirs;
    iterate := NewIter;
    setProperty := SetProperty;
    getProperty := GetProperty;
    merge := Merge;
    openRead := OpenRead;
  END;

REVEAL
  TFinder = TFinderPublic BRANDED OBJECT
    fileLocs: RefList.T; (* OF FileLoc *)
    rd: Rd.T; (* passed to "init" *)
  OVERRIDES
    init := TInit;
    addDir := TAddDir; 
  END;

TYPE
  TFileLoc = REF RECORD index: CARDINAL; elem: M3PathElem.T END;
 
TYPE
  IndexToExts = ARRAY [ORD(FIRST(M3Extension.T))..ORD(LAST(M3Extension.T))]
    OF M3Extension.T;

  (* data associated with each unit entered in the hash table *)
  Info = REF ARRAY OF RECORD
    dir: M3PathElem.T;  (* handle on associated directory, NIL => missing *)
    userData: REFANY;   (* place to hang other stuff, e.g. filestamp *)
  END;

PROCEDURE BuildHashTable(dirs: M3PathElemList.T;
    oldFinder: Finder;
    newFinder: Finder;
    errorHandler: ErrorHandler): Finder RAISES {OSError.E}=
  VAR
    d: M3PathElem.T;
  BEGIN
    InitHashTable(newFinder);
    newFinder.allDirs := dirs;
    (* Any directory which is read-only in 'dirs' AND appears
       in the old finder list, can have its info copied into
       the new hash table. 
    *)
    WHILE dirs # NIL DO
      d := dirs.head;
      IF oldFinder # NIL AND d.readOnly() AND
         AppearsIn(oldFinder.allDirs, d) THEN
        VAR
	  iter := oldFinder.table.iterate();
	  name: TEXT;
	  val: REFANY;
	  info: Info;
	BEGIN
	  WHILE iter.next(name, val) DO
	    info := NARROW(val, Info);
	    FOR i := 0 TO oldFinder.extCount-1 DO
	      IF info[i].dir # NIL AND
	          info[i].dir = d THEN
	        Add(newFinder, name, oldFinder.indexToExt[i], d);
		SetProperty(newFinder, name, oldFinder.indexToExt[i],
		    info[i].userData);
              END;
	    END; (* for *)
	  END; (* while *)
	END;
      ELSE
        (* needs scanning *)
        IF errorHandler = NIL THEN newFinder.addDir(d);
        ELSE
          TRY
            newFinder.addDir(d);
           EXCEPT OSError.E(ec) =>
             IF NOT errorHandler.callback(d, ec) THEN
               RETURN NIL
             END;
           END;
        END;
      END;
      dirs := dirs.tail;
    END; (* while *)
    RETURN newFinder;
  END BuildHashTable;

PROCEDURE AppearsIn(dirs: M3PathElemList.T; d: M3PathElem.T): BOOLEAN=
  BEGIN
    WHILE dirs # NIL DO
      IF dirs.head = d THEN RETURN TRUE END;
      dirs := dirs.tail;
    END; (* while *)
    RETURN FALSE;
  END AppearsIn;

PROCEDURE InitHashTable(f: Finder) RAISES {} =
  BEGIN
    IF (*Directory.CaseSensitive()*) TRUE THEN
      f.table := NEW(TextRefTbl.Default).init();
    ELSE f.table := NEW(CITextRefTbl.Default).init();
    END;
  END InitHashTable;

PROCEDURE AddDir(f: Finder; d: M3PathElem.T) RAISES {OSError.E} =
  VAR
    i := M3Directory.NewIter(d.text(), f.extSet);
    name: TEXT;
    ext: M3Extension.T;
  BEGIN
    WHILE i.next(name, ext) DO
      Add(f, name, ext, d);
    END; (* while *)
    i.close();
  END AddDir;

EXCEPTION TAddDirFailed;

PROCEDURE TAddDir(f: TFinder; d: M3PathElem.T) RAISES {} =
  VAR l := f.fileLocs;
      fileLoc: TFileLoc;
  BEGIN
    WHILE l # NIL DO
      fileLoc := NARROW(l.head, TFileLoc);
      IF fileLoc.elem = d THEN
        EXIT;
      ELSE
        l := l.tail;
      END
    END;
    IF l=NIL THEN <*FATAL TAddDirFailed*> BEGIN RAISE TAddDirFailed END;END;

    VAR name: TEXT; ext: M3Extension.T;
    BEGIN
      Rd.Seek(f.rd, fileLoc.index);
      LOOP
        TRY
          IF RdExtras.Skip(f.rd) = '@' THEN EXIT END;
          name := RdExtras.GetText(f.rd);
          IF M3Extension.Has(name, ext) AND ext IN f.extSet THEN
            Add(f, Pathname.Base(name), ext, d);
          END;
        EXCEPT
        | Rd.EndOfFile => EXIT
        END;
      END;
    END;
  END TAddDir;

PROCEDURE TInit(
    newFinder: TFinder;
    exts: M3Extension.TSet;
    rd: Rd.T;
    oldFinder: Finder := NIL)
    : Finder
    RAISES {OSError.E} =
  VAR
    refList: RefList.T := NIL;
    dirs: M3PathElemList.T := NIL;
  BEGIN
    LOOP
      TRY 
        IF RdExtras.Skip(rd, ASCII.Spaces, unget := FALSE) = '@' THEN
          VAR
            dirName := RdExtras.GetText(rd, unget := FALSE);
            index := Rd.Index(rd);
            elem := M3PathElem.FromText(dirName, dirName, TRUE);
          BEGIN
            refList := RefList.AppendD(refList, RefList.List1(
                           NEW(TFileLoc, index := index, elem := elem)));
            dirs := M3PathElemList.AppendD(dirs, M3PathElemList.List1(elem));
          END;
        END;
      EXCEPT
      | Rd.EndOfFile => EXIT
      END;
    END;
    newFinder.fileLocs := refList; newFinder.rd := rd;
    RETURN Init(newFinder, exts, dirs, oldFinder);
  END TInit;

PROCEDURE Init(
    newFinder: Finder;
    exts: M3Extension.TSet;
    dirs: M3PathElemList.T;
    oldFinder: Finder := NIL;
    errorHandler: ErrorHandler := NIL)
    : Finder
    RAISES {OSError.E} =
  BEGIN
    BasicInit(newFinder, exts);
    RETURN BuildHashTable(dirs, oldFinder, newFinder, errorHandler);
  END Init;

PROCEDURE BasicInit(
    newFinder: Finder;
    exts: M3Extension.TSet)=
  VAR
    extToIndex: ARRAY M3Extension.T OF CARDINAL;
    indexToExt: IndexToExts;
    count: CARDINAL := 0;
  BEGIN
    CountAndExtToIndex(exts, count, extToIndex, indexToExt);
    
    newFinder.extSet := exts; 
    newFinder.extCount := count; newFinder.extToIndex := extToIndex;
    newFinder.indexToExt := indexToExt;
  END BasicInit;

PROCEDURE CountAndExtToIndex(
    exts: M3Extension.TSet;
    VAR count: CARDINAL;
    VAR extToIndex: ARRAY M3Extension.T OF CARDINAL;
    VAR indexToExt: IndexToExts) RAISES {} =
  BEGIN
    count := 0;
    FOR i := FIRST(M3Extension.T) TO LAST(M3Extension.T) DO
      IF i IN exts THEN
        extToIndex[i] := count;
	indexToExt[count] := i;
        INC(count);
      END;
    END;
  END CountAndExtToIndex;

EXCEPTION Fatal;

PROCEDURE Exts(p: Finder): M3Extension.TSet RAISES {}=
  BEGIN
    RETURN p.extSet;
  END Exts;


PROCEDURE Find(
    m: Finder;
    name: TEXT;
    ext: M3Extension.T)
    : TEXT
    RAISES {M3FindFile.Failed}=
  BEGIN
    RETURN Pathname.Join(DirOf(m, name, ext).text(),
                            name, M3Extension.ToText(ext)) 
  END Find;

PROCEDURE DirOf(
    m: Finder;
    name: TEXT;
    ext: M3Extension.T): M3PathElem.T RAISES {M3FindFile.Failed}=
  VAR
    id: REFANY;
  BEGIN
    IF NOT ext IN m.extSet THEN <*FATAL Fatal*> BEGIN RAISE Fatal END; END;
    IF m.table.get(name, id) THEN
      VAR
        p := NARROW(id, Info);
	dir := p[m.extToIndex[ext]].dir;
      BEGIN
        IF dir # NIL THEN RETURN dir END;
      END;
    END; (* if *)
    RAISE M3FindFile.Failed;
  END DirOf;

PROCEDURE Add(
    m: Finder;
    name: TEXT;
    ext: M3Extension.T;
    dir: M3PathElem.T)
    RAISES {}=
  VAR
    id: REFANY;
    index := m.extToIndex[ext];
    info: Info;
  BEGIN
    IF m.table.get(name, id) THEN
      info := NARROW(id, Info);
      IF info[index].dir # NIL THEN
      	RETURN (* duplicate, later in path, ignored *)
      END; (* if *) 
    ELSE
      info := NEW(Info, m.extCount);
      FOR i := 0 TO m.extCount-1 DO
        WITH xinfo = info[i] DO
      	  xinfo.dir := NIL; xinfo.userData := NIL;
        END;
      END; (* for *)
      EVAL m.table.put(name, info);
    END; (* if *)
    info[index].dir := dir;
  END Add;


PROCEDURE NewIter(f: Finder): Iter RAISES {} =
  BEGIN
    RETURN NEW(Iter,
               hashIter := f.table.iterate(),
               f := f);
  END NewIter;

PROCEDURE Next(
    i: Iter;
    VAR (*out*) unitName: TEXT;
    VAR (*out*) ext: M3Extension.T;
    VAR (*out*) dir: M3PathElem.T)
    : BOOLEAN
    RAISES {} =
  VAR
    si: CARDINAL;
  BEGIN
    LOOP
      IF i.info = NIL THEN
        VAR
          val: REFANY;
        BEGIN
          IF NOT i.hashIter.next(unitName, val) THEN
	    RETURN FALSE;
	  END;
          i.info := NARROW(val, Info);
        END;
      END;
      (* got a unit, iterate the extensions *)
      dir := i.info[i.i].dir; si := i.i;
      INC(i.i);
      IF i.i >= i.f.extCount THEN i.i := 0; i.info := NIL; END;
      IF dir # NIL THEN
	ext := i.f.indexToExt[si];
	RETURN TRUE; 
      END;
    END; (* loop *)
  END Next;

PROCEDURE Dirs(f: Finder): M3PathElemList.T RAISES {}=
  BEGIN
    RETURN f.allDirs;
  END Dirs;

REVEAL Iter = IterPublic BRANDED OBJECT
    hashIter: CITextRefTbl.Iterator;
    f: Finder;
    i: CARDINAL := 0;
    info: Info := NIL;
  OVERRIDES
    next := Next;
    close := Close;
  END;

PROCEDURE Close(<*UNUSED*> iter: Iter)=
  BEGIN
  END Close;
    
PROCEDURE SetProperty(
    f: Finder;  
    unitName: TEXT;
    ext: M3Extension.T;
    value: REFANY)
    RAISES {M3FindFile.Failed}=
  VAR
    id: REFANY;
  BEGIN
    IF NOT ext IN f.extSet THEN <*FATAL Fatal*> BEGIN RAISE Fatal END; END;
    IF f.table.get(unitName, id) THEN
      VAR
        p := NARROW(id, Info);
	dir := p[f.extToIndex[ext]].dir;
      BEGIN
        IF dir # NIL THEN
	  p[f.extToIndex[ext]].userData := value;
        END;
      END;
    ELSE RAISE M3FindFile.Failed
    END;    
  END SetProperty;

PROCEDURE GetProperty(    
    f: Finder;  
    unitName: TEXT;
    ext: M3Extension.T)
    : REFANY RAISES {M3FindFile.Failed}=
  VAR
    id: REFANY;
  BEGIN
    IF NOT ext IN f.extSet THEN <*FATAL Fatal*> BEGIN RAISE Fatal END; END;
    IF f.table.get(unitName, id) THEN
      VAR
        p := NARROW(id, Info);
	dir := p[f.extToIndex[ext]].dir;
      BEGIN
        IF dir # NIL THEN
	  RETURN p[f.extToIndex[ext]].userData;
        END;
      END;
    END (* IF *);
    RAISE M3FindFile.Failed
  END GetProperty;

PROCEDURE OpenRead(f: Finder; name: TEXT; ext: M3Extension.T
      ): Rd.T RAISES {OSError.E, M3FindFile.Failed}=
  BEGIN
    RETURN FileRd.Open(f.find(name, ext));
  END OpenRead;

PROCEDURE Merge(self, f1, f2: Finder): Finder=
  VAR
    exts := f1.extSet;
  BEGIN
    IF f2 # NIL THEN exts := exts + f2.extSet END;
    BasicInit(self, exts);
    InitHashTable(self);
    MergeOne(f1, self);
    IF f2 # NIL THEN MergeOne(f2, self); END;
    RETURN self;
  END Merge;

PROCEDURE MergeOne(from, to: Finder)=
  VAR
    iter := from.table.iterate();
    name: TEXT;
    val: REFANY;
    info: Info;
    from_dirs := from.allDirs;
  BEGIN
    WHILE from_dirs # NIL DO
      IF NOT M3PathElemList.Member(to.allDirs, from_dirs.head) THEN
        to.allDirs := M3PathElemList.AppendD(to.allDirs,
            M3PathElemList.List1(from_dirs.head));
      END;
      from_dirs := from_dirs.tail;
    END;
    WHILE iter.next(name, val) DO
      info := NARROW(val, Info);
      FOR i := 0 TO from.extCount-1 DO
        IF info[i].dir # NIL THEN
          Add(to, name, from.indexToExt[i], info[i].dir);
          SetProperty(to, name, from.indexToExt[i],
		      info[i].userData);
        END;
      END;
    END;
  END MergeOne;

BEGIN
END M3DirFindFile.
