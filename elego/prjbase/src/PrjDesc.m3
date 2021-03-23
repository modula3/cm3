(*---------------------------------------------------------------------------*)
MODULE PrjDesc;

IMPORT Text, TextSeq, TextTextTbl, FileRd, Rd, FileWr, Wr, Pathname, 
       OSError, Thread, Fmt, Time, RCS_Date, TextWr, MxConfig;
IMPORT TextTextTextTbl, PkgBase, PoolSet, DependencyGraph, FSUtils,
       StdDepGraphNodeSeq, Tag, TextUtils, PathRepr, RegEx, Checkpoint,
       ProcessEnv, MsgX, MsgIF, Confirmation, PkgVC;
IMPORT SMsg AS Msg;
FROM TextReadingUtils IMPORT GetTokenOrString;
FROM TextUtils IMPORT MemberOfTextSeq, SubstEnvVars, SubstituteVariables;

(*---------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "PrjDesc Type 0.0" OBJECT
    collectionRootOrig : TEXT;
    collectionRoot     : TEXT;
    collectionList : TextSeq.T;
    collectionTbl  : TextTextTbl.T; (* collection names -> path *)
    packageList    : TextSeq.T;
    ignoredList    : TextSeq.T;
    missingList    : TextSeq.T;
    locationTbl    : TextTextTbl.T; (* package names -> path *)
    packageTbl     : TextTextTbl.T; (* package names -> collection names *)
    snapshotTbl    : TextTextTextTbl.T;
    releaseTbl     : TextTextTextTbl.T;
    poolset        : PoolSet.T;
    pkgKind        : PkgBase.Kind;
    depGraph       : DependencyGraph.T;
    depsMandatory  : BOOLEAN;
    externalShell  : TEXT;
    varTbl         : TextTextTbl.T;
    stateLabelTbl  : TextTextTextTbl.T;
    oldState       : Checkpoint.T;
    changedPkgDirs : TextSeq.T;
    useCache       : BOOLEAN;
    env            : ProcessEnv.T;
    msgif          : MsgIF.T;
    pkgvcAcc       : PoolSet.PkgVCAccessor;
    locs           : TextSeq.T := NIL;
    locsText       : TEXT := NIL;
    (* attributes *)
    name  : TEXT;
    desc  : TEXT;
    user  : TEXT;
    cdate : Time.T;
    mdate : Time.T;
  METHODS
    addRoot(p : Pathname.T) : Pathname.T := AddRoot;
    varTableCopy() : TextTextTbl.T := VarTableCopy;
  OVERRIDES
    init := Init;
    reinit := ReInit;
    parse := Parse;
    loadFile := LoadFile;
    pkgsOkay := PkgsOkay;
    getPoolSet := GetPoolSet;
    setExternalShell := SetExternalShell;
    setPreferredPkgKind := SetPreferredPkgKind;
    defineGlobalVar := DefineGlobalVar;
    deleteGlobalVar := DeleteGlobalVar;
    defineGlobalVars := DefineGlobalVars;
    collectionPath := CollectionPath;
    packages := Packages;
    locations := Locations;
    packagesCollection := PackagesCollection;
    collections := Collections;
    collectionsLocation := CollectionsLocation;
    snapshots := Snapshots;
    releases := Releases;
    snapshot := Snapshot;
    release := Release;
    defineSnapshot := DefineSnapshot;
    defineRelease := DefineRelease;
    getTags := GetTags;
    newSnapshot := NewSnapshot;
    newRelease := NewRelease;
    checkoutSnapshot := CheckoutSnapshot;
    checkoutRelease := CheckoutRelease;
    checkoutHead := CheckoutHead;
    checkoutTrunkOrBranchHead := CheckoutTrunkOrBranchHead;
    checkoutPackages := CheckoutPackages;
    readDepGraph  := ReadDepGraph;
    buildDepGraph := BuildDepGraph;
    writeDepGraph := WriteDepGraph;
    readDepGraphAsText  := ReadDepGraphAsText;
    writeDepGraphAsText := WriteDepGraphAsText;
    write := Write;
    writeSnapshot := WriteSnapshot;
    writeRelease := WriteRelease;
    packageUpdateSequence := PackageUpdateSequence;
    ignoredPackages := IgnoredPackages;
    missingPackages := MissingPackages;
    applyToPackages := ApplyToPackages;
    selectPackages  := SelectPackages;
    applyCmdListDirectly := ApplyCmdListDirectly;
    selectByCmdList := SelectByCmdList;
    dependendPackages := DependendPackages;
    packageDependencies := PackageDependencies;
    addDependingPackages := AddDependingPackages;
    modifiedPackages := ModifiedPackages;
    modifiedAndDependingPackages := ModifiedAndDependingPackages;
    upToDatePackages := UpToDatePackages;
    outOfDatePackages := OutOfDatePackages;
    outOfDateAndDependingPackages := OutOfDateAndDependingPackages;
    packagesWithConflicts := PackagesWithConflicts;
    testAllPackagesReleased := TestAllPackagesReleased;
    testNoPackageModified := TestNoPackageModified;
    checkCurrentLabels := CheckCurrentLabels;
    checkCurrentLabelsGen := CheckCurrentLabelsGen;
    cacheAllStateLabels := CacheAllStateLabels;
    checkLabelsOfSnapshot := CheckLabelsOfSnapshot;
    checkLabelsOfRelease := CheckLabelsOfRelease;
    newCheckpoint := NewCheckpoint;
    loadNewCheckpoint := LoadNewCheckpoint;
    writeCheckpoint := WriteCheckpoint;
    invalidateCachedUnsureVersionInfo := InvalidateCachedUnsureVersionInfo;
    invalidateCachedBuildInfo := InvalidateCachedBuildInfo;
    getName := GetName;
    getCreationDate := GetCreationDate;
    getModificationDate := GetModificationDate;
    getUser := GetUser;
    getDescription := GetDescription;
    setName := SetName;
    setCreationDate := SetCreationDate;
    setModificationDate := SetModificationDate;
    setUser := SetUser;
    setDescription := SetDescription;
    logText := LogText;
    snapshotText := SnapshotText;
    releaseText := ReleaseText;
    toText := ToText;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE GetName(self : T) : TEXT =
  BEGIN
    RETURN self.name;
  END GetName;

(*---------------------------------------------------------------------------*)
PROCEDURE GetCreationDate(self : T) : Time.T =
  BEGIN
    RETURN self.cdate;
  END GetCreationDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE GetModificationDate(self : T) : Time.T =
  BEGIN
    RETURN self.mdate;
  END GetModificationDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE GetUser(self : T) : TEXT =
  BEGIN
    RETURN self.user;
  END GetUser; 

(*---------------------------------------------------------------------------*)
PROCEDURE GetDescription(self : T) : TEXT =
  BEGIN
    RETURN self.desc;
  END GetDescription;

(*---------------------------------------------------------------------------*)
PROCEDURE SetName(self : T; name : TEXT) =
  BEGIN
    self.name := name;
  END SetName;

(*---------------------------------------------------------------------------*)
PROCEDURE SetCreationDate(self : T; date : Time.T) =
  BEGIN
    self.cdate := date;
  END SetCreationDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE SetModificationDate(self : T; date : Time.T) =
  BEGIN
    self.mdate := date;
  END SetModificationDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE SetUser(self : T; user : TEXT) =
  BEGIN
    self.user := user;
  END SetUser; 

(*---------------------------------------------------------------------------*)
PROCEDURE SetDescription(self : T; desc : TEXT) =
  BEGIN
    self.desc := desc;
  END SetDescription;

(*---------------------------------------------------------------------------*)
PROCEDURE LogText(self : T) : TEXT =
  VAR 
    res  : TEXT;
    pkg  : TEXT;
    ver  : TEXT;
    snap : TextTextTbl.T;
  BEGIN
    res := "snapshot/release description " & self.name & NL &
           "created by " & self.user;
    IF self.cdate = 0.0d0 THEN
      res := res & NL & "no creation date available" & NL;
    ELSE
      res := res & " at " & RCS_Date.FromTime(self.cdate) & NL;
    END;
    IF self.mdate = 0.0d0 THEN
      res := res & "no modification date available" & NL;
    ELSE
      res := res & "last modified at " & RCS_Date.FromTime(self.cdate) & NL;
    END;
    res := res & NL & "description:" & NL &
           self.desc & NL &
           "package versions:" & NL;
    IF NOT self.releaseTbl.get(self.name, snap) THEN
      IF NOT self.snapshotTbl.get(self.name, snap) THEN
        RETURN res & " !!! not found !!!" & NL;
      END;
    END;
    WITH iter = snap.iterate() DO
      WHILE iter.next(pkg, ver) DO
        res := res & "  " & pkg & "\t" & ver & NL;
      END;
    END;
    RETURN res;
  END LogText;

(*---------------------------------------------------------------------------*)
PROCEDURE AddRoot(self : T; p : Pathname.T) : Pathname.T =
  VAR pn := PathRepr.Native(p);
  BEGIN
    IF Pathname.Absolute(pn) THEN
      RETURN pn;
    ELSE
      RETURN Pathname.Join(self.collectionRoot, pn, NIL);
    END;
  END AddRoot;

(*---------------------------------------------------------------------------*)
PROCEDURE Locations(self : T) : TextSeq.T =
  VAR 
    loc :  TEXT;
  BEGIN
    IF self.locs = NIL THEN
      self.locs := NEW(TextSeq.T).init(self.packageList.size());
      FOR i := 0 TO self.packageList.size() - 1 DO
        WITH pkg = self.packageList.get(i) DO
          EVAL self.locationTbl.get(pkg, loc);
          self.locs.addhi(Pathname.Join(loc, pkg, NIL));
        END;
      END;
    END;
    RETURN self.locs;
  END Locations;

(*---------------------------------------------------------------------------*)
PROCEDURE ParseProjectDescription(self : T; rd : Rd.T) RAISES {Error} =
  VAR
    token, name, location : TEXT;
    tokenValid := FALSE;
    colNr := 1;
    alreadyKnown : BOOLEAN;

    (*-----------------------------------------------------------------------*)
    PROCEDURE NextToken() : TEXT RAISES {Error} =
      BEGIN
        TRY
          token := GetTokenOrString(rd);
        EXCEPT
          Rd.EndOfFile => token := "";
        | Rd.Failure => RAISE Error("error reading project description file");
        | Thread.Alerted => RAISE Error("interrupted while reading " 
          & "project description file");
        END;
        RETURN token;
      END NextToken;

    (*-----------------------------------------------------------------------*)
    PROCEDURE CheckName(name, where : TEXT) RAISES {Error} =
      BEGIN
        IF Text.Length(name) = 0 THEN
          RAISE Error("empty argument name after " & where);
        END;
      END CheckName;

    (*-----------------------------------------------------------------------*)
    PROCEDURE CheckName2(name, what : TEXT) RAISES {Error} =
      BEGIN
        IF Text.Length(name) = 0 THEN
          RAISE Error("empty " & what);
        END;
      END CheckName2;

    (*-----------------------------------------------------------------------*)
    PROCEDURE ParsePkgVersionBlock(what : TEXT) : TextTextTbl.T 
      RAISES {Error} = 
      VAR
        tbl := NEW(TextTextTbl.Default).init();
        name, version, res : TEXT;
      BEGIN
        TRY
	  WHILE NOT Rd.EOF(rd) DO
	    name := NextToken();
	    IF Text.Length(token) > 0 AND Text.GetChar(token, 0) = '#' THEN
	      (* comment till end of line *)
	      EVAL Rd.GetLine(rd);
	    ELSIF Text.Equal(token, "end") THEN
	      RETURN tbl; (* the one correct exit *)
	    ELSE
	      CheckName2(name, "package name in " & what & " block");
	      version := NextToken();
	      CheckName2(version, "version in " & what & " block");
	      IF tbl.get(name, res) THEN
		RAISE Error("multiple entry for package " & name & 
		      " in " & what);
	      ELSE
		EVAL tbl.put(name, version);
	      END;
	    END;
	  END;
        EXCEPT
          Rd.Failure => RAISE Error("error reading project description file");
        | Rd.EndOfFile => RAISE Error("premature eof in project description"
          & " file");
        | Thread.Alerted => RAISE Error("interrupted while reading " 
          & "project description file");
        END;
        RAISE Error("syntax error: missing `end' after " & what & " block");
      END ParsePkgVersionBlock;

    PROCEDURE ReadDesc() RAISES {Rd.Failure, Thread.Alerted, Rd.EndOfFile} =
      VAR 
        lines := NEW(TextSeq.T).init();
        cont := TRUE;
        line :  TEXT;
      BEGIN
        EVAL Rd.GetLine(rd);
        WHILE NOT Rd.EOF(rd) AND cont DO
          line := Rd.GetLine(rd);
          IF Text.Equal(line, "end") THEN
            cont := FALSE;
          ELSE
            lines.addhi(line);
          END;
        END;
        self.desc := TextUtils.TextSeqToText(lines, sep := NL);
      END ReadDesc;

  BEGIN (* ParseProjectDescription *)
    TRY
      WHILE NOT Rd.EOF(rd) DO
        IF tokenValid THEN
          tokenValid := FALSE;
        ELSE
          EVAL NextToken();
        END;
        IF Text.Length(token) > 0 THEN
	  IF Text.GetChar(token, 0) = '#' THEN
	    (* comment till end of line *)
	    EVAL Rd.GetLine(rd);
	  ELSIF Text.Equal(token, "collection") THEN
	    name := PathRepr.Native(NextToken());
	    CheckName(name, "collection");
            EVAL NextToken();
            IF Text.Equal(token, "at") OR
               Text.Equal(token, "=") THEN
              EVAL NextToken();
              location := PathRepr.Native(token);
            ELSE
              location := PathRepr.Native(name);
              name := "collection_" & Fmt.Int(colNr);
              INC(colNr);
              tokenValid := TRUE;
            END;
            alreadyKnown := MemberOfTextSeq(self.collectionList, name);
            IF NOT alreadyKnown THEN
              self.collectionList.addhi(name);
              EVAL self.collectionTbl.put(name, location);
            END;
	  ELSIF Text.Equal(token, "package") THEN
	    name := NextToken();
	    CheckName(name, "package");
            alreadyKnown := MemberOfTextSeq(self.packageList, name);
            IF NOT alreadyKnown THEN
              self.packageList.addhi(name);
            END;
            EVAL NextToken();
            IF Text.Equal(token, "in") OR
               Text.Equal(token, "from") THEN
              EVAL NextToken();
              IF NOT alreadyKnown THEN
		EVAL self.packageTbl.put(name, token);
		IF self.collectionTbl.get(token, location) THEN
		  EVAL self.locationTbl.put(name, self.addRoot(location));
		ELSE
		  RAISE Error("collection " & token & " not found");
		END;
              END;
            ELSE
              tokenValid := TRUE;
            END;
	  ELSIF Text.Equal(token, "collectionroot") OR
                Text.Equal(token, "collectionpath") OR
                Text.Equal(token, "root") OR 
                Text.Equal(token, "prefix") THEN
            name := token;
	    location := PathRepr.Native(NextToken());
	    CheckName(location, name);
            self.collectionRootOrig := location;
            IF NOT ProcessEnv.Defined(self.env, "HOME") THEN
              ProcessEnv.Set(self.env, "HOME", PathRepr.Native("/"));
            END;
            IF NOT ProcessEnv.Defined(self.env, "USER") THEN
              ProcessEnv.Set(self.env, "USER", "nobody");
            END;
            IF ProcessEnv.Defined(self.env, "PRJ_ROOT") THEN
                ProcessEnv.Set(
                    self.env, "PRJ_ROOT", 
                    PathRepr.Native(
                        ProcessEnv.Value(self.env, "PRJ_ROOT")));
            ELSE
              IF ProcessEnv.Defined(self.env, "collectionroot") THEN
                ProcessEnv.Set(
                    self.env, "PRJ_ROOT", 
                    PathRepr.Native(
                        ProcessEnv.Value(self.env, "collectionroot")));
              ELSIF ProcessEnv.Defined(self.env, "COLLECTIONROOT") THEN
                ProcessEnv.Set(
                    self.env, "PRJ_ROOT", 
                    PathRepr.Native(
                        ProcessEnv.Value(self.env, "COLLECTIONROOT")));
              ELSE
                ProcessEnv.Set(self.env, "PRJ_ROOT", "undefined_PRJ_ROOT");
              END;
            END;
            WITH prjRoot = ProcessEnv.Value(self.env, "PRJ_ROOT") DO
              IF Text.Equal(prjRoot, Pathname.Current) OR
                 Text.Equal(prjRoot, 
                            FSUtils.CanonicalPathname(Pathname.Current)) THEN
                MsgX.Warning(self.msgif, 
                             "project root dirctory (PRJ_ROOT) is set to " &
                             Pathname.Current);
              ELSIF NOT FSUtils.IsDir(prjRoot) THEN
                MsgX.Fatal(self.msgif, 
                           "project root dirctory (PRJ_ROOT) " & prjRoot & 
                           " does not xist");
              END;
            END;
            self.collectionRoot := SubstEnvVars(location, self.env);
            self.collectionRoot := SubstituteVariables(self.collectionRoot,
                                                       self.env);
            self.collectionRoot := 
                FSUtils.CanonicalPathname(self.collectionRoot);
	  ELSIF Text.Equal(token, "snapshot") THEN
	    name := NextToken();
	    CheckName(name, "snapshot");
            IF Text.Empty(self.name) THEN self.name := name END;
	    EVAL self.snapshotTbl.put(name, ParsePkgVersionBlock("snapshot"));
	  ELSIF Text.Equal(token, "release") THEN
	    name := NextToken();
	    CheckName(name, "release");
            IF Text.Empty(self.name) THEN self.name := name END;
	    EVAL self.releaseTbl.put(name, ParsePkgVersionBlock("release"));
	  ELSIF Text.Equal(token, "name") THEN
	    self.name := NextToken();
	  ELSIF Text.Equal(token, "user") OR Text.Equal(token, "creator") THEN
	    self.user := NextToken();
	  ELSIF Text.Equal(token, "mtime") THEN
	    token := NextToken();
            self.mdate := RCS_Date.ToTimeApprox(token);
	  ELSIF Text.Equal(token, "ctime") THEN
	    token := NextToken();
            self.cdate := RCS_Date.ToTimeApprox(token);
          ELSIF Text.Equal(token, "description") THEN
            ReadDesc();
	  ELSE
	    RAISE Error("syntax error at token " & token);
	  END;
        END;
      END;
    EXCEPT
      Rd.Failure => RAISE Error("error reading project description file");
    | Rd.EndOfFile => RAISE Error("premature eof in project description"
      & " file");
    | Thread.Alerted => RAISE Error("interrupted while reading " 
      & "project description file");
    | TextUtils.Error(e) => 
      RAISE Error("undefined variable in collectionroot " & e);
    | FSUtils.E(e) => RAISE Error(e);
    END;
  END ParseProjectDescription;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; fn : TEXT; cfg : PkgBase.T; check := FALSE;
               defaultCollectionRoot : TEXT := NIL; cpfn : TEXT;
               useCache := TRUE; env : ProcessEnv.T := NIL;
               msgif : MsgIF.T := NIL;
               pkgvcAcc : PoolSet.PkgVCAccessor := NIL;
               verboseCacheMsgs := TRUE;
	       preferredPkgKind : TEXT := NIL;
               depsMandatory := TRUE;
               cacheEarly := FALSE) : T 
  RAISES {Error} =
  VAR 
    rd  : FileRd.T;
    res : TEXT;
  BEGIN
    self.name := "";
    self.desc := "";
    self.user := "unknown";
    self.cdate := 0.0d0;
    self.mdate := 0.0d0;
    self.msgif := msgif;
    self.pkgvcAcc := pkgvcAcc;
    self.depsMandatory := depsMandatory;
    self.locs := NIL;
    self.locsText := NIL;
    IF env = NIL THEN
      self.env := ProcessEnv.Current();
    ELSE
      self.env := env;
    END;
    self.depGraph := NIL;
    self.useCache := useCache;
    TRY
      IF defaultCollectionRoot = NIL THEN
        WITH cur = FSUtils.CanonicalPathname(Pathname.Current) DO
          self.collectionRoot := cur;
          self.collectionRootOrig := cur;
        END;
      ELSE
        WITH defroot = FSUtils.CanonicalPathname(defaultCollectionRoot) DO
          self.collectionRoot := defroot;
          self.collectionRootOrig := defroot;
        END;
      END;
    EXCEPT
      FSUtils.E(e) => RAISE Error(e);
    END;
    self.collectionList := NEW(TextSeq.T).init(5);
    self.collectionTbl := NEW(TextTextTbl.Default).init(5);
    self.packageList := NEW(TextSeq.T).init(40);
    self.locationTbl := NEW(TextTextTbl.Default).init(40);
    self.packageTbl := NEW(TextTextTbl.Default).init(40);
    self.snapshotTbl := NEW(TextTextTextTbl.Default).init(40);
    self.releaseTbl := NEW(TextTextTextTbl.Default).init(40);
    TRY
      self.poolset := NEW(PoolSet.T).init(cfg, cpfn, useCache, 
                                          msgif := self.msgif,
                                          pkgvcAcc := self.pkgvcAcc,
                                          verboseCache := verboseCacheMsgs,
                                          prefkind := preferredPkgKind,
                                          cacheEarly := cacheEarly);
    EXCEPT
      PoolSet.Error(e) => RAISE Error(e);
    END;
    self.ignoredList := NIL;
    self.missingList := NIL;
    self.externalShell := NIL;
    self.varTbl := NEW(TextTextTbl.Default).init(10);
    self.stateLabelTbl := NEW(TextTextTextTbl.Default).init(40);
    IF cpfn = NIL OR NOT useCache OR NOT FSUtils.IsFile(cpfn) THEN
      self.oldState := NIL;
    ELSE
      self.oldState := Checkpoint.New(self.poolset.getFileCache(), self.msgif);
      TRY
        self.oldState.fromFile(cpfn);
        IF debugStateCache AND Msg.dFlag THEN
          self.poolset.dumpStateCache("PrjDesc.Init(): " &
            "current state:");
          MsgX.D(self.msgif, "old state:", level := 2);
          MsgX.D(self.msgif, self.oldState.toText(), level := 2);
          MsgX.D(self.msgif, 
                 "(current state and old state should be exactly the same)",
                 level := 2);
        END;
      EXCEPT
        Checkpoint.Error(e) => RAISE Error("error reading checkpoint: " & e);
      END;
    END;
    (* if there is no file to parse, we quit the initialization here *)
    IF fn = NIL THEN RETURN self END;
    TRY
      rd := FileRd.Open(fn);
    EXCEPT
      OSError.E => RAISE Error("cannot open file " & fn);
    END;
    ParseProjectDescription(self, rd);
    TRY
      Rd.Close(rd);
    EXCEPT ELSE END;
    (* fn parsed without errors *)
    FOR i := 0 TO self.collectionList.size() - 1 DO
      VAR loc : TEXT; BEGIN
        WITH col = self.collectionList.get(i) DO
          IF self.collectionTbl.get(col, loc) THEN
            TRY
              self.poolset.appendPool(self.addRoot(loc));
            EXCEPT
              PoolSet.Error(e) => RAISE Error(e);
            END;
          ELSE
            RAISE Error("internal error: collection " & col & " not in table")
          END;
        END;
      END;
    END;
    (* all collections of packages added as pools to be searched *)
    IF check OR self.useCache  THEN
      IF NOT self.poolset.checkAll(self.packageList, res, 
                                   self.missingList, 
                                   self.locationTbl, 
                                   checkHomogeneity := FALSE,
                                   ignoreMissingPackages := TRUE) THEN
        IF check THEN
          RAISE Error(res);
        END;
      END;
    END;
    (* all packages exist and are of the same type *)
    IF self.packageList.size() > 0 THEN
      self.pkgKind := self.poolset.pkgType(self.packageList.get(0));
    ELSE
      self.pkgKind := NIL;
    END;
    (* some state cache tests *)
    (*
    VAR
      sc1 := Checkpoint.New(self.poolset.getFileCache(), self.msgif);
      sc2 :  Checkpoint.T;
    BEGIN
      sc1.update("/usr/tmp");
      MsgX.T(self.msgif, "sc1 1:"); MsgX.T(self.msgif, sc1.toText());
      sc1.update("/usr/tmp");
      MsgX.T(self.msgif, "sc1 w:"); MsgX.T(self.msgif, sc1.toText());
      sc2 := Checkpoint.New(self.poolset.getFileCache(), self.msgif);
      sc2.update("/usr/tmp");
      MsgX.T(self.msgif, "sc2 1:"); MsgX.T(self.msgif, sc2.toText());
      sc2.update("/usr/tmp");
      MsgX.T(self.msgif, "sc2 w:"); MsgX.T(self.msgif, sc2.toText());
    END;
    *)
    (* end state cache tests *)
    IF self.oldState = NIL THEN
      self.oldState := self.poolset.cachedState();
    END;
    EvaluateStateCacheChanges(self, self.oldState);
    (* -- this cannot be done properly until the dependency graph is built *)
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE ReInit(self : T; fn : TEXT; cfg : PkgBase.T; check := FALSE;
                 defaultCollectionRoot : TEXT := NIL; cpfn : TEXT;
                 useCache := TRUE; env : ProcessEnv.T := NIL;
                 msgif : MsgIF.T := NIL; 
                 pkgvcAcc : PoolSet.PkgVCAccessor := NIL)
  RAISES {Error} =
  VAR 
    rd  : FileRd.T;
    res : TEXT;
  BEGIN
    self.msgif := msgif;
    self.pkgvcAcc := pkgvcAcc;
    IF env = NIL THEN
      self.env := ProcessEnv.Current();
    ELSE
      self.env := env;
    END;
    self.depGraph := NIL;
    self.useCache := useCache;
    self.locs := NIL;
    self.locsText := NIL;
    TRY
      IF defaultCollectionRoot = NIL THEN
        WITH cur = FSUtils.CanonicalPathname(Pathname.Current) DO
          self.collectionRoot := cur;
          self.collectionRootOrig := cur;
        END;
      ELSE
        WITH defroot = FSUtils.CanonicalPathname(defaultCollectionRoot) DO
          self.collectionRoot := defroot;
          self.collectionRootOrig := defroot;
        END;
      END;
    EXCEPT
      FSUtils.E(e) => RAISE Error(e);
    END;
    EVAL self.collectionList.init(5);
    EVAL NARROW(self.collectionTbl, TextTextTbl.Default).init(5);
    EVAL self.packageList.init(40);
    EVAL NARROW(self.locationTbl, TextTextTbl.Default).init(40);
    EVAL NARROW(self.packageTbl, TextTextTbl.Default).init(40);
    EVAL NARROW(self.snapshotTbl, TextTextTextTbl.Default).init(40);
    EVAL NARROW(self.releaseTbl, TextTextTextTbl.Default).init(40);
    TRY
      EVAL self.poolset.init(cfg, cpfn, useCache, msgif := self.msgif,
                             pkgvcAcc := self.pkgvcAcc);
    EXCEPT
      PoolSet.Error(e) => RAISE Error(e);
    END;
    self.ignoredList := NIL;
    self.missingList := NIL;
    self.externalShell := NIL;
    EVAL NARROW(self.varTbl, TextTextTbl.Default).init(10);
    EVAL NARROW(self.stateLabelTbl, TextTextTextTbl.Default).init(40);
    IF cpfn = NIL OR NOT FSUtils.IsFile(cpfn) THEN
      self.oldState := NIL;
    ELSE
      self.oldState := Checkpoint.New(self.poolset.getFileCache(), self.msgif);
      TRY
        self.oldState.fromFile(cpfn);
        IF debugStateCache AND Msg.dFlag THEN
          self.poolset.dumpStateCache("PrjDesc.Init(): " &
            "current state:");
          MsgX.D(self.msgif, "old state:", level := 2);
          MsgX.D(self.msgif, self.oldState.toText(), level := 2);
          MsgX.D(self.msgif, 
                 "(current state and old state should be exactly the same)",
                 level := 2);
        END;
      EXCEPT
        Checkpoint.Error(e) => RAISE Error("error reading checkpoint: " & e);
      END;
    END;
    TRY
      rd := FileRd.Open(fn);
    EXCEPT
      OSError.E => RAISE Error("cannot open file " & fn);
    END;
    ParseProjectDescription(self, rd);
    TRY
      Rd.Close(rd);
    EXCEPT ELSE END;
    (* fn parsed without errors *)
    FOR i := 0 TO self.collectionList.size() - 1 DO
      VAR loc : TEXT; BEGIN
        WITH col = self.collectionList.get(i) DO
          IF self.collectionTbl.get(col, loc) THEN
            TRY
              self.poolset.appendPool(self.addRoot(loc));
            EXCEPT
              PoolSet.Error(e) => RAISE Error(e);
            END;
          ELSE
            RAISE Error("internal error: collection " & col & " not in table")
          END;
        END;
      END;
    END;
    (* all collections of packages added as pools to be searched *)
    IF check OR self.useCache  THEN
      IF NOT self.poolset.checkAll(self.packageList, res, 
                                   self.missingList, 
                                   self.locationTbl, 
                                   checkHomogeneity := FALSE,
                                   ignoreMissingPackages := TRUE) THEN
        IF check THEN
          RAISE Error(res);
        END;
      END;
    END;
    (* all packages exist and are of the same type *)
    IF self.packageList.size() > 0 THEN
      self.pkgKind := self.poolset.pkgType(self.packageList.get(0));
    ELSE
      self.pkgKind := NIL;
    END;
    IF self.oldState = NIL THEN
      self.oldState := self.poolset.cachedState();
    END;
    EvaluateStateCacheChanges(self, self.oldState);
    (* -- this cannot be done properly until the dependency graph is built *)
  END ReInit;

(*---------------------------------------------------------------------------*)
PROCEDURE EvaluateStateCacheChanges(self : T; oldState : Checkpoint.T) 
  RAISES {Error} =
  VAR
    pkgdirs : TextSeq.T;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    MsgX.T(self.msgif, "evaluating state cache changes...");
    IF debugStateCache THEN
      self.poolset.dumpStateCache("EvaluateStateCacheChanges(): " &
        "current state:");
      MsgX.T(self.msgif, "old state:");
      TRY MsgX.T(self.msgif, self.oldState.toText()); EXCEPT ELSE END;
    END;
    self.changedPkgDirs := self.poolset.cachedState().diff(oldState);
    (* `changedPkgDirs' contains a list of directories whose fingerprint
       is different from that recorded in the last checkpoint (oldState).
       Since we cannot know from the changed fingerprint if anything
       relevant has changed, we need to reset certain cache atributes.
       For all packages with changed fingerprint the dependencies need 
       to be recomputed, they need to be rebuilt and re-checked for
       their modification status.
    *)
    pkgdirs := self.changedPkgDirs;
    FOR i := 0 TO pkgdirs.size() - 1 DO
      WITH dir = pkgdirs.get(i) DO
        TRY
          self.poolset.updateStateCache(dir, "need-mkdep-build-ship", 0);
          self.poolset.updateStateCache(dir, "clear-mod-unmod", 0);
        EXCEPT
          PoolSet.Error(e) => RAISE Error(e);
        END;
      END;
    END;
    IF debugStateCache THEN
      self.poolset.dumpStateCache("EvaluateStateCacheChanges(end): " &
        "current state:");
    END;
    (* self.changedPkgDirs is now defined *)
  END EvaluateStateCacheChanges;

(*---------------------------------------------------------------------------*)
PROCEDURE EvaluateStateCacheChangesForDependendPackages(
    self : T;
    pkgs : TextSeq.T := NIL) 
  RAISES {Error} =
  VAR
    pkgdirs : TextSeq.T;
    dir     : TEXT;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    MsgX.T(self.msgif, "readjusting state cache...");
    IF debugStateCache THEN
      self.poolset.dumpStateCache(
               "EvaluateStateCacheChangesForDependendPackages(): " & 
               "current state:");
    END;
    <* ASSERT self.depGraph # NIL *>
    <* ASSERT self.changedPkgDirs # NIL *>
    (* If packages have changed, they need to be rebuilt and reshipped.
       Consequently, all packages depending on all changed packages
       need to be rebuilt and reshipped, too. 
    *)
    IF pkgs = NIL THEN
      pkgs := NEW(TextSeq.T).init();
      FOR i := 0 TO self.changedPkgDirs.size() - 1 DO
        WITH pkg = Pathname.Last(self.changedPkgDirs.get(i)) DO
          pkgs.addhi(pkg);
        END;
      END;
    END;
    pkgs := AddDependingPackages(self, pkgs);
    pkgdirs := NEW(TextSeq.T).init(pkgs.size());
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        IF self.locationTbl.get(pkg, dir) THEN
          pkgdirs.addhi(Pathname.Join(dir, pkg, NIL));
        ELSE
          RAISE Error("no location known for package " & pkg);
        END;
      END;
    END;
    (* depGraph # NIL AND pkgdirs = dirs(changed + dependend pkgs) *)
    FOR i := 0 TO pkgdirs.size() - 1 DO
      WITH dir = pkgdirs.get(i) DO
        TRY
          self.poolset.updateStateCache(dir, "need-build-ship", 0);
        EXCEPT
          PoolSet.Error(e) => RAISE Error(e);
        END;
      END;
    END;
    IF debugStateCache THEN
      self.poolset.dumpStateCache(
               "EvaluateStateCacheChangesForDependendPackages(end): " &
               "current state");
    END;
  END EvaluateStateCacheChangesForDependendPackages;

(*---------------------------------------------------------------------------*)
PROCEDURE Parse(self : T; rd : Rd.T) RAISES {Error} =
  BEGIN
    self.locs := NIL;
    self.locsText := NIL;
    ParseProjectDescription(self, rd);
    IF self.useCache THEN 
      self.changedPkgDirs := self.poolset.cachedState().diff(self.oldState);
    END;
  END Parse; 

(*---------------------------------------------------------------------------*)
PROCEDURE LoadFile(self : T; fn : TEXT) RAISES {Error} =
  VAR
    rd  : FileRd.T;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
    EXCEPT
      OSError.E => RAISE Error("cannot open file " & fn);
    END;
    Parse(self, rd);
    TRY
      Rd.Close(rd);
    EXCEPT ELSE END;
  END LoadFile;

(*---------------------------------------------------------------------------*)
PROCEDURE PkgsOkay(self : T; VAR res : TEXT) : BOOLEAN =
  BEGIN
    RETURN self.poolset.checkAll(self.packageList, res, self.missingList,
                                 self.locationTbl, FALSE);
  END PkgsOkay; 

(*---------------------------------------------------------------------------*)
PROCEDURE GetPoolSet(self : T) : PoolSet.T =
  BEGIN
    RETURN self.poolset;
  END GetPoolSet;

(*---------------------------------------------------------------------------*)
PROCEDURE SetExternalShell(self : T; shell : TEXT) =
  BEGIN
    self.externalShell := shell;
  END SetExternalShell;

(*---------------------------------------------------------------------------*)
PROCEDURE SetPreferredPkgKind(self : T; k : PkgBase.Kind) =
  BEGIN
    self.poolset.setPreferredPkgKind(k);
  END SetPreferredPkgKind;

(*---------------------------------------------------------------------------*)
PROCEDURE DefineGlobalVar(self : T; name, val : TEXT) =
  BEGIN
    EVAL self.varTbl.put(name, val);
  END DefineGlobalVar;

(*---------------------------------------------------------------------------*)
PROCEDURE DeleteGlobalVar(self : T; name : TEXT) =
  VAR value : TEXT;
  BEGIN
    EVAL self.varTbl.delete(name, value);
  END DeleteGlobalVar;

(*---------------------------------------------------------------------------*)
PROCEDURE DefineGlobalVars(self : T; vars : TextTextTbl.T) =
  VAR
    name, val : TEXT;
    iter := vars.iterate();
  BEGIN
    WHILE iter.next(name, val) DO
      EVAL self.varTbl.put(name, val);
    END;
  END DefineGlobalVars;

(*---------------------------------------------------------------------------*)
PROCEDURE VarTableCopy(self : T) : TextTextTbl.T = 
  VAR 
    n, v :  TEXT;
    res  := NEW(TextTextTbl.Default).init(self.varTbl.size() + 10);
    iter := self.varTbl.iterate();
  BEGIN
    WHILE iter.next(n, v) DO
      EVAL res.put(n, v);
    END;
    RETURN res;
  END VarTableCopy;

(*---------------------------------------------------------------------------*)
PROCEDURE CollectionPath(self : T) : TEXT =
  BEGIN
    RETURN self.collectionRoot;
  END CollectionPath;

(*---------------------------------------------------------------------------*)
PROCEDURE Collections(self : T) : TextSeq.T =
  BEGIN
    RETURN self.collectionList;
  END Collections;

(*---------------------------------------------------------------------------*)
PROCEDURE CollectionsLocation(self : T) : TextTextTbl.T =
  BEGIN
    RETURN self.collectionTbl;
  END CollectionsLocation; 

(*---------------------------------------------------------------------------*)
PROCEDURE Packages(self : T) : TextSeq.T =
  BEGIN
    RETURN self.packageList;
  END Packages;

(*---------------------------------------------------------------------------*)
PROCEDURE PackagesCollection(self : T) : TextTextTbl.T =
  BEGIN
    RETURN self.packageTbl;
  END PackagesCollection; 

(*---------------------------------------------------------------------------*)
PROCEDURE Snapshots(self : T) : TextSeq.T =
  VAR
    iter := self.snapshotTbl.iterate();
    res := NEW(TextSeq.T).init(self.snapshotTbl.size());
    name : TEXT;
    tbl  : TextTextTbl.T;
  BEGIN
    WHILE iter.next(name, tbl) DO
      res.addhi(name);
    END;
    RETURN res;
  END Snapshots;

(*---------------------------------------------------------------------------*)
PROCEDURE Releases(self : T) : TextSeq.T =
  VAR
    iter := self.releaseTbl.iterate();
    res := NEW(TextSeq.T).init(self.releaseTbl.size());
    name : TEXT;
    tbl  : TextTextTbl.T;
  BEGIN
    WHILE iter.next(name, tbl) DO
      res.addhi(name);
    END;
    RETURN res;
  END Releases;

(*---------------------------------------------------------------------------*)
PROCEDURE MakeHead(self : T) : TextTextTbl.T =
  VAR
    head := NEW(TextTextTbl.Default).init(self.packageList.size());
  BEGIN
    FOR i := 0 TO self.packageList.size() - 1 DO
      WITH pkg = self.packageList.get(i) DO
        EVAL head.put(pkg, "head");
      END;
    END;
    RETURN head;
  END MakeHead;

(*---------------------------------------------------------------------------*)
PROCEDURE Snapshot(self : T; name : TEXT) : TextTextTbl.T =
  VAR
    tbl  : TextTextTbl.T;
  BEGIN
    IF Text.Equal(name, "head") THEN
      RETURN MakeHead(self);
    ELSIF self.snapshotTbl.get(name, tbl) THEN
      RETURN tbl;
    ELSE
      RETURN NIL;
    END;
  END Snapshot;

(*---------------------------------------------------------------------------*)
PROCEDURE Release(self : T; name : TEXT) : TextTextTbl.T =
  VAR
    tbl  : TextTextTbl.T;
  BEGIN
    IF self.releaseTbl.get(name, tbl) THEN
      RETURN tbl;
    ELSE
      RETURN NIL;
    END;
  END Release;

(*---------------------------------------------------------------------------*)
PROCEDURE CurrentTagList(self : T) : TextTextTbl.T
  RAISES {Error} =
  VAR
    pkg, fn :  TEXT;
    tagText :  TEXT;
    tag     := NEW(Tag.T);
    snap    :  TextTextTbl.T;
  BEGIN
    snap := NEW(TextTextTbl.Default).init(self.packageList.size());
    TRY
      FOR i := 0 TO self.packageList.size() - 1 DO
        pkg := self.packageList.get(i);
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "--- looking for current version of package " & 
            pkg & " ---");
        END;
        IF self.poolset.execAction(pkg, "currenttag", 
                                   tagText,
                                   self.externalShell,
                                   self.varTbl) = 0 THEN
          fn := "PkgCT";
        ELSE
          RAISE Error("cannot get current tag for package " & pkg);
        END;
        IF tagText = NIL THEN
          tagText := TextUtils.Compress(self.poolset.fileContents(pkg, fn));
          self.poolset.setVal(pkg, "current-tag", tagText);
        END;
        EVAL tag.initFromText(tagText);
        IF NOT tag.okay() THEN
          RAISE Error("tag `" & tagText & "' for package " & pkg &
                " seems to be incorrect");
        END;
        EVAL snap.put(pkg, tag.denotation());
      END;
    EXCEPT
      Error(e) => RAISE Error(e); 
      | PoolSet.Error(e) => RAISE Error(e);
    END;
    RETURN snap;
  END CurrentTagList; 

(*---------------------------------------------------------------------------*)
PROCEDURE DefineSnapshot(self : T; name : TEXT;
                         snap : TextTextTbl.T) RAISES {Error} =
  VAR
    dummy   :  TextTextTbl.T;
  BEGIN
    IF self.snapshotTbl.get(name, dummy) THEN
      RAISE Error("snapshot " & name & " already exists");
    END;
    EVAL self.snapshotTbl.put(name, snap); (* cannot exist already *)
  END DefineSnapshot;

(*---------------------------------------------------------------------------*)
PROCEDURE DefineRelease(self : T; name : TEXT;
                        snap : TextTextTbl.T) RAISES {Error} =
  VAR
    dummy   :  TextTextTbl.T;
  BEGIN
    IF self.releaseTbl.get(name, dummy) THEN
      RAISE Error("release " & name & " already exists");
    END;
    EVAL self.releaseTbl.put(name, snap); (* cannot exist already *)
  END DefineRelease; 

(*---------------------------------------------------------------------------*)
PROCEDURE GetTags(self : T; packageList : TextSeq.T) : TextTextTbl.T 
  RAISES {Error} =
  VAR
    pkg, fn :  TEXT;
    tagText :  TEXT;
    tag     := NEW(Tag.T);
    snap    :  TextTextTbl.T;
  BEGIN
    snap := NEW(TextTextTbl.Default).init(packageList.size());
    TRY
      FOR i := 0 TO packageList.size() - 1 DO
        pkg := packageList.get(i);
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "--- package " & pkg & " ---");
        END;
        IF self.poolset.execAction(pkg, "isrelease", tagText,
                                   self.externalShell,
                                   self.varTbl) = 0 THEN
          EVAL self.poolset.execAction(pkg, "currentreleasetag", tagText,
                                       self.externalShell, self.varTbl);
          fn := "PkgCRT";
        ELSIF self.poolset.execAction(pkg, "currentdeveltag", tagText,
                                      self.externalShell,
                                      self.varTbl) = 0 THEN
          fn := "PkgCDT";
        ELSE
          RAISE Error("cannot get tag for package " & pkg);
        END;
        IF tagText = NIL THEN
          tagText := TextUtils.Compress(self.poolset.fileContents(pkg, fn));
          IF Text.Equal(fn, "PkgCRT") THEN
            self.poolset.setVal(pkg, "current-release-tag", tagText);
          ELSE
            self.poolset.setVal(pkg, "current-devel-tag", tagText);
          END;
        END;
        EVAL tag.initFromText(tagText);
        IF NOT tag.okay() THEN
          RAISE Error("tag `" & tagText & "' for package " & pkg &
                " seems to be incorrect");
        END;
        EVAL snap.put(pkg, tag.denotation());
      END;
    EXCEPT
      Error(e) => RAISE Error(e); 
    | PoolSet.Error(e) => RAISE Error(e);
    END;
    RETURN snap;
  END GetTags; 

(*---------------------------------------------------------------------------*)
PROCEDURE NewSnapshot(self : T; name : TEXT) RAISES {Error} =
  VAR
    pkg, fn :  TEXT;
    tagText :  TEXT;
    dummy   :  TEXT;
    tag     := NEW(Tag.T);
    snap    :  TextTextTbl.T;
  BEGIN
    IF self.snapshotTbl.get(name, snap) THEN
      RAISE Error("snapshot " & name & " already exists");
    END;
    snap := NEW(TextTextTbl.Default).init(self.packageList.size());
    TRY
      FOR i := 0 TO self.packageList.size() - 1 DO
        pkg := self.packageList.get(i);
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "--- package " & pkg & " ---");
        END;
        IF self.poolset.execAction(pkg, "modified", dummy,
                                   self.externalShell,
                                   self.varTbl) = 0 THEN
          RAISE Error("package " & pkg & " is locally modified. " &
                "Please commit your changes first.");
        ELSIF self.poolset.execAction(pkg, "isrelease", tagText,
                                      self.externalShell,
                                      self.varTbl) = 0 THEN
          EVAL self.poolset.execAction(pkg, "currentreleasetag", tagText,
                                       self.externalShell, self.varTbl);
          fn := "PkgCRT";
        ELSIF self.poolset.execAction(pkg, "currentdeveltag", tagText,
                                      self.externalShell,
                                      self.varTbl) = 0 THEN
          fn := "PkgCDT";
        ELSE
          RAISE Error("cannot get tag for package " & pkg);
        END;
        IF tagText = NIL THEN
          tagText := TextUtils.Compress(self.poolset.fileContents(pkg, fn));
          IF Text.Equal(fn, "PkgCRT") THEN
            self.poolset.setVal(pkg, "current-release-tag", tagText);
          ELSE
            self.poolset.setVal(pkg, "current-devel-tag", tagText);
          END;
        END;
        EVAL tag.initFromText(tagText);
        IF NOT tag.okay() THEN
          RAISE Error("tag `" & tagText & "' for package " & pkg &
                " seems to be incorrect");
        END;
        EVAL snap.put(pkg, tag.denotation());
      END;
    EXCEPT
      Error(e) => RAISE Error(e); 
    | PoolSet.Error(e) => RAISE Error(e);
    END;
    EVAL self.snapshotTbl.put(name, snap); (* cannot exist already *)
  END NewSnapshot;

(*---------------------------------------------------------------------------*)
PROCEDURE NewRelease(self : T; name : TEXT) RAISES {Error} =
  VAR
    pkg, fn :  TEXT;
    tagText :  TEXT;
    dummy   :  TEXT;
    tag     := NEW(Tag.T);
    rel     :  TextTextTbl.T;
  BEGIN
    IF self.releaseTbl.get(name, rel) THEN
      RAISE Error("release " & name & " already exists");
    END;
    rel := NEW(TextTextTbl.Default).init(self.packageList.size());
    TRY
      FOR i := 0 TO self.packageList.size() - 1 DO
        pkg := self.packageList.get(i);
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "--- package " & pkg & " ---");
        END;
        IF self.poolset.execAction(pkg, "modified", dummy,
                                   self.externalShell,
                                   self.varTbl) = 0 THEN
          RAISE Error("package " & pkg & " is locally modified. " &
                "Please commit your changes first.");
        ELSIF self.poolset.execAction(pkg, "isrelease", tagText,
                                      self.externalShell,
                                      self.varTbl) = 0 THEN
          EVAL self.poolset.execAction(pkg, "currentreleasetag", tagText,
                                       self.externalShell,
                                       self.varTbl);
          fn := "PkgCRT";
        ELSE
          RAISE Error("cannot get release tag for package " & pkg);
        END;
        IF tagText = NIL THEN
          tagText := TextUtils.Compress(self.poolset.fileContents(pkg, fn));
          IF Text.Equal(fn, "PkgCRT") THEN
            self.poolset.setVal(pkg, "current-release-tag", tagText);
          ELSE
            self.poolset.setVal(pkg, "current-devel-tag", tagText);
          END;
        END;
        EVAL tag.initFromText(tagText);
        IF NOT tag.okay() THEN
          RAISE Error("tag `" & tagText & "' for package " & pkg &
                " seems to be incorrect");
        END;
        EVAL rel.put(pkg, tag.denotation());
      END;
    EXCEPT
      Error(e) => RAISE Error(e); 
    | PoolSet.Error(e) => RAISE Error(e);
    END;
    EVAL self.releaseTbl.put(name, rel); (* cannot exist already *)
  END NewRelease; 

(*---------------------------------------------------------------------------*)
PROCEDURE Checkout(self : T; name : TEXT; tab : TextTextTbl.T;
                   heedModifiedPackages := TRUE) 
  RAISES {Error} =

  PROCEDURE GetPackage(pkg, tag : TEXT) RAISES {Error} =
    BEGIN
      IF Msg.tFlag THEN
        MsgX.T(self.msgif, "--- get package " & pkg & " with tag " & 
          tag & " ---");
      END;
      IF NOT self.locationTbl.get(pkg, loc) THEN
        RAISE Error("package " & pkg & " not found");
      END;
      TRY
        exists := self.poolset.exists(pkg, loc);
      EXCEPT
        PoolSet.Error(e) => RAISE Error(e);
      END;
      IF exists THEN
        (* package is already checked out, just update it *)
        TRY
          IF self.poolset.execAction(pkg, "modified", dummy,
                                     self.externalShell,
                                     self.varTbl) = 0 THEN
            IF heedModifiedPackages THEN
              WITH msg = "package " & pkg & " is locally modified. " &
                  "Continue" DO
                IF NOT PkgVC.confirmation.okay(msg) THEN
                  RAISE Error("package " & pkg & " is locally modified. " &
                        "Please commit your changes first.");
                END;
              END;
            ELSE
              MsgX.Warning(self.msgif, "package " & pkg & 
                " is locally modified.");
            END
          END
        EXCEPT
          PoolSet.Error(e) => RAISE Error(e);
        | Error(e) => RAISE Error(e);
        END;
        par := self.varTableCopy();
        EVAL par.put("TAG", tag);
        TRY
          IF self.poolset.execAction(pkg, "update", dummy,
                                     self.externalShell, par) # 0 THEN
            IF dummy = NIL THEN
              dummy := "(no further explanation)";
            END;
            RAISE Error("update of package " & pkg & " failed:" & NL & dummy);
          END;
        EXCEPT
          PoolSet.Error(e) => RAISE Error(e);
        | Error(e) => RAISE Error(e);
        END;
      ELSE
        (* package must be checked out *)
        IF NOT self.packageTbl.get(pkg, col) THEN
          RAISE Error("collection for package " & pkg & " not found");
        END;
        IF NOT self.collectionTbl.get(col, loc) THEN
          RAISE Error("location of collection " & col & " not found");
        END;
        par := self.varTableCopy();
        EVAL par.put("TAG", tag);
        EVAL par.put("LOCATION", loc);
        EVAL par.put("PKG", pkg);
        TRY
          IF self.poolset.checkout(pkg, "checkout", self.externalShell, 
                                   self.collectionRoot, par) # 0 THEN
            RAISE Error("checkout of package " & pkg & " failed");
          END;
          (* remove the package from the list of missing packages,
             assuming it is at most listed once... *)
          IF self.missingList # NIL AND self.missingList.size() > 0 THEN
            IF Text.Equal(self.missingList.getlo(), pkg) THEN
              EVAL self.missingList.remlo();
            ELSIF Text.Equal(self.missingList.gethi(), pkg) THEN
              EVAL self.missingList.remhi();
            ELSE
              FOR i := 1 TO self.missingList.size() - 2 DO
                WITH act = self.missingList.get(i) DO
                  IF Text.Equal(act, pkg) THEN
                    self.missingList.put(i, self.missingList.remhi());
                    EXIT;
                  END;
                END;
              END;
            END;
          END;
        EXCEPT
          PoolSet.Error(e) => RAISE Error(e);
        | Error(e) => RAISE Error(e);
        END;
      END;
    END GetPackage;

  VAR
    iter : TextTextTbl.Iterator;
    pkg, tag, col, loc, dummy : TEXT;
    exists : BOOLEAN;
    par : TextTextTbl.T;
  BEGIN
    IF tab = NIL THEN
      (* If we haven't got a configuration (mapping from packages to
         versions), we try to get the packages using the given name. *)
      IF self = NIL OR self.packageList = NIL THEN
        RAISE Error("undefined project");
      END;
      FOR i := 0 TO self.packageList.size() - 1 DO
        pkg := self.packageList.get(i);
        GetPackage(pkg, name);
      END;
    ELSE
      iter := tab.iterate();
      WHILE iter.next(pkg, tag) DO
        GetPackage(pkg, tag);
      END;
    END;
  END Checkout;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckoutSnapshot(self : T; name : TEXT) RAISES {Error} =
  VAR
    snap := self.snapshot(name);
  BEGIN
    Checkout(self, name, snap);
  END CheckoutSnapshot;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckoutRelease(self : T; name : TEXT) RAISES {Error} =
  VAR
    release := self.release(name);
  BEGIN
    Checkout(self, name, release);
  END CheckoutRelease;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckoutHead(self : T) RAISES {Error} =
  BEGIN
    Checkout(self, "", MakeHead(self), heedModifiedPackages := FALSE);
  END CheckoutHead;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckoutTrunkOrBranchHead(self : T; pkgs : TextSeq.T)
  RAISES {Error} =
  VAR
    head    := NEW(TextTextTbl.Default).init(self.packageList.size());
    tagText :  TEXT;
    fn      :  TEXT;
    tag     := NEW(Tag.T);
    isrel   :  BOOLEAN;
  BEGIN
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "--- inspecting package " & pkg & " ---");

        END;
        TRY
	  IF self.poolset.execAction(pkg, "isrelease", tagText,
				     self.externalShell,
                                     self.varTbl) = 0 THEN
	    EVAL self.poolset.execAction(pkg, "currentreleasetag", tagText,
					 self.externalShell,
                                         self.varTbl);
	    fn := "PkgCRT";
	    isrel := TRUE;
	  ELSIF self.poolset.execAction(pkg, "currentdeveltag", tagText,
					self.externalShell,
                                        self.varTbl) = 0 THEN
	    fn := "PkgCDT";
	    isrel := FALSE;
	  ELSE
	    RAISE Error("cannot get tag for package " & pkg);
	  END;
          IF tagText = NIL THEN
            tagText := TextUtils.Compress(self.poolset.fileContents(pkg, fn));
            IF Text.Equal(fn, "PkgCRT") THEN
              self.poolset.setVal(pkg, "current-release-tag", tagText);
            ELSE
              self.poolset.setVal(pkg, "current-devel-tag", tagText);
            END;
          END;
        EXCEPT
          PoolSet.Error(e) => RAISE Error(e);
        END;
        EVAL tag.initFromText(tagText);
        IF NOT tag.okay() THEN
          RAISE Error("tag `" & tagText & "' for package " & pkg &
                " seems to be incorrect");
        END;
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "    current tag is " & tagText);
        END;
        IF isrel THEN
          tag := Tag.NewStableBranch(tag);
          tagText := tag.denotation();
        ELSE
          tagText := "head";
        END;
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "    tip of branch tag is " & tagText);
        END;
        EVAL head.put(pkg, tagText);
      END;
    END;
    Checkout(self, "", head, FALSE);
  END CheckoutTrunkOrBranchHead;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckoutPackages(self : T; pkgs : TextSeq.T; tag := "head")
  RAISES {Error} =
  VAR
    head := NEW(TextTextTbl.Default).init(self.packageList.size());
    snap : TextTextTbl.T := NIL; 
  BEGIN
    IF NOT Text.Equal(tag, "head") THEN
      snap := self.release(tag);
      IF snap = NIL THEN
        snap := self.snapshot(tag);
      END;
    END;
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        IF snap = NIL THEN
          EVAL head.put(pkg, tag);
        ELSE
          VAR
            t : TEXT;
          BEGIN
            EVAL snap.get(pkg, t);
            IF t = NIL THEN
              t := tag;
            END;
            EVAL head.put(pkg, t);
          END;
        END;
      END;
    END;
    Checkout(self, "", head, FALSE);
  END CheckoutPackages; 

(*---------------------------------------------------------------------------*)
PROCEDURE BuildDepGraph(
    self : T; 
    confirmationCl : Confirmation.Closure := NIL) RAISES {Error} =
  VAR
    pkg  :  TEXT;
    loc  :  TEXT;
    res  :  TEXT;
    path :  Pathname.T;
    errs := "";
    okay := TRUE;
    par  :  TextTextTbl.T;
    locs := self.locations();
    dummy: TEXT;
    depFileExists : BOOLEAN;
    thisPkgDepsOkay : BOOLEAN;
  BEGIN
    MsgX.T(self.msgif, "building dependency graph...");
    IF self.missingList # NIL AND self.missingList.size() > 0 THEN
      MsgX.T(self.msgif, "checking out missing packages first...");
      WITH msg = "The following packages are missing in your workspace:" & NL &
           TextUtils.TextSeqToText(self.missingList, NL) & NL &
           "Shall I check them out now" DO
        IF confirmationCl # NIL THEN
          IF confirmationCl.okay(msg) THEN
            self.checkoutPackages(self.missingList);
          ELSE
            RAISE Error("cannot build dependency graph without all packages");
          END;
        END;
      END;
    END;
    self.depGraph := DependencyGraph.New(NIL);
    FOR i := 0 TO self.packageList.size() - 1 DO
      pkg := self.packageList.get(i);
      self.depGraph.addElem(pkg, action := NIL, phony := TRUE);
    END;
    IF self.locationTbl.size() < self.packageList.size() THEN
      IF NOT self.poolset.checkAll(self.packageList, res, self.missingList,
                                   self.locationTbl, 
                                   checkHomogeneity := FALSE,
                                   ignoreMissingPackages := FALSE) THEN
        RAISE Error(res);
      END;
      FOR i := 0 TO self.packageList.size() - 1 DO
        pkg := self.packageList.get(i);
        EVAL self.locationTbl.put(
                      pkg, 
                      Pathname.Prefix(self.poolset.pkgPath(pkg)));
      END;
    END;
    TRY
      par := self.varTableCopy();
      IF self.locsText = NIL THEN
        self.locsText := TextUtils.TextSeqToText(locs);
      END;
      EVAL par.put("LOCATIONS", self.locsText);
      FOR i := 0 TO self.packageList.size() - 1 DO
        pkg := self.packageList.get(i);
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "--- package " & pkg & ": mkdep ---");
        END;
        EVAL par.put("PKG", pkg);
        EVAL self.locationTbl.get(pkg, loc);
        EVAL par.put("LOCATION", loc);
        IF NOT self.poolset.exists(pkg, loc) THEN
          RAISE Error("package " & pkg & " has disappeared from location " &
                loc);
        END;
        thisPkgDepsOkay := TRUE;
        path := Pathname.Join(self.poolset.pkgPath(pkg), "PkgDep", NIL);
        IF self.poolset.execAction(pkg, "mkdep", dummy,
                                   self.externalShell, par) # 0 THEN
          errs := errs & "building dependencies for package " & pkg &
                      " at " & path & " failed" & NL;
          thisPkgDepsOkay := FALSE;
          IF self.depsMandatory THEN
            okay := FALSE;
          END;
        END;
        (* dependencies should now be listed in collection/pkg/PkgDep *)
        depFileExists := FSUtils.Exists(path);
        IF depFileExists THEN
          IF NOT self.depGraph.addFromDependFile(path) THEN
            errs := errs & "adding dependencies for package " & pkg &
                        " at " & path & " failed" & NL;
            thisPkgDepsOkay := FALSE;
            IF self.depsMandatory THEN
              okay := FALSE;
            END;
          END;
        ELSE
          thisPkgDepsOkay := FALSE;
          IF self.depsMandatory THEN
            okay := FALSE;
          END;
        END;
        IF NOT self.depsMandatory AND NOT thisPkgDepsOkay THEN
          MsgX.Warning(self.msgif, "no dependecies for package " & pkg);
        END;
      END;
    EXCEPT
      PoolSet.Error(e) => RAISE Error(e);
    END;
    IF self.depsMandatory AND NOT okay THEN
      RAISE Error(errs);
    END;
    IF self.useCache THEN
      EvaluateStateCacheChangesForDependendPackages(self);
    END;
  END BuildDepGraph;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadDepGraph(self : T; fn : TEXT) RAISES {Error} =
  BEGIN
    IF self.depGraph = NIL THEN
      self.depGraph := DependencyGraph.New(NIL);
    END;
    IF NOT self.depGraph.load(fn) THEN
      RAISE Error("reading dependency graph from file " & fn & "failed");
    END;
    IF self.useCache THEN
      EvaluateStateCacheChangesForDependendPackages(self);
    END;
  END ReadDepGraph;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteDepGraph(self : T; fn : TEXT) RAISES {Error} =
  BEGIN
    IF NOT self.depGraph.save(fn) THEN
      RAISE Error("writing dependency graph to file " & fn & "failed");
    END;
  END WriteDepGraph;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadDepGraphAsText(self : T; fn : TEXT) RAISES {Error} =
  BEGIN
    IF self.depGraph = NIL THEN
      self.depGraph := DependencyGraph.New(NIL);
    END;
    IF NOT self.depGraph.loadAsText(fn) THEN
      RAISE Error("reading dependency graph from file " & fn & "failed");
    END;
    IF self.useCache THEN
      EvaluateStateCacheChangesForDependendPackages(self);
    END;
  END ReadDepGraphAsText;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteDepGraphAsText(self : T; fn : TEXT) RAISES {Error} =
  BEGIN
    IF NOT self.depGraph.saveAsText(fn) THEN
      RAISE Error("writing dependency graph to file " & fn & "failed");
    END;
  END WriteDepGraphAsText;

(*---------------------------------------------------------------------------*)
PROCEDURE PutPkgVersionBlock(wr : Wr.T; tbl : TextTextTbl.T) 
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    iter := tbl.iterate();
    name, version : TEXT;
  BEGIN
    WHILE iter.next(name, version) DO
      Wr.PutText(wr, "  " & name & " " & version & NL);
    END;
    Wr.PutText(wr, "end" & NL);
  END PutPkgVersionBlock;

(*---------------------------------------------------------------------------*)
PROCEDURE ConvertToText(self : T; snapshotName : TEXT := NIL;
                        releaseName  : TEXT := NIL) : TEXT RAISES {Error} =
  VAR
    wr  := TextWr.New();
    res :  TEXT;
  BEGIN
    TRY
      WriteInternalWr(self, wr, snapshotName, releaseName);
    FINALLY
      TRY
        res := TextWr.ToText(wr);
        Wr.Close(wr);
      EXCEPT ELSE
        RAISE Error("closing text writer failed");
      END;
    END;
    RETURN res;
  END ConvertToText;

(*---------------------------------------------------------------------------*)
PROCEDURE SnapshotText(self : T; snapshotName : TEXT) : TEXT RAISES {Error} =
  BEGIN
    RETURN ConvertToText(self, snapshotName, NIL);
  END SnapshotText;

(*---------------------------------------------------------------------------*)
PROCEDURE ReleaseText(self : T; snapshotName : TEXT) : TEXT RAISES {Error} =
  BEGIN
    RETURN ConvertToText(self, NIL, snapshotName);
  END ReleaseText;

(*---------------------------------------------------------------------------*)
PROCEDURE ToText(self : T) : TEXT RAISES {Error} =
  BEGIN
    RETURN ConvertToText(self, "", "");
  END ToText;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteInternal(self : T; fn : TEXT;
                        snapshotName : TEXT := NIL; (* "" <=> all *)
                        releaseName : TEXT := NIL)  (* "" <=> all *)
  RAISES {Error} =
  VAR wr : Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open(fn);
    EXCEPT ELSE
      RAISE Error("opening file " & fn & " failed");
    END;
    TRY
      WriteInternalWr(self, wr, snapshotName, releaseName);
    FINALLY
      TRY
        Wr.Close(wr);
      EXCEPT ELSE
        RAISE Error("closing file " & fn & " failed");
      END;
    END;
  END WriteInternal;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteInternalWr(self : T; wr : Wr.T;
                          snapshotName : TEXT := NIL; (* "" <=> all *)
                          releaseName : TEXT := NIL)  (* "" <=> all *)
  RAISES {Error} =
  VAR
    name : TEXT;
    loc  : TEXT;
    tbl  : TextTextTbl.T;
    nameDone := FALSE;
  BEGIN 
    TRY
      Wr.PutText(wr, "collectionroot " & self.collectionRootOrig & NL);
      FOR i := 0 TO self.collectionList.size() - 1 DO
        name := self.collectionList.get(i);
        EVAL self.collectionTbl.get(name, loc);
        Wr.PutText(wr, "collection " & name & "	at " & loc & NL);
      END;
      Wr.PutText(wr, NL);
      (* collections written *)

      FOR i := 0 TO self.packageList.size() - 1 DO
        name := self.packageList.get(i);
        EVAL self.packageTbl.get(name, loc); 
        Wr.PutText(wr, "package " & name & "	in " & loc & NL);
      END;
      Wr.PutText(wr, NL);
      (* packages written *)

      IF snapshotName # NIL AND Text.Empty(snapshotName) THEN
        VAR
          iter := self.snapshotTbl.iterate();
	BEGIN
	  WHILE iter.next(name, tbl) DO
	    Wr.PutText(wr, "snapshot " & name & NL);
	    PutPkgVersionBlock(wr, tbl);
	  END;
	END;
        Wr.PutText(wr, "name \"" & self.name & "\"" & NL);
        nameDone := TRUE;
      ELSIF snapshotName # NIL THEN
        IF self.snapshotTbl.get(snapshotName, tbl) THEN
          Wr.PutText(wr, "snapshot " & snapshotName & NL);
          PutPkgVersionBlock(wr, tbl);
        ELSE
          RAISE Error("snapshot " & snapshotName & " not found");
        END;
      END;
      Wr.PutText(wr, NL);
      (* snapshots written *)

      IF releaseName # NIL AND Text.Empty(releaseName)THEN
	VAR
	  iter := self.releaseTbl.iterate();
	BEGIN
	  WHILE iter.next(name, tbl) DO
	    Wr.PutText(wr, "release " & name & NL);
	    PutPkgVersionBlock(wr, tbl);
	  END;
	END;
        IF NOT nameDone THEN
          Wr.PutText(wr, "name \"" & self.name & "\"" & NL);
        END;
      ELSIF releaseName # NIL THEN
        IF self.releaseTbl.get(releaseName, tbl) THEN
          Wr.PutText(wr, "release " & releaseName & NL);
          PutPkgVersionBlock(wr, tbl);
        ELSE
          RAISE Error("release " & releaseName & " not found");
        END;
      END;
      Wr.PutText(wr, NL);
      (* releases written *)

      Wr.PutText(wr, "creator \"" & self.user & "\"" & NL);
      IF self.cdate # 0.0d0 THEN
        Wr.PutText(wr, "ctime \"" & RCS_Date.FromTime(self.cdate) & "\"" & NL);
      END;
      IF self.mdate # 0.0d0 THEN
        Wr.PutText(wr, "mtime \"" & RCS_Date.FromTime(self.mdate) & "\"" & NL);
      END;
      Wr.PutText(wr, "description" & NL);
      Wr.PutText(wr, self.desc);
      Wr.PutText(wr, NL & "end" & NL);
    EXCEPT
      Wr.Failure => RAISE Error("writing project description failed");
    | Thread.Alerted => RAISE Error("interrupted writing project description");
    END;
  END WriteInternalWr;

(*---------------------------------------------------------------------------*)
PROCEDURE Write(self : T; fn : TEXT) RAISES {Error} =
  BEGIN 
    WriteInternal(self, fn, "", "");
  END Write;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteSnapshot(self : T; fn : TEXT; name : TEXT) RAISES {Error} =
  BEGIN
    WriteInternal(self, fn, name, NIL);
  END WriteSnapshot;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteRelease(self : T; fn : TEXT; name : TEXT) RAISES {Error} =
  BEGIN
    WriteInternal(self, fn, NIL, name);
  END WriteRelease;

(*---------------------------------------------------------------------------*)
PROCEDURE PackageUpdateSequence(self : T) : TextSeq.T =
  BEGIN
    IF self.depGraph = NIL THEN
      TRY
        BuildDepGraph(self);
      EXCEPT
        Error(e) =>
        MsgX.Fatal(self.msgif, "cannot build package dependency graph: " & e);
      END;
    END;
    VAR
      nodes := self.depGraph.topologicalSort();
      res   := NEW(TextSeq.T).init(self.packageList.size());
    BEGIN
      FOR i := 0 TO nodes.size() - 1 DO
        WITH act = nodes.get(i) DO
          IF MemberOfTextSeq(self.packageList, act.name()) THEN
            res.addhi(act.name());
          END;
        END;
      END;
      RETURN res;
    END;
  END PackageUpdateSequence;

(*---------------------------------------------------------------------------*)
PROCEDURE IgnoredPackages(self : T) : TextSeq.T =
  BEGIN
    IF self.ignoredList # NIL THEN
      RETURN self.ignoredList;
    END;
    VAR
      graphNodes := self.depGraph.nodes();
    BEGIN
      self.ignoredList := NEW(TextSeq.T).init(graphNodes.size());
      FOR i := 0 TO graphNodes.size() - 1 DO
        WITH act = graphNodes.get(i) DO
          IF NOT MemberOfTextSeq(self.packageList, act.name()) THEN
            self.ignoredList.addhi(act.name());
          END;
        END;
      END;
    END;
    RETURN self.ignoredList;
  END IgnoredPackages;

(*---------------------------------------------------------------------------*)
PROCEDURE MissingPackages(self : T) : TextSeq.T =
  BEGIN
    RETURN self.missingList;
  END MissingPackages;

(*---------------------------------------------------------------------------*)
PROCEDURE ApplyToPackages(self : T; 
                          action  : PkgBase.Action; 
                          action2 : PkgBase.Action := NIL; 
                          action3 : PkgBase.Action := NIL; 
                          cpkgs   : TextSeq.T := NIL;
                          ordered := TRUE;
                          breakOnZeroReturn := FALSE;
                          breakOnError := TRUE; 
                          breakOnFailure := TRUE;
                          tag1Values : TextTextTbl.T := NIL;
                          tag2Values : TextTextTbl.T := NIL) : INTEGER 
  RAISES {Error} =

  PROCEDURE CheckStateCacheIfBuild(pkg, action : TEXT) RAISES {Error} =
    VAR 
      loc, dir : TEXT;
      ret : INTEGER;
      res : TEXT;
    BEGIN
      IF self.useCache AND TextUtils.Contains(action, "build") THEN
        (* If we execute a build action, we need to invalidate the
           attributes that indicate `built or shipped' for all
           packages that depend on the newly built package if the
           build action is really executed. *)
        IF NOT self.locationTbl.get(pkg, loc) THEN
          RAISE Error("no location for package " & pkg);
        END;
        dir := Pathname.Join(loc, pkg, NIL);
        TRY
          IF NOT self.poolset.actionProbablyNeeded(pkg, dir, action, 
                                                   ret, res) THEN
            RETURN;
          END;
        EXCEPT
          PoolSet.Error(e) => RAISE Error(e);
        END;
        WITH deps = NEW(TextSeq.T).init() DO
          deps.addhi(pkg);
          FOR i := 0 TO deps.size() - 1 DO
            WITH p = deps.get(i) DO
              IF NOT self.locationTbl.get(p, loc) THEN
                RAISE Error("no location for package " & p);
              END;
              dir := Pathname.Join(loc, p, NIL);
              IF NOT Text.Equal(p, pkg) THEN
                TRY
                  self.poolset.updateStateCache(dir, "need-build-ship", 0);
                EXCEPT
                  PoolSet.Error(e) => RAISE Error(e);
                END;
              END;
            END;
          END;
        END;
      END;
    END CheckStateCacheIfBuild;

  PROCEDURE CheckAndDisplayResult(res : INTEGER; rtext : TEXT) =
    BEGIN
      IF rtext # NIL THEN
        IF NOT Text.Empty(rtext) THEN
          MsgX.T(self.msgif, rtext);
        END;
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "exit code was " & Fmt.Int(res));
        END;
      END;
    END CheckAndDisplayResult;

  VAR
    loc  :  TEXT;
    upds :  TextSeq.T;
    pkgs :  TextSeq.T;
    res  :  INTEGER;
    locs := self.locations();
    par  := self.varTableCopy();
    rtext:  TEXT;
  BEGIN
    IF ordered THEN
      IF cpkgs = NIL THEN
        pkgs := self.packageUpdateSequence();
      ELSE
        upds := self.packageUpdateSequence();
        pkgs := NEW(TextSeq.T).init(cpkgs.size());
        FOR i := 0 TO upds.size() - 1 DO
          WITH pkg = upds.get(i) DO
            IF MemberOfTextSeq(cpkgs, pkg) THEN
              pkgs.addhi(pkg);
            END;
          END;
        END;
      END;
    ELSE
      IF cpkgs = NIL THEN
        pkgs := self.packageList;
      ELSE
        pkgs := cpkgs;
      END;
    END;
    IF self.locsText = NIL THEN
      self.locsText := TextUtils.TextSeqToText(locs);
    END;
    EVAL par.put("LOCATIONS", self.locsText );
    res := 0;
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        IF Msg.tFlag THEN
          MsgX.T(self.msgif, "--- package " & pkg & " ---");
        END;
        EVAL par.put("PKG", pkg);
        EVAL self.locationTbl.get(pkg, loc);
        EVAL par.put("LOCATION", loc);
        VAR val : TEXT; BEGIN
          IF tag1Values # NIL AND tag1Values.get(pkg, val) AND val # NIL THEN
            EVAL par.put("TAG1", val);
          END;
          IF tag2Values # NIL AND tag2Values.get(pkg, val) AND val # NIL THEN
            EVAL par.put("TAG2", val);
          END;
        END;
        TRY
          CheckStateCacheIfBuild(pkg, action);
          IF Msg.vFlag THEN
            MsgX.V(self.msgif, "--- package " & pkg & ": applying action 1: " &
              action & " ---", level := 2);
          END;
          res := ExecutionFailure;
          rtext := NIL;
          res := self.poolset.execAction(pkg, action, rtext, 
                                         self.externalShell, par);
          CheckAndDisplayResult(res, rtext);
          IF action2 # NIL THEN
            IF NOT breakOnError OR (res = 0 AND NOT breakOnZeroReturn) OR
              (res # 0 AND breakOnZeroReturn) THEN
              CheckStateCacheIfBuild(pkg, action2);
              IF Msg.vFlag THEN
                MsgX.V(self.msgif, "--- package " & pkg & 
                  ": applying action 2: " & action2 & " ---", level := 2);
              END;
              res := ExecutionFailure;
              rtext := NIL;
              res := self.poolset.execAction(pkg, action2, rtext,
                                             self.externalShell, par);
              CheckAndDisplayResult(res, rtext);
            END;
          END;
          IF action3 # NIL THEN
            IF NOT breakOnError OR (res = 0 AND NOT breakOnZeroReturn) OR
              (res # 0 AND breakOnZeroReturn) THEN
              CheckStateCacheIfBuild(pkg, action3);
              IF Msg.vFlag THEN
                MsgX.V(self.msgif, "--- package " & pkg & 
                  ": applying action 3: " & action3 & " ---", level := 2);
              END;
              rtext := NIL;
              res := ExecutionFailure;
              res := self.poolset.execAction(pkg, action3, rtext,
                                             self.externalShell, par);
              CheckAndDisplayResult(res, rtext);
            END;
          END;
          IF breakOnError THEN
            IF breakOnZeroReturn THEN
              IF res = 0 THEN
                EXIT;
              END;
            ELSE
              IF res # 0 THEN
                EXIT;
              END;
            END;
          END;
        EXCEPT
          PoolSet.Error(e) =>
          IF breakOnFailure THEN
            RAISE Error(e);
          ELSE
            (* silently skip exception *)
            MsgX.V(self.msgif, "caught and ignored exception");
          END;
        END;
      END;
    END;
    RETURN res;
  END ApplyToPackages;

(*---------------------------------------------------------------------------*)
PROCEDURE ApplyCmdListDirectly(self    : T;
                               cmd     : TEXT;
                               cpkgs   : TextSeq.T := NIL;
                               ordered := TRUE;
                               breakOnZeroReturn := FALSE;
                               breakOnError := TRUE; 
                               breakOnFailure := TRUE) : INTEGER 
  RAISES {Error} =
  VAR
    upds : TextSeq.T;
    pkgs : TextSeq.T;
    res  : INTEGER;
  BEGIN
    IF ordered THEN
      IF cpkgs = NIL THEN
        pkgs := self.packageUpdateSequence();
      ELSE
        upds := self.packageUpdateSequence();
        pkgs := NEW(TextSeq.T).init(cpkgs.size());
        FOR i := 0 TO upds.size() - 1 DO
          WITH pkg = upds.get(i) DO
            IF MemberOfTextSeq(cpkgs, pkg) THEN
              pkgs.addhi(pkg);
            END;
          END;
        END;
      END;
    ELSE
      IF cpkgs = NIL THEN
        pkgs := self.packageList;
      ELSE
        pkgs := cpkgs;
      END;
    END;
    res := 0;
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        IF Msg.tFlag THEN
          MsgX.T(self.msgif, "--- package " & pkg & " ---");
        END;
        TRY
          IF Msg.vFlag THEN
            MsgX.V(self.msgif, "--- package " & pkg & ": applying command: " &
              cmd & " ---", level := 2);
          END;
          res := ExecutionFailure;
          res := self.poolset.execCmdList(pkg, cmd, self.externalShell);
          IF breakOnError THEN
            IF breakOnZeroReturn THEN
              IF res = 0 THEN
                EXIT;
              END;
            ELSE
              IF res # 0 THEN
                EXIT;
              END;
            END;
          END;
        EXCEPT
          PoolSet.Error(e) =>
          IF breakOnFailure THEN
            RAISE Error(e);
          ELSE
            (* silently skip exception *)
            MsgX.V(self.msgif, "caught and ignored exception");
          END;
        END;
      END;
    END;
    RETURN res;
  END ApplyCmdListDirectly;

(*---------------------------------------------------------------------------*)
PROCEDURE SelectPackages(self : T; 
                         pred : PkgBase.Action; 
                         ordered := TRUE;
                         selectOnZeroReturn := TRUE;
                         breakOnFailure := TRUE;
                         tag1Values : TextTextTbl.T := NIL;
                         tag2Values : TextTextTbl.T := NIL) : TextSeq.T
  RAISES {Error} =
  VAR
    loc  :  TEXT;
    pkgs :  TextSeq.T;
    res  := NEW(TextSeq.T).init();
    ret  := 0;
    locs := self.locations();
    par  := self.varTableCopy();
    done :  BOOLEAN;
    dummy:  TEXT;
  BEGIN
    IF ordered THEN
      pkgs := self.packageUpdateSequence();
    ELSE
      pkgs := self.packageList;
    END;
    IF self.locsText = NIL THEN
      self.locsText := TextUtils.TextSeqToText(locs);
    END;
    EVAL par.put("LOCATIONS", self.locsText);
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        done := FALSE;
        IF NOT done THEN
          IF Msg.vFlag THEN
            MsgX.V(self.msgif, "--- package " & pkg & ": selecting by " & 
              pred & " ---");
          END;
          EVAL par.put("PKG", pkg);
          EVAL self.locationTbl.get(pkg, loc);
          EVAL par.put("LOCATION", loc);
          VAR val : TEXT; BEGIN
            IF tag1Values # NIL AND tag1Values.get(pkg, val) AND val # NIL THEN
              EVAL par.put("TAG1", val);
            END;
            IF tag2Values # NIL AND tag2Values.get(pkg, val) AND val # NIL THEN
              EVAL par.put("TAG2", val);
            END;
          END;
          TRY
            ret := ExecutionFailure;
            ret := self.poolset.execAction(pkg, pred, dummy, 
                                           self.externalShell, par);
            done := TRUE;
          EXCEPT
            PoolSet.Error(e) =>
            IF breakOnFailure THEN
              RAISE Error(e);
            ELSE
              (* silently skip exception *)
              MsgX.V(self.msgif, 
                     "caught and ignored exception testing package " & pkg);
            END;
          END;
        END;
        IF done THEN
          IF selectOnZeroReturn THEN
            IF ret = 0 THEN
              res.addhi(pkg);
            END;
          ELSE
            IF ret # 0 THEN
              res.addhi(pkg);
            END;
          END;
        END;
      END;
    END;
    RETURN res;
  END SelectPackages;

(*---------------------------------------------------------------------------*)
PROCEDURE SelectByCmdList(self : T;
                          cmd  : TEXT;
                          ordered := TRUE;
                          selectOnZeroReturn := TRUE;
                          breakOnFailure := TRUE) : TextSeq.T RAISES {Error} =
  VAR
    pkgs : TextSeq.T;
    res  := NEW(TextSeq.T).init();
    ret  := 0;
  BEGIN
    IF ordered THEN
      pkgs := self.packageUpdateSequence();
    ELSE
      pkgs := self.packageList;
    END;
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        IF Msg.vFlag THEN
          MsgX.V(self.msgif, "--- package " & pkg & ": selecting by `" & 
            cmd & "' ---");
        END;
        TRY
          ret := ExecutionFailure;
          ret := self.poolset.execCmdList(pkg, cmd, self.externalShell);
          IF selectOnZeroReturn THEN
            IF ret = 0 THEN
              res.addhi(pkg);
            END;
          ELSE
            IF ret # 0 THEN
              res.addhi(pkg);
            END;
          END;
        EXCEPT
          PoolSet.Error(e) =>
          IF breakOnFailure THEN
            RAISE Error(e);
          ELSE
            (* silently skip exception *)
            MsgX.V(self.msgif, "caught and ignored exception");
          END;
        END;
      END;
    END;
    RETURN res;
  END SelectByCmdList;

(*---------------------------------------------------------------------------*)
PROCEDURE DependendPackages(self : T; pkg : TEXT) : TextSeq.T RAISES {} =
  VAR
    dep  : StdDepGraphNodeSeq.T;
    res  : TextSeq.T := NEW(TextSeq.T).init();
  BEGIN
    dep := self.depGraph.dependingNodes(pkg);
    FOR j := 0 TO dep.size() - 1 DO
      WITH dpkg = dep.get(j).name() DO
        res.addhi(dpkg);
      END;
    END;
    RETURN res;
  END DependendPackages;

(*---------------------------------------------------------------------------*)
PROCEDURE PackageDependencies(self : T; pkg : TEXT) : TextSeq.T RAISES {} =
  VAR
    dep  : StdDepGraphNodeSeq.T;
    res  : TextSeq.T := NEW(TextSeq.T).init();
  BEGIN
    dep := self.depGraph.nodeDependencies(pkg);
    FOR j := 0 TO dep.size() - 1 DO
      WITH dpkg = dep.get(j).name() DO
        res.addhi(dpkg);
      END;
    END;
    RETURN res;
  END PackageDependencies;

(*---------------------------------------------------------------------------*)
PROCEDURE ModifiedPackages(self : T) : TextSeq.T RAISES {Error} =
  VAR
    res : TextSeq.T;
  BEGIN
    res := self.selectPackages("modified", ordered := FALSE,
                               selectOnZeroReturn := TRUE,
                               breakOnFailure := TRUE);
    RETURN res;
  END ModifiedPackages;

(*---------------------------------------------------------------------------*)
PROCEDURE OutOfDatePackages(self : T) : TextSeq.T RAISES {Error} =
  VAR
    res : TextSeq.T;
  BEGIN
    res := self.selectPackages("uptodate", ordered := FALSE,
                               selectOnZeroReturn := FALSE,
                               breakOnFailure := TRUE);
    RETURN res;
  END OutOfDatePackages; 

(*---------------------------------------------------------------------------*)
PROCEDURE UpToDatePackages(self : T) : TextSeq.T RAISES {Error} =
  VAR
    res : TextSeq.T;
  BEGIN
    res := self.selectPackages("uptodate", ordered := FALSE,
                               selectOnZeroReturn := TRUE,
                               breakOnFailure := TRUE);
    RETURN res;
  END UpToDatePackages; 

(*---------------------------------------------------------------------------*)
PROCEDURE AddDependingPackages(self : T; pkgs : TextSeq.T) : TextSeq.T =
  VAR
    dep  : StdDepGraphNodeSeq.T;
    depp : TextSeq.T := NEW(TextSeq.T).init();
    res  : TextSeq.T := NEW(TextSeq.T).init();
    uds  : TextSeq.T;
  BEGIN
    IF self.depGraph = NIL THEN
      TRY
        BuildDepGraph(self);
      EXCEPT
        Error(e) =>
        MsgX.Fatal(self.msgif, "cannot build package dependency graph: " & e);
      END;
    END;
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH mpkg = pkgs.get(i) DO
        depp.addhi(mpkg);
        dep := self.depGraph.dependingNodes(mpkg);
        FOR j := 0 TO dep.size() - 1 DO
          WITH dpkg = dep.get(j).name() DO
            depp.addhi(dpkg);
          END;
        END;
      END;
    END;
    (* depp contains all elements of pkgs and dependend packages, but probably
       a number of times and in wrong order *)
    uds := self.packageUpdateSequence();
    (* uds contains the correct order of updates for packages *)
    FOR i := 0 TO uds.size() - 1 DO
      WITH pkg = uds.get(i) DO
        IF MemberOfTextSeq(depp, pkg) THEN
          res.addhi(pkg);
        END;
      END;
    END;
    (* all the packages in depp have been added to res in the order of
       their occurence in uds *)
    RETURN res;
  END AddDependingPackages; 

(*---------------------------------------------------------------------------*)
PROCEDURE ModifiedAndDependingPackages(self : T) : TextSeq.T RAISES {Error} =
  VAR
    mod  : TextSeq.T;
    res  : TextSeq.T;
  BEGIN
    mod := self.selectPackages("modified", ordered := FALSE,
                               selectOnZeroReturn := TRUE,
                               breakOnFailure := TRUE);
    (* mod contains all locally modified packages *)
    res := self.addDependingPackages(mod);
    RETURN res;
  END ModifiedAndDependingPackages; 

(*---------------------------------------------------------------------------*)
PROCEDURE OutOfDateAndDependingPackages(self : T) : TextSeq.T RAISES {Error} =
  VAR
    ood  : TextSeq.T;
    res  : TextSeq.T;
  BEGIN
    ood := self.selectPackages("uptodate", ordered := FALSE,
                               selectOnZeroReturn := FALSE,
                               breakOnFailure := TRUE);
    (* mod contains all locally modified packages *)
    res := self.addDependingPackages(ood);
    RETURN res;
  END OutOfDateAndDependingPackages; 

(*---------------------------------------------------------------------------*)
PROCEDURE PackagesWithConflicts(self : T) : TextSeq.T RAISES {Error} =
  VAR
    res : TextSeq.T;
  BEGIN
    res := self.selectPackages("conflicts", ordered := FALSE,
                               selectOnZeroReturn := TRUE,
                               breakOnFailure := TRUE);
    RETURN res;
  END PackagesWithConflicts; 

(*---------------------------------------------------------------------------*)
PROCEDURE TestAllPackagesReleased(self : T) : BOOLEAN =
  BEGIN
    TRY
      RETURN self.applyToPackages("isrelease", ordered := FALSE,
                                  breakOnError := TRUE, 
                                  breakOnFailure := TRUE) = 0;
    EXCEPT
      Error => RETURN FALSE;
    END;
  END TestAllPackagesReleased;

(*---------------------------------------------------------------------------*)
PROCEDURE TestNoPackageModified(self : T) : BOOLEAN =
  VAR sel : TextSeq.T;
  BEGIN
    TRY
      sel := self.selectPackages("modified", ordered := FALSE,
                                 selectOnZeroReturn := TRUE,
                                 breakOnFailure := TRUE);
      (* sel contains all modified packages *)
      RETURN sel.size() = 0;
    EXCEPT
      Error => RETURN FALSE;
    END;
  END TestNoPackageModified; 

(*---------------------------------------------------------------------------*)
PROCEDURE BuildStateLabelTable(self : T; lazy := FALSE) : TextTextTextTbl.T 
  RAISES {Error} =
  VAR
    pkg, fn  :  TEXT;
    tag      :  TEXT;
    label    :  TEXT;
    tabText  :  TEXT;
    tabLines :  TextSeq.T;
    tbl      :  TextTextTbl.T;      (* tag -> label *)
    res      :  TextTextTextTbl.T;  (* pkgname -> ( tag -> label ) *)
    found    :  BOOLEAN;
    dummy    :  TEXT;
  BEGIN
    res := NEW(TextTextTextTbl.Default).init(self.packageList.size());
    TRY
      fn := ".labels";
      FOR i := 0 TO self.packageList.size() - 1 DO
        pkg := self.packageList.get(i);
        IF lazy THEN
          TRY
            tabText := self.poolset.fileContents(pkg, fn);
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "--- using old state labels of package " & 
                pkg & " ---");
            END;
            found := TRUE;
          EXCEPT ELSE
            found := FALSE;
          END;
        END;
        IF NOT lazy OR NOT found THEN
          IF Msg.vFlag THEN
            MsgX.V(self.msgif, "--- reading state labels from package " & 
              pkg & " ---");
          END;
	  IF self.poolset.execAction(pkg, "listlabels", dummy,
				     self.externalShell,
				     self.varTbl) = 0 THEN
	  ELSE
	    RAISE Error("cannot get state labels for package " & pkg);
	  END;
	  tabText := self.poolset.fileContents(pkg, fn);
        END;
        tbl := NEW(TextTextTbl.Default).init();
        tabLines := TextUtils.Split(tabText, "\n");
        FOR j := 0 TO tabLines.size() - 1 DO
          WITH line = tabLines.get(j) DO
            WITH elems = TextUtils.Split(line, "-->") DO
              IF elems.size() = 2 THEN
                tag   := TextUtils.Compress(elems.get(0));
                label := TextUtils.Compress(elems.get(1));
                EVAL tbl.put(tag, label);
              ELSE
                IF NOT Text.Empty(TextUtils.Compress(line)) THEN
                  MsgX.Warning(self.msgif, 
                               "invalid line in state list table ignored: " &
                               line);
                END;
              END;
            END;
          END;
        END;
        EVAL res.put(pkg, tbl);
      END;
    EXCEPT
      Error(e) => RAISE Error(e); 
      | PoolSet.Error(e) => RAISE Error(e);
    END;
    RETURN res;
  END BuildStateLabelTable;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheAllStateLabels(self : T; lazy := FALSE) RAISES {Error} =
  BEGIN
    self.stateLabelTbl := BuildStateLabelTable(self, lazy);
  END CacheAllStateLabels;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckLabelsInternal(self : T; snap : TextTextTbl.T;
                              pattern : TEXT) : TextSeq.T
  RAISES {Error} =
  VAR
    res  := NEW(TextSeq.T).init();
    iter := snap.iterate();
    tbl  :  TextTextTbl.T;
    re   :  RegEx.Pattern;
    pkg, tag, label : TEXT;
  BEGIN
    TRY
      re := RegEx.Compile(pattern);
    EXCEPT
      RegEx.Error(t) => RAISE Error("cannot compile regex " & pattern &
                                    ": " & t);
    END;
    WHILE iter.next(pkg, tag) DO
      IF self.stateLabelTbl.get(pkg, tbl) THEN
        IF tbl.get(tag, label) THEN
          IF RegEx.Execute(re, label) # -1 THEN
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "  found match in package " & pkg & 
                " version " & tag & " label " & label);
            END;
            res.addhi(pkg);
          END;
        ELSE
          MsgX.Error(self.msgif, "cannot get state label for pkg " & pkg &
            " tag " & tag);
        END;
      ELSE
        MsgX.Error(self.msgif, "cannot get state labels for pkg " & pkg);
      END;
    END;
    RETURN res;
  END CheckLabelsInternal;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckCurrentLabelsGen(self : T; pattern : TEXT;
                                useCachedLabels := FALSE) : TextSeq.T 
  RAISES {Error} =
  VAR
    snap := CurrentTagList(self);
  BEGIN
    IF NOT useCachedLabels THEN
      CacheAllStateLabels(self);
    END;
    RETURN CheckLabelsInternal(self, snap, pattern);
  END CheckCurrentLabelsGen;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckCurrentLabels(self : T; pattern : TEXT;
                             lazy := FALSE) : TextSeq.T 
  RAISES {Error} =
  VAR
    pkg, fn  :  TEXT;
    label    :  TEXT;
    res      := NEW(TextSeq.T).init();
    found    :  BOOLEAN;
    re       :  RegEx.Pattern;
  BEGIN
    TRY
      re := RegEx.Compile(pattern);
    EXCEPT
      RegEx.Error(t) => RAISE Error("cannot compile regex " & pattern &
                                    ": " & t);
    END;
    TRY
      fn := ".label";
      FOR i := 0 TO self.packageList.size() - 1 DO
        pkg := self.packageList.get(i);
        IF lazy THEN
          TRY
            label := TextUtils.Compress(self.poolset.fileContents(pkg, fn));
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "--- using old state label of package " & 
                pkg & " ---");
            END;
            found := TRUE;
          EXCEPT ELSE
            found := FALSE;
          END;
        END;
        IF NOT lazy OR NOT found THEN
          IF Msg.vFlag THEN
            MsgX.V(self.msgif, "--- reading state label from package " & 
              pkg & " ---");
          END;
	  IF self.poolset.execAction(pkg, "currentlabel", label,
				     self.externalShell,
				     self.varTbl) = 0 THEN
	  ELSE
	    RAISE Error("cannot get current state label for package " & pkg);
	  END;
          IF label = NIL THEN
            label := TextUtils.Compress(self.poolset.fileContents(pkg, fn));
            self.poolset.setVal(pkg, "current-label", label);
          END;
        END;
        IF RegEx.Execute(re, label) # -1 THEN
          IF Msg.vFlag THEN
            MsgX.V(self.msgif, "  found match in package " & pkg & " label " 
            & label);
          END;
          res.addhi(pkg);
        END;
      END;
    EXCEPT
      Error(e) => RAISE Error(e); 
      | PoolSet.Error(e) => RAISE Error(e);
    END;
    RETURN res;
  END CheckCurrentLabels;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckLabelsOfSnapshot(self : T; name : TEXT; pattern : TEXT;
                                useCachedLabels := FALSE) : TextSeq.T 
  RAISES {Error} =
  VAR
    snap : TextTextTbl.T;
  BEGIN
    IF NOT useCachedLabels THEN
      CacheAllStateLabels(self);
    END;
    snap := Snapshot(self, name);
    IF snap = NIL THEN
      RAISE Error("there is no snapshot " & name);
    END;
    RETURN CheckLabelsInternal(self, snap, pattern);
  END CheckLabelsOfSnapshot;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckLabelsOfRelease(self : T; name : TEXT; pattern : TEXT;
                               useCachedLabels := FALSE) : TextSeq.T
  RAISES {Error} =
  VAR
    snap : TextTextTbl.T;
  BEGIN
    IF NOT useCachedLabels THEN
      CacheAllStateLabels(self);
    END;
    snap := Release(self, name);
    IF snap = NIL THEN
      RAISE Error("there is no release " & name);
    END;
    RETURN CheckLabelsInternal(self, snap, pattern);
  END CheckLabelsOfRelease;

(*---------------------------------------------------------------------------*)
PROCEDURE NewCheckpoint(self : T; fn : TEXT; update := FALSE) RAISES {Error} =
  VAR
    cp : Checkpoint.T;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    TRY
      cp := self.poolset.newCheckpoint(update);
      cp.toFile(fn);
    EXCEPT
      PoolSet.Error(e) => RAISE Error(e);
    | Checkpoint.Error(e) => RAISE Error("NewCheckpoint: " & e);
    END
  END NewCheckpoint;

(*---------------------------------------------------------------------------*)
PROCEDURE LoadNewCheckpoint(self : T; fn : TEXT; 
                            update := FALSE) RAISES {Error} =
  VAR
    cp : Checkpoint.T;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    MsgX.T(self.msgif, "loading new checkpoint from file " & fn);
    self.oldState := self.poolset.cachedState();
    TRY
      cp := Checkpoint.New(self.poolset.getFileCache(), self.msgif);
      cp.fromFile(fn);
      IF update THEN
        IF NOT Msg.vFlag THEN
          MsgX.T(self.msgif, 
                 "scanning all packages (loading new checkpoint)...");
        END;
        cp.update();
      END;
    EXCEPT
      Checkpoint.Error(e) => RAISE Error("error reading checkpoint: " & e);
    END;
    self.poolset.replaceStateCache(cp);
  END LoadNewCheckpoint;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteCheckpoint(self : T; fn : TEXT) RAISES {Error} =
  VAR
    cp : Checkpoint.T;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    MsgX.T(self.msgif, "writing checkpoint to file " & fn);
    TRY
      cp := self.poolset.cachedState();
      cp.toFile(fn);
    EXCEPT
      Checkpoint.Error(e) => RAISE Error("WriteCheckpoint: " & e);
    END
  END WriteCheckpoint;

(*---------------------------------------------------------------------------*)
PROCEDURE InvalidateCachedUnsureVersionInfo(self : T) RAISES {Error} =
  VAR pkg, loc : TEXT;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    MsgX.T(self.msgif, "purging unsure version info...");
    FOR i := 0 TO self.packageList.size() - 1 DO
      pkg := self.packageList.get(i);
      IF NOT self.locationTbl.get(pkg, loc) THEN
        RAISE Error("no location for package " & pkg);
      END;
      WITH dir = Pathname.Join(loc, pkg, NIL) DO
        TRY
          self.poolset.updateStateCache(dir, "clear-utd-nocfl", 0);
          self.poolset.updateStateCache(dir, "clear-mod-unmod", 0);
          self.poolset.updateStateCache(dir, "clear-tags", 0);
        EXCEPT
          PoolSet.Error(e) => RAISE Error(e);
        END;
      END;
    END;
  END InvalidateCachedUnsureVersionInfo;

(*---------------------------------------------------------------------------*)
PROCEDURE InvalidateCachedBuildInfo(self : T) RAISES {Error} =
  VAR pkg, loc : TEXT;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    MsgX.T(self.msgif, "purging build info...");
    FOR i := 0 TO self.packageList.size() - 1 DO
      pkg := self.packageList.get(i);
      IF NOT self.locationTbl.get(pkg, loc) THEN
        RAISE Error("no location for package " & pkg);
      END;
      WITH dir = Pathname.Join(loc, pkg, NIL) DO
        TRY
          self.poolset.updateStateCache(dir, "need-build-ship", 0);
          self.poolset.updateStateCache(dir, "need-mkdep-build-ship", 0);
        EXCEPT
          PoolSet.Error(e) => RAISE Error(e);
        END;
      END;
    END;
  END InvalidateCachedBuildInfo; 

(*---------------------------------------------------------------------------*)
VAR NL := "\n";
BEGIN
  IF MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.WIN32 THEN
    NL := "\r\n";
  END;
END PrjDesc.
