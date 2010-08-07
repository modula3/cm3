(*---------------------------------------------------------------------------*)
MODULE ProjectManager EXPORTS Main;

IMPORT Text, TextRd, TextSeq, TextTextTbl, ParseParams, Rsrc, Time, FileWr,
       Pathname, Env, Process, RefList, Stdio, Wr, Fmt, OSError, Rd, Scan,
       FmtTime;
IMPORT (* FSFixed AS *) FS;
IMPORT SMsg AS Msg, PkgBase, PkgBaseBundle, PrjDesc, ProjectManagerBundle, 
       Snapshots, FSUtils, TextUtils, CompactRC, Copyright,
       Release, PoolSet, SimpleScanner, ScanToken,
       Checkpoint, RsrcUtils, PkgVC, APN AS APN, DirStack,
       Confirmation, Tag, ChangeSet, OSSpecials, PkgVCUtils, Creation,
       TextReadingUtils, Version, SortedTextChangeSetTbl, 
       SortedTimeChangeSetTbl, SortedTextPrjDescTbl, SortedTimePrjDescTbl;
FROM TextUtils IMPORT MemberOfTextSeq;


(*--------------------------------------------------------------------------*)
REVEAL
  SimpleScanner.Token = 
    ScanToken.T BRANDED "ScanToken prjm 0.0" OBJECT
    METHODS
    END;

(*---------------------------------------------------------------------------*)
PROCEDURE M(msg : TEXT) =
  BEGIN
    TRY
      Wr.PutText(Stdio.stdout, msg & "\n");
    EXCEPT ELSE
      Msg.Fatal("cannot write to stdout", 1000);
    END;
  END M;

(*---------------------------------------------------------------------------*)
PROCEDURE OutTextSeq(header : TEXT; seq : TextSeq.T) =
  BEGIN
    IF NOT Msg.tFlag THEN RETURN END;
    M(header);
    IF seq.size() > 0 THEN
      FOR i := 0 TO seq.size() - 1 DO
        M("  " & seq.get(i));
      END;
    ELSE
      M("none");
    END;
    M("");
  END OutTextSeq;

(*---------------------------------------------------------------------------*)
PROCEDURE TOutTextSeq(header : TEXT; seq : TextSeq.T) =
  BEGIN
    IF NOT Msg.tFlag THEN RETURN END;
    Msg.T(header);
    IF seq.size() > 0 THEN
      FOR i := 0 TO seq.size() - 1 DO
        Msg.T("  " & seq.get(i));
      END;
    ELSE
      Msg.T("none");
    END;
    Msg.T("");
  END TOutTextSeq;

(*---------------------------------------------------------------------------*)
PROCEDURE VOutTextSeq(header : TEXT; seq : TextSeq.T) =
  BEGIN
    IF NOT Msg.vFlag THEN RETURN END;
    Msg.V(header);
    IF seq.size() > 0 THEN
      FOR i := 0 TO seq.size() - 1 DO
        Msg.V("  " & seq.get(i));
      END;
    ELSE
      Msg.T("none");
    END;
    Msg.V("");
  END VOutTextSeq;

(*---------------------------------------------------------------------------*)
PROCEDURE OutTextTable(header : TEXT; tbl : TextTextTbl.T) =
  VAR 
    iter : TextTextTbl.Iterator;
    key, val : TEXT;
    empty := TRUE;
  BEGIN
    IF tbl = NIL THEN
      Msg.Error2("OutTextTable", "internal error: tbl is NIL");
      RETURN;
    END;
    M(header);
    iter := tbl.iterate();
    WHILE iter.next(key, val) DO
      empty := FALSE;
      M("  " & key & "	" & val);
    END;
    IF empty THEN
      M("none");
    END;
    M("");
  END OutTextTable;

(*---------------------------------------------------------------------------*)
PROCEDURE TOutTextTable(header : TEXT; tbl : TextTextTbl.T) =
  VAR 
    iter := tbl.iterate();
    key, val : TEXT;
    empty := TRUE;
  BEGIN
    Msg.T(header);
    WHILE iter.next(key, val) DO
      empty := FALSE;
      Msg.T("  " & key & "	" & val);
    END;
    IF empty THEN
      Msg.T("none");
    END;
    Msg.T("");
  END TOutTextTable;

(*---------------------------------------------------------------------------*)
TYPE
  Action = {BuildLocal, BuildProject, BuildGlobal, BuiltOk,
            ShipLocal,  ShipProject,  ShipGlobal,
            Clean, RealClean, Diff, CDiff, UDiff,
            IsRelease, IsModified, ShowModified, IsOutOfDate, ShowOutOfDate,
            HasConflicts, ShowConflicts,
            Checkout, CommitDevel, CommitRelease, CommitLocalFiles,
            NewSnapshot, NewRelease, MakeRelease, StableRelease,
            Apply, OrderedApply, ApplyAction, OrderedApplyAction,
            SelectBy, OrderedSelectBy,
            CheckImports, DependingNodes, 
            CreatePkgOvrFiles,
            ShowPackages, ShowPackageKinds, ShowSrcDirectories,
            ShowUpdateSequence,
            ShowSnapshot, ShowRelease, EditSnapshot, EditRelease,
            ShowPackagePaths, SnapshotLog, ReleaseLog,
            ShowReleases, ShowSnapshots, ShowChangeSets, ChangeSetLog,
            ChangeSetDiff, ChangeSetCDiff, ChangeSetUDiff, 
            EditChangeSet, MergeChangeSet,
            ListKinds, DumpKinds,
            CheckState,
            ShowStateCache, NewStateCache, ShowShortStatus, ShowLongStatus,
            PurgeUnsureVersionInfo, PurgeBuildInfo, Import, Export,
            Undefined
  };

  ActionSet = SET OF Action;

  CommitType = {Major, Minor, Patch};

CONST
  BuildAction     = ActionSet{Action.BuildLocal, Action.BuildProject, 
                              Action.BuildGlobal};

  ShipAction      = ActionSet{Action.ShipLocal,  Action.ShipProject,  
                              Action.ShipGlobal};

  CleanAction     = ActionSet{Action.Clean, Action.RealClean};

  DiffAction      = ActionSet{Action.Diff, Action.CDiff, Action.UDiff,
                              Action.ChangeSetDiff, Action.ChangeSetCDiff,
                              Action.ChangeSetUDiff};

  PredicateAction = ActionSet{Action.IsRelease, Action.IsModified,
                              Action.IsOutOfDate, Action.HasConflicts};

  PkgCommitAction = ActionSet{Action.CommitDevel, Action.CommitRelease};

  PkgFreezeAction = ActionSet{Action.NewSnapshot, Action.NewRelease,
                              Action.MakeRelease};

  ApplyAction     = ActionSet{Action.ApplyAction, Action.OrderedApplyAction};

  ApplyCmdListAction  = ActionSet{Action.Apply, Action.OrderedApply};

  SelectByCmdListAction  = ActionSet{Action.SelectBy, Action.OrderedSelectBy};

  PreParseAction  = ActionSet{Action.BuildLocal, Action.BuildProject, 
                              Action.BuildGlobal, Action.ShipGlobal, 
                              Action.ShipLocal,  Action.ShipProject,
                              Action.Diff, Action.CDiff, Action.UDiff,
                              Action.ChangeSetDiff, Action.ChangeSetCDiff,
                              Action.ChangeSetUDiff, Action.MergeChangeSet,
                              Action.IsRelease, Action.IsModified,
                              Action.IsOutOfDate,
                              Action.HasConflicts, Action.ShowConflicts,
                              Action.ShowModified, Action.ShowOutOfDate, 
                              Action.Clean, Action.RealClean,
                              Action.CommitDevel, Action.CommitRelease,
                              Action.NewSnapshot, Action.NewRelease,
                              Action.MakeRelease, Action.ApplyAction,
                              Action.StableRelease,
                              Action.Apply, Action.OrderedApply,
                              Action.OrderedApplyAction,
                              Action.SelectBy, Action.OrderedSelectBy,
                              Action.DependingNodes, Action.CheckImports,
                              Action.CreatePkgOvrFiles,
                              Action.ShowPackages, Action.ShowPackageKinds,
                              Action.ShowSrcDirectories, 
                              Action.ShowUpdateSequence,
                              Action.ShowSnapshot, Action.ShowRelease, 
                              Action.EditSnapshot, Action.EditRelease,
                              Action.ShowPackagePaths,
                              Action.ShowSnapshots, Action.ShowReleases,
                              Action.ShowChangeSets, Action.ChangeSetLog,
                              Action.SnapshotLog, Action.ReleaseLog,
                              Action.CheckState,
                              Action.ShowStateCache, Action.NewStateCache,
                              Action.ShowShortStatus,
                              Action.ShowLongStatus,
                              Action.PurgeUnsureVersionInfo,
                              Action.PurgeBuildInfo,
                              Action.BuiltOk,
                              Action.Import, Action.Export,
                              Action.Checkout};

  PreCheckAction  = ActionSet{Action.BuildLocal, Action.BuildProject, 
                              Action.BuildGlobal, Action.ShipGlobal, 
                              Action.ShipLocal,  Action.ShipProject,
                              Action.MergeChangeSet,
                              Action.IsRelease, Action.IsModified,
                              Action.IsOutOfDate,
                              Action.HasConflicts, Action.ShowConflicts,
                              Action.ShowModified, Action.ShowOutOfDate, 
                              Action.Clean, Action.RealClean,
                              Action.CreatePkgOvrFiles,
                              Action.CommitDevel, Action.CommitRelease,
                              Action.NewSnapshot, Action.NewRelease,
                              Action.MakeRelease, Action.ApplyAction,
                              Action.Apply, Action.OrderedApply,
                              Action.OrderedApplyAction,
                              Action.SelectBy, Action.OrderedSelectBy,
                              Action.ShowPackages, Action.ShowPackageKinds,
                              Action.ShowSrcDirectories, 
                              Action.ShowUpdateSequence,
                              Action.CheckState,
                              Action.BuiltOk,
                              Action.DependingNodes, Action.CheckImports};

  PreDepAction    = ActionSet{Action.BuildLocal, Action.BuildProject, 
                              Action.BuildGlobal, Action.ShipGlobal, 
                              Action.ShipLocal,  Action.ShipProject,
                              Action.CommitRelease,
                              Action.OrderedSelectBy, Action.OrderedApply, 
                              Action.OrderedApplyAction,
                              Action.ShowUpdateSequence,
                              Action.CreatePkgOvrFiles,
                              Action.DependingNodes, Action.CheckImports};

  NoCacheAction   = ActionSet{Action.StableRelease, Action.ShowSnapshots,
                              Action.ShowReleases, Action.ShowChangeSets,
                              Action.EditChangeSet, Action.ChangeSetLog,
                              Action.SnapshotLog, Action.ReleaseLog,
                              Action.ShowSnapshot, Action.ShowRelease, 
                              Action.EditSnapshot, Action.EditRelease,
                              Action.ShowPackagePaths,
                              Action.Import, Action.Export,
                              Action.ListKinds, Action.DumpKinds};

  NoSnapsAction   = ActionSet{Action.ListKinds, Action.DumpKinds,
                              Action.IsRelease, Action.IsModified,
                              Action.IsOutOfDate,
                              Action.HasConflicts, Action.ShowConflicts,
                              Action.ShowModified, Action.ShowOutOfDate, 
                              Action.BuiltOk,
                              Action.ShowStateCache, Action.NewStateCache,
                              Action.ShowShortStatus,
                              Action.ShowLongStatus,
                              Action.PurgeUnsureVersionInfo,
                              Action.PurgeBuildInfo,
                              Action.ShowPackages, Action.ShowPackageKinds,
                              Action.ShowSrcDirectories, 
                              Action.ShowUpdateSequence,
                              Action.Apply, Action.OrderedApply,
                              Action.OrderedApplyAction,
                              Action.CheckImports,
                              Action.CheckState,
                              Action.SelectBy, Action.OrderedSelectBy}
                    + BuildAction + ShipAction + CleanAction;

(*---------------------------------------------------------------------------*)
CONST
  ActStateFN    = "ActState";
  OldStateFN    = "OldState";
  PrjDescFN     = "PrjDesc";
  PrjMagicFN    = "PrjMagic";
  PrjDepGraphFN = "DepGraph";
  PrjCheckpFN   = ".checkpoint";
  PkgOvrFN      = "PkgOvr";

(*---------------------------------------------------------------------------*)
VAR (* Main *)
  homeDir          := Env.Get("HOME");
  user             := Env.Get("USER");
  rsrcPath         :  Rsrc.Path;
  noAction         :  BOOLEAN;
  nodep            := FALSE;
  depsMandatory    := TRUE;
  nTargets         :  CARDINAL;
  targets          :  TextSeq.T;
  action           := Action.Undefined;
  stopOnErrors     := TRUE;
  stopOnFailures   := TRUE;
  prjFileName      := PrjDescFN;
  actPrjFileName   := PrjDescFN;
  prjMagicFile     := PrjMagicFN;
  prjDepGraphFile  := PrjDepGraphFN;
  cfg              := NEW(PkgBase.T);
  cfgData          :  TEXT;
  prjdesc          :  PrjDesc.T;
  commitType       :  CommitType := CommitType.Patch;
  cmdList          :  TEXT;
  externalShell    :  TEXT := NIL;
  dependendPkgs    := FALSE;
  onlyModified     := FALSE;
  onlyOutOfDate    := FALSE;
  onlyNotReleased  := FALSE;
  modifiedAndDeps  := FALSE;
  outOfDateAndDeps := FALSE;
  force            := FALSE;
  forceRelease     := FALSE;
  useStateCache    := TRUE;
  updateStateCache := FALSE;
  verboseCache     := TRUE;
  cacheEarly       := FALSE;
  modifiedPkgs     :  TextSeq.T := NIL;
  outOfDatePkgs    :  TextSeq.T := NIL;
  snapshotName     :  TEXT;
  newName          :  TEXT := NIL;
  packageKind      :  PkgBase.Kind := NIL;
  defaultPackageKind :  PkgBase.Kind := NIL;
  snaps            :  Snapshots.T;
  env              :  TextTextTbl.T;
  vars             :  TextTextTbl.T;
  stateLabelPattern:  TEXT := NIL;
  collectionroot   :  TEXT := Pathname.Current;
  lazy             :  BOOLEAN;
  inCheckpFileName := PrjCheckpFN;
  outCheckpFileName:= PrjCheckpFN;
  useInternalVC    := TRUE;
  useVC            := TRUE;
  pkgvcCreator     :  PoolSet.PkgVCCreator;
  platform         :  TEXT;
  packageDir       :  TEXT := ".";
  prjName          :  TEXT := "undefined";
  prjRoot          :  TEXT := ".";
  fileName         :  TEXT := NIL;
  name             :  TEXT := NIL;
  commitFile       :  TEXT := NIL;
  commitMsg        :  TEXT := NIL;
  tag1             :  TEXT := NIL;
  tag2             :  TEXT := NIL;
  tag1vals         :  TextTextTbl.T := NIL;
  tag2vals         :  TextTextTbl.T := NIL;
  tag1prjdesc      :  PrjDesc.T := NIL;
  tag2prjdesc      :  PrjDesc.T := NIL;
  changeSetName    :  TEXT := NIL;
  changeSet        :  ChangeSet.T := NIL;
  importRelevanceLevel := 2;
  autoovr          := TRUE;
  savePreReleases  := FALSE;
  sort             := Snapshots.Sort.None;
  sortUp           := TRUE;
  longListing      := FALSE;
  sortByModificationDate := FALSE;

  (* former constants, now configurable*)
  snapshotDir      := "snaps";

(*---------------------------------------------------------------------------*)
PROCEDURE CommitHookDefined(hook : TEXT) : BOOLEAN =
  BEGIN
    RETURN CompactRC.Defined(env, hook) OR 
           CompactRC.Defined(env, "external-commit-hook");
  END CommitHookDefined;

(*---------------------------------------------------------------------------*)
PROCEDURE InitGlobalVars() =

  VAR
    cfgDataRd : TextRd.T;
    hosttype  : TEXT;
    ostype    : TEXT;
  BEGIN
    vars := NEW(TextTextTbl.Default).init();
    env := CompactRC.Eval(ProjectManagerBundle.Get());
    env := CompactRC.Evaluate(env);
    defaultPackageKind := CompactRC.ComputePkgKind(env);
    EVAL env.get("HOME", homeDir); 
    EVAL env.get("USER", user);
    EVAL cfg.init(env);
    hosttype := CompactRC.GetValue(env, "tpc-hosttype");
    IF hosttype = NIL THEN
      hosttype := CompactRC.GetValue(env, "tpc-hosttype-default");
    END;
    IF hosttype = NIL THEN
      hosttype := "unknown";
    END;
    ostype := CompactRC.GetValue(env, "tpc-ostype");
    IF ostype = NIL THEN
      ostype := CompactRC.GetValue(env, "tpc-ostype-default");
    END;
    IF ostype = NIL THEN
      ostype := "unknown";
    END;
    platform := hosttype & "-" & ostype;
    (* use a platform dependend checkpoint file if it exists *)
    IF FSUtils.IsFile(inCheckpFileName & "-" & platform) OR NOT
       FSUtils.IsFile(inCheckpFileName) THEN
      inCheckpFileName := inCheckpFileName & "-" & platform;
      outCheckpFileName := inCheckpFileName;
    END;
    packageKind := Env.Get("PKGKIND");
    IF packageKind = NIL THEN
      IF NOT env.get("pkgkind", packageKind) THEN
        packageKind := NIL;
      END;
    END;
    IF CompactRC.Defined(env, "collectionroot") THEN
      collectionroot := CompactRC.GetValue(env, "collectionroot");
    END;
    IF CompactRC.Defined(env, "internal-vc") THEN
      useInternalVC := 
          TextUtils.BoolVal(CompactRC.GetValue(env, "internal-vc"), TRUE);
    END;
    IF CompactRC.Defined(env, "prjm-internal-vc") THEN
      useInternalVC := 
          TextUtils.BoolVal(CompactRC.GetValue(env, "prjm-internal-vc"), TRUE);
    END;
    IF CompactRC.Defined(env, "enforce-pkgdeps") THEN
      depsMandatory := 
          TextUtils.BoolVal(CompactRC.GetValue(env, "enforce-pkgdeps"), TRUE);
    END;
    IF CompactRC.Defined(env, "cvspath") THEN
      EVAL env.put("cvspath", CompactRC.GetValue(env, "cvspath"));
    END;
    IF CompactRC.Defined(env, "editor") THEN
      EVAL env.put("editor", CompactRC.GetValue(env, "editor"));
    END;
    VAR
      cacheIgnoreDirs : TEXT := NIL;
      cacheIgnoreFiles : TEXT := NIL;
      fingerprintIgnoreDirs : TEXT := NIL;
      fingerprintIgnoreFiles : TEXT := NIL;
    BEGIN
      EVAL env.get("filecache-ignore-dirs", cacheIgnoreDirs);
      EVAL env.get("filecache-ignore-files", cacheIgnoreFiles);
      EVAL env.get("fingerprint-ignore-dirs", fingerprintIgnoreDirs);
      EVAL env.get("fingerprint-ignore-files", fingerprintIgnoreFiles);
      TRY
        Checkpoint.DefineIgnorePatterns(
            cacheIgnoreDirs, cacheIgnoreFiles,
            fingerprintIgnoreDirs, fingerprintIgnoreFiles);
      EXCEPT
        Checkpoint.Error(e) => 
        Msg.Error("wrong pattern in config file: " & e);
      END;
    END;
    IF CompactRC.Defined(env, "configpath") THEN
      WITH configpath = CompactRC.GetValue(env, "configpath") DO
        Msg.V("  using CONFIGPATH from .compactrc or compactrc:\n    " &
          configpath);
        rsrcPath := NIL;
        WITH p = TextUtils.SubstChar(TextUtils.Compress(configpath),
                                     ';', ' ') DO
          (* FIXME: this will break if filenames contain blanks. Use
             TextUtils.Tokenize instead. *)
          WITH trd = TextRd.New(p) DO
            TRY
              WHILE NOT Rd.EOF(trd) DO
                WITH dir = TextReadingUtils.GetToken(trd) DO
                  IF rsrcPath = NIL THEN
                    rsrcPath := RefList.List1(dir);
                  ELSE
                    rsrcPath := RefList.AppendD(rsrcPath, RefList.List1(dir));
                  END;
                END;
              END;
            EXCEPT ELSE END; 
          END;
        END;
      END;
    ELSE
      Msg.V("  using DEFAULT CONFIGPATH");
      rsrcPath := Rsrc.BuildPath(
                      Pathname.Join(homeDir, "compact", NIL),
                      "/usr/contrib/lib/compact",
                      "/usr/local/lib/compact",
                      "/opt/compact");
    END;
    rsrcPath := RefList.AppendD(rsrcPath, 
                                RefList.List1(
                                    ProjectManagerBundle.Get()));
                                    (* PkgBaseBundle.Get())); *)
    (* rsrcPath defined *)
    cfgData := CompactRC.GetRsrcText("PkgBase.DefaultData", 
                                     PkgBaseBundle.Get(), rsrcPath, env);
    cfgDataRd := TextRd.New(cfgData);
    IF NOT cfg.addDefs(cfgDataRd) THEN
      Msg.Fatal("error in PkgBase.DefaultData", 1001);
    END;
    (* initializing local version control backend *)
    TRY
      packageDir := DirStack.GetWorkingDir();
      PkgVC.VC.setPackageRoot(APN.New(packageDir));
      prjName := Pathname.Last(packageDir);
      prjRoot := packageDir;
    EXCEPT 
      DirStack.Error(t) => Msg.Error(t); Process.Exit(2);
    | PkgVC.E(t) => Msg.Error(t); Process.Exit(2); 
    END;
    PkgVC.VC.setEnvironment(env);
  END InitGlobalVars;

(*---------------------------------------------------------------------------*)
PROCEDURE InitSnapshots() = 
  BEGIN
    TRY
      IF NOT FSUtils.IsDir(snapshotDir) THEN
        WITH msg = "The snapshot directory `" & snapshotDir &
             "' does not exist.\n" &
             "Shall I create it now" DO
          IF NOT force AND NOT PkgVC.confirmation.okay(msg) THEN
            IF action IN NoSnapsAction THEN
              Msg.Warning("no snapshot directory - subsequent actions "&
                "may fail");
            ELSE
              Msg.Fatal("cannot continue without snapshot directory");
            END;
          END;
        END;
        IF force THEN
          Msg.T("creating new snapshot directory");
        ELSE
          Msg.V("creating new snapshot directory");
        END;
      END;
      IF useVC THEN
        snaps := NEW(Snapshots.T).init(snapshotDir, cfg, PkgVC.VC);
      ELSE
        snaps := NEW(Snapshots.T).init(snapshotDir, cfg);
      END;
    EXCEPT
      Snapshots.Error(e) => Msg.Fatal("cannot read snapshot directory: " & e);
    END;
  END InitSnapshots;

(*---------------------------------------------------------------------------*)
PROCEDURE Usage() =
  BEGIN
    TRY
      RsrcUtils.PageResource(rsrcPath, "ShortUsageHelp", pageit);
    EXCEPT
      RsrcUtils.Error(e) =>
      TRY Wr.PutText(Stdio.stderr, "error listing help: " & e & "\n") 
      EXCEPT ELSE END;
    END;
    Process.Exit(0);
  END Usage;

VAR pageit := TRUE;

(*---------------------------------------------------------------------------*)
VAR pp := NEW(ParseParams.T).init(Stdio.stderr);

(*---------------------------------------------------------------------------*)
PROCEDURE PreEvalArguments() =
  BEGIN
    TRY
      pageit := NOT pp.keywordPresent("-nopager") AND 
                NOT pp.keywordPresent("-pipe");
      (* release version option *)
      IF pp.keywordPresent("-version") THEN
        Release.Show();
        Process.Exit(0);
      END;
      (* creation date option *)
      IF pp.keywordPresent("-created") THEN
        M(Creation.Date & " on " & Creation.System);
        Process.Exit(0);
      END;
      (* some message and trace options *)
      Msg.vFlag := pp.keywordPresent("-v"); (* be verbose *)
      Msg.dFlag := pp.keywordPresent("-d"); (* debug messages *)
      Msg.tFlag := NOT pp.keywordPresent("-q"); (* do not run quiet *)
      noAction  := pp.keywordPresent("-n"); (* don't execute anything *)
      stopOnErrors := NOT pp.keywordPresent("-k");
      stopOnFailures := NOT pp.keywordPresent("-f");
      PrjDesc.debugStateCache := pp.keywordPresent("-debugStateCache");
    EXCEPT
      ParseParams.Error => Msg.Fatal("parameter error", 3); <*NOWARN*>
    END;
  END PreEvalArguments;

(*---------------------------------------------------------------------------*)
PROCEDURE EvalArguments() =

  PROCEDURE CheckCommitType() =
    BEGIN
      IF pp.keywordPresent("patch") THEN
	commitType := CommitType.Patch;
      ELSIF pp.keywordPresent("minor") THEN
	commitType := CommitType.Minor;
      ELSIF pp.keywordPresent("major") THEN
	commitType := CommitType.Major;
      END;
    END CheckCommitType;

  BEGIN
    TRY
      (* help option *)
      IF pp.keywordPresent("-h") OR pp.keywordPresent("-help") THEN
        Usage();
      END;
      (* man option *)
      IF pp.keywordPresent("-man") OR pp.keywordPresent("-desc") THEN
        TRY
          RsrcUtils.PageResource(rsrcPath, "UsageHelp", pageit);
        EXCEPT
          RsrcUtils.Error(e) =>
          TRY Wr.PutText(Stdio.stdout, "error listing description: " & 
            e & "\n") 
          EXCEPT ELSE END;
        END;
        Process.Exit(0);
      END;
      (* copyright option *)
      IF pp.keywordPresent("-cr") OR pp.keywordPresent("-copyright") THEN
        Copyright.Show(Copyright.T.All);
        Process.Exit(0);
      END;
      (* some message and trace options *)
      onlyModified := pp.keywordPresent("-m");
      onlyOutOfDate := pp.keywordPresent("-o") OR 
                           pp.keywordPresent("-outofdate");
      useStateCache := NOT pp.keywordPresent("-nocache") AND 
                       NOT pp.keywordPresent("-noc");
      IF pp.keywordPresent("-qc") OR pp.keywordPresent("quietCache") OR
         pp.keywordPresent("-quietcache") THEN
        verboseCache := FALSE;
      END;
      IF pp.keywordPresent("-ce") OR pp.keywordPresent("cacheEarly") OR
         pp.keywordPresent("-cacheearly") THEN
        cacheEarly := TRUE;
      END;
      IF pp.keywordPresent("-message") OR pp.keywordPresent("-msg") THEN
        commitMsg := pp.getNext();
      END;
      IF pp.keywordPresent("-file") THEN
        commitFile := pp.getNext();
      END;
      IF pp.keywordPresent("-nostdin") OR
         pp.keywordPresent("-usegui") THEN
        PkgVC.confirmation := NEW(Confirmation.ExternalClosure, 
                                  cmd := "confirm");
      END;
      IF pp.keywordPresent("-md") OR pp.keywordPresent("-modifieddeps")THEN
	onlyModified := TRUE;
	modifiedAndDeps := TRUE;
      END;
      IF pp.keywordPresent("-od") OR pp.keywordPresent("-outofdatedeps") THEN
	onlyOutOfDate := TRUE;
	outOfDateAndDeps := TRUE;
      END;
      IF pp.keywordPresent("-nd") OR pp.keywordPresent("-nodep") THEN
	nodep := TRUE;
        depsMandatory := FALSE;
      END;
      dependendPkgs := pp.keywordPresent("-dep") OR 
                           pp.keywordPresent("-dependend");
      lazy := pp.keywordPresent("-lazy");

      savePreReleases := pp.keywordPresent("-saveprereleases") OR
                             pp.keywordPresent("-saveall"); 

      IF pp.keywordPresent("-fr") OR pp.keywordPresent("-forcerelease")THEN
        forceRelease := TRUE;
      END;

      IF pp.keywordPresent("-F") OR pp.keywordPresent("-force")THEN
        force := TRUE;
      END;

      IF pp.keywordPresent("-noivc") OR 
         pp.keywordPresent("-nointernalvc") OR 
         pp.keywordPresent("-nointernalversioncontrol") THEN
        useInternalVC := FALSE;
      END;
      IF pp.keywordPresent("-novc") OR 
         pp.keywordPresent("-noversioncontrol") THEN
        useVC := FALSE;
      END;
      WHILE pp.keywordPresent("-D") DO
        VAR
          n, v :  TEXT;
          def  := pp.getNext();
          seq  := TextUtils.Split(def, "=");
        BEGIN
          IF seq.size() < 2 THEN
            Msg.Fatal("variable definition without `=': " & def);
          END;
          v := TextUtils.Compress(seq.remhi());
          WHILE seq.size() > 0 DO
            n := TextUtils.Compress(seq.remlo());
            IF Text.Empty(n) THEN
              Msg.Fatal("empty variable name in " & def);
            END;
            EVAL vars.put(n, v);
            Msg.D("  " & n & " = " & v);
          END;
        END;
      END;
      (* implicit environment variables TAG1 and TAG2 *)
      IF pp.keywordPresent("-t") THEN
        tag1 := pp.getNext(); 
      END;
      IF pp.keywordPresent("-t") THEN
        tag2 := pp.getNext(); 
      END;

      (* named change set *)
      IF pp.keywordPresent("-changeset") OR
        pp.keywordPresent("-cs")THEN
        changeSetName := pp.getNext(); 
      END;

      (* explicit project filename *)
      IF    pp.keywordPresent("-p") THEN
	prjFileName := pp.getNext();
	prjMagicFile := Pathname.Prefix(prjFileName);
	prjMagicFile := Pathname.Join(prjMagicFile, PrjMagicFN, NIL);
	prjDepGraphFile := Pathname.Prefix(prjFileName);
	prjDepGraphFile := Pathname.Join(prjDepGraphFile, PrjDepGraphFN, NIL);
        actPrjFileName := prjFileName;
      END;

      (* explicit checkpoint file *)
      IF pp.keywordPresent("-checkpointfile") OR pp.keywordPresent("-cpf") THEN
        inCheckpFileName := pp.getNext();
        outCheckpFileName := inCheckpFileName;
      END;

      (* explicit snapshot directory *)
      IF pp.keywordPresent("-snapdir") OR pp.keywordPresent("-sd") THEN
        snapshotDir := pp.getNext();
      END;

      (* external shell *)
      IF pp.keywordPresent("-shell") OR pp.keywordPresent("-sh") THEN
	externalShell := pp.getNext();
      END;

      (* explicit package kind *)
      IF pp.keywordPresent("-pkgkind") OR pp.keywordPresent("-kind") THEN
	packageKind := pp.getNext();
      END;

      (* import relevance level for PkgOvr files *)
      IF pp.keywordPresent("-relevancelevel") OR
         pp.keywordPresent("-importlevel") OR
         pp.keywordPresent("-irl") THEN
        TRY
          importRelevanceLevel := Scan.Int(pp.getNext());
        EXCEPT ELSE
          Msg.Fatal("cannot convert import relevance level (integer)");
        END;
      END;
      IF pp.keywordPresent("-novr") THEN
        autoovr := FALSE;
      END;

      (* building *)
      IF    pp.keywordPresent("-localbuild") OR 
	    pp.keywordPresent("-build") OR
	    pp.keywordPresent("-buildlocal") THEN
	action := Action.BuildLocal;
      ELSIF pp.keywordPresent("-projectbuild") OR 
	    pp.keywordPresent("-buildproject") THEN
	action := Action.BuildProject;
      ELSIF pp.keywordPresent("-globalbuild") OR 
	    pp.keywordPresent("-buildglobal") THEN
	action := Action.BuildGlobal;
      ELSIF pp.keywordPresent("-builtokay") OR 
	    pp.keywordPresent("-builtok") OR
	    pp.keywordPresent("-bok") THEN
	action := Action.BuiltOk;
      ELSIF pp.keywordPresent("-createpkgovrfiles") OR 
	    pp.keywordPresent("-pkgovr") OR
	    pp.keywordPresent("-saveimports") OR
	    pp.keywordPresent("-saveimps") OR
	    pp.keywordPresent("-imps") THEN
	action := Action.CreatePkgOvrFiles;
        snapshotName := pp.getNext();
      (* resource information *)
      ELSIF pp.keywordPresent("-listpkgkinds") OR 
	    pp.keywordPresent("-listkinds") THEN
	action := Action.ListKinds;
      ELSIF pp.keywordPresent("-dumpallpkgkinds") OR 
	    pp.keywordPresent("-dumppkgkinds") OR
	    pp.keywordPresent("-dumpkinds") THEN
	action := Action.DumpKinds;
      (* checks *)
      ELSIF pp.keywordPresent("-check") THEN
	action := Action.CheckImports;
        IF pp.keywordPresent("-label") OR pp.keywordPresent("-l") THEN
          action := Action.CheckState;
          stateLabelPattern := pp.getNext();
        END;
      ELSIF pp.keywordPresent("-showpackages") OR
            pp.keywordPresent("-showpkgs") THEN
	action := Action.ShowPackages;
      ELSIF pp.keywordPresent("-showsrcdirectories") OR
            pp.keywordPresent("-showsdirs") THEN
	action := Action.ShowSrcDirectories;
      ELSIF pp.keywordPresent("-dependencies") OR
	    pp.keywordPresent("-showdeps") OR
	    pp.keywordPresent("-deps") THEN
	action := Action.DependingNodes;
      ELSIF pp.keywordPresent("-updatesequence") OR
	    pp.keywordPresent("-upseq") OR
	    pp.keywordPresent("-tsort") THEN
	action := Action.ShowUpdateSequence;
      ELSIF pp.keywordPresent("-showpackagekinds") OR
	    pp.keywordPresent("-showkinds") OR
	    pp.keywordPresent("-kinds") THEN
	action := Action.ShowPackageKinds;
      ELSIF pp.keywordPresent("-showsnapshots") OR
	    pp.keywordPresent("-showsnaps") OR
	    pp.keywordPresent("-snaps") THEN
	action := Action.ShowSnapshots;
        sort := Snapshots.Sort.ByName;
        longListing := pp.keywordPresent("-l");
      ELSIF pp.keywordPresent("-showreleases") OR
	    pp.keywordPresent("-showrels") OR
	    pp.keywordPresent("-rels") THEN
	action := Action.ShowReleases;
        sort := Snapshots.Sort.ByName;
        longListing := pp.keywordPresent("-l");
      ELSIF pp.keywordPresent("-showsnapshot") OR
	    pp.keywordPresent("-showsnap") OR
	    pp.keywordPresent("-ssnap") THEN
	action := Action.ShowSnapshot;
        longListing := pp.keywordPresent("-l");
      ELSIF pp.keywordPresent("-showrelease") OR
	    pp.keywordPresent("-showrel") OR
	    pp.keywordPresent("-srel") THEN
	action := Action.ShowRelease;
        longListing := pp.keywordPresent("-l");
      ELSIF pp.keywordPresent("-editsnapshot") OR
	    pp.keywordPresent("-editsnap") OR
	    pp.keywordPresent("-esnap") THEN
	action := Action.EditSnapshot;
        snapshotName := pp.getNext();
      ELSIF pp.keywordPresent("-editrelease") OR
	    pp.keywordPresent("-editrel") OR
	    pp.keywordPresent("-erel") THEN
	action := Action.EditRelease;
        snapshotName := pp.getNext();
      ELSIF pp.keywordPresent("-showpackagepaths") OR
	    pp.keywordPresent("-showpkgpaths") OR
	    pp.keywordPresent("-spp") THEN
	action := Action.ShowPackagePaths;
      ELSIF pp.keywordPresent("-showchangesets") OR
	    pp.keywordPresent("-showcs") OR
	    pp.keywordPresent("-scs") THEN
	action := Action.ShowChangeSets;
        sort := Snapshots.Sort.ByName
      ELSIF pp.keywordPresent("-changesetlog") OR
	    pp.keywordPresent("-cslog") OR
	    pp.keywordPresent("-csl") THEN
	action := Action.ChangeSetLog;
        sort := Snapshots.Sort.ByDate;
      ELSIF pp.keywordPresent("-snapshotlog") OR
	    pp.keywordPresent("-snaplog") OR
	    pp.keywordPresent("-ssl") THEN
	action := Action.SnapshotLog;
        sort := Snapshots.Sort.ByDate;
        IF pp.keywordPresent("-mtime") THEN sortByModificationDate := TRUE END;
      ELSIF pp.keywordPresent("-releaselog") OR
	    pp.keywordPresent("-rellog") OR
	    pp.keywordPresent("-rl") THEN
	action := Action.ReleaseLog;
        sort := Snapshots.Sort.ByDate;
        IF pp.keywordPresent("-mtime") THEN sortByModificationDate := TRUE END;
      ELSIF pp.keywordPresent("-import") OR
	    pp.keywordPresent("-imp") THEN
	action := Action.Import;
        fileName := pp.getNext();
        name := pp.getNext();
      ELSIF pp.keywordPresent("-export") OR
	    pp.keywordPresent("-exp") THEN
	action := Action.Export;
        name := pp.getNext();
        fileName := pp.getNext();
      (* shipping *)
      ELSIF pp.keywordPresent("-localship") OR 
	    pp.keywordPresent("-shiplocal") THEN
	action := Action.ShipLocal;
      ELSIF pp.keywordPresent("-projectship") OR 
	    pp.keywordPresent("-shipproject") THEN
	action := Action.ShipProject;
      ELSIF pp.keywordPresent("-globalship") OR 
	    pp.keywordPresent("-shipglobal") THEN
	action := Action.ShipGlobal;
      (* cleaning *)
      ELSIF pp.keywordPresent("-clean") THEN
	action := Action.Clean;
      ELSIF pp.keywordPresent("-realclean") THEN
	action := Action.RealClean;
      ELSIF pp.keywordPresent("-newstatecache") OR
            pp.keywordPresent("-newcache") THEN
	action := Action.NewStateCache;
      ELSIF pp.keywordPresent("-purgeunsureversioninfo") OR
            pp.keywordPresent("-purgeunsureinfo") OR
            pp.keywordPresent("-pui") THEN
	action := Action.PurgeUnsureVersionInfo;
      ELSIF pp.keywordPresent("-purgebuildinfo") OR
            pp.keywordPresent("-purgebi") OR
            pp.keywordPresent("-pbi") THEN
	action := Action.PurgeBuildInfo;
      (* predicates *)
      ELSIF pp.keywordPresent("-isrelease") OR 
	    pp.keywordPresent("-isrel") OR
	    pp.keywordPresent("-rel") THEN
	action := Action.IsRelease;
      ELSIF pp.keywordPresent("-ismodified") OR 
	    pp.keywordPresent("-ismod") OR
	    pp.keywordPresent("-mod") THEN
	action := Action.IsModified;
      ELSIF pp.keywordPresent("-isoutofdate") OR 
	    pp.keywordPresent("-isood") OR
	    pp.keywordPresent("-ood") THEN
	action := Action.IsOutOfDate;
      ELSIF pp.keywordPresent("-hasconflicts") OR 
	    pp.keywordPresent("-hascfl") OR
	    pp.keywordPresent("-cfl") THEN
	action := Action.HasConflicts;
      (* version and configuration control *)
      ELSIF pp.keywordPresent("-diff") THEN
	action := Action.Diff;
      ELSIF pp.keywordPresent("-cdiff") THEN
	action := Action.CDiff;
      ELSIF pp.keywordPresent("-udiff") THEN
	action := Action.UDiff;
      ELSIF pp.keywordPresent("-editchangeset") OR
	    pp.keywordPresent("-editcs") OR
	    pp.keywordPresent("-ecs") THEN
        action := Action.EditChangeSet;
      ELSIF pp.keywordPresent("-mergechangeset") OR
	    pp.keywordPresent("-mergecs") OR
	    pp.keywordPresent("-mcs") THEN
        action := Action.MergeChangeSet;
      ELSIF pp.keywordPresent("-showmodified") OR 
	    pp.keywordPresent("-showmod") OR
	    pp.keywordPresent("-smod") THEN
	action := Action.ShowModified;
      ELSIF pp.keywordPresent("-showoutofdate") OR 
	    pp.keywordPresent("-showood") OR
	    pp.keywordPresent("-sood") THEN
	action := Action.ShowOutOfDate;
      ELSIF pp.keywordPresent("-showconflicts") OR 
	    pp.keywordPresent("-showcfl") OR
	    pp.keywordPresent("-scfl") THEN
	action := Action.ShowConflicts;
      ELSIF pp.keywordPresent("-checkout") OR 
	    pp.keywordPresent("-co") OR
	    pp.keywordPresent("-get") OR
	    pp.keywordPresent("-up") OR
	    pp.keywordPresent("-upd") OR
	    pp.keywordPresent("-update") THEN
	action := Action.Checkout;
        snapshotName := pp.getNext();
      ELSIF pp.keywordPresent("-commit") OR 
	    pp.keywordPresent("-ci") THEN
	action := Action.CommitDevel;
	CheckCommitType();
      ELSIF pp.keywordPresent("-commitrelease") OR 
	    pp.keywordPresent("-commitrel") OR
	    pp.keywordPresent("-cirel") THEN
	action := Action.CommitRelease;
	CheckCommitType();
      ELSIF pp.keywordPresent("-commitlocal") OR 
	    pp.keywordPresent("-ciloc") OR
	    pp.keywordPresent("-cil") THEN
	action := Action.CommitLocalFiles;
	CheckCommitType();
      ELSIF pp.keywordPresent("-makesnapshot") OR 
	    pp.keywordPresent("-snapshot") OR
	    pp.keywordPresent("-snap") THEN
	action := Action.NewSnapshot;
        snapshotName := pp.getNext();
      ELSIF pp.keywordPresent("-makerelease") OR 
	    pp.keywordPresent("-release") OR
	    pp.keywordPresent("-makerel") THEN
	action := Action.MakeRelease;
        snapshotName := pp.getNext();
      ELSIF pp.keywordPresent("-makestablerelease") OR 
	    pp.keywordPresent("-stablerelease") OR
	    pp.keywordPresent("-makestable") THEN
	action := Action.StableRelease;
        snapshotName := pp.getNext();
        TRY
          IF pp.next < NUMBER(pp.arg^) AND 
             NOT pp.parsed[pp.next] THEN
            newName := pp.getNext();
          END;
        EXCEPT
          ParseParams.Error => newName := NIL;
        END;
      ELSIF pp.keywordPresent("-newrelease") OR 
	    pp.keywordPresent("-newrel") THEN
	action := Action.NewRelease;
        IF pp.testNext("major") THEN
          commitType := CommitType.Major;
        ELSIF pp.testNext("minor") THEN
          commitType := CommitType.Minor;
        ELSIF pp.testNext("patch") THEN
          commitType := CommitType.Patch;
        ELSE
          Msg.Fatal("please specify an explicit commit type");
        END;
        snapshotName := pp.getNext();
      ELSIF pp.keywordPresent("-orderedapply") OR 
	    pp.keywordPresent("-ordapply") OR
	    pp.keywordPresent("-oapp") THEN
	action := Action.OrderedApply;
	cmdList := pp.getNext();
      ELSIF pp.keywordPresent("-apply") OR 
	    pp.keywordPresent("-app") THEN
	action := Action.Apply;
	cmdList := pp.getNext();
      ELSIF pp.keywordPresent("-applyaction") OR 
	    pp.keywordPresent("-action") THEN
	action := Action.ApplyAction;
	cmdList := pp.getNext();
      ELSIF pp.keywordPresent("-orderedapplyaction") OR 
	    pp.keywordPresent("-ordaction") THEN
	action := Action.OrderedApplyAction;
	cmdList := pp.getNext();
      ELSIF pp.keywordPresent("-orderedselectby") OR 
	    pp.keywordPresent("-ordselectby") OR
	    pp.keywordPresent("-osel") THEN
	action := Action.OrderedSelectBy;
	cmdList := pp.getNext();
      ELSIF pp.keywordPresent("-selectby") OR 
	    pp.keywordPresent("-sel") THEN
	action := Action.SelectBy;
	cmdList := pp.getNext();
      ELSIF pp.keywordPresent("-showstatecache") OR 
	    pp.keywordPresent("-showcache") OR
	    pp.keywordPresent("-sc") THEN
	action := Action.ShowStateCache;
      ELSIF pp.keywordPresent("-showshortstatus") OR 
	    pp.keywordPresent("-shortstatus") OR
	    pp.keywordPresent("-shortstat") OR
	    pp.keywordPresent("-sstat") THEN
	action := Action.ShowShortStatus;
      ELSIF pp.keywordPresent("-showlongstatus") OR 
	    pp.keywordPresent("-longstatus") OR
	    pp.keywordPresent("-longstat") OR
	    pp.keywordPresent("-lstat") THEN
	action := Action.ShowLongStatus;
      ELSE
        VAR m := "unknown command"; BEGIN
          TRY m := m & ": " & pp.getNext() EXCEPT ELSE END;
          m := m & "\n\nFor a short usage help, type `prjm -help'.\n" &
                   "Use `prjm -man' to read the inline manual.\n";
          pp.error(m);
        END;
      END;

      (* sorting order overrides, must be evaluated last *)
      IF pp.keywordPresent("-nosort") THEN
        sort := Snapshots.Sort.None;
      ELSIF pp.keywordPresent("-byname") THEN
        sort := Snapshots.Sort.ByName;
      ELSIF pp.keywordPresent("-bydate") THEN
        sort := Snapshots.Sort.ByDate;
      END;
      IF pp.keywordPresent("-reverse") OR 
         pp.keywordPresent("-down") THEN
        sortUp := FALSE;
      END;

      (* add more options before this line *)
      pp.skipParsed();
      nTargets := NUMBER(pp.arg^) - pp.next;

      targets := NEW(TextSeq.T).init(nTargets);
      FOR i := 1 TO nTargets DO
	VAR t := pp.getNext(); BEGIN
	  IF Text.GetChar(t, 0) = '-' THEN
	    Msg.Fatal("unrecognized option: " & t, 2);
	  ELSE
	    targets.addhi(t);
	  END;
	END;
      END;

      pp.finish();
    EXCEPT
      ParseParams.Error => Msg.Fatal("parameter error", 3);
    END;
    IF NOT FSUtils.IsFile(inCheckpFileName) THEN
      inCheckpFileName := NIL;
    END;
  END EvalArguments;

(*---------------------------------------------------------------------------*)
PROCEDURE CreateDependencyGraph() =
  VAR done := FALSE;
  BEGIN
    Msg.V("setting up package dependency graph...");
    IF nodep THEN
      TRY 
        Msg.V("  reading package dependency graph from file...");
        prjdesc.readDepGraphAsText(prjDepGraphFile);
        done := TRUE;
      EXCEPT
        PrjDesc.Error => 
        Msg.Error("cannot read dependency graph from file " &
          prjDepGraphFile & ", must rebuild it...");
      END;
    END;
    IF NOT done THEN
      Msg.V("building package dependency graph...");
      TRY 
	prjdesc.buildDepGraph(PkgVC.confirmation);
	prjdesc.writeDepGraphAsText(prjDepGraphFile);
        (* test and debug code
	prjdesc.writeDepGraphAsText("depgraph.txt");
        VAR prjd2 := NEW(PrjDesc.T).init(prjFileName, cfg, FALSE;
                                         preferredPkgKind := packageKind);
        BEGIN
          TRY 
            prjd2.readDepGraphAsText("depgraph.txt");
            Msg.Debug("depgraph read successfully");
            prjd2.writeDepGraphAsText("depgraph2.txt");
          EXCEPT
          ELSE
            Msg.Debug("reading depgraph failed");
          END;
        END;
        *)
      EXCEPT
	PrjDesc.Error(e) => Msg.Fatal(e, 5);
      END;
    END;
  END CreateDependencyGraph;

(*---------------------------------------------------------------------------*)
PROCEDURE ListAllPackageKinds() =
  VAR
    pkgs := prjdesc.packages();
    poolset := prjdesc.getPoolSet();
  BEGIN
    Msg.T("The packages in your project are of the following kind:");
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        WITH loc = poolset.pkgType(pkg) DO
          M(Fmt.F("  %-24s is a %-s package", pkg, loc));
        END;
      END;
    END;
    Msg.T("");
  END ListAllPackageKinds;

(*---------------------------------------------------------------------------*)
PROCEDURE ListAllPackagesAndLocations() =
  VAR
    pkgs := prjdesc.packages();
    poolset := prjdesc.getPoolSet();
  BEGIN
    Msg.T("The following packages are contained in your project:");
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        WITH loc = poolset.pkgPath(pkg) DO
          M(Fmt.F("  %-24s at %-s", pkg, loc));
        END;
      END;
    END;
    Msg.T("");
  END ListAllPackagesAndLocations;

(*---------------------------------------------------------------------------*)
PROCEDURE ListAllSrcDirectories() =
  (* Ausgabe der lesbaren Source-Directories aller beteiligten Packages
     (z.B. geeignet als dir-arg fuer m3gdb). Loesung z.Z. nur provisorisch,
     da nicht fuer die entsprechenden PackageKind erfolgten SourceDeklarationen
     aus pkgconf.dat beruecksichtigt werden. *)
  VAR
    pkgs := prjdesc.packages();
    poolset := prjdesc.getPoolSet();
  BEGIN
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        WITH loc = poolset.pkgPath(pkg) DO
          M(Pathname.Join(loc, "src", NIL));
        END;
      END;
    END;
    Msg.T("");
  END ListAllSrcDirectories; 

(*---------------------------------------------------------------------------*)
PROCEDURE ShowUpdateSequence() =
  VAR
    pkgs := prjdesc.packageUpdateSequence();
  BEGIN
    Msg.T("The packages are always updated in the following order:");
    FOR i := 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        M(Fmt.F("  %-s", pkg));
      END;
    END;
    Msg.T("");
  END ShowUpdateSequence; 

(*---------------------------------------------------------------------------*)
PROCEDURE CheckImports() =
  VAR memo := Msg.vFlag;
  BEGIN
    Msg.vFlag := TRUE;
    ListAllPackagesAndLocations();
    ListAllPackageKinds();
    VOutTextSeq("The following packages are imported but ignored:",
                prjdesc.ignoredPackages());
    Msg.vFlag := memo;
  END CheckImports;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckStateLabel() =

  (*-------------------------------------------------------------------------*)
  PROCEDURE DisplayResults(m : TEXT; res : TextSeq.T) =
    BEGIN
      IF res.size() = 0 THEN
        M("no " & m & " found matching pattern " & stateLabelPattern);
      ELSE
        OutTextSeq(m & " found matching pattern " & stateLabelPattern & ":",
                   res);
      END;
    END DisplayResults;

  VAR 
    res : TextSeq.T;
  BEGIN
    IF stateLabelPattern = NIL THEN RETURN END;
    TRY
      IF nTargets = 0 THEN
        res := prjdesc.checkCurrentLabels(stateLabelPattern, lazy);
        DisplayResults("current state labels", res);
      ELSE
        prjdesc.cacheAllStateLabels(lazy);
        FOR i := 0 TO nTargets - 1 DO
          WITH name = targets.get(i) DO
            IF Text.Equal(name, "current") THEN
              res := prjdesc.checkCurrentLabelsGen(stateLabelPattern, TRUE);
              DisplayResults("current state labels", res);
            ELSIF TextUtils.MemberOfTextSeq(prjdesc.snapshots(), name) THEN
              Msg.V("  checking snapshot " & name);
              res := prjdesc.checkLabelsOfSnapshot(name, stateLabelPattern,
                                                   TRUE);
              DisplayResults("state labels of snapshot " & name, res);
            ELSIF TextUtils.MemberOfTextSeq(prjdesc.releases(), name) THEN
              Msg.V("  checking release " & name);
              res := prjdesc.checkLabelsOfRelease(name, stateLabelPattern,
                                                  TRUE);
              DisplayResults("state labels of release " & name, res);
            ELSE
              Msg.Error(name & " is no snapshot or release");
            END;
          END;
        END;
      END;
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("checking state labels aborted: " &
        e, 2);
    END;
  END CheckStateLabel;

(*---------------------------------------------------------------------------*)
PROCEDURE GetAndCheckPackages() : TextSeq.T =
  VAR 
    pkgs, all, notreleased, tmpPkgs : TextSeq.T;
    selText := "";
  BEGIN
    IF onlyNotReleased THEN
      TRY
        notreleased := prjdesc.selectPackages("isrelease", FALSE, FALSE, TRUE);
      EXCEPT
        PrjDesc.Error(t) => Msg.Fatal(t);
      END;
    END;
    IF nTargets = 0 THEN
      IF onlyModified THEN
        pkgs := modifiedPkgs;
        IF modifiedAndDeps THEN
          selText := "modified and dependend";
        ELSE
          selText := "modified";
        END;
      ELSIF onlyOutOfDate THEN
        pkgs := outOfDatePkgs;
        IF outOfDateAndDeps THEN
          selText := "out-of-date and dependend";
        ELSE
          selText := "out-of-date";
        END;
      ELSE
        pkgs := prjdesc.packages();
        selText := "";
      END;
      IF onlyNotReleased THEN
        tmpPkgs := pkgs;
        pkgs := NEW(TextSeq.T).init(notreleased.size());
        FOR i := 0 TO notreleased.size() - 1 DO
          WITH pkg = notreleased.get(i) DO
            IF MemberOfTextSeq(tmpPkgs, pkg) THEN
              pkgs.addhi(pkg);
            END;
          END;
        END;
        IF Text.Empty(selText) THEN
          selText := "unreleased";
        ELSE
          selText := "unreleased and " & selText;
        END;
      END;
      IF selText = NIL THEN
        selText := "all";
      END;
      VOutTextSeq("using " & selText & " packages:", pkgs);
    ELSE
      pkgs := NEW(TextSeq.T).init(targets.size());
      IF onlyModified THEN
        all := modifiedPkgs;
      ELSIF onlyOutOfDate THEN
        all := outOfDatePkgs;
      ELSE
        all  := prjdesc.packages();
      END; 
      FOR i := 0 TO targets.size() - 1 DO
        WITH pkg = targets.get(i) DO
          IF MemberOfTextSeq(all, pkg) THEN
            IF onlyNotReleased THEN
              IF MemberOfTextSeq(notreleased, pkg) THEN 
                pkgs.addhi(pkg);
              END;
            ELSE
              pkgs.addhi(pkg);
            END;
          ELSE
            Msg.Error("package " & pkg & " is not contained in the project");
          END;
        END;
      END;
      IF dependendPkgs THEN
        pkgs := prjdesc.addDependingPackages(pkgs);
      END;
      TOutTextSeq("considering only the following packages:", pkgs);
    END;
    RETURN pkgs;
  END GetAndCheckPackages;

(*---------------------------------------------------------------------------*)
PROCEDURE ApplySymbolicAction(action : TEXT; inOrder := FALSE) : INTEGER =
  VAR
    res  : INTEGER;
    pkgs : TextSeq.T;
  BEGIN
    pkgs := GetAndCheckPackages();
    IF noAction THEN
      TOutTextSeq("The action `" & action &
        "' would be applied to the following packages:",
        pkgs);
      RETURN 0;
    END;
    TRY
      res := prjdesc.applyToPackages(action, NIL, NIL,
                                     pkgs,
                                     ordered := inOrder, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := stopOnErrors,
                                     breakOnFailure := stopOnFailures,
                                     tag1Values := tag1vals,
                                     tag2Values := tag2vals);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("application terminated with exception: " &
        e, 1000);
    END;
    RETURN res;
  END ApplySymbolicAction; 

(*---------------------------------------------------------------------------*)
PROCEDURE CommitPackages(action : Action; commitType : CommitType;) : INTEGER =
  VAR
    commitaction : TEXT;
    committype   : TEXT;
    res  : INTEGER := 0;
    pkgs : TextSeq.T;
    pre  : TextTextTbl.T;
    post : TextTextTbl.T;
    log  : TEXT := commitMsg;
    lb := OSSpecials.LineBreak;
    checkLogMsg := FALSE;
    checkaction := "project-change-set";

  PROCEDURE GetAndCheckLogMsg() =
    BEGIN
      checkLogMsg := CommitHookDefined("external-project-change-set-hook");
      IF log = NIL THEN
        (* get change set description *)
        log := lb & lb &
          "PKG: Please enter a log message for change set " &
          changeSetName &"." & lb &
          "PKG: It should focus on the meaning of all the changes " & lb &
          "PKG: in a project or application context." & lb &
          "PKG: Try to be as exact and informative as you can." & lb &
          "PKG: All lines beginning with PKG: will be erased." & lb;
        WITH editor = CompactRC.GetValue(env, "editor") DO
          log := PkgVCUtils.GetMessage(editor, NIL, msg := log);
        END;
      END;
      IF log = NIL THEN
        IF checkLogMsg THEN
          Msg.Fatal("You haven't specified a log message for this " &
            "change set.\n");
        ELSE
          Msg.Warning("You haven't specified a log message for this " &
            "change set.\n");
          Msg.Warning("You may do this later by manually editing the " &
            "change set file\n");
          Msg.Warning("with \"prjm -editchangeset " & changeSetName &
            "\".");
        END;
      END;
      IF log # NIL THEN
        IF checkLogMsg THEN
          TRY
            PkgVCUtils.CheckCommitMsg(log, NIL, prjName, prjRoot, user, 
                                      NIL, checkaction, changeSetName,
                                      env);
          EXCEPT
            PkgVCUtils.E(e) => Msg.Fatal("log message not accepted: " & e);
          END;
        END;
        changeSet.setDescription(log);
      END;
    END GetAndCheckLogMsg;

  BEGIN
    CASE commitType OF
      CommitType.Major => committype := "major";
    | CommitType.Minor => committype := "minor";
    | CommitType.Patch => committype := "patch";
    END;
    IF action = Action.CommitDevel THEN
      commitaction  := "commitdevel";
      Msg.V("committing development code...");
    ELSIF action = Action.CommitRelease THEN
      commitaction  := "commitrelease";
      Msg.V("committing release code...");
    ELSE
      Msg.Fatal("internal error: unexpected commit action", 1001);
    END;
    pkgs := GetAndCheckPackages();
    IF noAction THEN
      TOutTextSeq("The action `" & commitaction &
        "' would be applied to the following packages:",
        pkgs);
      RETURN 0;
    END;
    TRY
      IF action = Action.CommitRelease THEN
        EverythingBuiltOkay();
      END;
      IF changeSetName # NIL THEN
        IF snaps.changeSetDefined(changeSetName) THEN
          Msg.Error("A change set named " & changeSetName & 
            " already exists.");
          RETURN 900;
        END;
        pre := prjdesc.getTags(pkgs);
        changeSet := NEW(ChangeSet.T).init(changeSetName);
        changeSet.setUser(user);
        changeSet.setDate(Time.Now());
        GetAndCheckLogMsg();
      END;
      IF NOT noAction THEN
        res := prjdesc.applyToPackages(commitaction & committype, NIL, NIL,
                                       pkgs,
                                       ordered := FALSE, 
                                       breakOnZeroReturn := FALSE, 
                                       breakOnError := stopOnErrors,
                                       breakOnFailure := stopOnFailures);
      END;
      IF res = 0 THEN
        IF changeSetName # NIL THEN
          post := prjdesc.getTags(pkgs);
          changeSet.setDate(Time.Now());
          VAR 
            pkg, tag1, tag2 : TEXT; 
            iter := pre.iterate();
          BEGIN
            WHILE iter.next(pkg, tag1) DO
              IF NOT post.get(pkg, tag2) THEN
                RAISE ChangeSet.Error
                ("missing package in post-change-status: " & pkg);
              END;
              changeSet.add(pkg, tag1, tag2);
            END;
          END;
          snaps.putChangeSet(changeSetName, changeSet);
          IF useVC AND NOT noAction THEN
            IF commitMsg = NIL AND log # NIL THEN
              log := 
                "new change set " & changeSetName &":" & lb & lb &
                log & lb & lb &
                "PKG: Please enter a local commit message for change set " & 
                lb & "PKG: " & changeSetName &"." & lb &
                "PKG: If you have made no other changes than the creation " &
                "of this change set," & lb &
                "PKG: the above message will probably be appropriate. " & lb;
              WITH editor = CompactRC.GetValue(env, "editor") DO
                log := PkgVCUtils.GetMessage(editor, NIL, msg := log,
                                             failIfUnchanged := FALSE);
              END;
              IF log # NIL THEN
                commitMsg := log;
              END;
            ELSIF commitMsg # NIL AND 
              NOT TextUtils.Contains(commitMsg, changeSetName) THEN
              commitMsg := 
                "new change set " & changeSetName &":" & lb & lb &
                commitMsg & lb;
            END;
            CommitLocalFiles(PkgVC.CommitType.Minor);
          END;
        END;
      END;
    EXCEPT
      ChangeSet.Error(e) => res := 900;
      Msg.Error(e);
    | Snapshots.Error(e) => res := 900;
      Msg.Error(e);
    | PrjDesc.Error(e) => res := 1000;
      Msg.Error("committing terminated with exception: " & e);
      (* FIXME: save partial changeset description to temporary file *)
    END;
    RETURN res;
  END CommitPackages;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecEditChangeSet() =
  VAR
    old, new : TEXT;
  BEGIN
    IF changeSetName = NIL OR changeSet = NIL THEN
      Msg.Fatal("no valid change set specified");
    END;
    IF NOT snaps.changeSetDefined(changeSetName) THEN
      Msg.Fatal("A change set named " & changeSetName & " does not exist.");
    END;
    old := changeSet.toText();
    WITH editor = CompactRC.GetValue(env, "editor") DO
      new := PkgVCUtils.GetMessage(editor, NIL, msg := old);
    END;
    IF new # NIL THEN
      Msg.V("saving altered change set description");
      WITH cs = NEW(ChangeSet.T).init(changeSet.getName()) DO
        TRY
          cs.parse(TextRd.New(new), "<in-memory copy>");
          snaps.putChangeSet(changeSetName, cs, ovwr := TRUE);
        EXCEPT
          ChangeSet.Error(e) => 
          Msg.Fatal("cannot parse change set description: " & e);
        | Snapshots.Error(e) => 
          Msg.Fatal("cannot overwrite change set description: " & e);
        END;
      END;
    ELSE
      Msg.V("change set unchanged");
    END;
  END ExecEditChangeSet;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecPredicate() =
  VAR
    mod : TextSeq.T;
  BEGIN
    IF action = Action.IsRelease THEN
      IF prjdesc.testAllPackagesReleased() THEN
        Msg.T("All packages are checked out as released versions.");
        WriteCheckpoint();
        Process.Exit(0);
      ELSE
        Msg.T("At least one package is no released version.");
        WriteCheckpoint();
        Process.Exit(1);
      END;
    ELSIF action = Action.IsModified THEN
      TRY
        mod := prjdesc.modifiedPackages();
      EXCEPT 
        PrjDesc.Error(e) => Msg.Fatal("test failed with exception: " &
          e & ", some packages may be modified", 1000);
      END;
      IF mod.size() > 0 THEN
        TOutTextSeq("There are locally modified packages:", mod);
        WriteCheckpoint();
        Process.Exit(0);
      ELSE
        Msg.T("No packages are locally modified.");
        WriteCheckpoint();
        Process.Exit(1);
      END;
    ELSIF action = Action.IsOutOfDate THEN
      PurgeUnsureVersionInfoIfNotLazy();
      TRY
        mod := prjdesc.outOfDatePackages();
      EXCEPT 
        PrjDesc.Error(e) => Msg.Fatal("test failed with exception: " &
          e & ", some packages may be out-of-date", 1000);
      END;
      IF mod.size() > 0 THEN
        TOutTextSeq("There are out-of-date packages:", mod);
        WriteCheckpoint();
        Process.Exit(0);
      ELSE
        Msg.T("All packages are up-to-date.");
        WriteCheckpoint();
        Process.Exit(1);
      END;
    ELSIF action = Action.HasConflicts THEN
      PurgeUnsureVersionInfoIfNotLazy();
      TRY
        mod := prjdesc.packagesWithConflicts();
      EXCEPT 
        PrjDesc.Error(e) => Msg.Fatal("test failed with exception: " &
          e & ", some packages may have conflicts", 1000);
      END;
      IF mod.size() > 0 THEN
        TOutTextSeq("There are packages with conflicts:", mod);
        WriteCheckpoint();
        Process.Exit(0);
      ELSE
        Msg.T("No packages have conflicts.");
        WriteCheckpoint();
        Process.Exit(1);
      END;
    ELSE
      Msg.Fatal("internal error: unexpected predicate action", 1001);
    END;
  END ExecPredicate;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecBuildAction() =
  VAR
    buildaction, shipaction : TEXT;
    res  : INTEGER;
    pkgs : TextSeq.T;
  BEGIN
    IF action = Action.BuildGlobal THEN
      buildaction := "build";
      shipaction  := "shipglobal";
      Msg.V("building for global pool...");
    ELSIF action = Action.BuildProject THEN
      buildaction := "build";
      shipaction  := "shipproject";
      Msg.V("building for project pool...");
    ELSIF action = Action.BuildLocal THEN
      buildaction := "buildlocal";
      shipaction  := "shiplocal";
      Msg.V("building for local pool...");
    ELSE
      Msg.Fatal("internal error: unexpected build action", 1001);
    END;
    pkgs := GetAndCheckPackages();
    IF noAction THEN
      TOutTextSeq("The actions `" & buildaction & "' and `" & shipaction &
        "' would be applied to the following packages in correct order:",
        pkgs);
      WriteCheckpoint();
      Process.Exit(0);
    END;
    IF onlyOutOfDate THEN
      (* need to get the newest versions first *)
      TRY
        prjdesc.checkoutTrunkOrBranchHead(pkgs);
      EXCEPT
        PrjDesc.Error(e) => Msg.Fatal("update terminated with exception: " &
          e, 1000);
      END;
    END;
    TRY
      res := prjdesc.applyToPackages(buildaction, shipaction, NIL,
                                     pkgs,
                                     ordered := TRUE, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := stopOnErrors,
                                     breakOnFailure := stopOnFailures);
    EXCEPT
      PrjDesc.Error(e) => WriteCheckpoint();
      Msg.Fatal("building terminated with exception: " & e, 1000);
    END;
    WriteCheckpoint();
    IF res = 0 THEN
      Msg.V("building terminated successfully");
      Process.Exit(0);
    ELSE
      Msg.Fatal("building terminated with result code " & Fmt.Int(res), 1000);
    END;
  END ExecBuildAction; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecCleanAction() =
  VAR
    cleanaction : TEXT;
    res  : INTEGER;
    pkgs : TextSeq.T;
  BEGIN
    IF action = Action.Clean THEN
      cleanaction := "clean";
      Msg.V("cleaning...");
    ELSIF action = Action.RealClean THEN
      cleanaction := "realclean";
      Msg.V("cleaning thoroughly...");
    ELSE
      Msg.Fatal("internal error: unexpected cleaning action", 1001);
    END;
    pkgs := GetAndCheckPackages();
    IF noAction THEN
      TOutTextSeq("the action `" & cleanaction & 
        "' would be applied to the following packages:",
        pkgs);
      WriteCheckpoint();
      Process.Exit(0);
    END;
    TRY
      res := prjdesc.applyToPackages(cleanaction, NIL, NIL,
                                     pkgs,
                                     ordered := FALSE, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := stopOnErrors,
                                     breakOnFailure := stopOnFailures);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("cleaning terminated with exception: " &
        e, 1000);
    END;
    WriteCheckpoint();
    IF res = 0 THEN
      Msg.V("cleaning terminated successfully");
      Process.Exit(0);
    ELSE
      Msg.Fatal("cleaning terminated with result code " & Fmt.Int(res), 1000);
    END;
  END ExecCleanAction; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShipAction() =
  VAR
    shipaction : TEXT;
    res : INTEGER;
    pkgs : TextSeq.T;
  BEGIN
    IF action = Action.ShipGlobal THEN
      shipaction  := "shipglobal";
      Msg.V("shipping to global pool...");
    ELSIF action = Action.ShipProject THEN
      shipaction  := "shipproject";
      Msg.V("shipping to project pool...");
    ELSIF action = Action.ShipLocal THEN
      shipaction  := "shiplocal";
      Msg.V("shipping to local pool...");
    ELSE
      Msg.Fatal("internal error: unexpected ship action", 1001);
    END;
    pkgs := GetAndCheckPackages();
    IF noAction THEN
      TOutTextSeq("The action `" & shipaction &
        "' would be applied to the following packages in correct order:",
        pkgs);
      WriteCheckpoint();
      Process.Exit(0);
    END;
    TRY
      res := prjdesc.applyToPackages(shipaction, NIL, NIL,
                                     pkgs,
                                     ordered := TRUE, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := stopOnErrors,
                                     breakOnFailure := stopOnFailures);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("shipping terminated with exception: " &
        e, 1000);
    END;
    WriteCheckpoint();
    IF res = 0 THEN
      Msg.V("shipping terminated successfully");
      Process.Exit(0);
    ELSE
      Msg.Fatal("shipping terminated with result code " & Fmt.Int(res), 1000);
    END;
  END ExecShipAction; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecDiffAction() =
  VAR
    cmd := "diff";
    diffPrefix := "";
    res : INTEGER;
    pkgs : TextSeq.T;
    pkgs2 : TextSeq.T;
    curpkgs : TextSeq.T;
    tag1pkgs : TextSeq.T := NIL;
    tag2pkgs : TextSeq.T := NIL;
    nMissing := 0;
  BEGIN
    IF action = Action.CDiff THEN
      diffPrefix := "c";
    ELSIF action = Action.UDiff THEN
      diffPrefix := "u";
    END;
    IF changeSet = NIL THEN
      pkgs := GetAndCheckPackages();
    ELSE
      pkgs := changeSet.packages();
      tag1vals := changeSet.preState();
      tag2vals := changeSet.postState();
      cmd := "diff2";
    END;
    pkgs2 := NEW(TextSeq.T).init();
    curpkgs := prjdesc.packages();
    IF tag1prjdesc # NIL THEN
      tag1pkgs := tag1prjdesc.packages();
      cmd := "diff1";
    END;
    IF tag2prjdesc # NIL THEN
      tag2pkgs := tag2prjdesc.packages();
      cmd := "diff2";
    END;
    FOR i:= 0 TO pkgs.size() - 1 DO
      WITH pkg = pkgs.get(i) DO
        IF tag1pkgs # NIL AND NOT MemberOfTextSeq(tag1pkgs, pkg) THEN
          M("package " & pkg & " is not contained in snapshot " & tag1);
          INC(nMissing);
        ELSIF tag2pkgs # NIL AND NOT MemberOfTextSeq(tag2pkgs, pkg) THEN
          M("package " & pkg & " is not contained in snapshot " & tag2);
          INC(nMissing);
        ELSE
          pkgs2.addhi(pkg);
        END;
      END;
    END;
    IF tag1pkgs # NIL THEN
      FOR i:= 0 TO tag1pkgs.size() - 1 DO
        WITH pkg = tag1pkgs.get(i) DO
          IF NOT MemberOfTextSeq(curpkgs, pkg) THEN
            M("package " & pkg & " is not contained in snapshot " & tag1);
            INC(nMissing);
          END;
        END;
      END;
    END;
    IF tag2pkgs # NIL THEN
      FOR i:= 0 TO tag2pkgs.size() - 1 DO
        WITH pkg = tag2pkgs.get(i) DO
          IF NOT MemberOfTextSeq(curpkgs, pkg) THEN
            M("package " & pkg & " is not contained in snapshot " & tag1);
            INC(nMissing);
          END;
        END;
      END;
    END;
    IF nMissing > 0 THEN
      M("Up to " & Fmt.Int(nMissing) & 
        " difference listings will be missing.");
    END;

    IF noAction THEN
      TOutTextSeq("The action `" & diffPrefix & cmd & 
        "' would be applied to the following packages:",
        pkgs);
      Process.Exit(0);
    END;
    TRY
      prjdesc.defineGlobalVar("diffPrefix", diffPrefix);
      res := prjdesc.applyToPackages(cmd, NIL, NIL,
                                     pkgs2,
                                     ordered := FALSE, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := stopOnErrors,
                                     breakOnFailure := stopOnFailures,
                                     tag1Values := tag1vals,
                                     tag2Values := tag2vals);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("diffing terminated with exception: " &
        e, 1000);
    END;
    IF res = 0 THEN
      Msg.V("diffing terminated successfully");
      Process.Exit(0);
    ELSE
      Msg.Fatal("diffing terminated with result code " & Fmt.Int(res), 1000);
    END;
  END ExecDiffAction;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecMergeChangeSet() =
  VAR
    cmd := "merge2";
    res : INTEGER;
    pkgs : TextSeq.T;
  BEGIN
    IF changeSet = NIL THEN
      Msg.Fatal("no change set", 1000);
    END;
    pkgs := changeSet.packages();
    tag1vals := changeSet.preState();
    tag2vals := changeSet.postState();
    IF noAction THEN
      TOutTextSeq("The action `" & cmd & 
        "' with tags from change set " & changeSetName & 
        "\nwould be applied to the following packages:",
        pkgs);
      Process.Exit(0);
    END;
    TRY
      res := prjdesc.applyToPackages(cmd, NIL, NIL,
                                     pkgs,
                                     ordered := FALSE, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := stopOnErrors,
                                     breakOnFailure := stopOnFailures,
                                     tag1Values := tag1vals,
                                     tag2Values := tag2vals);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("merging terminated with exception: " &
        e, 1000);
    END;
    WriteCheckpoint();
    IF res = 0 THEN
      Msg.V("application of change set " & changeSetName & 
        " was successful");
      Process.Exit(0);
    ELSE
      Msg.Fatal("application of change set " & changeSetName &
        "  terminated with result code " & Fmt.Int(res), 1000);
    END;
  END ExecMergeChangeSet;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecPackageCommitAction() =
  VAR
    res  := CommitPackages(action, commitType);
  BEGIN
    WriteCheckpoint();
    IF res = 0 THEN
      Msg.V("committing terminated successfully");
      Process.Exit(0);
    ELSE
      Msg.Fatal("committing terminated with result code " & 
        Fmt.Int(res), 1000);
    END;
  END ExecPackageCommitAction; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecCommitLocalFiles() =
  BEGIN
    CommitLocalFiles(commitType);
  END ExecCommitLocalFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE CommitLocalFiles(commitType : PkgVC.CommitType) =
  VAR file : APN.T := NIL;
  BEGIN
    TRY
      IF noAction THEN
        Msg.V("all local files would be committed now...");
        RETURN;
      END;
      IF NOT Text.Equal(actPrjFileName, ActStateFN) THEN
        WITH f = APN.New(actPrjFileName) DO
          IF NOT PkgVC.VC.known(f) THEN
            IF NOT PkgVC.VC.add(f) THEN
              Msg.Fatal("cannot put " & f.denotation() & 
                " under version control", 1000);
            END;
          END;
        END;
      END;
      snaps.everythingUnderVersionControl();
      IF commitFile # NIL THEN
        file := APN.New(commitFile);
      END;
      PkgVC.VC.commitChanges(commitType, commitMsg, file);
    EXCEPT
      PkgVC.E(t) => Msg.Fatal(t);
    | Snapshots.Error(t) => Msg.Fatal(t);
    END;
  END CommitLocalFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE PackageImportDefs(snap : TextTextTbl.T; pkg : TEXT;
                            relevanceLevel := 2) : TextTextTbl.T =
  VAR
    imports  : TextSeq.T;
    tag      : Tag.T;
    ver      : Version.T;
    res      : TextTextTbl.T;
    imp, rev : TEXT; 
  BEGIN
    imports := prjdesc.packageDependencies(pkg);
    res := NEW(TextTextTbl.Default).init();
    FOR i := 0 TO imports.size() - 1 DO
      imp := imports.get(i);
      IF snap.get(imp, rev) THEN
        tag := Tag.New(rev);
        ver := tag.version();
        IF relevanceLevel < 3 THEN
          ver.patchlevel := Version.Undefined;
        END;
        IF relevanceLevel < 2 THEN
          ver.minor := Version.Undefined;
        END;
        IF relevanceLevel < 1 THEN
          ver.major := Version.Undefined;
        END;
        EVAL res.put(imp, ver.toText());
      ELSE
        Msg.V(imp & " not in snapshot/project, dependency omitted in imports");
      END;
    END;
    RETURN res;
  END PackageImportDefs;

(*---------------------------------------------------------------------------*)
PROCEDURE PkgOvrFile(impdefs : TextTextTbl.T) : TEXT =
  VAR
    pkg, ver : TEXT;
    iter := impdefs.iterate();
    res := "";
  BEGIN
    WHILE iter.next(pkg, ver) DO
      res := res & "import(\"" & pkg & "\", " & ver & ")\n";
    END;
    RETURN res;
  END PkgOvrFile;

(*---------------------------------------------------------------------------*)
PROCEDURE WritePkgOvrFile(pn : Pathname.T; data : TEXT; rev : TEXT) =
  VAR
    wr : FileWr.T;
  BEGIN
    Msg.T("new package imports in " & pn);
    IF noAction THEN RETURN END;
    TRY
      wr := FileWr.Open(pn);
      Wr.PutText(wr, "# This file was generated by the ComPact Project " &
        "Manager.\n");
      Wr.PutText(wr, "# Date: " & FmtTime.Long(Time.Now()) & "\n");
      Wr.PutText(wr, "# Base revision: " & rev & "\n");
      Wr.PutText(wr, data);
      Wr.Close(wr);
    EXCEPT ELSE
      Msg.Error("cannot write file " & pn);
    END;
  END WritePkgOvrFile; 

(*---------------------------------------------------------------------------*)
PROCEDURE CreatePkgOvrFiles(snap : TextTextTbl.T; pkgs : TextSeq.T := NIL) =
  VAR 
    iter        := snap.iterate();
    pkgset      := prjdesc.getPoolSet();
    pkg, rev    : TEXT;
    loc, pkgOvr : TEXT;
    impdefs     : TextTextTbl.T;
  BEGIN
    Msg.V("saving new import information in package roots...");
    WHILE iter.next(pkg, rev) DO
      IF pkgs = NIL OR TextUtils.MemberOfTextSeq(pkgs, pkg) THEN
        loc := pkgset.pkgPath(pkg);
        pkgOvr := Pathname.Join(loc, PkgOvrFN, NIL);
        impdefs := PackageImportDefs(snap, pkg, importRelevanceLevel);
        WritePkgOvrFile(pkgOvr, PkgOvrFile(impdefs), rev);
      END;
    END;
  END CreatePkgOvrFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecCreatePkgOvrFiles() =
  VAR
    pkgs : TextSeq.T;
    prjd : PrjDesc.T;
    snap : TextTextTbl.T;
    type : TEXT;
  BEGIN
    TRY
      prjd := snaps.getSnapshot(snapshotName);
      snap := prjd.snapshot(snapshotName);
      type := "snapshot ";
    EXCEPT
      Snapshots.Error =>
      TRY
        prjd := snaps.getRelease(snapshotName);
        snap := prjd.release(snapshotName);
        type := "release ";
      EXCEPT
        Snapshots.Error => snap := NIL;
      END;
    END;
    IF snap = NIL THEN
      Msg.Fatal("no snapshot or release " & snapshotName, 1001);
    END;
    pkgs := GetAndCheckPackages();
    CreatePkgOvrFiles(snap, pkgs);
  END ExecCreatePkgOvrFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE EverythingBuiltOkay() =
  VAR 
    pkgs := GetAndCheckPackages();
    res  :  INTEGER;
  BEGIN
    Msg.V("checking if everything has been successfully built");
    TRY
      res := prjdesc.applyToPackages("builtok", NIL, NIL,
                                     pkgs,
                                     ordered := TRUE, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := TRUE,
                                     breakOnFailure := TRUE);
      IF res = 0 THEN
        Msg.V("everything seems to have been built okay");
      ELSE
        Msg.Fatal("build check exited with " & Fmt.Int(res), 1);
      END;
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("build check failed: " & e, 1);
    END;
  END EverythingBuiltOkay;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecFreezeAction() =
  VAR 
    snap : TextTextTbl.T;
    msg  : TEXT;
    res  : INTEGER;
    log  : TEXT := commitMsg;
    lb   := OSSpecials.LineBreak;
    checkLogMsg := FALSE;
    checkaction := "project-release";

  PROCEDURE GetAndSetLogMsg() =
    BEGIN
      IF log = NIL THEN
        log := lb & lb &
          "PKG: Please enter a log message for snapshot/release " &
          snapshotName &"." & lb &
          "PKG: It should focus on its content and intended use. " & lb &
          "PKG: Try to be as exact and informative as you can." & lb &
          "PKG: All lines beginning with PKG: will be erased." & lb;
        WITH editor = CompactRC.GetValue(env, "editor") DO
          log := PkgVCUtils.GetMessage(editor, NIL, msg := log);
        END;
      END;
      IF log = NIL THEN
        IF checkLogMsg THEN
          Msg.Fatal("You haven't specified a log message for this " &
            "snapshot/release set.\n");
        ELSE
          Msg.Warning("You haven't specified a log message for this " &
            "snapshot/release set.\n");
          Msg.Warning("You may do this later by manually editing the " &
            "snapshot file\n");
          Msg.Warning("with \"prjm -editsnapshot " & snapshotName &
            "\".");
        END;
      END;
      IF log # NIL THEN
        IF checkLogMsg THEN
          TRY
            PkgVCUtils.CheckCommitMsg(log, NIL, prjName, prjRoot, user, NIL,
                                      checkaction, snapshotName, env);
          EXCEPT
            PkgVCUtils.E(e) => Msg.Fatal("log message not accepted: " & e);
          END;
        END;
        prjdesc.setDescription(log);
      END;
    END GetAndSetLogMsg; 

  BEGIN
    TRY
      IF action = Action.NewSnapshot THEN
        Msg.V("creating new snapshot...");
        prjdesc.newSnapshot(snapshotName);
        checkLogMsg := CommitHookDefined("external-project-snapshot-hook");
        checkaction := "project-snapshot";
      ELSIF action = Action.MakeRelease THEN
        Msg.V("creating release snapshot...");
        IF NOT force THEN
          EverythingBuiltOkay();
        END;
        prjdesc.newRelease(snapshotName);
        checkLogMsg := CommitHookDefined("external-project-release-hook");
      ELSIF action = Action.NewRelease THEN
        Msg.V("creating new release and release snapshot...");
        IF NOT force THEN
          EverythingBuiltOkay();
        END;
        checkLogMsg := CommitHookDefined("external-project-release-hook");
        IF NOT forceRelease THEN
          onlyNotReleased := TRUE;
        END;
        res := CommitPackages(Action.CommitRelease, commitType);
        IF res # 0 THEN
          Msg.Fatal("committing terminated with result code " & 
            Fmt.Int(res), 1000);
        END;
        IF autoovr THEN
          Msg.V("creating intermediate release snapshot pre_" & snapshotName);
          prjdesc.newRelease("pre_" & snapshotName);
        ELSE
          Msg.V("creating release snapshot " & snapshotName);
          prjdesc.newRelease(snapshotName);
        END;
      ELSE
        Msg.Fatal("internal error: unexpected freeze action", 1001);
      END;
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal(e, 8);
    END;
    prjdesc.setUser(user);
    prjdesc.setCreationDate(Time.Now());
    prjdesc.setModificationDate(Time.Now());
    IF action = Action.NewRelease AND autoovr THEN
      prjdesc.setDescription("automatically generated by the Elego ComPact " &
        "project manager");
    ELSE
      GetAndSetLogMsg();
    END;
    TRY
      IF action = Action.NewSnapshot THEN
        Msg.V("writing snapshot...");
        snaps.putSnapshot(snapshotName, prjdesc);
        snap := prjdesc.snapshot(snapshotName);
        msg := "snapshot created of the following packages:";
      ELSE
        IF action = Action.NewRelease AND autoovr THEN
          IF savePreReleases THEN
            Msg.V("writing pre-release...");
            snaps.putRelease("pre_" & snapshotName, prjdesc);
          ELSE
            Msg.V("abandoning pre-release configuration...");
          END;
          snap := prjdesc.release("pre_" & snapshotName);
        ELSE
          Msg.V("writing release...");
          snaps.putRelease(snapshotName, prjdesc);
          snap := prjdesc.release(snapshotName);
        END;
        msg := "release created of the following packages:";
      END;
      IF action = Action.NewRelease AND autoovr THEN
        CreatePkgOvrFiles(snap);
        TRY
          Msg.V("adding " & PkgOvrFN & " files...");
          EVAL prjdesc.applyCmdListDirectly("pkgvm -add " & PkgOvrFN, NIL,
                                            ordered := FALSE,
                                            breakOnZeroReturn := FALSE,
                                            breakOnError := FALSE, 
                                            breakOnFailure := TRUE);
          
          Msg.V("committing " & PkgOvrFN & " to release branches...");
          res := prjdesc.applyToPackages("commitrelease" & "patch", NIL, NIL,
                                         NIL, (* all packages *)
                                         ordered := FALSE, 
                                         breakOnZeroReturn := FALSE, 
                                         breakOnError := stopOnErrors,
                                         breakOnFailure := stopOnFailures);
          IF res # 0 THEN
            Msg.Error("Couldn't commit all " & PkgOvrFN & " files.");
            Msg.Error("The new release is incomplete, " & snapshotName &
                      " does not yet exist, but pre_" & snapshotName &
                      " does.");
            Msg.Fatal("Aborting creation of release " & snapshotName);
          END;
        EXCEPT
          PrjDesc.Error(e) => 
          Msg.Error("error during addition of " & PkgOvrFN & " files.");
          Msg.Fatal(e, 8);
        END;
        TRY
          GetAndSetLogMsg();
          Msg.V("creating release snapshot " & snapshotName);
          prjdesc.newRelease(snapshotName);
          Msg.V("writing release...");
          snaps.putRelease(snapshotName, prjdesc);
          snap := prjdesc.release(snapshotName);
        EXCEPT
          PrjDesc.Error(e) => 
          Msg.Fatal("cannot make release " & snapshotName & ": " & e, 8);
        END;
      END;
    EXCEPT
      Snapshots.Error(e) => Msg.Fatal("error writing project state: " &
        e, 3);
    END;
    IF snap = NIL THEN
      Msg.Fatal("snapshot seems not to have been created");
    END;
    TOutTextTable(msg, snap);
    IF useVC AND NOT noAction THEN
      VAR lct := PkgVC.CommitType.Minor; ctname : TEXT; BEGIN
        IF action = Action.NewSnapshot THEN
          lct := PkgVC.CommitType.Minor;
          ctname := "snapshot";
        ELSE
          lct := PkgVC.CommitType.Major;
          ctname := "release";
        END;
        IF commitMsg = NIL AND log # NIL THEN
          log := 
            "new " & ctname & " " & snapshotName &":" & lb & lb &
            log & lb & lb &
            "PKG: Please enter a local commit message for " & ctname & 
            lb & "PKG: " & snapshotName &"." & lb &
            "PKG: If you have made no other changes than the creation " &
            "of this " & ctname & "," & lb &
            "PKG: the above message will probably be appropriate. " & lb;
          WITH editor = CompactRC.GetValue(env, "editor") DO
            log := PkgVCUtils.GetMessage(editor, NIL, msg := log,
                                         failIfUnchanged := FALSE);
          END;
          IF log # NIL THEN
            commitMsg := log;
          END;
        ELSIF commitMsg # NIL AND 
          NOT TextUtils.Contains(commitMsg, snapshotName) THEN
          commitMsg := 
            "new " & ctname & " " & snapshotName &":" & lb & lb &
            commitMsg & lb;
        END;
        CommitLocalFiles(lct);
      END;
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecFreezeAction;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecMakeStableRelease() =
  VAR
    release  : PrjDesc.T;
    rel, res : TextTextTbl.T;
    msg      : TEXT;
    log      : TEXT := NIL;
    lb       := OSSpecials.LineBreak;
  BEGIN
    TRY
      release := snaps.getRelease(snapshotName);
      rel := release.release(snapshotName);
      IF newName = NIL THEN
        newName := snapshotName & "_stable";
      END;
      IF rel = NIL THEN
        Msg.Fatal("release " & snapshotName & " not found");
      END;
      res := NEW(TextTextTbl.Default).init(rel.size());
      VAR
        iter := rel.iterate();
        pkg, tag: TEXT;
        stag : Tag.T;
      BEGIN
        WHILE iter.next(pkg, tag) DO
          stag := Tag.New(tag);
          stag := Tag.NewStableBranch(stag);
          EVAL res.put(pkg, stag.denotation());
        END;
      END;
      IF noAction THEN
        rel := res;
        msg := 
            "new stable release would be created of the following packages:";
      ELSE
        release.defineRelease(newName, res);
        snaps.putRelease(newName, release);
        rel := release.release(newName);
        msg := "stable release created of the following packages:";
      END;
    EXCEPT
      PrjDesc.Error(e)   => Msg.Fatal(e);
    | Snapshots.Error(e) => Msg.Fatal(e);
    END;
    TOutTextTable(msg, rel);
    IF useVC THEN
      IF commitMsg = NIL THEN
        log := 
          "new stable release configuration " & newName & ":" & lb & lb &
          "PKG: Please enter a local commit message for " & 
          lb & "PKG: " & newName &"." & lb &
          "PKG: If you have made no other changes than the creation " &
          "of this configuration," & lb &
          "PKG: the above message will probably be appropriate. " & lb;
        WITH editor = CompactRC.GetValue(env, "editor") DO
          log := PkgVCUtils.GetMessage(editor, NIL, msg := log,
                                       failIfUnchanged := FALSE);
        END;
        IF log # NIL THEN
          commitMsg := log;
        END;
      ELSIF NOT TextUtils.Contains(commitMsg, newName) THEN
        commitMsg := 
          "new stable release configuration " & newName & ":" & lb & 
          commitMsg & lb;
      END;
      CommitLocalFiles(PkgVC.CommitType.Major);
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecMakeStableRelease;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecCheckout() =
  VAR
    name  :  TEXT;
    rev   :  TEXT;
    isrel := FALSE;
    head  := FALSE;
    tab   :  TextTextTbl.T;
    msg   :  TextSeq.T;

  (*-------------------------------------------------------------------------*)
  PROCEDURE InvalidateStateCache() =
    BEGIN
      TRY
        prjdesc.reinit(actPrjFileName, cfg, FALSE, 
                       collectionroot, inCheckpFileName,
                       TRUE, env,
                       pkgvcAcc := pkgvcCreator);
        IF packageKind # NIL THEN
          prjdesc.setPreferredPkgKind(packageKind);
        END;
        prjdesc.invalidateCachedUnsureVersionInfo();
        CreateDependencyGraph();
        prjdesc.writeCheckpoint(outCheckpFileName);
      EXCEPT
        PrjDesc.Error(e) => Msg.Fatal("cannot write checkpoint file: " & e);
      END;
    END InvalidateStateCache;

  BEGIN (* ExecCheckout *)
    name := snapshotName;
    TRY
      IF Text.Equal(name, "head") THEN
        Msg.V("checking out current development versions...");
        head := TRUE;
        PrjDescLoadHead();
      ELSIF MemberOfTextSeq(snaps.listReleases(), name) THEN
        isrel := TRUE;
        prjdesc := snaps.getRelease(name);
        prjdesc.defineGlobalVars(env);
        prjdesc.defineGlobalVars(vars);
        prjdesc.writeRelease(ActStateFN, name);
        Msg.V("checking out release " & name & "...");
      ELSIF MemberOfTextSeq(snaps.listSnapshots(), name) THEN
        prjdesc := snaps.getSnapshot(name);
        prjdesc.defineGlobalVars(env);
        prjdesc.defineGlobalVars(vars);
        prjdesc.writeSnapshot(ActStateFN, name);
        Msg.V("checking out snapshot " & name & "...");
      ELSE
        Msg.Warning("There is no release or snapshot with name " & name);
        IF force THEN
          Msg.Warning("Trying to use " & name & " as CVS tag");
        ELSE
          WITH msg = "Would you like to checkout the project with `" &
               name & "' as CVS tag" DO
            IF NOT PkgVC.confirmation.okay(msg) THEN
              Msg.Fatal("checkout aborted by user");
            END;
          END;
        END;
      END;
    EXCEPT
      PrjDesc.Error(e)   => Msg.Fatal(e);
    | Snapshots.Error(e) => Msg.Fatal(e);
    END;
    IF noAction THEN
      IF nTargets = 0 THEN
        msg := NEW(TextSeq.T).init();
        IF head THEN
          FOR i := 0 TO prjdesc.packages().size() - 1 DO
            WITH pkg = prjdesc.packages().get(i) DO
              msg .addhi("checking out " & pkg & " as head");
            END;
          END;
        ELSE
          IF isrel THEN
            tab := prjdesc.release(name);
          ELSE
            tab := prjdesc.snapshot(name);
          END;
          WITH iter = tab.iterate() DO
            WHILE iter.next(name, rev) DO
              msg.addhi("checking out " & name & " as " & rev);
            END;
          END;
        END;
      ELSE
        msg := targets;
      END;
      TOutTextSeq("The following packages would be checked out:", msg);
      (* WriteCheckpoint(); *)
      Process.Exit(0);
    END;
    TRY
      IF nTargets = 0 THEN
        IF head THEN
          prjdesc.checkoutHead();
        ELSIF isrel THEN
          prjdesc.checkoutRelease(name);
        ELSE
          prjdesc.checkoutSnapshot(name);
        END;
      ELSE
        prjdesc.checkoutPackages(targets, name);
      END;
      IF updateStateCache THEN
        InvalidateStateCache();
      END;
    EXCEPT
      PrjDesc.Error(e) => 
      IF useStateCache THEN
        WriteCheckpoint();
      ELSE
        InvalidateStateCache();
      END;
      Msg.Fatal("error checking out project: "& e, 3);
    END;
    (* WriteCheckpoint(); *)
    Process.Exit(0);
  END ExecCheckout;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecApplyAction() =
  VAR
    res : INTEGER;
    heedOrder := action = Action.OrderedApplyAction;
  BEGIN
    res := ApplySymbolicAction(cmdList, heedOrder);
    WriteCheckpoint();
    IF res = 0 THEN
      Msg.V("application terminated successfully");
    ELSIF stopOnErrors THEN
      Msg.Fatal("application terminated with result code " & 
        Fmt.Int(res), 1000);
    END;
    Process.Exit(0);
  END ExecApplyAction;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecApplyCmdList() =
  VAR
    res : INTEGER;
    pkgs : TextSeq.T;
    heedOrder := action = Action.OrderedApply;
  BEGIN
    pkgs := GetAndCheckPackages();
    IF noAction THEN
      TOutTextSeq("The action `" & cmdList &
        "' would be applied to the following packages:",
        pkgs);
      WriteCheckpoint();
      Process.Exit(0);
    END;
    WriteCheckpoint();
    IF heedOrder THEN
      Msg.V("applying `" & cmdList & "' to ordered list of packages...");
    ELSE
      Msg.V("applying `" & cmdList & "' to unordered list of packages...");
    END;
    TRY
      res := prjdesc.applyCmdListDirectly(
                 cmdList, 
                 pkgs,
                 ordered := heedOrder,
                 breakOnZeroReturn := FALSE, 
                 breakOnError := stopOnErrors,
                 breakOnFailure := stopOnFailures);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("application terminated with exception: " &
        e, 1000);
    END;
    IF res = 0 THEN
      Msg.V("application terminated successfully");
      Process.Exit(0);
    ELSE
      IF stopOnErrors THEN
        Msg.Fatal("application terminated with result code " & 
          Fmt.Int(res), 1000);
      ELSE
        Msg.V("last application terminated with result code " & Fmt.Int(res));
        Process.Exit(0);
      END;
    END;
  END ExecApplyCmdList;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecSelectByCmdList() =
  VAR
    res  : TextSeq.T;
    pkgs : TextSeq.T;
    heedOrder := action = Action.OrderedApply;
  BEGIN
    pkgs := GetAndCheckPackages();
    IF noAction THEN
      TOutTextSeq("The action `" & cmdList &
        "' would be applied to the following packages for selecting:",
        pkgs);
      WriteCheckpoint();
      Process.Exit(0);
    END;
    IF heedOrder THEN
      Msg.V("selecting by `" & cmdList & 
        "' from ordered list of packages...");
    ELSE
      Msg.V("selecting by `" & cmdList & 
        "' from unordered list of packages...");
    END;
    TRY
      res := prjdesc.selectByCmdList(
                 cmdList, 
                 ordered := heedOrder,
                 selectOnZeroReturn := TRUE, 
                 breakOnFailure := stopOnFailures);
    EXCEPT
      PrjDesc.Error(e) => WriteCheckpoint();
      Msg.Fatal("selection terminated with exception: " & e, 1000);
    END;
    TOutTextSeq("The following packages were selected:", res); 
    WriteCheckpoint();
    Process.Exit(0);
  END ExecSelectByCmdList; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowDependencies() =
  VAR all := nTargets = 0;
  BEGIN
    IF all THEN
      targets := prjdesc.packages();
      nTargets := targets.size();
    END;
    FOR i := 0 TO nTargets - 1 DO
      WITH pkg = targets.get(i) DO
	TOutTextSeq("package " & pkg & " is needed by:",
		    prjdesc.dependendPackages(pkg));
      END;
    END;
    IF all THEN
      ShowUpdateSequence();
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecShowDependencies; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowPackages() =
  BEGIN
    ListAllPackagesAndLocations();
  END ExecShowPackages;  

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowPackagePaths() =
  VAR
    paths := prjdesc.locations();
    collectionroot := prjdesc.collectionPath();
    crlen := Text.Length(collectionroot);
    loc   : TEXT;
    arcs  : Pathname.Arcs;
  BEGIN
    Msg.T("Package paths relative to collectionroot:");
    FOR i := 0 TO paths.size() - 1 DO
      loc := paths.get(i);
      IF TextUtils.Pos(loc, collectionroot) = 0 THEN
        loc := Text.Sub(loc, crlen);
        IF Pathname.Absolute(loc) THEN
          TRY
            arcs := Pathname.Decompose(loc);
            EVAL arcs.remlo();
            arcs.addlo(NIL);
            loc := Pathname.Compose(arcs);
          EXCEPT
            Pathname.Invalid => Msg.Error("invalid pathname: " & loc);
          END;
        END;
      END;
      M(loc);
    END;
    Msg.T("");
  END ExecShowPackagePaths; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowSrcDirectories() =
  BEGIN
    ListAllSrcDirectories();
  END ExecShowSrcDirectories;  

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowPackageKinds() =
  BEGIN
    ListAllPackageKinds();
  END ExecShowPackageKinds; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowSnapshotOrRelease() =
  VAR 
    snap : TextTextTbl.T;
    name : TEXT;
    type : TEXT := "snapshot or release" ;
    res  : TEXT;
    prjd : PrjDesc.T;
  BEGIN
    FOR i := 0 TO nTargets - 1 DO
      name := targets.get(i);
      IF Text.Equal(name, "head") THEN
        snap := prjdesc.snapshot(name);
        type := "current configuration ";
      ELSE
        TRY
          prjd := snaps.getSnapshot(name);
          snap := prjd.snapshot(name);
          type := "snapshot ";
          res  := prjd.toText();
        EXCEPT
          Snapshots.Error =>
          TRY
            prjd := snaps.getRelease(name);
            snap := prjd.release(name);
            type := "release ";
            res  := prjd.toText();
          EXCEPT
            Snapshots.Error => snap := NIL;
          | PrjDesc.Error(e) => Msg.Fatal("cannot convert snapshot: " & e);
          END;
        | PrjDesc.Error(e) => Msg.Fatal("cannot convert snapshot: " & e);
        END;
      END;
      IF snap = NIL THEN
        M("no snapshot or release " & name);
      ELSE
        IF longListing THEN
          M(type & name);
          M(res);
        ELSE
          OutTextTable(type & name, snap);
        END;
      END;
    END;
  END ExecShowSnapshotOrRelease; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecEditSnapshotOrRelease() =
  VAR
    old, new : TEXT;
    snap : TextTextTbl.T;
    prjd : PrjDesc.T;
    type : TEXT;
    rd   : Rd.T;
  BEGIN
    IF snapshotName = NIL THEN
      Msg.Fatal("no valid snapshot/release specified");
    END;
    IF action = Action.EditRelease THEN 
      TRY
        prjd := snaps.getRelease(snapshotName);
        snap := prjd.release(snapshotName);
        type := "release ";
      EXCEPT
        Snapshots.Error => 
        Msg.Fatal("A release named " & snapshotName & " does not exist.");
      END;
    END;
    IF action = Action.EditSnapshot THEN
      TRY
        prjd := snaps.getSnapshot(snapshotName);
        snap := prjd.snapshot(snapshotName);
        type := "snapshot ";
      EXCEPT
        Snapshots.Error =>
        Msg.Fatal("A snapshot named " & snapshotName & " does not exist.");
      END;
    END;
    TRY
      IF action = Action.EditRelease THEN
        old := prjd.releaseText(snapshotName);
      ELSE
        old := prjd.snapshotText(snapshotName);
      END;
    EXCEPT
      PrjDesc.Error(e) => 
      Msg.Fatal("cannot convert " & type & " description: " & e);
    END;
    WITH editor = CompactRC.GetValue(env, "editor") DO
      new := PkgVCUtils.GetMessage(editor, NIL, msg := old);
    END;
    IF new # NIL THEN
      Msg.V("saving altered " & type & "  description");
      TRY
        EVAL prjd.init(NIL, cfg, FALSE, 
                       collectionroot, inCheckpFileName,
                       FALSE, env,
                       pkgvcAcc := pkgvcCreator,
                       verboseCacheMsgs := verboseCache,
                       preferredPkgKind := packageKind);
        prjdesc.defineGlobalVars(env);
        prjdesc.defineGlobalVars(vars);
        rd := TextRd.New(new);
        prjd.parse(rd);
        prjd.setModificationDate(Time.Now());
        TRY Rd.Close(rd) EXCEPT ELSE END;
        IF action = Action.EditRelease THEN
          snaps.putRelease(snapshotName, prjd, TRUE);
        ELSE
          snaps.putSnapshot(snapshotName, prjd, TRUE);
        END;
      EXCEPT
        PrjDesc.Error(e) => 
          Msg.Fatal("cannot parse " & type & " description: " & e);
      | Snapshots.Error(e) => 
        Msg.Fatal("cannot overwrite " & type & " description: " & e);
      END;
    ELSE
      Msg.V(type & " unchanged");
    END;
  END ExecEditSnapshotOrRelease; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecExport() =
  VAR
    data : TEXT;
    prjd : PrjDesc.T;
    cs   : ChangeSet.T;
    type : TEXT;
  BEGIN
    IF snaps.releaseDefined(name) THEN
      TRY
        prjd := snaps.getRelease(name);
        type := "release ";
        data := prjd.releaseText(name);
      EXCEPT
        Snapshots.Error(e) =>
        Msg.Fatal("snaps directory or snaps/snaps.idx corrupt: " & e);
      | PrjDesc.Error(e) =>
        Msg.Fatal("snaps directory or snaps/snaps.idx corrupt: " & e);
      END;
    ELSIF snaps.snapshotDefined(name) THEN
      TRY
        prjd := snaps.getSnapshot(name);
        type := "snapshot ";
        data := prjd.snapshotText(name);
      EXCEPT
        Snapshots.Error(e) =>
        Msg.Fatal("snaps directory or snaps/snaps.idx corrupt: " & e);
      | PrjDesc.Error(e) =>
        Msg.Fatal("snaps directory or snaps/snaps.idx corrupt: " & e);
      END;
    ELSIF snaps.changeSetDefined(name) THEN
      TRY
        cs := snaps.getChangeSet(name);
        type := "change set ";
        data := cs.toText();
      EXCEPT
        Snapshots.Error(e) =>
        Msg.Fatal("snaps directory or snaps/snaps.idx corrupt: " & e);
      END;
    ELSE
      Msg.Fatal("no release, snapshot, or change set named " & name & 
        " defined");
    END;
    IF FSUtils.Exists(fileName) THEN
      IF NOT FSUtils.IsFile(fileName) THEN
        Msg.Fatal(fileName & " exists and is no ordinary file");
      END;
      WITH msg = "The file `" & fileName &
             "' already exists.\n" &
             "Overwrite" DO
        IF NOT force AND NOT PkgVC.confirmation.okay(msg) THEN
          Msg.Fatal("aborted");
        END;
      END;
    END;
    TRY
      IF NOT noAction THEN
        FSUtils.PutFile(fileName, data);
      END;
    EXCEPT
      FSUtils.E(e) => Msg.Fatal(e);
    END;
    Msg.V(type & name & " saved in file " & fileName);
  END ExecExport;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecImport() =
  VAR
    data   :  TEXT;
    prjd   :  PrjDesc.T;
    type   :  TEXT;
    rd     :  Rd.T;
    log    :  TEXT := NIL;
    lb     := OSSpecials.LineBreak;
    action := "new ";
  BEGIN
    IF NOT FSUtils.Exists(fileName) THEN
      Msg.Fatal("no file " & fileName);
    END;
    IF NOT FSUtils.IsFile(fileName) THEN
      Msg.Fatal(fileName & "is no ordinary file");
    END;
    TRY
      data := FSUtils.FileContents(fileName);
    EXCEPT
      FSUtils.E(e) => Msg.Fatal(e);
    END;
    IF TextUtils.Pos(TextUtils.SkipLeft(data), "changeset") = 0 THEN
      (* import a change set *)
      type := "change set";
      IF snaps.changeSetDefined(name) THEN
        WITH msg = "The change set `" & name &
             "' already exists.\n" &
             "Overwrite" DO
          IF NOT force AND NOT PkgVC.confirmation.okay(msg) THEN
            Msg.Fatal("aborted");
          END;
        END;
        action := "changed ";
      END;
      WITH cs = NEW(ChangeSet.T).init(name) DO
        TRY
          cs.parse(TextRd.New(data), "<in-memory copy>");
          Msg.V("import change set " & name);
          IF NOT noAction THEN
            snaps.putChangeSet(name, cs, ovwr := TRUE);
          END;
        EXCEPT
          ChangeSet.Error(e) => 
          Msg.Fatal("cannot parse change set description: " & e);
        | Snapshots.Error(e) => 
          Msg.Fatal("cannot overwrite change set description: " & e);
        END;
      END;
    ELSE
      (* import a snapshot or release *)
      type := "snapshot or release configuration";
      TRY
        prjd := NEW(PrjDesc.T).init(NIL, cfg, FALSE, 
                                    collectionroot, inCheckpFileName,
                                    FALSE, env,
                                    pkgvcAcc := pkgvcCreator,
                                    verboseCacheMsgs := verboseCache,
                                    preferredPkgKind := packageKind);
        prjdesc.defineGlobalVars(env);
        prjdesc.defineGlobalVars(vars);
        rd := TextRd.New(data);
        prjd.parse(rd);
        prjd.setModificationDate(Time.Now());
        TRY Rd.Close(rd) EXCEPT ELSE END;
        IF prjd.release(name) # NIL THEN
          type := "release configuration";
          IF snaps.releaseDefined(name) THEN
            WITH msg = "The release `" & name &
                 "' already exists.\n" &
                 "Overwrite" DO
              IF NOT force AND NOT PkgVC.confirmation.okay(msg) THEN
                Msg.Fatal("aborted");
              END;
            END;
            action := "changed ";
          END;
          Msg.V("import release configuration " & name);
          IF NOT noAction THEN
            snaps.putRelease(name, prjd, TRUE);
          END;
        ELSIF prjd.snapshot(name) # NIL THEN
          type := "snapshot configuration";
          IF snaps.snapshotDefined(name) THEN
            WITH msg = "The snapshot `" & name &
                 "' already exists.\n" &
                 "Overwrite" DO
              IF NOT force AND NOT PkgVC.confirmation.okay(msg) THEN
                Msg.Fatal("aborted");
              END;
            END;
            action := "changed ";
          END;
          Msg.V("import snapshot configuration " & name);
          IF NOT noAction THEN
            snaps.putSnapshot(name, prjd, TRUE);
          END;
        ELSE
          VAR
            rels := prjd.releases();
            snaps := prjd.snapshots();
            relText := "<none>";
            snapText := "<none>";
          BEGIN
            IF rels.size() > 0 THEN
              relText := TextUtils.TextSeqToText(rels);
            END;
            IF snaps.size() > 0 THEN
              snapText := TextUtils.TextSeqToText(snaps);
            END;
            Msg.Fatal("no snapshot or release configuration `" & name &
              "' in file " & fileName & 
              "\nincluded releases: " & relText &
              "\nincluded snapshots: " & snapText);
          END;
        END;
      EXCEPT
        PrjDesc.Error(e) => 
          Msg.Fatal("cannot parse " & type & " description: " & e);
      | Snapshots.Error(e) => 
        Msg.Fatal("cannot overwrite " & type & " description: " & e);
      END;
    END;
    IF useVC AND NOT noAction THEN
      IF commitMsg = NIL THEN
        log := 
          action & type & " " & name &":" & lb & lb &
          "PKG: Please enter a local commit message for " & type &
          lb & "PKG: " & name &"." & lb &
          "PKG: If you have made no other changes than the import " &
          "of this " & type & "," & lb &
          "PKG: the above message will probably be appropriate. " & lb;
        WITH editor = CompactRC.GetValue(env, "editor") DO
          log := PkgVCUtils.GetMessage(editor, NIL, msg := log,
                                       failIfUnchanged := FALSE);
        END;
        IF log # NIL THEN
          commitMsg := log;
        END;
      ELSIF commitMsg # NIL AND 
        NOT TextUtils.Contains(commitMsg, name) THEN
        commitMsg := 
          action & type & " " & name &":" & lb & lb &
          commitMsg & lb;
      END;
      CommitLocalFiles(PkgVC.CommitType.Minor);
    END;
  END ExecImport;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowSnapshots() =
  BEGIN
    TRY
      OutTextSeq("all recorded project snapshots:", 
                 snaps.listSnapshots(sort, sortUp));
    EXCEPT
      Snapshots.Error(e) =>
      Msg.Fatal("cannot list snapshots: " & e, 1000);
    END;
  END ExecShowSnapshots;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowReleases() =
  BEGIN
    TRY
      OutTextSeq("all released project configurations:",
                 snaps.listReleases(sort, sortUp));
    EXCEPT
      Snapshots.Error(e) =>
      Msg.Fatal("cannot list releases: " & e, 1000);
    END;
  END ExecShowReleases;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowChangeSets() =
  BEGIN
    TRY
      OutTextSeq("all recorded project change sets:", 
                 snaps.listChangeSets(sort, sortUp));
    EXCEPT
      Snapshots.Error(e) =>
      Msg.Fatal("cannot list change sets: " & e, 1000);
    END;
  END ExecShowChangeSets; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowChangeSetLog() =

  PROCEDURE ShowOne(cs : ChangeSet.T) =
    BEGIN
      M("----------------------------");
      M(cs.logText());
    END ShowOne;

  VAR
    csByName : SortedTextChangeSetTbl.T;
    nameIter : SortedTextChangeSetTbl.Iterator;
    csByDate : SortedTimeChangeSetTbl.T;
    dateIter : SortedTimeChangeSetTbl.Iterator;
    name     : TEXT;
    date     : Time.T;
    cs       : ChangeSet.T;
  BEGIN
    TRY
      IF sort = Snapshots.Sort.ByName THEN
        csByName := snaps.changeSetsByName();
        nameIter := csByName.iterateOrdered(sortUp);
        WHILE nameIter.next(name, cs) DO
          ShowOne(cs);
        END;
      ELSE
        csByDate := snaps.changeSetsByDate();
        dateIter := csByDate.iterateOrdered(sortUp);
        WHILE dateIter.next(date, cs) DO
          ShowOne(cs);
        END;
      END;
    EXCEPT
      Snapshots.Error(e) => Msg.Error(e);
    END;
  END ExecShowChangeSetLog; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowSnapshotOrReleaseLog() =

  PROCEDURE ShowOne(prjd : PrjDesc.T) =
    BEGIN
      M("----------------------------");
      M(prjd.logText());
    END ShowOne;

  VAR
    prjdByName : SortedTextPrjDescTbl.T;
    nameIter   : SortedTextPrjDescTbl.Iterator;
    prjdByDate : SortedTimePrjDescTbl.T;
    dateIter   : SortedTimePrjDescTbl.Iterator;
    name       : TEXT;
    date       : Time.T;
    prjd       : PrjDesc.T;
  BEGIN
    TRY
      IF sort = Snapshots.Sort.ByName THEN
        IF action = Action.SnapshotLog THEN
          prjdByName := snaps.snapshotsByName();
        ELSE
          prjdByName := snaps.releasesByName();
        END;
        nameIter := prjdByName.iterateOrdered(sortUp);
        WHILE nameIter.next(name, prjd) DO
          ShowOne(prjd);
        END;
      ELSE
        IF action = Action.SnapshotLog THEN
          prjdByDate := snaps.snapshotsByDate(sortByModificationDate);
        ELSE
          prjdByDate := snaps.releasesByDate(sortByModificationDate);
        END;
        dateIter := prjdByDate.iterateOrdered(sortUp);
        WHILE dateIter.next(date, prjd) DO
          ShowOne(prjd);
        END;
      END;
    EXCEPT
      Snapshots.Error(e) => Msg.Error(e);
    END;
  END ExecShowSnapshotOrReleaseLog; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowModified() =
  VAR
    res  : INTEGER;
    pkgs : TextSeq.T;
  BEGIN
    pkgs := GetAndCheckPackages();
    TRY
      res := prjdesc.applyToPackages("checkmodified", NIL, NIL,
                                     pkgs,
                                     ordered := FALSE, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := FALSE,
                                     breakOnFailure := stopOnFailures);
    EXCEPT
      PrjDesc.Error(e) => WriteCheckpoint();
      Msg.Fatal("check terminated with exception: " & e, 1000);
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecShowModified; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowOutOfDate() =
  VAR
    res  : INTEGER;
    pkgs : TextSeq.T;
  BEGIN
    pkgs := GetAndCheckPackages();
    TRY
      res := prjdesc.applyToPackages("checkuptodate", NIL, NIL,
                                     pkgs,
                                     ordered := FALSE, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := FALSE,
                                     breakOnFailure := stopOnFailures);
    EXCEPT
      PrjDesc.Error(e) => WriteCheckpoint();
      Msg.Fatal("check terminated with exception: " & e, 1000);
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecShowOutOfDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowConflicts() =
  VAR
    res  : INTEGER;
    pkgs : TextSeq.T;
  BEGIN
    pkgs := GetAndCheckPackages();
    TRY
      res := prjdesc.applyToPackages("checkconflicts", NIL, NIL,
                                     pkgs,
                                     ordered := FALSE, 
                                     breakOnZeroReturn := FALSE, 
                                     breakOnError := FALSE,
                                     breakOnFailure := stopOnFailures);
    EXCEPT
      PrjDesc.Error(e) => WriteCheckpoint();
      Msg.Fatal("check terminated with exception: " & e, 1000);
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecShowConflicts; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowUpdateSequence() =
  BEGIN
    ShowUpdateSequence();
    WriteCheckpoint();
    Process.Exit(0);
  END ExecShowUpdateSequence;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecNewStateCache() =
  BEGIN
    Msg.V("re-initializing state cache...");
    TRY
      prjdesc.newCheckpoint(outCheckpFileName);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("cannot create new checkpoint: " &
        e, 1000);
    END;
    Process.Exit(0);
  END ExecNewStateCache; 
      
(*---------------------------------------------------------------------------*)
PROCEDURE ExecPurgeUnsureVersionInfo() =
  BEGIN
    Msg.V("invalidating unsure version info in cache...");
    TRY
      prjdesc.invalidateCachedUnsureVersionInfo();
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("cannot clean state cache: " &
        e, 1000);
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecPurgeUnsureVersionInfo; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecPurgeBuildInfo() =
  BEGIN
    Msg.V("invalidating build information in cache...");
    TRY
      prjdesc.invalidateCachedBuildInfo();
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("cannot clean state cache: " &
        e, 1000);
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecPurgeBuildInfo; 

(*---------------------------------------------------------------------------*)
PROCEDURE PurgeUnsureVersionInfoIfNotLazy() =
  BEGIN
    IF lazy OR NOT useStateCache THEN RETURN END;
    TRY
      prjdesc.invalidateCachedUnsureVersionInfo();
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("cannot clean state cache: " &
        e, 1000);
    END;
  END PurgeUnsureVersionInfoIfNotLazy; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowStateCache() =
  VAR cpt : TEXT;
  BEGIN
    IF NOT useStateCache THEN
      Msg.Fatal("Show state cache cannot be used together with -nocache", 3);
    END;
    WITH cp = prjdesc.getPoolSet().cachedState() DO
      TRY
        cpt := cp.toText();
        M(cpt);
      EXCEPT ELSE
        Msg.Error("cannot convert checkpoint to text");
      END;
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecShowStateCache; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecShowStatus() =
  VAR 
    cp   : Checkpoint.T;
    dir  : TEXT;
    pkg  : TEXT;
    tag  : TEXT;
    line : TEXT;
    dirs : TextSeq.T;
    attr : Checkpoint.AttrSet;

  PROCEDURE VCAttrToText(attr : Checkpoint.AttrSet) : TEXT = 
    VAR res := "";
    BEGIN
      IF Checkpoint.Attr.Changed IN attr THEN
        res := res & " changed";
      END;
      IF Checkpoint.Attr.Modified IN attr THEN
        res := res & " modified";
      END;
      IF Checkpoint.Attr.UpToDate IN attr THEN
        res := res & " up-to-date";
      END;
      IF Checkpoint.Attr.OutOfDate IN attr THEN
        res := res & " out-of-date";
      END;
      IF Checkpoint.Attr.Conflicts IN attr THEN
        res := res & " conflicts";
      END;
      IF Checkpoint.Attr.IsRelease IN attr THEN
        res := res & " release";
      END;
      RETURN res;
    END VCAttrToText;

  PROCEDURE OtherAttrToText(attr : Checkpoint.AttrSet) : TEXT = 
    VAR res := "";
    BEGIN
      IF Checkpoint.Attr.DepMade IN attr THEN
        res := res & " depend-done";
      END;
      IF Checkpoint.Attr.BuildOkL IN attr THEN
        res := res & " built-locally";
      END;
      IF Checkpoint.Attr.BuildOk IN attr THEN
        res := res & " built";
      END;
      IF Checkpoint.Attr.BuildFailed IN attr THEN
        res := res & " build-failed";
      END;
      IF Checkpoint.Attr.ShippedToLP IN attr THEN
        res := res & " shipped-to-local-pool";
      END;
      IF Checkpoint.Attr.ShippedToPP IN attr THEN
        res := res & " shipped-to-project-pool";
      END;
      IF Checkpoint.Attr.ShippedToGP IN attr THEN
        res := res & " shipped-to-global-pool";
      END;
      RETURN res;
    END OtherAttrToText;

  VAR
    pkgSet : PoolSet.T;
  BEGIN
    IF NOT useStateCache THEN
      Msg.Fatal("Show status is a cache operation, you cannot use it " &
        "together with -nocache", 3);
    END;
    pkgSet := prjdesc.getPoolSet();
    cp := pkgSet.cachedState();
    dirs := cp.dirs();
    FOR i := 0 TO dirs.size() - 1 DO
      dir := dirs.get(i);
      pkg := Pathname.Last(dir);
      TRY
        tag := cp.getVal(dir, "current-tag");
        IF tag = NIL THEN
          EVAL pkgSet.getAndCacheVersionState(pkg);
          tag := cp.getVal(dir, "current-tag");
        END;
        attr := cp.getAttr(dir);
        line := pkg & ": " & tag & VCAttrToText(attr);
        IF action = Action.ShowLongStatus THEN
          line := line & OtherAttrToText(attr);
        END;
        M(line);
      EXCEPT
        Checkpoint.Error(e) => 
        Msg.Error("cannot get attributes of package " &
          pkg & ": " & e);
      | PoolSet.Error(e) => 
        Msg.Error("cannot update version state cache of package " &
          pkg & ": " & e);
      END;
    END;
    WriteCheckpoint();
    Process.Exit(0);
  END ExecShowStatus;  

(*---------------------------------------------------------------------------*)
PROCEDURE ExecListPackageKinds() =
  (* List all defined packages kinds (from PkgBase.DefaultData). *)
  BEGIN
    Msg.V("listing all defined package kinds:");
    VAR l := cfg.kindList(); BEGIN
      WHILE l # NIL DO
        M(l.head);
        l := l.tail;
      END;
    END;
  END ExecListPackageKinds;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecDumpAllPackageKinds() =
  BEGIN
    M(cfgData);
  END ExecDumpAllPackageKinds;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteCheckpoint() =
  BEGIN
    IF NOT useStateCache THEN RETURN END;
    IF prjdesc = NIL THEN
      Msg.V("no project description read, not checkpoint update");
      RETURN;
    END;
    TRY
      prjdesc.writeCheckpoint(outCheckpFileName);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal("cannot write checkpoint file: " & e);
    END;
  END WriteCheckpoint;

(*---------------------------------------------------------------------------*)
PROCEDURE Rename(from, to : Pathname.T) =
  BEGIN
    Msg.V("renaming " & from & " to " & to & "...");
    TRY
      FS.Rename(from, to);
    EXCEPT
      OSError.E => Msg.Fatal("cannot rename file " & from & " to " & to);
    END;
  END Rename;

(*---------------------------------------------------------------------------*)
PROCEDURE PrjDescLoadHead() =
  BEGIN
    Msg.V("parsing project description file " & prjFileName & "...");
    actPrjFileName := prjFileName;
    TRY
      prjdesc := NEW(PrjDesc.T).init(actPrjFileName, cfg, FALSE, 
                                     collectionroot, inCheckpFileName,
                                     useStateCache, env,
                                     pkgvcAcc := pkgvcCreator,
                                     verboseCacheMsgs := verboseCache,
                                     preferredPkgKind := packageKind,
                                     depsMandatory := depsMandatory,
                                     cacheEarly := cacheEarly);
      prjdesc.defineGlobalVars(env);
      prjdesc.defineGlobalVars(vars);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal(e, 3);
    END;
    IF FSUtils.IsFile(ActStateFN) THEN
      Rename(ActStateFN, OldStateFN);
    END;
  END PrjDescLoadHead;

(*---------------------------------------------------------------------------*)
PROCEDURE PrjDescActState() =
  BEGIN
    Msg.V("parsing project description file " & ActStateFN & "...");
    actPrjFileName := ActStateFN;
    TRY
      prjdesc := NEW(PrjDesc.T).init(actPrjFileName, cfg, FALSE, 
                                     collectionroot, inCheckpFileName,
                                     useStateCache, env,
                                     pkgvcAcc := pkgvcCreator,
                                     verboseCacheMsgs := verboseCache,
                                     preferredPkgKind := packageKind,
                                     depsMandatory := depsMandatory,
                                     cacheEarly := cacheEarly);
      prjdesc.defineGlobalVars(env);
      prjdesc.defineGlobalVars(vars);
    EXCEPT
      PrjDesc.Error(e) => Msg.Fatal(e, 3);
    END;
  END PrjDescActState;

(*---------------------------------------------------------------------------*)
PROCEDURE PrjDescOldState() = <* NOWARN *>
  BEGIN
    Rename(OldStateFN, ActStateFN);
    PrjDescActState();
  END PrjDescOldState;

(*---------------------------------------------------------------------------*)
BEGIN (* Main *)
  Msg.tFlag := TRUE;

  PreEvalArguments();
  Msg.V("initializing global variables...");
  InitGlobalVars();

  Msg.V("evaluating command line arguments...");
  EvalArguments();

  (* disabled in for CM3
  IF DemoCheck1.IsDemoVersion() THEN
    IF (Msg.vFlag OR Msg.dFlag OR Msg.tFlag) THEN
      DemoCheck1.Message();
    END;
  ELSE
    IF NOT Release.KeyCheck(MiniEnv.pass) THEN
      Msg.Fatal("invalid passphrase");
    END;
  END;
  *)

  IF action IN NoCacheAction THEN
    useStateCache := FALSE;
  END;

  IF useStateCache THEN
    Msg.V("using checkpoint file " & outCheckpFileName);
  END;

  IF action = Action.Checkout THEN
    IF useStateCache THEN
      updateStateCache := TRUE;
    END;
    useStateCache := FALSE;
  END;

  IF useInternalVC THEN
    pkgvcCreator := NEW(PoolSet.PkgVCCreator);
    pkgvcCreator.env := env;
    pkgvcCreator.msgif := NIL;
  ELSE
    pkgvcCreator := NIL;
  END;

  IF packageKind = NIL THEN
    packageKind := defaultPackageKind;
  END;

  IF action IN PreParseAction THEN
    IF FSUtils.IsFile(ActStateFN) THEN
      Msg.V("using project description from file " & ActStateFN);
      PrjDescActState();
    ELSE
      Msg.V("using project description from file " & prjFileName);
      PrjDescLoadHead()
    END;
  END;
  IF NOT action IN NoSnapsAction THEN
    InitSnapshots();
  END;

  IF FSUtils.IsFile(prjMagicFile) THEN
    Msg.Warning("Found an obsolete project magic file, will ignore it...");
  END;

  IF prjdesc # NIL THEN
    Msg.V("the root of all package collections is " & 
      prjdesc.collectionPath());
    IF packageKind # NIL THEN
      Msg.V("setting preferred package kind to " & packageKind);
      prjdesc.setPreferredPkgKind(packageKind);
    END;
  END;

  IF action IN PreCheckAction THEN
    IF prjdesc.missingPackages() # NIL THEN
      Msg.V("checking out missing packages...");
      TRY
        prjdesc.checkoutPackages(prjdesc.missingPackages());
      EXCEPT
        PrjDesc.Error(e) => Msg.Error("checkout failed: " & e);
      END;
    END;
    Msg.V("checking all packages...");
    VAR res : TEXT; BEGIN
      IF NOT prjdesc.pkgsOkay(res) THEN
        Msg.Fatal(res, 4);
      END;
    END;
  END;

  IF externalShell # NIL AND prjdesc # NIL THEN
    Msg.V("setting external shell to " & externalShell & "...");
    prjdesc.setExternalShell(externalShell);
  END;

  IF (action IN PreDepAction OR Msg.vFlag OR modifiedAndDeps OR
      outOfDateAndDeps OR dependendPkgs OR useStateCache) AND 
    action # Action.Checkout AND action # Action.CommitLocalFiles AND
    action # Action.EditChangeSet AND prjdesc # NIL
   THEN
    CreateDependencyGraph();
  END;

  IF Msg.vFlag AND action # Action.Checkout AND 
     action # Action.CommitLocalFiles AND
     action # Action.EditChangeSet AND 
     prjdesc # NIL THEN
    CheckImports();
  END;

  IF onlyModified AND prjdesc # NIL THEN
    Msg.V("looking for locally modified packages...");
    TRY
      IF modifiedAndDeps THEN
        modifiedPkgs := prjdesc.modifiedAndDependingPackages();
      ELSE
        modifiedPkgs := prjdesc.modifiedPackages();
      END;
    EXCEPT 
      PrjDesc.Error(e) => Msg.Fatal("test failed with exception: " &
        e & ", some packages may be modified", 1000);
    END;
  ELSIF onlyOutOfDate AND prjdesc # NIL THEN
    Msg.V("looking for out-of-date packages...");
    PurgeUnsureVersionInfoIfNotLazy();
    TRY
      IF outOfDateAndDeps THEN
        outOfDatePkgs := prjdesc.outOfDateAndDependingPackages();
      ELSE
        outOfDatePkgs := prjdesc.outOfDatePackages();
      END;
    EXCEPT 
      PrjDesc.Error(e) => Msg.Fatal("test failed with exception: " &
        e & ", some packages may be out-of-date", 1000);
    END;
  END;

  IF prjdesc # NIL THEN
    IF tag1 # NIL THEN
      IF Text.Equal(tag1, "head") THEN
        tag1prjdesc := prjdesc;
        tag1vals := tag1prjdesc.snapshot(tag1);
      ELSE
        TRY
          tag1prjdesc := snaps.getSnapshot(tag1);
          tag1vals := tag1prjdesc.snapshot(tag1);
        EXCEPT
          Snapshots.Error =>
          TRY
            tag1prjdesc := snaps.getRelease(tag1);
            tag1vals := tag1prjdesc.release(tag1);
          EXCEPT
            Snapshots.Error(e) => Msg.Error(e);
          END;
        END;
      END;
    END;
    IF tag2 # NIL THEN
      IF Text.Equal(tag2, "head") THEN
        tag2prjdesc := prjdesc;
        tag2vals := tag2prjdesc.snapshot(tag2);
      ELSE
        TRY
          tag2prjdesc := snaps.getSnapshot(tag2);
          tag2vals := tag2prjdesc.snapshot(tag2);
        EXCEPT
          Snapshots.Error =>
          TRY
            tag2prjdesc := snaps.getRelease(tag2);
            tag2vals := tag2prjdesc.release(tag2);
          EXCEPT
            Snapshots.Error(e) => Msg.Error(e);
          END;
        END;
      END;
    END;
  END;
  IF changeSetName = NIL AND 
    (action = Action.MergeChangeSet OR
     action = Action.EditChangeSet) THEN
    IF nTargets > 0 THEN
      changeSetName := targets.get(0);
    END;
  END;
  IF changeSetName # NIL AND NOT action IN PkgCommitAction THEN
    TRY
      changeSet := snaps.getChangeSet(changeSetName);
    EXCEPT
      Snapshots.Error(e) => Msg.Fatal(e)
    END;
  END;

  Msg.V("executing user command...");
  IF action IN PredicateAction THEN
    ExecPredicate();
  ELSIF action IN BuildAction THEN
    ExecBuildAction();
  ELSIF action IN CleanAction THEN
    ExecCleanAction();
  ELSIF action IN ShipAction THEN
    ExecShipAction();
  ELSIF action IN DiffAction THEN
    ExecDiffAction();
  ELSIF action IN PkgCommitAction THEN
    ExecPackageCommitAction();
  ELSIF action IN ApplyCmdListAction THEN
    ExecApplyCmdList();
  ELSIF action IN ApplyAction THEN
    ExecApplyAction();
  ELSIF action IN SelectByCmdListAction THEN
    ExecSelectByCmdList();
  ELSIF action IN PkgFreezeAction THEN
    ExecFreezeAction();
  ELSIF action = Action.StableRelease THEN
    ExecMakeStableRelease();
  ELSIF action = Action.Checkout THEN
    ExecCheckout();
  ELSIF action = Action.CommitLocalFiles THEN
    ExecCommitLocalFiles();
  ELSIF action = Action.CheckImports THEN
    CheckImports();
  ELSIF action = Action.CheckState THEN
    CheckStateLabel();
  ELSIF action = Action.DependingNodes THEN
    ExecShowDependencies();
  ELSIF action = Action.ShowPackages THEN
    ExecShowPackages();
  ELSIF action = Action.ShowSrcDirectories THEN
    ExecShowSrcDirectories();
  ELSIF action = Action.ShowPackagePaths THEN
    ExecShowPackagePaths();
  ELSIF action = Action.ShowPackageKinds THEN
    ExecShowPackageKinds();
  ELSIF action = Action.ShowModified THEN
    ExecShowModified();
  ELSIF action = Action.ShowOutOfDate THEN
    ExecShowOutOfDate();
  ELSIF action = Action.ShowConflicts THEN
    ExecShowConflicts();
  ELSIF action = Action.ShowUpdateSequence THEN
    ExecShowUpdateSequence();
  ELSIF action = Action.ShowSnapshot OR action = Action.ShowRelease THEN
    ExecShowSnapshotOrRelease();
  ELSIF action = Action.EditSnapshot OR action = Action.EditRelease THEN
    ExecEditSnapshotOrRelease();
  ELSIF action = Action.ShowSnapshots THEN
    ExecShowSnapshots();
  ELSIF action = Action.ShowReleases THEN
    ExecShowReleases();
  ELSIF action = Action.ShowChangeSets THEN
    ExecShowChangeSets();
  ELSIF action = Action.ChangeSetLog THEN
    ExecShowChangeSetLog();
  ELSIF action = Action.SnapshotLog OR action = Action.ReleaseLog THEN
    ExecShowSnapshotOrReleaseLog();
  ELSIF action = Action.EditChangeSet THEN
    ExecEditChangeSet();
  ELSIF action = Action.MergeChangeSet THEN
    ExecMergeChangeSet();
  ELSIF action = Action.ShowStateCache THEN
    ExecShowStateCache();
  ELSIF action = Action.ShowShortStatus THEN
    ExecShowStatus();
  ELSIF action = Action.ShowLongStatus THEN
    ExecShowStatus();
  ELSIF action = Action.NewStateCache THEN
    ExecNewStateCache();
  ELSIF action = Action.PurgeUnsureVersionInfo THEN
    ExecPurgeUnsureVersionInfo();
  ELSIF action = Action.PurgeBuildInfo THEN
    ExecPurgeBuildInfo();
  ELSIF action = Action.ListKinds THEN
    ExecListPackageKinds();
  ELSIF action = Action.DumpKinds THEN
    ExecDumpAllPackageKinds();
  ELSIF action = Action.CreatePkgOvrFiles THEN
    ExecCreatePkgOvrFiles();
  ELSIF action = Action.BuiltOk THEN
    EverythingBuiltOkay();
  ELSIF action = Action.Import THEN
    ExecImport();
  ELSIF action = Action.Export THEN
    ExecExport();
  ELSE
    Msg.Fatal("Sorry, this seems to be not yet implemented", 1001);
  END;

  WriteCheckpoint();
  Process.Exit(0);
END ProjectManager.
