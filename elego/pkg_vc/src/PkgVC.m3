(*---------------------------------------------------------------------------*)
MODULE PkgVC;

IMPORT TextSeq, TextTextTbl, Rd, Wr, FileWr, Text, TextWr, Env,
       Process, OSError, Pathname, FileRd, TextRd, TextTextSeqTbl, Thread,
       Time, FmtTime, TextTupleSeq;
IMPORT FSUtils, Tag, TagSort, TagSeq, TextUtils, DirStack, TextReadingUtils,
       CVS, FileObj, FileRevisionSeq, MsgX, MsgIF, Confirmation, PathRepr,
       CVSLockInfo, CVSLockInfoSeq, TextLockInfoTbl, OSSpecials;
IMPORT APN AS APN, APNSeq AS APNSeq,
       APNHashTbl AS APNHashTbl;
IMPORT TextExtras AS TextEx;
IMPORT PkgVCUtils;
IMPORT (* FSFixed AS *) FS;

(*---------------------------------------------------------------------------*)
REVEAL T = Public BRANDED OBJECT 
    mu         : MUTEX;
    pkgRoot    : APN.T;
    pkgName    : TEXT;
    env        : TextTextTbl.T;
    editor     : TEXT;
    repository : TEXT;
    dcvsrepo   : TEXT;
    usedrepo   : TEXT;
    user       : TEXT;
    cvs        : CVS.T;
    vcignore   : TEXT;
    msgif      : MsgIF.T;
    locking    : LockType;
    createNewDirs : BOOLEAN;
    pruneEmptyDirs : BOOLEAN;
    allVersionControlledFiles : APNSeq.T;
    preferDCVS : BOOLEAN := FALSE;
    isDcvsRepo : BOOLEAN := FALSE;
    useDcvsChangeSets : BOOLEAN := FALSE;
  METHODS
    updateI(tag : Tag.T) RAISES {E} := UpdateI;
    normalizedPkgFN(fn : APN.T) : APN.T RAISES {E} := NormalizedPkgFN;
    useDCVS(): BOOLEAN := UseDCVS;
  OVERRIDES
    init := Init;
    flushCache := FlushCache;
    ignorePatterns := IgnorePatterns;
    packageRelativePathname := PackageRelativePathname;
    setPackageRoot := SetPackageRoot;
    setEnvironment := SetEnvironment;
    getEnvironment := GetEnvironment;
    setMode := SetMode;
    lockingScheme := LockingScheme;
    newCollection := NewCollection;
    checkout := Checkout;
    update := Update;
    commitChanges := CommitChanges;
    commitRelease := CommitRelease;
    commitToChangeBranch := CommitToChangeBranch;
    tagAll := TagAll;
    merge := Merge;
    log := Log;
    niceLog := NiceLog;
    diff := Diff;
    annotate := Annotate;
    add := Add ;
    remove := Remove;
    setLocking := SetLocking;
    lockForEdit := LockForEdit;
    revert := Revert;
    editors := Editors;
    editorInfo := EditorInfo;
    lockedByMe := LockedByMe;
    lockedByOther := LockedByOther;
    known := Known;
    upToDate := UpToDate;
    modified := Modified;
    conflicts := Conflicts;
    tagList := TagList;
    tags := Tags;
    tagExists := TagExists;
    isSticky := IsSticky;
    isRelease := IsRelease;
    isReleaseBranch := IsReleaseBranch;
    latestReleaseBranch := LatestReleaseBranch;
    isChangeBranch := IsChangeBranch;
    latestChangeBranch := LatestChangeBranch;
    currentDevelopmentTag := CurrentDevelopmentTag;
    nextDevelopmentTag := NextDevelopmentTag;
    currentReleaseTag := CurrentReleaseTag;
    nextReleaseTag := NextReleaseTag;
    currentLocalTag := CurrentLocalTag;
    packageDirectory := PackageDirectory;
    overwritePackageDirectory := OverwritePackageDirectory;
    updatePackageDirectory := UpdatePackageDirectory;
    addFiles := AddFiles;
    versionControlledFiles := VersionControlledFiles;
    checkVersionControlStatus := CheckVersionControlStatus;
    getFileStatus := GetFileStatus;
    stateLabels := StateLabels;
    stateLabel := StateLabel;
    setLabel := SetLabel;
    labelLog := LabelLog;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; msgif : MsgIF.T := NIL) : T =
  BEGIN
    self.mu := NEW(MUTEX);
    self.msgif := msgif;
    self.createNewDirs  := TRUE;
    self.pruneEmptyDirs := TRUE;
    self.lastVCMsg := "";
    self.pkgRoot := APN.New(".");
    self.pkgName := "NoPackage";
    self.locking := LockType.Binary;
    self.env := NIL;
    self.editor := NIL;
    self.repository := NIL;
    self.dcvsrepo := NIL;
    self.usedrepo := NIL;
    self.cvs := NEW(CVS.T).init(msgif);
    self.allVersionControlledFiles := NIL;
    self.vcignore := "PkgCDT PkgCRT PkgCT PkgDep PkgOverrides PkgErr" &
                     " .errors .label .labels *~ *.bak *.obj *.o *.a" &
                     " *.gz *.zip *.so *.So *.old *.orig *.rej core *.core " &
                     " *.flc #* .#* ,* *.Z *.io *.mo *.ix *.mx *.so.* .del-*";
    self.user := Env.Get("LOGNAME");
    IF self.user = NIL THEN
      self.user := Env.Get("USER");
    END;
    IF self.user = NIL THEN
      self.user := "unknown";
    END;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE DetectRepository(self: T) =
  BEGIN
    IF self.pkgRoot # NIL THEN
      WITH root = self.pkgRoot.denotation(),
           dcvsRoot = Pathname.Join(Pathname.Join(root, "DCVS"), "Root"),
           cvsRoot = Pathname.Join(Pathname.Join(root, "CVS"), "Root")
       DO
        IF FSUtils.IsFile(dcvsRoot) THEN
          TRY
            self.repository := FSUtils.FileContents(dcvsRoot);
          EXCEPT
            FSUtils.E(e) => 
            MsgX.Error(self.msgif, "cannot read " & dcvsRoot & ": " & e);
          END;
        ELSIF FSUtils.IsFile(cvsRoot) THEN
          TRY
            self.repository := FSUtils.FileContents(cvsRoot);
          EXCEPT
            FSUtils.E(e) => 
            MsgX.Error(self.msgif, "cannot read " & cvsRoot & ": " & e);
          END;
        END
      END;
      IF self.repository # NIL AND 
         Text.GetChar(self.repository, Text.Length(self.repository) - 1) =
         '\n' THEN
        self.repository := TextUtils.RemoveChars(self.repository);
      END;
    END;
  END DetectRepository;

(*---------------------------------------------------------------------------*)
PROCEDURE UseDCVS(self: T): BOOLEAN =
  BEGIN
    RETURN self.isDcvsRepo;
  END UseDCVS;

(*---------------------------------------------------------------------------*)
PROCEDURE FlushCache(self : T) =
  BEGIN
    LOCK self.mu DO
      self.cvs.flushCache();
      self.allVersionControlledFiles := NIL;
    END;
  END FlushCache;

(*---------------------------------------------------------------------------*)
PROCEDURE IgnorePatterns(self : T) : TEXT =
  VAR 
    t  : TEXT := "";
    fn : TEXT;
    default := self.vcignore;
  BEGIN
    fn := Pathname.Join(self.pkgRoot.denotation(), ".vcignore", NIL);
    IF FSUtils.IsFile(fn) THEN
      TRY
        VAR rd := FileRd.Open(fn); patterns : TextSeq.T; BEGIN
          TRY
            t := Rd.GetText(rd, LAST(INTEGER));
          FINALLY
            Rd.Close(rd);
          END;
          patterns := TextReadingUtils.Tokenize(t);
          IF Text.Equal(patterns.get(0), "!") THEN
            default := "";
            EVAL patterns.remlo();
          END;
          t := TextUtils.TextSeqToText(patterns, " ");
        END;  
      EXCEPT ELSE 
      END;
    END;
    RETURN default & " " & t;
  END IgnorePatterns;

(*---------------------------------------------------------------------------*)
PROCEDURE PackageRelativePathname(self : T; pn : Pathname.T) : Pathname.T 
  RAISES {E} =
  VAR
    rpn := PathRepr.Native(self.pkgRoot.denotation());
    i   :  CARDINAL := 0;
  BEGIN
    TRY
      IF NOT Pathname.Absolute(pn) AND NOT FSUtils.Exists(pn) THEN
        WITH nfn = Pathname.Join(self.pkgRoot.denotation(), pn, NIL) DO
          IF FSUtils.Exists(nfn) THEN
            pn := nfn;
          END;
        END;
      END;
      pn := FSUtils.CanonicalPathname(pn);
    EXCEPT
      FSUtils.E(c) => RAISE E(c);
    END;
    IF TextEx.FindSub(pn, rpn, i) AND i = 0 THEN
      RETURN Text.Sub(pn, Text.Length(rpn) + 1, LAST(CARDINAL));
    ELSE
      RAISE E("pathname " & pn & " not contained in package");
    END;
  END PackageRelativePathname;

(*---------------------------------------------------------------------------*)
PROCEDURE NormalizedPkgFN(self : T; fn : APN.T) : APN.T RAISES {E} =
  BEGIN
    IF fn = NIL THEN
      fn := APN.New(PkgTagsFN);
    ELSE
      WITH prp = self.packageRelativePathname(fn.denotation()) DO
        IF Text.Empty(prp) THEN
          RETURN self.pkgRoot;
        ELSE
          fn := APN.New(prp);
        END;
      END;
    END;
    fn := APN.Join(self.pkgRoot, fn, NIL);
    IF FileObj.Exists(fn) THEN
      RETURN fn;
    ELSE
      RETURN self.pkgRoot;
    END;
  END NormalizedPkgFN;

(*---------------------------------------------------------------------------*)
PROCEDURE SetPackageRoot(self : T; rootDir : APN.T) RAISES {E}=
  VAR
    rd  : TEXT;
    ard : TEXT;
  BEGIN
    LOCK self.mu DO
      self.cvs.flushCache();
      self.lastVCMsg := "";
      MsgX.D(self.msgif, "(PkgVC.VC).setPackageRoot(" & rootDir.denotation() &
        ")");
      IF rootDir.isAbsolute() THEN
        self.pkgRoot := rootDir;
      ELSE
        TRY
          rd := rootDir.denotation();
          MsgX.D(self.msgif, "rootDir: " & rd);
          ard := FSUtils.CanonicalPathname(rd);
          MsgX.D(self.msgif, "rootDir absolute: " & ard);
          self.pkgRoot := APN.New(ard);
        EXCEPT 
          FSUtils.E(code) => 
            RAISE E("SetPackageRoot: cannot get " &
                      "absolute pathname: OSError: " & code);
        ELSE
          Process.Crash("Cannot get pathname of package root directory");
        END;
      END;
      self.pkgName := APN.Last(self.pkgRoot).denotation();
      DetectRepository(self);
    END;
  END SetPackageRoot; 

(*---------------------------------------------------------------------------*)
PROCEDURE SetMode(<* UNUSED *> self : T; m : Mode) =
  BEGIN
    IF m = Mode.Test THEN
      CVS.noAction := TRUE;
    ELSE
      CVS.noAction := FALSE;
    END;
  END SetMode;

(*---------------------------------------------------------------------------*)
PROCEDURE SetEnvironment(self : T; env : TextTextTbl.T;
                         repoMapping : TextTupleSeq.T := NIL) =
  VAR val : TEXT;
      workspaceKindDetected : BOOLEAN := FALSE;
  BEGIN
    LOCK self.mu DO
      self.env := env;
      IF env.get("editor", val) THEN
        self.editor := val;
        self.cvs.setCVSEDITOR(val);
        MsgX.D(self.msgif, "setting CVSEDITOR to " & val);
      END;
      IF env.get("create-dcvs-change-sets", val) THEN
        self.useDcvsChangeSets := TextUtils.BoolVal(val);
      END;
      IF env.get("prefer-dcvs", val) THEN
        self.preferDCVS := TextUtils.BoolVal(val, TRUE);
        self.cvs.setPreferCVS(NOT self.preferDCVS);
        IF self.preferDCVS THEN
          MsgX.D(self.msgif, "preferring DCVS to CVS");
        ELSE
          MsgX.D(self.msgif, "preferring CVS to DCVS");
        END;
      END;
      IF self.repository = NIL THEN
        DetectRepository(self);
      END;
      IF self.repository = NIL THEN
        MsgX.D(self.msgif, "no (D)CVS/Root found in package root");
      ELSE
        IF repoMapping = NIL THEN
          MsgX.Warning(self.msgif,
                       "cannot classify workspace for repository " &
                       self.repository & " due to missing mapping");
        ELSE
          FOR i := 0 TO repoMapping.size() - 1 DO
            WITH t = repoMapping.get(i) DO
              IF Text.Equal(t.getFst(), self.repository) THEN
                IF Text.Equal(t.getScd(), "DCVS") OR
                  Text.Equal(t.getScd(), "dcvs") THEN
                  self.dcvsrepo := self.repository;
                  MsgX.D(self.msgif, "DCVS workspace detected");
                  MsgX.D(self.msgif, "setting DCVSROOT to " & self.dcvsrepo);
                  self.preferDCVS := TRUE;
                  self.cvs.setDCVSROOT(self.dcvsrepo);
                  workspaceKindDetected := TRUE;
                  self.isDcvsRepo := TRUE;
                ELSE
                  MsgX.D(self.msgif, "CVS workspace detected");
                  MsgX.D(self.msgif, "setting CVSROOT to " & self.repository);
                  self.preferDCVS := FALSE;
                  self.cvs.setCVSROOT(self.repository);
                  workspaceKindDetected := TRUE;
                  self.isDcvsRepo := FALSE;
                END;
              END;
            END;
          END;
          IF NOT workspaceKindDetected THEN
            MsgX.Warning(self.msgif,
                         "cannot classify workspace for repository " &
                         self.repository & " due to missing mapping element");
          END;
        END;
      END;
      IF workspaceKindDetected THEN
        self.cvs.setPreferCVS(NOT self.preferDCVS);
      ELSE
        IF env.get("repository", val) AND NOT self.preferDCVS THEN
          self.repository := val;
          MsgX.D(self.msgif, "setting CVSROOT to " & val);
          self.cvs.setCVSROOT(val);
          self.usedrepo := val;
        END;
        IF env.get("dcvs-repository", val) AND self.preferDCVS THEN
          self.dcvsrepo := val;
          MsgX.D(self.msgif, "setting DCVSROOT to " & val);
          self.cvs.setDCVSROOT(val);
          IF self.preferDCVS THEN
            self.isDcvsRepo := TRUE;
            self.usedrepo := val;
          END;
        END;
        IF self.repository = NIL AND self.dcvsrepo = NIL THEN
          MsgX.Warning(self.msgif, "No repository definition has been " & 
            "found.\n This is probably due to an incomplete or syntactially " &
            "incorrect\n comapctrc or ~/.compactrc file. Without a correct " &
            "definition of\n the repository, several version control " &
            "actions (like checkout)\n are likely to fail.");
        END;
      END;
      IF env.get("cvspath", val) THEN
        self.cvs.setCVSPath(APN.New(val));
        MsgX.D(self.msgif, "setting cvspath to " & val);
      END;
      IF env.get("dcvspath", val) THEN
        self.cvs.setDCVSPath(APN.New(val));
        MsgX.D(self.msgif, "setting dcvspath to " & val);
      END;
      IF env.get("vcignore", val) THEN
        self.vcignore := val;
        MsgX.D(self.msgif, "setting vcignore to " & val);
      END;
      IF env.get("vc-ignore", val) THEN
        self.vcignore := val;
        MsgX.D(self.msgif, "setting vcignore to " & val);
      END;
      IF env.get("user", val) THEN
        self.user := val;
        MsgX.D(self.msgif, "setting USER to " & val);
      END;
      IF env.get("vc-locking", val) THEN
        IF Text.Equal(val, "none") OR Text.Equal(val, "lazy") THEN
          self.locking := LockType.None;
        ELSIF Text.Equal(val, "binary") OR Text.Equal(val, "default") THEN
          self.locking := LockType.Binary;
        ELSIF Text.Equal(val, "all") OR Text.Equal(val, "strict") THEN
          self.locking := LockType.All;
        ELSE
          MsgX.Error(self.msgif, "unknown locking scheme: " & val);
        END;
      END;
      IF env.get("vc-options", val) THEN
        WITH options = TextUtils.Split(val, ",") DO
          FOR i := 0 TO options.size() - 1 DO
            WITH option = TextUtils.Compress(options.get(i)) DO
              IF Text.Equal(option, "prune") THEN
                self.pruneEmptyDirs := TRUE;
              ELSIF Text.Equal(option, "noprune") THEN
                self.pruneEmptyDirs := FALSE;
              ELSIF Text.Equal(option, "create") THEN
                self.createNewDirs := TRUE;
              ELSIF Text.Equal(option, "nocreate") THEN
                self.createNewDirs := FALSE;
              (* add more options here *)
              ELSE
                MsgX.Error(self.msgif, "unhandled version control option: " & 
                  option);
              END;
            END;
          END;
        END;
      END;
    END
  END SetEnvironment;

(*---------------------------------------------------------------------------*)
PROCEDURE GetEnvironment(self : T) : TextTextTbl.T =
  BEGIN
    RETURN self.env;
  END GetEnvironment;

(*---------------------------------------------------------------------------*)
PROCEDURE LockingScheme(self : T) : LockType =
  BEGIN
    RETURN self.locking;
  END LockingScheme; 

(*---------------------------------------------------------------------------*)
PROCEDURE NewCollection(self : T; path : APN.T;
                        msg : TEXT := NIL; msgFile : APN.T := NIL) RAISES {E} =
  VAR mfile : TEXT := NIL;
  BEGIN
    IF NOT FileObj.Exists(path) THEN
      FSUtils.MakeDir(path.denotation());
    END;
    IF msgFile # NIL THEN
      mfile := msgFile.denotation();
    END;
    self.cvs.setCVSIgnore(self.ignorePatterns());
    TRY
      IF NOT FileObj.Exists(path) THEN
	RAISE E("Cannot create directories.");
      ELSIF FileObj.IsDir(APN.Join(path, APN.New("CVS"), NIL)) THEN
	RAISE E(path.denotation() & " is already under CVS control");
      ELSIF NOT self.cvs.import(path, "no_vendor_tag", "empty_import", 
                                self.lastVCMsg, msg, mfile) THEN 
	RAISE E("Import failed.");
      END;
    EXCEPT
      CVS.E(m) => RAISE E("CVS import failed: " & m);
    END;
    FSUtils.RemoveDir(path.denotation());
    IF NOT self.cvs.checkout(path, "head", self.lastVCMsg) THEN
      RAISE E("Checkout failed: " & self.lastVCMsg);
    END;
  END NewCollection;

(*---------------------------------------------------------------------------*)
PROCEDURE Checkout(self : T; pkgname : TEXT; tag : Tag.T) RAISES {E} =
  VAR 
    topdir : TEXT;
    arcs   : Pathname.Arcs;
  BEGIN
    TRY
      arcs := Pathname.Decompose(pkgname);
    EXCEPT
      Pathname.Invalid => RAISE E("Invalid pathname: " & pkgname);
    END;
    IF arcs.remlo() # NIL THEN
      RAISE E("Cannot checkout package with absolute pathname");
    END;
    topdir := arcs.remlo();
    LOCK self.mu DO
      self.cvs.setCVSIgnore(self.ignorePatterns());
      WITH pkgpath = APN.New(pkgname) DO
        IF NOT self.cvs.checkout(pkgpath, tag.denotation(FALSE), 
                                 self.lastVCMsg) THEN
          RAISE E("Checkout failed: " & self.lastVCMsg);
        END;
        (* The next line is a workaround for more or less broken versions
           of CVS that leave Entries.Log files and incomplete Entries files 
           in the workspace after initial checkout. *)
        TRY
          EVAL self.cvs.tags(APN.New(topdir), "", local := TRUE);
          EVAL self.cvs.tags(pkgpath, "", local := FALSE);
        EXCEPT
          CVS.E(m) => RAISE E("Checkout failed: " & m);
        END;
      END;
    END;
  END Checkout; 

(*---------------------------------------------------------------------------*)
PROCEDURE SymbolicRevisionName(self : T; tag : Tag.T; avoidHead := FALSE)
  : TEXT RAISES {E} =

  PROCEDURE Head() : TEXT RAISES {E} =
    BEGIN
      IF avoidHead THEN
        RETURN CurrentDevelopmentTag(self).denotation(FALSE);
      ELSE
        RETURN "head";
      END;
    END Head;

  BEGIN
    IF tag = Tag.Head THEN
      RETURN Head();
    ELSIF tag = Tag.Tip THEN
      tag := CurrentLocalTag(self);
      IF tag = Tag.Head THEN
        RETURN Head();
      END;
      IF tag.kind() = Tag.Kind.Release THEN
        IF tag.isStableBranchTag() THEN
          RETURN tag.denotation();
        ELSE
          tag := Tag.NewStableBranch(tag);
          RETURN tag.denotation();
        END;
      ELSIF tag.kind() = Tag.Kind.Devel THEN
        RETURN Head();
      ELSE
        RAISE E("This kind of tag is not yet supported: " & 
              tag.denotation(FALSE));
      END;
    ELSIF tag = Tag.LastTip THEN
      IF NOT LatestReleaseBranch(self, tag) THEN
        RAISE E("cannot get latest release branch tag");
      END;
      RETURN tag.denotation(FALSE);
    ELSE
      RETURN tag.denotation(FALSE);
    END;
  END SymbolicRevisionName;

(*---------------------------------------------------------------------------*)
PROCEDURE Update(self : T; tag : Tag.T) RAISES {E} =
  BEGIN
    UpdateI(self, tag);
  END Update; 

(*---------------------------------------------------------------------------*)
PROCEDURE UpdateI(self : T; tag : Tag.T) RAISES {E} =
  VAR
    rev := SymbolicRevisionName(self, tag);
  BEGIN
    IF NOT self.cvs.update(self.pkgRoot, rev, self.lastVCMsg,
                           self.createNewDirs, self.pruneEmptyDirs) THEN
      RAISE E("Update failed: " & self.lastVCMsg);
    END;
  END UpdateI; 

(*---------------------------------------------------------------------------*)
PROCEDURE CommitChanges(self : T; ct : CommitType; 
                        msg : TEXT := NIL; msgFile : APN.T := NIL) RAISES {E} =
  VAR 
    tag : Tag.T;
    nextDevelTag : Tag.T;
    msgFileName  : TEXT := NIL;
    changeDesc   : TEXT := NIL;
  BEGIN
    LOCK self.mu DO
    self.cvs.setCVSIgnore(self.ignorePatterns());
    IF IsReleaseBranch(self, tag) THEN
      self.lastVCMsg := tag.denotation();
      RAISE E("The package is checked out on a release branch");
    ELSIF msgFile # NIL THEN
      msgFileName := msgFile.denotation();
    END;
    IF NOT UpToDate(self) THEN
      (* Someone else was faster... *)
      WITH t = "This package is not up-to-date.\n" &
               "Shall I update it for you (merge changes " &
               "from the repository)" DO
        IF NOT confirmation.okay(t) THEN
          RAISE E("The local workspace is not up-to-date.");
        END;
      END;
      UpdateI(self, Tag.Head);
      (* Everything has been updated to the head of the main trunk. *)
      TRY
        IF self.cvs.conflicts(self.pkgRoot, self.lastVCMsg) THEN
          RAISE E("There are unresolved version conflicts.");       
        END;
      EXCEPT
        CVS.E(m) => RAISE E("Status check failed: " & m);
      END;
    END;
    (* Everything seems to be up-to-date. *)
    nextDevelTag := NextDevelopmentTag(self, ct);
    TRY
      changeDesc := self.cvs.changeDesc(self.pkgRoot);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
    IF msgFileName = NIL AND msg = NIL THEN
      (* We try to get a commit message before we update the PkgTags
         file, just in case we are interrupted... *)
      msg := PkgVCUtils.GetCommitMessage(self.editor, self.msgif,
                                         changeDesc, self.pkgName);
      IF msg = NIL THEN
        RAISE E("Won't commit without valid log message.");
      END;
    ELSIF msg # NIL AND changeDesc # NIL THEN
      msg := PkgVCUtils.MsgWithoutPkgInfoLines(msg & lb & lb & changeDesc,
                                               self.msgif);
    END;
    TRY
      PkgVCUtils.CheckCommitMsg(msg, msgFileName, self.pkgName, 
                                self.pkgRoot.denotation(), 
                                self.user, self.usedrepo, 
                                "package-commit", nextDevelTag.denotation(),
                                self.env);
    EXCEPT
      PkgVCUtils.E(e) => RAISE E("commit check failed: " & e);
    END;
    UpdateTagFile(self, nextDevelTag);
    TRY
      IF UseDCVS(self) AND self.useDcvsChangeSets THEN
        IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName, 
                               desc := changeDesc,
                               pkg := self.pkgName,
                               changeSetName := nextDevelTag.denotation()) THEN
          RAISE E("Commit command failed");
        END;
      ELSE
        IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName, 
                               desc := changeDesc,
                               pkg := self.pkgName) THEN
          RAISE E("Commit command failed");
        END;
      END;
    EXCEPT
      CVS.E(msg) => RAISE E("Commit aborted: " & msg);
    END;
    MsgX.T(self.msgif, "Tagging everything as " & nextDevelTag.denotation());
    TagAll(self, nextDevelTag);
    END (* lock *);
  END CommitChanges; 

(*---------------------------------------------------------------------------*)
PROCEDURE CommitRelease(self : T; ct : CommitType; 
                        msg : TEXT := NIL; msgFile : APN.T := NIL) RAISES {E} =
  VAR
    tag            : Tag.T;
    branchTag      : Tag.T;
    branchStartTag : Tag.T;
    nextReleaseTag : Tag.T;
    msgFileName    : TEXT := NIL;
    changeDesc     : TEXT := NIL;
  BEGIN
    LOCK self.mu DO
    self.cvs.flushCache();
    self.cvs.setCVSIgnore(self.ignorePatterns());
    IF ct = CommitType.Patch THEN
      IF NOT IsReleaseBranch(self, tag) THEN
        IF NOT LatestReleaseBranch(self, tag) THEN
          (* There never has been a release before. *)
          RAISE E("The package is not checked out on a release branch,\n" &
                  "nor has there ever been a release to patch.");
        END;
        self.lastVCMsg := tag.denotation();
        WITH t = "This package is not checked out on a stable " &
             "release branch.\n" 
               & "Shall I do that for you now" DO
          IF NOT confirmation.okay(t) THEN
            RAISE E("The package is not checked out on a release branch");
          END;
        END;
        (* We have got a usable tag. *)
        UpdateI(self, tag);
        (* Everything has been updated to the branch. *)
        TRY
          IF self.cvs.conflicts(self.pkgRoot, self.lastVCMsg) THEN
            RAISE E("There are unresolved version conflicts.");        
          END;
        EXCEPT
          CVS.E(m) => RAISE E("Status check failed: " & m);
        END;
        (* No conflicts during update *)
        WITH t = self.lastVCMsg & "\n\nContinue with commit" DO
          IF NOT confirmation.okay(t) THEN
            RAISE E("Interrupted by user");
          END;
        END;
        (* commit confirmed *)
      END;
      IF msgFile # NIL THEN
        msgFileName := msgFile.denotation();
      END;
      nextReleaseTag := NextReleaseTag(self, ct, tag);
      TRY
        changeDesc := self.cvs.changeDesc(self.pkgRoot);
      EXCEPT
        CVS.E(m) => RAISE E("Status check failed: " & m);
      END;
      IF msgFileName = NIL AND msg = NIL THEN
        (* We try to get a commit message before we update the PkgTags
           file, just in case we are interrupted... *)
        msg := PkgVCUtils.GetCommitMessage(self.editor, self.msgif,
                                           changeDesc, self.pkgName);
        IF msg = NIL THEN
          RAISE E("Won't release without valid log message.");
        END;
      ELSIF msg # NIL AND changeDesc # NIL THEN
        msg := PkgVCUtils.MsgWithoutPkgInfoLines(msg & lb & lb & changeDesc,
                                                 self.msgif);
      END;
      TRY
        PkgVCUtils.CheckCommitMsg(msg, msgFileName, self.pkgName, 
                                  self.pkgRoot.denotation(), 
                                  self.user, self.usedrepo,
                                  "package-release", 
                                  nextReleaseTag.denotation(),
                                  self.env);
      EXCEPT
        PkgVCUtils.E(e) => RAISE E("commit check failed: " & e);
      END;
      UpdateTagFile(self, nextReleaseTag);
      TRY
        IF UseDCVS(self) AND self.useDcvsChangeSets THEN
          IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName,
                                 desc := changeDesc,
                                 pkg := self.pkgName,
                                 changeSetName :=
                                     nextReleaseTag.denotation()) THEN
            RAISE E("Commit command failed");
          END;
        ELSE
          IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName,
                                 desc := changeDesc,
                                 pkg := self.pkgName) THEN
            RAISE E("Commit command failed");
          END;
        END;
      EXCEPT
        CVS.E(msg) => RAISE E("Commit aborted: " & msg);
      END;
      MsgX.T(self.msgif, "Tagging everything as " & 
        nextReleaseTag.denotation());
      (* tagging confirmed *)
      TagAll(self, nextReleaseTag);
      RETURN;
    END;

    (* Major or minor release commit. We have to add a new stable
       release branch. *)
    IF FilesAddedOrRemoved(self) THEN
      RAISE E("Commit release is currently not possible with added or " &
              "\n            removed files. Try a development commit first.");
    END;
    IF NOT UpToDate(self) THEN
      (* Someone else was faster... *)
      WITH t = "This package is not up-to-date.\n"  & 
           "Shall I update it for you now " & 
           "(merge changes from the repository)" DO
        IF confirmation.okay(t) THEN
          UpdateI(self, Tag.Head);
          (* Everything has been updated to the head of the main trunk. *)
          TRY
            IF self.cvs.conflicts(self.pkgRoot, self.lastVCMsg) THEN
              RAISE E("There are unresolved version conflicts.");       
            END;
          EXCEPT
            CVS.E(m) => RAISE E("Status check failed: " & m);
          END;
          (* Everything seems to be up-to-date. *)
        ELSE
          (* There is no need that everything is up-to-date, as we can
             commit to the branch. *)
        END;
      END;
    END;

    nextReleaseTag := NextReleaseTag(self, ct);
    branchTag := Tag.NewStableBranch(nextReleaseTag);
    branchStartTag := Tag.NewBranchStartTag(nextReleaseTag);
    MsgX.T(self.msgif, "Creating new branch " & branchTag.denotation());
    TagAll(self, branchTag, branch := TRUE);
    MsgX.T(self.msgif, "Tagging everything as " & 
      branchStartTag.denotation());
    TagAll(self, branchStartTag); 
    MsgX.T(self.msgif, "Updating to new branch " & branchTag.denotation());
    UpdateI(self, branchTag);

    IF msgFile # NIL THEN
      msgFileName := msgFile.denotation();
    END;
    TRY
      changeDesc := self.cvs.changeDesc(self.pkgRoot);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
    IF msgFileName = NIL AND msg = NIL THEN
      (* We try to get a commit message before we update the PkgTags
         file, just in case we are interrupted... *)
      msg := PkgVCUtils.GetCommitMessage(self.editor, self.msgif,
                                         changeDesc, self.pkgName);
    ELSIF msg # NIL AND changeDesc # NIL THEN
      msg := PkgVCUtils.MsgWithoutPkgInfoLines(msg & lb & lb & changeDesc,
                                               self.msgif);
    END;
    TRY
      PkgVCUtils.CheckCommitMsg(msg, msgFileName, self.pkgName, 
                                self.pkgRoot.denotation(), 
                                self.user, self.usedrepo,
                                "package-release", 
                                nextReleaseTag.denotation(),
                                self.env);
    EXCEPT
      PkgVCUtils.E(e) => RAISE E("commit check failed: " & e);
    END;
    UpdateTagFile(self, nextReleaseTag);
    MsgX.T(self.msgif, "Committing to new branch " & branchTag.denotation());
    TRY
      IF UseDCVS(self) AND self.useDcvsChangeSets THEN
        IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName,
                               desc := self.cvs.changeDesc(self.pkgRoot),
                               pkg := self.pkgName,
                               changeSetName :=
                                   nextReleaseTag.denotation()) THEN
          RAISE E("Commit command failed");
        END;
      ELSE
        IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName,
                               desc := self.cvs.changeDesc(self.pkgRoot),
                               pkg := self.pkgName) THEN
          RAISE E("Commit command failed");
        END;
      END;
    EXCEPT
      CVS.E(msg) => RAISE E("Commit aborted: " & msg);
    END;
    MsgX.T(self.msgif, "Tagging everything as " & 
      nextReleaseTag.denotation());
    TagAll(self, nextReleaseTag); 
    END (* lock *);
  END CommitRelease; 

(*---------------------------------------------------------------------------*)
PROCEDURE CommitToChangeBranch(self : T;
                               changeName : TEXT; changeType : Tag.Kind;
                               ct : CommitType; newBranch := FALSE;
                               msg : TEXT := NIL; msgFile : APN.T := NIL)
  RAISES {E} =
  VAR
    tag            : Tag.T;
    branchTag      : Tag.T;
    branchStartTag : Tag.T;
    nextChangeTag  : Tag.T;
    newChangeTag   : Tag.T;
    msgFileName    : TEXT := NIL;
    changeDesc     : TEXT := NIL;
  BEGIN
    LOCK self.mu DO
    self.cvs.flushCache();
    self.cvs.setCVSIgnore(self.ignorePatterns());
    IF NOT newBranch THEN
      IF NOT IsChangeBranch(self, tag) THEN
        IF NOT LatestChangeBranch(self, changeType, tag) THEN
          (* There never has been a change before. *)
          RAISE E("The package is not checked out on a change branch,\n" &
                  "nor has there ever been a change/fix/feature to add to.\n" &
                  "Please create an appropriate branch first.");
        END;
        self.lastVCMsg := tag.denotation();
        WITH t = "This package is not checked out on a stable " &
             "change branch.\nShall I do that for you now" DO
          IF NOT confirmation.okay(t) THEN
            RAISE E("The package is not checked out on a change branch.");
          END;
        END;
        (* We have got a usable tag. *)
        MsgX.D(self.msgif, "Update to tag " & tag.debugInfo()); 
        UpdateI(self, tag);
        (* Everything has been updated to the branch. *)
        TRY
          IF self.cvs.conflicts(self.pkgRoot, self.lastVCMsg) THEN
            RAISE E("There are unresolved version conflicts.");        
          END;
        EXCEPT
          CVS.E(m) => RAISE E("Status check failed: " & m);
        END;
        (* No conflicts during update *)
        WITH t = self.lastVCMsg & "\n\nContinue with commit" DO
          IF NOT confirmation.okay(t) THEN
            RAISE E("Interrupted by user");
          END;
        END;
        (* commit confirmed *)
      END;
      IF tag.kind() # changeType THEN
        WITH t = "This package checked out on a branch " &
             Tag.KindToText(tag.kind()) & 
             "\n but you requested a " & Tag.KindToText(changeType) &
             ".\nContinue nonetheless" DO
          IF NOT confirmation.okay(t) THEN
            RAISE E("The package is not checked out on the requested kind" &
                    " of branch.");
          END;
        END;
      END;
      MsgX.D(self.msgif, "tag = " & tag.denotation(FALSE)); 
      IF msgFile # NIL THEN
        msgFileName := msgFile.denotation();
      END;
      nextChangeTag := NextChangeTag(self, changeName, changeType, ct, tag);
      MsgX.D(self.msgif, "nextChangeTag = " & nextChangeTag.denotation()); 
      MsgX.D(self.msgif, nextChangeTag.debugInfo()); 
      TRY
        changeDesc := self.cvs.changeDesc(self.pkgRoot);
      EXCEPT
        CVS.E(m) => RAISE E("Status check failed: " & m);
      END;
      IF msgFileName = NIL AND msg = NIL THEN
        (* We try to get a commit message before we update the PkgTags
           file, just in case we are interrupted... *)
        msg := PkgVCUtils.GetCommitMessage(self.editor, self.msgif,
                                           changeDesc, self.pkgName);
        IF msg = NIL THEN
          RAISE E("Won't change without valid log message.");
        END;
      ELSIF msg # NIL AND changeDesc # NIL THEN
        msg := PkgVCUtils.MsgWithoutPkgInfoLines(msg & lb & lb & changeDesc,
                                                 self.msgif);
      END;
      TRY
        PkgVCUtils.CheckCommitMsg(msg, msgFileName, self.pkgName, 
                                  self.pkgRoot.denotation(), 
                                  self.user, self.usedrepo,
                                  "package-change", 
                                  nextChangeTag.denotation(),
                                  self.env);
      EXCEPT
        PkgVCUtils.E(e) => RAISE E("commit check failed: " & e);
      END;
      UpdateTagFile(self, nextChangeTag);
      TRY
        IF UseDCVS(self) AND self.useDcvsChangeSets THEN
          IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName,
                                 desc := changeDesc,
                                 pkg := self.pkgName,
                                 changeSetName :=
                                     nextChangeTag.denotation()) THEN
            RAISE E("Commit command failed");
          END;
        ELSE
          IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName,
                                 desc := changeDesc,
                                 pkg := self.pkgName) THEN
            RAISE E("Commit command failed");
          END;
        END;
      EXCEPT
        CVS.E(msg) => RAISE E("Commit aborted: " & msg);
      END;
      MsgX.T(self.msgif, "Tagging everything as " & 
        nextChangeTag.denotation());
      (* tagging confirmed *)
      TagAll(self, nextChangeTag);
      RETURN;
    END;

    (* creation of a new change branch. *)
    (* FIXME: Why not?
    IF FilesAddedOrRemoved(self) THEN
      RAISE E("Commit change is currently not possible with added or " &
              "\n            removed files. Try a development commit first.");
    END;
    *)
    IF NOT UpToDate(self) THEN
      (* Someone else was faster... *)
      WITH t = "This package is not up-to-date.\n"  & 
           "Shall I update it for you now " & 
           "(merge changes from the repository)" DO
        IF confirmation.okay(t) THEN
          UpdateI(self, Tag.Head);
          (* Everything has been updated to the head of the main trunk. *)
          TRY
            IF self.cvs.conflicts(self.pkgRoot, self.lastVCMsg) THEN
              RAISE E("There are unresolved version conflicts.");       
            END;
          EXCEPT
            CVS.E(m) => RAISE E("Status check failed: " & m);
          END;
          (* Everything seems to be up-to-date. *)
        ELSE
          (* There is no need that everything is up-to-date, as we can
             commit to the branch. *)
        END;
      END;
    END;

    newChangeTag := Tag.Construct(changeType, self.pkgName, 0, 0, 0,
                                  change := changeName, branch := FALSE);
    branchTag := Tag.NewBranch(newChangeTag);
    branchStartTag := Tag.NewBranchStart(newChangeTag);
    MsgX.D(self.msgif, "newChangeTag: " & newChangeTag.debugInfo());
    MsgX.D(self.msgif, "branchTag: " & branchTag.debugInfo());
    MsgX.D(self.msgif, "branchStartTag: " & branchStartTag.debugInfo());
    IF self.tagExists(newChangeTag) THEN
      RAISE E("Tag " & newChangeTag.denotation() & " already exists");
    END;
    IF self.tagExists(branchStartTag) THEN
      RAISE E("Tag " & branchStartTag.denotation() & " already exists");
    END;
    IF self.tagExists(branchTag) THEN
      RAISE E("Tag " & branchTag.denotation() & " already exists");
    END;
    MsgX.T(self.msgif, "Creating new branch " & branchTag.denotation());
    TagAll(self, branchTag, branch := TRUE);
    MsgX.T(self.msgif, "Tagging everything as " & 
      branchStartTag.denotation());
    TagAll(self, branchStartTag); 
    MsgX.T(self.msgif, "Updating to new branch " & branchTag.denotation());
    UpdateI(self, branchTag);

    IF msgFile # NIL THEN
      msgFileName := msgFile.denotation();
    END;
    TRY
      changeDesc := self.cvs.changeDesc(self.pkgRoot);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
    IF msgFileName = NIL AND msg = NIL THEN
      (* We try to get a commit message before we update the PkgTags
         file, just in case we are interrupted... *)
      msg := PkgVCUtils.GetCommitMessage(self.editor, self.msgif,
                                         changeDesc, self.pkgName);
    ELSIF msg # NIL AND changeDesc # NIL THEN
      msg := PkgVCUtils.MsgWithoutPkgInfoLines(msg & lb & lb & changeDesc,
                                               self.msgif);
    END;
    TRY
      PkgVCUtils.CheckCommitMsg(msg, msgFileName, self.pkgName, 
                                self.pkgRoot.denotation(), 
                                self.user, self.usedrepo,
                                "package-change", 
                                newChangeTag.denotation(),
                                self.env);
    EXCEPT
      PkgVCUtils.E(e) => RAISE E("commit check failed: " & e);
    END;
    UpdateTagFile(self, newChangeTag);
    MsgX.T(self.msgif, "Committing to new branch " & branchTag.denotation());
    TRY
      IF UseDCVS(self) AND self.useDcvsChangeSets THEN
        IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName,
                               desc := self.cvs.changeDesc(self.pkgRoot),
                               pkg := self.pkgName,
                               changeSetName := newChangeTag.denotation()) THEN
          RAISE E("Commit command failed");
        END;
      ELSE
        IF NOT self.cvs.commit(self.pkgRoot, msg, msgFileName,
                               desc := self.cvs.changeDesc(self.pkgRoot),
                               pkg := self.pkgName) THEN
          RAISE E("Commit command failed");
        END;
      END;
    EXCEPT
      CVS.E(msg) => RAISE E("Commit aborted: " & msg);
    END;
    MsgX.T(self.msgif, "Tagging everything as " & newChangeTag.denotation());
    TagAll(self, newChangeTag); 
    END (* lock *);
  END CommitToChangeBranch; 

(*---------------------------------------------------------------------------*)
PROCEDURE TagAll(self : T; tag : Tag.T; 
                 branch := FALSE; force := FALSE) RAISES {E} =
  VAR
    prefix  :  TEXT;
    allTags :  TextSeq.T;
  BEGIN
    (* tag okay *)
    self.lastVCMsg := tag.denotation();
    IF NOT force THEN
      (* tagging not forced, so we check explicitly *)
      prefix := tag.kindAsText();
      TRY
        IF UseDCVS(self) THEN
          allTags := self.cvs.tagsAndSnaps(self.pkgRoot, prefix);
        ELSE
          allTags := self.cvs.tags(self.pkgRoot, prefix);
        END;
        (* 
        MsgX.D(self.msgif, "TagAll: " & tag.denotation(FALSE));
        MsgX.D(self.msgif, "allTags: " & TextUtils.TextSeqToText(allTags));
        *)
      EXCEPT
        CVS.E(m) => RAISE E("Tag listing failed: " & m);
      END;
      IF Tag.ContainedInList(allTags, tag) THEN
        RAISE E("The given tag is already in use.");
      END;
    END;
    IF UseDCVS(self) AND NOT branch THEN
      IF NOT self.cvs.snapshot(self.pkgRoot, tag.denotation(FALSE)) THEN
        RAISE E("Snapshot command failed");
      END;
    END;
    IF NOT self.cvs.tag(self.pkgRoot, tag.denotation(FALSE), 
                        branch, force) THEN
      RAISE E("Tagging command failed");
    END;
  END TagAll; 

(*---------------------------------------------------------------------------*)
CONST
  PkgTagsFN      = "PkgTags";
  SavedPkgTagsFN = "SavedPkgTags";

(*---------------------------------------------------------------------------*)
<*UNUSED*> PROCEDURE TagFileExists(self : T) : BOOLEAN =
  VAR
    tfn  := TagFileName(self);
  BEGIN
    RETURN FSUtils.IsFile(tfn);
  END TagFileExists;

(*---------------------------------------------------------------------------*)
PROCEDURE TagFileName(self : T) : TEXT =
  BEGIN
    RETURN Pathname.Join(self.pkgRoot.denotation(), PkgTagsFN, NIL);
  END TagFileName;

(*---------------------------------------------------------------------------*)
PROCEDURE LastTagFileEntry(self : T) : TEXT RAISES {E} =
  VAR
    tfn  := TagFileName(self);
    rd   :  FileRd.T;
    line :  TEXT;
    data :  TEXT;
    seq  :  TextSeq.T;
  BEGIN
    TRY
      rd := FileRd.Open(tfn);
    EXCEPT ELSE
      RAISE E("cannot open file " & tfn);
    END;
    TRY
      data := Rd.GetText(rd, LAST(CARDINAL));
    EXCEPT ELSE
      RAISE E("cannot read file " & tfn);
    END;
    TRY Rd.Close(rd) EXCEPT ELSE END;
    seq := TextUtils.Split(data, "\n");
    WHILE seq.size() > 0 DO
      line := TextUtils.Compress(seq.remhi());
      IF NOT Text.Empty(line) THEN
        RETURN line;
      END;
    END;
    RAISE E("no valid entry in " & tfn);
  END LastTagFileEntry;

(*---------------------------------------------------------------------------*)
PROCEDURE SaveTagFile(self : T) RAISES {E} =
  VAR
    tfn  := TagFileName(self);
    stfn := Pathname.Join(self.pkgRoot.denotation(), SavedPkgTagsFN, NIL);
  BEGIN
    IF NOT FSUtils.Exists(tfn) THEN RETURN END;
    (* PkgTags exists *)
    IF FSUtils.IsFile(stfn) THEN
      TRY
        FS.DeleteFile(stfn);
      EXCEPT
        OSError.E => RAISE E("cannot remove file " & stfn);
      END;
    END;
    (* SavedPkgTags does not exist *)
    TRY
      FS.Rename(tfn, stfn);
    EXCEPT
      OSError.E => RAISE E("cannot rename file " & tfn);
    END;
    (* PkgTags renamed to SavedPkgTags *)
  END SaveTagFile;

(*---------------------------------------------------------------------------*)
PROCEDURE RestoreTagFile(self : T) RAISES {E} =
  VAR
    tfn  := TagFileName(self);
    stfn := Pathname.Join(self.pkgRoot.denotation(), SavedPkgTagsFN, NIL);
  BEGIN
    IF NOT FSUtils.Exists(stfn) THEN RETURN END;
    (* SavedPkgTags exists *)
    IF FSUtils.IsFile(tfn) THEN
      TRY
        FS.DeleteFile(tfn);
      EXCEPT
        OSError.E => RAISE E("cannot remove file " & tfn);
      END;
    END;
    (* PkgTags does not exist *)
    TRY
      FS.Rename(stfn, tfn);
    EXCEPT
      OSError.E => RAISE E("cannot rename file " & stfn);
    END;
    (* SavedPkgTags renamed to PkgTags *)
  END RestoreTagFile;

(*---------------------------------------------------------------------------*)
PROCEDURE UpdateTagFile(self : T; tag : Tag.T) RAISES {E} =
  VAR
    tfn  := TagFileName(self);
    atfn := APN.New(tfn);
    wr   :  Wr.T;
    rev  :  TEXT;
    stat :  TEXT;
  BEGIN
    TRY
      wr := FileWr.OpenAppend(tfn);
    EXCEPT
      OSError.E => RAISE E("cannot open tags file " & tfn);
    END;
    TRY
      Wr.PutText(wr, tag.denotation() & "\n");
      Wr.Close(wr);
    EXCEPT ELSE
      RAISE E("cannot write to tags file " & tfn);
    END;
    TRY
      self.cvs.status(atfn, rev, stat);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
    IF Text.Equal(stat, "Unknown") THEN
      TRY
        IF NOT self.cvs.add(atfn) THEN
          RAISE E("cannot add tags file " & tfn & " to the repository");
        END;
      EXCEPT
        CVS.E(m) => RAISE E("cvs add failed: " & m);
      END;
    END;
  END UpdateTagFile;

(*---------------------------------------------------------------------------*)
PROCEDURE ForceCommitTagFile(self : T; msg : TEXT) RAISES {E} =
  VAR
    tfn  := TagFileName(self);
    atfn := APN.New(tfn);
  BEGIN
    TRY
      IF NOT self.cvs.commit(atfn, msg, force := TRUE,
                             pkg := self.pkgName) THEN
        RAISE E("Commit of PkgTags failed");
      END;
    EXCEPT
      CVS.E(msg) => RAISE E("Commit of PkgTags aborted: " & msg);
    END;
  END ForceCommitTagFile;

(*---------------------------------------------------------------------------*)
PROCEDURE Merge(self : T; tag : Tag.T; tag2 : Tag.T := NIL;
                with_d_option :BOOLEAN := FALSE) RAISES {E} =
  VAR
    desttag      : Tag.T;
    mergetag     : Tag.T;
    lastMergeTag : Tag.T;
    newMergeTag  : Tag.T;
    branch       : BOOLEAN;
    msg          : TEXT;

  (*-------------------------------------------------------------------------*)
  PROCEDURE LastMergeTag() : Tag.T RAISES {E} = 
    VAR 
      res      : Tag.T := NIL;
      mpattern : TEXT;
      mtags    : TextSeq.T;
    BEGIN
      (* pre: tag is a stable branch tag, mergetag is the corresponding
         merge tag *)
      mpattern := mergetag.base(4);
      (* mpattern is the pattern we have to look for in all merge tags
         of the package to find out if there have been previous merges.
         It is of the form merge_<pkgname>_<major>_<minor>.
      *)
      mtags := self.tagList(mpattern);
      (* mtags contains all merge tags of the specified branch *)
      FOR i := 0 TO mtags.size() - 1 DO
	WITH mtagtext = mtags.get(i) DO
	  WITH mtag = NEW(Tag.T).initFromText(mtagtext) DO
            WITH mversion = mtag.mergeDestVersion() DO
              IF desttag.version().isDevel() AND mversion.isDevel() OR
                (* merge tag into the main trunk or *)
                mversion.compatible(desttag.version()) THEN
                (* merge tag into another release trunk *)
                IF res = NIL THEN
                  res := mtag;
                ELSE
                  IF res.mergeDestVersion().less(mversion) THEN
                    res := mtag;
                  END;
                END;
              END;
            END;
	  END;
	END;
      END;
      RETURN res;
    END LastMergeTag;

  (*-------------------------------------------------------------------------*)
  PROCEDURE NewMergeTag() : Tag.T RAISES {E} = 
    VAR 
      res : Tag.T;
    BEGIN
      (* pre: tag is a stable branch tag, mergetag is the corresponding
         merge tag, desttag describes the destination of the merge *)
      TRY
        IF UseDCVS(self) THEN
          WITH rbtags = self.cvs.tagsAndSnaps(self.pkgRoot, tag.base(4)) DO
            res := Tag.LatestTag(rbtags, Tag.Kind.Release);
          END;
        ELSE
          WITH rbtags = self.cvs.tags(self.pkgRoot, tag.base(4)) DO
            res := Tag.LatestTag(rbtags, Tag.Kind.Release);
          END;
        END;
      EXCEPT
        CVS.E(m) => RAISE E("Tag listing failed: " & m);
      END;
      res := Tag.NewMergeTag(res, desttag.version());
      RETURN res;
    END NewMergeTag;

  (*-------------------------------------------------------------------------*)
  BEGIN (* Merge *)
    LOCK self.mu DO
    IF tag = NIL THEN RAISE E("Merge called with tag = NIL") END;
    IF tag2 = NIL THEN
      msg := "The current tag used as destination of the merge is ";
      IF tag = Tag.LastTip THEN
        (* compute the `real' name of the last version on the last
           release branch, since last_any_0_0_0 won;t help us much *)
        VAR 
          tags := Tags(self, "release_");
        BEGIN
          IF tags.size() = 0 THEN
            RAISE E("There seems to be no release branch last-tip could " &
                  "refer to.");
          END;
          tag := NIL;
          FOR i := 0 TO tags.size() - 1 DO
            WITH act = tags.get(i) DO
              IF act.isStableBranchTag() THEN
                IF tag = NIL THEN
                  tag := act;
                ELSE
                  IF tag.compare(act) < 0 THEN
                    tag := act;
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
      IF tag.isStableBranchTag() THEN
        (* We do an automatic three point merge here for convenient handling
           of fixes on release branches. *)
        IF self.isReleaseBranch(desttag) THEN
          branch := TRUE;
          TRY
            IF UseDCVS(self) THEN
              WITH rbtags =
                   self.cvs.tagsAndSnaps(self.pkgRoot, desttag.base(4)) DO
                (* rbtags contains all release tags from this release branch *)
                desttag := Tag.LatestTag(rbtags, Tag.Kind.Release);
                (* we take the latest tag as destination *)
              END;
            ELSE
              WITH rbtags = self.cvs.tags(self.pkgRoot, desttag.base(4)) DO
                (* rbtags contains all release tags from this release branch *)
                desttag := Tag.LatestTag(rbtags, Tag.Kind.Release);
                (* we take the latest tag as destination *)
              END;
            END;
          EXCEPT
            CVS.E(m) => RAISE E("Tag listing failed: " & m);
          END;
          MsgX.T(self.msgif, msg & desttag.denotation() & "."); 
        ELSIF self.isRelease(desttag) THEN
          RAISE E("Merge into non-branch version is not supported by pkgm.\n" 
          & "Please update to a release branch or the development head,\n"
          & "or use the explicit merge by specifying two tags.");
        ELSIF self.isSticky(desttag)  THEN
          RAISE E("Merge into unknown sticky version is not supported by pkgm.\n" 
          & "Please update to a release branch or the development head,\n"
          & "or use the explicit merge by specifying two tags.");
        ELSE
          desttag := self.currentDevelopmentTag();
          branch := FALSE;
          MsgX.T(self.msgif, msg & desttag.denotation() & "."); 
        END;
        mergetag := Tag.NewMergeTag(tag);
        lastMergeTag := LastMergeTag();
        newMergeTag := NewMergeTag();
        (*
        IF mergetag # NIL THEN
          MsgX.T(self.msgif, "merge tag is      " & mergetag.denotation());
        END;
        *)
        IF lastMergeTag # NIL THEN
          MsgX.T(self.msgif, "The last merge tag found on that branch is " & 
            lastMergeTag.denotation() & ".");
        END;
        IF newMergeTag # NIL THEN
          MsgX.T(self.msgif, "The new merge tag is " & 
            newMergeTag.denotation() & ".");
        END;
        IF lastMergeTag = NIL THEN
          (* No merge has been marked via a tag, so we do a complete
             merge now and write a first merge tag. *)
          SaveTagFile(self);
          MsgX.T(self.msgif, "Merging the changes from branch " & 
            tag.denotation(FALSE) & 
            " into the current workspace...");
          IF NOT self.cvs.merge(self.pkgRoot, tag.denotation(FALSE), NIL,
                                with_d_option, self.lastVCMsg) THEN
            TRY RestoreTagFile(self) EXCEPT ELSE END; 
            (* don't risk another failure ^^^ *)
            RAISE E("Merge failed and may be incomplete. " & self.lastVCMsg);
          END;
          RestoreTagFile(self);
        ELSE
          (* Implicit three point merge *)
          IF lastMergeTag.equal(newMergeTag) THEN
            MsgX.T(self.msgif, "There seems to be nothing to merge.");
            RETURN;
          END;
          SaveTagFile(self);
          MsgX.T(self.msgif, "Merging the differences from " & 
            lastMergeTag.denotation() &
            " to " & tag.denotation(FALSE) & " into the current workspace...");
          IF NOT self.cvs.merge(self.pkgRoot, lastMergeTag.denotation(), 
                                tag.denotation(FALSE), with_d_option,
                                self.lastVCMsg) THEN
            TRY RestoreTagFile(self) EXCEPT ELSE END; 
            (* don't risk another failure ^^^ *)
            RAISE E("Merge failed. " & self.lastVCMsg);
          END;
          RestoreTagFile(self);
        END;
        MsgX.T(self.msgif, "Tagging everything with the new merge tag " &
          newMergeTag.denotation() & "...");
        IF NOT self.cvs.tagAgain(self.pkgRoot, tag.denotation(FALSE),
                                 newMergeTag.denotation()) THEN
          RAISE E("Tagging with " & newMergeTag.denotation() & " failed");
        END;
      ELSE
        (* Just an ordinary merge *)
        SaveTagFile(self);
        MsgX.T(self.msgif, "Merging the changes from branch " & 
          tag.denotation(FALSE) & " into the current workspace...");
        IF NOT self.cvs.merge(self.pkgRoot, tag.denotation(FALSE), NIL,
                              with_d_option, self.lastVCMsg) THEN
          TRY RestoreTagFile(self) EXCEPT ELSE END; 
          (* don't risk another failure ^^^ *)
          RAISE E("Merge failed. " & self.lastVCMsg);
        END;
        RestoreTagFile(self);
      END;
    ELSE
      (* Explicit three point merge *)
      SaveTagFile(self);
      MsgX.T(self.msgif, "Merging the differences from " & 
        tag.denotation(FALSE) &
        " to " & tag2.denotation() & " into the current workspace...");
      IF NOT self.cvs.merge(self.pkgRoot, tag.denotation(FALSE),
                            tag2.denotation(FALSE), with_d_option,
                            self.lastVCMsg) THEN
        TRY RestoreTagFile(self) EXCEPT ELSE END; 
        (* don't risk another failure ^^^ *)
        RAISE E("Merge failed: " & self.lastVCMsg);
      END;
      RestoreTagFile(self);
    END;
    END (* lock *);
  END Merge;

(*---------------------------------------------------------------------------*)
PROCEDURE Log(self : T; fn : APN.T := NIL) : TEXT RAISES {E} =
  BEGIN
    self.cvs.setCVSIgnore(self.ignorePatterns());
    fn := self.normalizedPkgFN(fn);
    IF NOT FileObj.Exists(fn) THEN
      RAISE E("No log available for " & fn.denotation());
    END;
    TRY
      RETURN self.cvs.logFile(fn);
    EXCEPT
      CVS.E(m) => RAISE E("cvs log failed: " & m);
    END;
  END Log;

(*---------------------------------------------------------------------------*)
PROCEDURE RCSRevLast(r: TEXT): TEXT =
  BEGIN
    WITH pos = Text.FindCharR(r, '.') DO
      IF pos = -1 THEN
        RETURN r;
      ELSE
        RETURN Text.Sub(r, pos+1);
      END;
    END;
  END RCSRevLast;

(*---------------------------------------------------------------------------*)
PROCEDURE RCSRevPrefix(r: TEXT): TEXT =
  BEGIN
    WITH pos = Text.FindCharR(r, '.') DO
      IF pos = -1 THEN
        RETURN "";
      ELSE
        RETURN Text.Sub(r, 0, pos);
      END;
    END;
  END RCSRevPrefix; 

(*---------------------------------------------------------------------------*)
VAR 
  lprefix := "";
  tprefix := "    ";
  hprefix := "";
  hsep    := "    ";
  hsuffix := "\n\n";

(*---------------------------------------------------------------------------*)
PROCEDURE NiceLog(self : T; fn : APN.T := NIL;
                  displayTags := TRUE; displayRevs := TRUE;
                  displayLog := TRUE) : TEXT RAISES {E} =

  PROCEDURE SeparatorLine(line : TEXT; ch : CHAR) : TEXT =
    (* Return `line' if it only consists of '-' characters, else NIL *)
    VAR len := Text.Length(line);
    BEGIN
      IF len = 0 THEN
        RETURN NIL;
      END;
      FOR i := 0 TO len - 1 DO
        IF Text.GetChar(line, i) # ch THEN
          RETURN NIL;
        END;
      END;
      RETURN line;
    END SeparatorLine;

  VAR
    log         : TEXT;
    revTagTab   : TextTextSeqTbl.T;
    tagRevTab   : TextTextTbl.T;
    revStateTab : TextTextTbl.T;
    res         : TextWr.T;
    rd          : TextRd.T;
    line, rev,
    date, state,
    author,
    name, val,
    sep, psep   : TEXT := NIL;
    tseq, tseq2 : TextSeq.T;
    pos         : INTEGER;
    skip        := TRUE;
    pkgLog      := FALSE;
    tags        : TagSeq.T;
  BEGIN (* NiceLog *)
    (* Normalize file pathname *)
    IF fn = NIL THEN
      pkgLog := TRUE;
    END;
    fn := self.normalizedPkgFN(fn);
    res := TextWr.New();
    TRY
      log := Log(self, fn);
      revTagTab := self.cvs.revisionTags(fn);
      tagRevTab := self.cvs.taggedRevisions(fn);
      revStateTab := self.cvs.revisionsAndLabels(log);
      Wr.PutText(res, "---------------------------------------");
      Wr.PutText(res, "---------------------------------------\n");
      IF pkgLog THEN
        IF self.pkgName # NIL THEN
          Wr.PutText(res, "package " & self.pkgName & "\n\n");
        ELSE
          Wr.PutText(res, "unknown package\n\n");
        END;
      ELSE
        Wr.PutText(res, "file " & fn.denotation() & "\n\n");
      END;
      IF displayTags THEN
        tags := Tags(self, "", fn);
        Wr.PutText(res, "all revisions:\n");
        FOR i := 0 TO tags.size() - 1 DO
          WITH tag = tags.get(i) DO
            WITH tt = Tag.OriginalText(tag) DO
              IF tagRevTab.get(tt, rev) AND revStateTab.get(rev, val) THEN
                val := ": " & val;
              ELSE
                val := "";
              END;
              Wr.PutText(res, tprefix & tt & val & "\n");
            END;
          END;
        END;
      END;
      rd := TextRd.New(log);
      WHILE NOT Rd.EOF(rd) DO
        line := Rd.GetLine(rd);
        sep := SeparatorLine(line, '-');
        IF skip THEN
          IF TextUtils.Pos(line, "description:") = 0 THEN
            skip := FALSE;
          END;
        ELSE
          IF sep = NIL AND psep # NIL THEN
            (* line after separator *)
            IF TextUtils.Pos(line, "revision ") = 0 THEN
              rev := TextUtils.Compress(Text.Sub(line, 9));
              line := Rd.GetLine(rd);
              tseq := TextUtils.Split(line, ";");
              date := "????/??/?? ??:??:??";
              state := "unknown";
              author := "unknown";
              FOR i := 0 TO tseq.size() - 1 DO
                WITH field = tseq.get(i) DO
                  pos := TextUtils.Pos(field, ":");
                  IF pos > -1 THEN
                    name := TextUtils.Compress(Text.Sub(field, 0, pos));
                    val  := TextUtils.Compress(Text.Sub(field, pos + 1));
                    IF Text.Equal(name, "date") THEN
                      date := val;
                    ELSIF Text.Equal(name, "author") THEN
                      author := val;
                    ELSIF Text.Equal(name, "state") THEN
                      state := val;
                    END;
                  END;
                END;
              END;
              IF displayRevs OR displayLog THEN
                Wr.PutText(res, "\n");
                Wr.PutText(res, "---------------------------------------");
                Wr.PutText(res, "---------------------------------------\n");
                Wr.PutText(res, hprefix & date & hsep & author & 
                  hsep & state & hsuffix);
              END;
              IF displayRevs THEN
                IF revTagTab.get(rev, tseq) THEN
                  FOR i := 0 TO tseq.size() - 1 DO
                    WITH tag = tseq.get(i) DO
                      Wr.PutText(res, tprefix & "revision " & tag & "\n");
                    END;
                  END;
                END;
              END;
              line := Rd.GetLine(rd);
              IF TextUtils.Pos(line, "branches: ") = 0 THEN
                tseq := TextUtils.Split(Text.Sub(line, 10), ";");
                line := NIL;
                IF displayRevs THEN
                  FOR i := 0 TO tseq.size() - 1 DO
                    WITH bnum = TextUtils.Compress(tseq.get(i)) DO
                      rev := RCSRevPrefix(bnum) & ".0." & RCSRevLast(bnum);
                      IF revTagTab.get(rev, tseq2) THEN
                        FOR j := 0 TO tseq2.size() - 1 DO
                          WITH tag = tseq2.get(j) DO
                            WITH tt = NEW(Tag.T).initFromText(tag) DO
                              IF tt.isStableBranchTag() THEN
                                Wr.PutText(res, tprefix & "branch " & 
                                  tag & "\n");
                              END;
                            END;
                          END;
                        END;
                      ELSE
                        IF NOT Text.Empty(bnum) THEN
                          Wr.PutText(res, tprefix & "branch " & bnum & "\n");
                        END;
                      END;
                    END;
                  END;
                END;
              END;
              IF displayRevs OR displayLog THEN
                Wr.PutText(res, "\n");
              END;
              IF line # NIL AND displayLog THEN
                IF SeparatorLine(line, '=') = NIL THEN
                  Wr.PutText(res, lprefix & line & "\n");
                END;
              END;
            ELSE
              IF SeparatorLine(line, '=') = NIL AND displayLog THEN
                Wr.PutText(res, lprefix & line & "\n");
              END;
            END;
          ELSIF sep = NIL THEN
            (* probably text line *)
            IF TextUtils.Pos(line, "RCS file: ") = 0 THEN
              val := TextUtils.Compress(Text.Sub(line, 10));
              Wr.PutText(res, "---------------------------------------");
              Wr.PutText(res, "---------------------------------------\n");
              Wr.PutText(res, "file " & fn.denotation() & "\n\n");
              skip := TRUE;
            ELSE
              IF SeparatorLine(line, '=') = NIL AND displayLog THEN
                Wr.PutText(res, lprefix & line & "\n");
              END;
            END;
          END;
        END;
        psep := sep;
      END;
    EXCEPT
      CVS.E(e) => RAISE E(e);
    | Rd.Failure => RAISE E("reader failure");
    | Wr.Failure => RAISE E("writer failure");
    | Rd.EndOfFile => (* skip *)
    | Thread.Alerted => RAISE E("unexpected interrupt");
    END;
    RETURN TextWr.ToText(res);
  END NiceLog; 

(*---------------------------------------------------------------------------*)
PROCEDURE Diff(self : T; from : Tag.T := NIL; to : Tag.T := NIL;
               udiff := FALSE; cdiff := FALSE;
               flist : APNSeq.T := NIL) : TEXT RAISES {E} =
  VAR 
    fromText, toText, rpn, res : TEXT;
    rlist : APNSeq.T := NIL;
  BEGIN
    IF from = NIL THEN
      fromText := NIL; 
    ELSE
      fromText := SymbolicRevisionName(self, from, avoidHead := TRUE);
    END;
    IF to = NIL THEN
      toText := NIL;
    ELSE
      toText := SymbolicRevisionName(self, to, avoidHead := TRUE);
    END;
    IF flist # NIL THEN
      rlist := NEW(APNSeq.T).init(flist.size());
      FOR i := 0 TO flist.size() - 1 DO
        WITH pn = flist.get(i) DO
          TRY
            rpn := self.packageRelativePathname(pn.denotation());
            rlist.addhi(APN.New(rpn));
          EXCEPT
            E(c) => MsgX.Error(self.msgif, c);
          END;
        END;
      END;
    END;
    IF self.cvs.diff(self.pkgRoot, fromText, toText, 
                     udiff, cdiff, rlist, res) THEN
      RETURN res;
    ELSE
      RAISE E("diff failed: " & res);
    END;
  END Diff;

(*---------------------------------------------------------------------------*)
PROCEDURE Annotate(self : T; flist : APNSeq.T := NIL) : TEXT RAISES {E} =
  VAR 
    rpn   : TEXT;
    rlist : APNSeq.T := NIL;
  BEGIN
    IF flist # NIL THEN
      rlist := NEW(APNSeq.T).init(flist.size());
      FOR i := 0 TO flist.size() - 1 DO
        WITH pn = flist.get(i) DO
          TRY
            rpn := self.packageRelativePathname(pn.denotation());
            rlist.addhi(APN.New(rpn));
          EXCEPT
            E(c) => MsgX.Error(self.msgif, c);
          END;
        END;
      END;
    END;
    TRY
      RETURN self.cvs.annotate(self.pkgRoot, rlist);
    EXCEPT
      CVS.E(e) => RAISE E(e);
    END;
  END Annotate; 

(* The following methods do not raise exceptions:                            *)
(*---------------------------------------------------------------------------*)
PROCEDURE AddToKnownFiles(self : T; obj : APN.T) = 
  (* If self.allVersionControlledFiles has cached the files known to
     CVS, add the canonical absolute pathname of obj to this list. *)
  VAR
    cwd : Pathname.T;
  BEGIN
    IF self.allVersionControlledFiles = NIL THEN RETURN; END;
    IF NOT APN.Absolute(obj) THEN
      TRY 
        cwd := DirStack.GetWorkingDir();
      EXCEPT ELSE
        MsgX.Fatal(self.msgif, "cannot get current working directory");
      END;
      obj := APN.Join(APN.New(cwd), obj, NIL);
    END;
    TRY
      WITH pn = FSUtils.CanonicalPathname(obj.denotation()) DO
        obj := APN.New(pn);
      END;
    EXCEPT
      FSUtils.E(code) => 
        MsgX.Fatal(self.msgif, "PkgVC.Known: cannot get " &
          "absolute pathname: OSError: " & code);
    END;
    self.allVersionControlledFiles.addhi(obj);
  END AddToKnownFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE Add(self : T; obj : APN.T; recursive   := FALSE; 
                                     interactive := FALSE;
                                     binary      := FALSE) : BOOLEAN 
  RAISES {E} =

  VAR addedPNs := NEW(APNHashTbl.Default).init();

  (*-------------------------------------------------------------------------*)
  PROCEDURE AddI(self : T; obj : APN.T; recursive   := FALSE; 
                 interactive := FALSE;
                 binary      := FALSE) : BOOLEAN  RAISES {E} =
    
    (*-----------------------------------------------------------------------*)
    PROCEDURE AddMissingDirs(obj : APN.T) RAISES {E} =
      VAR 
        arcs   : Pathname.Arcs;
        parent : APN.T;
        dummy  : INTEGER;
      BEGIN
        IF addedPNs.get(obj, dummy) THEN 
          MsgX.D(self.msgif, " --- directory " & obj.denotation() & 
            " already added");
          RETURN;
        END;
        TRY
          arcs := Pathname.Decompose(obj.denotation());
        EXCEPT
          Pathname.Invalid => RETURN;
        END;
        IF arcs.size() < 2 THEN RETURN END;
        parent := APN.Prefix(obj);
        IF FileObj.IsDir(obj) THEN
          IF NOT FileObj.IsDir(APN.Join(obj, APN.New("CVS"), NIL)) THEN
            (* this directory is not under version control *)
            AddMissingDirs(parent);
            MsgX.V(self.msgif, " --- directory " & obj.denotation() & 
              " implicitly added");
            TRY
              EVAL self.cvs.add(obj, FALSE);
              EVAL addedPNs.put(obj, 2);
            EXCEPT
              CVS.E(m) => RAISE E("cvs add failed: " & m);
            END;
          END;
        ELSIF FileObj.IsFile(obj) THEN
          AddMissingDirs(parent);
        END;
      END AddMissingDirs;

    (*-----------------------------------------------------------------------*)
    PROCEDURE AddFile(obj : APN.T) : BOOLEAN RAISES {E} = 
      VAR 
        dummy  : INTEGER;
      BEGIN
        IF FileObj.IsFile(obj) AND self.known(obj) THEN
          MsgX.T(self.msgif, " --- file " & obj.denotation() & 
            " already under version control");
          RETURN TRUE;
        ELSIF FileObj.IsDir(obj) AND 
              FileObj.IsDir(APN.Join(obj, APN.New("CVS"), NIL)) THEN
          MsgX.T(self.msgif, " --- directory " & obj.denotation() & 
            " already under version control");
          confirmed := TRUE;
          RETURN TRUE;
        END;
        IF interactive THEN
          confirmed := confirmation.okay("add file " & obj.denotation());
          IF NOT confirmed THEN
            RETURN TRUE;
          END;
        END;
        TRY
          AddMissingDirs(APN.Prefix(obj));
          IF addedPNs.get(obj, dummy) THEN 
            MsgX.V(self.msgif, " --- file/directory " & obj.denotation() & 
              " already added");
            RETURN TRUE;
          END;
          MsgX.V(self.msgif, " --- add " & obj.denotation());
          WITH ret = self.cvs.add(obj, binary) DO
            EVAL addedPNs.put(obj, 1);
            AddToKnownFiles(self, obj);
            IF self.locking = LockType.All OR 
               self.locking = LockType.Binary AND binary THEN
              MsgX.V(self.msgif, " --- strict locking " & obj.denotation());
              self.cvs.watch(obj, "on", recursive);
            END;
            RETURN ret;
          END;
        EXCEPT
          CVS.E(m) => RAISE E("cvs add failed: " & m);
        END;
      END AddFile;

    (*-----------------------------------------------------------------------*)
    VAR (* AddI *)
      okay      := TRUE;
      fn        :  TEXT;
      confirmed :  BOOLEAN;
    BEGIN
      IF NOT FileObj.Exists(obj) THEN 
        MsgX.Warning(self.msgif, obj.denotation() & " does not exist");
        RETURN FALSE;
      ELSIF Text.Equal(APN.Last(obj).denotation(), "CVS") THEN
        RETURN TRUE; (* hack: never try to add CVS directories *)
      ELSIF FileObj.IsDir(obj) THEN
        MsgX.V(self.msgif, " --- directory " & obj.denotation());
        self.cvs.flushCache();
        AddMissingDirs(APN.Prefix(obj));
        okay := AddFile(obj);
        IF okay AND recursive AND (NOT interactive OR confirmed) THEN
          TRY
            WITH it = FS.Iterate(obj.denotation()) DO
              WHILE it.next(fn) DO
                WITH fnbase = APN.New(fn) DO
                  WITH fnfull = APN.Join(obj, fnbase, NIL) DO
                    okay := AddI(self, fnfull, recursive, interactive) 
                    AND okay;
                  END;
                END;
              END;
            END;
          EXCEPT ELSE
            RETURN FALSE;
          END;
        END;
        RETURN okay;
      ELSIF FileObj.IsFile(obj) THEN
        self.cvs.flushCache();
        RETURN AddFile(obj);
      ELSE
        MsgX.Warning(self.msgif, "unknown file type: " & obj.denotation());
        RETURN TRUE;
      END;
    END AddI; 

  BEGIN
    LOCK self.mu DO
      RETURN AddI(self, obj, recursive, interactive, binary);
    END (* lock *);
  END Add;

(*---------------------------------------------------------------------------*)
PROCEDURE Remove(self : T; obj : APN.T; recursive := FALSE; 
                 interactive := FALSE) : BOOLEAN RAISES {E} =

  (*-------------------------------------------------------------------------*)
  PROCEDURE RemoveI(self : T; obj : APN.T; recursive := FALSE; 
                   interactive := FALSE) : BOOLEAN RAISES {E} =

    (*-----------------------------------------------------------------------*)
    PROCEDURE RemoveFile(obj : APN.T) : BOOLEAN RAISES {E} = 
      BEGIN
        IF FileObj.IsFile(obj) AND NOT self.known(obj) THEN
          MsgX.V(self.msgif, " --- file " & obj.denotation() & 
            " is not under version control");
          RETURN TRUE;
        ELSIF FileObj.IsDir(obj) AND 
              NOT FileObj.IsDir(APN.Join(obj, APN.New("CVS"), NIL)) THEN
          MsgX.V(self.msgif, " --- directory " & obj.denotation() & 
            " is not under version control");
          RETURN TRUE;
        END;
        IF interactive THEN
          IF NOT confirmation.okay("remove file " & obj.denotation()) THEN
            RETURN TRUE;
          END;
        END;
        TRY
          FS.DeleteFile(obj.denotation());
        EXCEPT ELSE
          RETURN FALSE;
        END;
        TRY
          RETURN self.cvs.remove(obj);
        EXCEPT
          CVS.E(m) => RAISE E("cvs remove failed: " & m);
        END;
      END RemoveFile;

    (*-----------------------------------------------------------------------*)
    VAR (* RemoveI *)
      okay := TRUE;
      fn   : TEXT;
    BEGIN
      IF NOT FileObj.Exists(obj) THEN
        IF self.known(obj) THEN 
          TRY
            RETURN self.cvs.remove(obj);
          EXCEPT
            CVS.E(m) => RAISE E("cvs remove failed: " & m);
          END;
        ELSE
          MsgX.Warning(self.msgif, obj.denotation() & " does not exist");
          RETURN FALSE; 
        END;
      ELSIF Text.Equal(APN.Last(obj).denotation(), "CVS") THEN
        RETURN TRUE; (* hack: never try to remove CVS directories *)
      ELSIF FileObj.IsDir(obj) THEN
        (* We cannot remove directories from CVS. *)
        MsgX.V(self.msgif, " --- directory " & obj.denotation());
        self.cvs.flushCache();
        IF okay AND recursive THEN
          TRY
            WITH it = FS.Iterate(obj.denotation()) DO
              WHILE it.next(fn) DO
                WITH fnbase = APN.New(fn) DO
                  WITH fnfull = APN.Join(obj, fnbase, NIL) DO
                    okay := RemoveI(self, fnfull, recursive, interactive) AND
                    okay;
                  END;
                END;
              END;
            END;
          EXCEPT ELSE
            RETURN FALSE;
          END;
        END;
        RETURN okay;
      ELSIF FileObj.IsFile(obj) THEN
        self.cvs.flushCache();
        RETURN RemoveFile(obj);
      ELSE
        MsgX.Warning(self.msgif, "unknown file type: " & obj.denotation());
        RETURN TRUE;
      END;
    END RemoveI; 

  BEGIN (* Remove *)
    LOCK self.mu DO
      RETURN RemoveI(self, obj, recursive, interactive);
    END (* lock *);
  END Remove;

(*---------------------------------------------------------------------------*)
PROCEDURE SetLocking(self : T; obj : APN.T; cmd := "on"; recursive := TRUE)
  RAISES {E} =
  BEGIN
    IF obj = NIL THEN
      obj := self.pkgRoot;
      recursive := TRUE;
    END;
    obj := self.normalizedPkgFN(obj);
    LOCK self.mu DO
      TRY
        self.cvs.watch(obj, cmd, recursive);
      EXCEPT
        CVS.E(m) => RAISE E("cannot set strict locking for " & 
          obj.denotation() & ": " & m);
      END;
    END;
  END SetLocking;

(*---------------------------------------------------------------------------*)
PROCEDURE LockForEdit(self : T; obj : APN.T; recursive := TRUE) RAISES {E} =
  BEGIN
    IF obj = NIL THEN
      obj := self.pkgRoot;
      recursive := TRUE;
    END;
    obj := self.normalizedPkgFN(obj);
    IF NOT self.lockedByMe(obj, recursive) THEN
      LOCK self.mu DO
        TRY
          self.cvs.edit(obj, recursive);
          (* As there is a race, we have to check again *)
          IF NOT self.lockedByMe(obj, recursive) THEN
            self.cvs.unedit(obj, recursive);
            WITH eds = self.cvs.editors(obj, recursive) DO
              IF eds.size() > 0 THEN
                RAISE E(obj.denotation() & " already locked by " & eds.get(0));
              ELSE
                RAISE E("cannot lock " & obj.denotation());
              END;
            END;
          END;
        EXCEPT
          CVS.E(m) => RAISE E("cannot lock file " & obj.denotation() &
            " for editing: " & m);
        END;
      END;
    END;
  END LockForEdit;

(*---------------------------------------------------------------------------*)
PROCEDURE Revert(self : T; obj : APN.T; recursive := TRUE) RAISES {E} =
  VAR
    lockInfo : TextLockInfoTbl.T;
    iter     : TextLockInfoTbl.Iterator;
    liseq    : CVSLockInfoSeq.T;
    li       : CVSLockInfo.T;
    fn       : TEXT;
    list     : TextSeq.T;
    modified : TextSeq.T;
    rect := "";
  BEGIN
    IF obj = NIL THEN
      obj := self.pkgRoot;
      recursive := TRUE;
    END;
    obj := self.normalizedPkgFN(obj);
    IF recursive THEN rect := "recursive " END;
    MsgX.V(self.msgif, "revert " & rect & obj.denotation());
    (* set ignore patterns *)
    self.cvs.setCVSIgnore(self.ignorePatterns());
    (* get state of files and distribute names according to status *)
    MsgX.V(self.msgif, "locking for changed files");
    TRY
      list := self.cvs.stateList(self.pkgRoot);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
    modified := NEW(TextSeq.T).init();
    FOR i := 0 TO list.size() - 1 DO
      WITH line = list.get(i) DO
        VAR
          state := Text.GetChar(line, 0);
          name  :  TEXT;
        BEGIN
          name  := PathRepr.Native(Text.Sub(line, 2)); 
          IF state = 'A' OR state = 'R' OR state = 'M' OR state = 'C' THEN
            modified.addhi(name);
          END;
        END;
      END;
    END;
    (* modified contains a list of all locally modified, added, or removed
       files *)
    LOCK self.mu DO
      TRY
        MsgX.V(self.msgif, "getting CVS lock info for package");
        lockInfo := self.cvs.editorInfo(self.pkgRoot, TRUE);
      EXCEPT
        CVS.E(m) => RAISE E("cannot get lock info for " & 
          self.pkgRoot.denotation() & ": " & m);
      END;
      iter := lockInfo.iterate();
      WHILE iter.next(fn, liseq) DO
        FOR i := 0 TO liseq.size() -1 DO
          li := liseq.get(i);
          WITH rfn = Pathname.Prefix(self.pkgRoot.denotation(APN.Type.Native)),
               mfn = Pathname.Join(rfn, li.fn, NIL) DO
            IF TextUtils.Pos(mfn, obj.denotation(APN.Type.Native)) = 0 THEN
              IF TextUtils.MemberOfTextSeq(modified, li.fn) THEN
                MsgX.V(self.msgif, "locked and modified: " & mfn);
                IF FSUtils.IsFile(mfn) THEN
                  WITH time = FmtTime.Long(Time.Now()),
                       tstr = TextUtils.SubstChar(time, ' ', '_'),
                       pref = Pathname.Prefix(mfn),
                       last = Pathname.Last(mfn),
                       name = ".#" & last & "_" & tstr,
                       bfn  = Pathname.Join(pref, name, NIL) DO
                    TRY
                      MsgX.V(self.msgif, "backing up " & mfn & " to " & bfn);
                      FSUtils.Cp(mfn, bfn);
                    EXCEPT
                      FSUtils.E(e) => 
                      RAISE E("cannot create backup file " & bfn &
                              " for modified " & mfn & ": " & e);
                    END;
                  END;
                END;
              END;
              IF Text.Equal(self.user, li.user) THEN
                TRY
                  MsgX.V(self.msgif, "reverting " & mfn);
                  self.cvs.unedit(APN.New(mfn), recursive);
                EXCEPT
                  CVS.E(m) => RAISE E("cannot revert file " & 
                    obj.denotation() & " after editing: " & m);
                END;
              ELSE
                MsgX.V(self.msgif, "ignoring " & mfn & " locked by " & 
                  li.user);
              END;
            END;
          END;
        END;
      END;
    END;
  END Revert;

(*---------------------------------------------------------------------------*)
PROCEDURE NormalizeArgPN(self : T; VAR obj : APN.T; VAR recursive : BOOLEAN)
  RAISES {E} = 
  BEGIN
    IF obj = NIL THEN
      obj := self.pkgRoot;
      recursive := TRUE;
    END;
    obj := self.normalizedPkgFN(obj);
  END NormalizeArgPN;

(*---------------------------------------------------------------------------*)
PROCEDURE EditorInfo(self : T; obj : APN.T; recursive := TRUE)
  : TextLockInfoTbl.T RAISES {E} =
  BEGIN
    NormalizeArgPN(self, obj, recursive);
    LOCK self.mu DO
      TRY
        RETURN self.cvs.editorInfo(obj, recursive);
      EXCEPT
        CVS.E(m) => RAISE E("cannot get list of editors for " & 
          obj.denotation() & ": " & m);
      END;
    END;
  END EditorInfo;

(*---------------------------------------------------------------------------*)
PROCEDURE Editors(self : T; obj : APN.T; recursive := TRUE) : TextSeq.T
  RAISES {E} =
  BEGIN
    NormalizeArgPN(self, obj, recursive);
    LOCK self.mu DO
      TRY
        RETURN self.cvs.editors(obj, recursive);
      EXCEPT
        CVS.E(m) => RAISE E("cannot get list of editors for " & 
          obj.denotation() & ": " & m);
      END;
    END;
  END Editors;

(*---------------------------------------------------------------------------*)
PROCEDURE LockedByMe(self : T; obj : APN.T; recursive := TRUE) : BOOLEAN
  RAISES {E} =
  BEGIN
    NormalizeArgPN(self, obj, recursive);
    TRY
      WITH eds = self.cvs.editors(obj, recursive) DO
        RETURN eds.size() = 1 AND Text.Equal(self.user, eds.get(0));
      END;
    EXCEPT
      CVS.E(m) => RAISE E("cannot get list of editors for " & 
        obj.denotation() & ": " & m);
    END;
  END LockedByMe;

(*---------------------------------------------------------------------------*)
PROCEDURE LockedByOther(self : T; obj : APN.T; recursive := TRUE) : BOOLEAN
  RAISES {E} =
  BEGIN
    NormalizeArgPN(self, obj, recursive);
    TRY
      WITH eds = self.cvs.editors(obj, recursive) DO
        RETURN eds.size() # 1 OR NOT Text.Equal(self.user, eds.get(0));
      END;
    EXCEPT
      CVS.E(m) => RAISE E("cannot get list of editors for " & 
        obj.denotation() & ": " & m);
    END;
  END LockedByOther;

(*---------------------------------------------------------------------------*)
PROCEDURE Known(self : T; obj : APN.T) : BOOLEAN  =
  VAR 
    cwd : TEXT;
    vcf : APNSeq.T;  
  BEGIN
    IF FileObj.IsDir(obj) THEN
      RETURN FileObj.IsDir(APN.Join(obj, APN.New("CVS"), NIL));
    END;
    self.cvs.setCVSIgnore(self.ignorePatterns());
    IF self.allVersionControlledFiles = NIL THEN
      TRY
        vcf := VersionControlledFiles(self);
      EXCEPT 
        E(e) => 
        MsgX.Fatal(self.msgif, "cannot get version controlled files: " & e);
      END;
      self.allVersionControlledFiles := NEW(APNSeq.T).init();
      FOR i := 0 TO vcf.size() - 1 DO
        WITH f = vcf.get(i) DO
          IF f # NIL AND NOT APN.Absolute(f) THEN
            IF self.pkgRoot.isAbsolute() THEN
              self.allVersionControlledFiles.addhi(
                       APN.Join(self.pkgRoot, f, NIL));
            ELSE
              VAR
                rel := APN.Join(self.pkgRoot, f, NIL);
                abs :  APN.T;
              BEGIN
                TRY
                  abs := APN.New(FSUtils.CanonicalPathname(rel.denotation()));
                  self.allVersionControlledFiles.addhi(abs);
                EXCEPT
                  FSUtils.E(code) => 
                  MsgX.Error(self.msgif, "PkgVC.Known: cannot get " &
                    "absolute pathname for " & rel.denotation() & 
                    ": OSError: " & code);
                END;
              END;
            END;
          END;
        END;
      END;
    END;
    (* allVersionControlledFiles contains absolute pathnames for all
       elements of this package found to be under version control *)
    IF NOT APN.Absolute(obj) THEN
      TRY 
        cwd := DirStack.GetWorkingDir();
      EXCEPT ELSE
        MsgX.Fatal(self.msgif, "cannot get current working directory");
      END;
      obj := APN.Join(APN.New(cwd), obj, NIL);
    END;
    TRY
      WITH pn = FSUtils.CanonicalPathname(obj.denotation()) DO
        obj := APN.New(pn);
      END;
    EXCEPT
      FSUtils.E(code) => 
        MsgX.Fatal(self.msgif, "PkgVC.Known: cannot get " &
          "absolute pathname: OSError: " & code);
    END;
    FOR i := 0 TO self.allVersionControlledFiles.size() - 1 DO
      WITH f = self.allVersionControlledFiles.get(i) DO
        IF APN.Equal(f, obj) THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END Known;

(*---------------------------------------------------------------------------*)
PROCEDURE UpToDate(self : T) : BOOLEAN RAISES {E} =
  BEGIN
    self.cvs.setCVSIgnore(self.ignorePatterns());
    TRY
      RETURN self.cvs.upToDate(self.pkgRoot, self.lastVCMsg);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
  END UpToDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE Modified(self : T) : BOOLEAN RAISES {E} =
  BEGIN
    self.cvs.setCVSIgnore(self.ignorePatterns());
    TRY
      RETURN self.cvs.modified(self.pkgRoot, self.lastVCMsg);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
  END Modified; 

(*---------------------------------------------------------------------------*)
PROCEDURE Conflicts(self : T) : BOOLEAN RAISES {E} =
  BEGIN
    self.cvs.setCVSIgnore(self.ignorePatterns());
    TRY
      RETURN self.cvs.conflicts(self.pkgRoot, self.lastVCMsg);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
  END Conflicts;  

(*---------------------------------------------------------------------------*)
PROCEDURE TagList(self : T; prefix : TEXT) : TextSeq.T RAISES {E} =
  VAR pkgTagsOrRoot := self.normalizedPkgFN(NIL);
  BEGIN
    TRY
      IF UseDCVS(self) THEN
        RETURN self.cvs.tagsAndSnaps(pkgTagsOrRoot, prefix,
                                     pkgName := self.pkgName);
      ELSE
        RETURN self.cvs.tags(pkgTagsOrRoot, prefix);
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END TagList; 

(*---------------------------------------------------------------------------*)
PROCEDURE TagExists(self : T; t : Tag.T) : BOOLEAN RAISES {E} =
  VAR 
    pkgTagsOrRoot := self.normalizedPkgFN(NIL);
    list : TextSeq.T;
    tag := t.denotation();
  BEGIN
    TRY
      IF UseDCVS(self) THEN
        list := self.cvs.tagsAndSnaps(pkgTagsOrRoot, tag);
      ELSE
        list := self.cvs.tags(pkgTagsOrRoot, tag);
      END;
      FOR i := 0 TO list.size() -1 DO
        WITH found = list.get(i) DO
          IF Text.Equal(tag, found) THEN
            RETURN TRUE;
          END;
        END;
      END;
      RETURN FALSE;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END TagExists; 

(*---------------------------------------------------------------------------*)
PROCEDURE Tags(self : T; prefix : TEXT; fn : APN.T := NIL) : TagSeq.T
  RAISES {E} =
  BEGIN
    TRY
      IF fn = NIL THEN
        fn := self.normalizedPkgFN(NIL);
      END;
      VAR
        tags : TextSeq.T;
        n    : INTEGER;
        arr  : REF ARRAY OF Tag.T;
        res  : TagSeq.T;
      BEGIN
        IF UseDCVS(self) THEN
          tags := self.cvs.tagsAndSnaps(fn, prefix, pkgName := self.pkgName);
        ELSE
          tags := self.cvs.tags(fn, prefix);
        END;
        n := tags.size();
        arr := NEW(REF ARRAY OF Tag.T, n);
        res := NEW(TagSeq.T).init(n);
        FOR i := 0 TO n - 1 DO
          arr^[i] := Tag.New(tags.get(i));
        END;
        TagSort.Sort(arr^);
        FOR i := 0 TO n - 1 DO
          res.addhi(arr^[i]);
        END;
        RETURN res;
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END Tags; 

(*---------------------------------------------------------------------------*)
PROCEDURE IsSticky(self : T; VAR tag : Tag.T) : BOOLEAN RAISES {E} =
  VAR pkgTagsOrRoot := self.pkgRoot; (* self.normalizedPkgFN(NIL); *)
      (* REVIEWME: We check _all_ files to ensure that there really
         is a common sticky tag. This may be paranoid or just conservative. *)
  BEGIN
    TRY
      VAR
        stickyTags := self.cvs.currentStickyTags(pkgTagsOrRoot);
      BEGIN
        IF stickyTags.size() # 1 THEN
          (* To be on a consistent release branch, 
             there must be exactly one sticky tag. *)
          RETURN FALSE;
        END;
        tag := Tag.New(stickyTags.get(0));
        RETURN TRUE;
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END IsSticky;

(*---------------------------------------------------------------------------*)
PROCEDURE IsReleaseBranch(self : T; VAR tag : Tag.T) : BOOLEAN RAISES {E} =
  VAR pkgTagsOrRoot := self.pkgRoot; (* self.normalizedPkgFN(NIL); *)
      (* REVIEWME: We check _all_ files to ensure that this really
         is a release. This may be paranoid or just conservative. *)
  BEGIN
    TRY
      VAR
        stickyTags := self.cvs.currentStickyTags(pkgTagsOrRoot);
      BEGIN
        IF stickyTags.size() # 1 THEN
          (* To be on a consistent release branch, 
             there must be exactly one sticky tag. *)
          RETURN FALSE;
        END;
        tag := Tag.New(stickyTags.get(0));
        RETURN tag.isStableBranchTag();
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END IsReleaseBranch; 

(*---------------------------------------------------------------------------*)
PROCEDURE IsChangeBranch(self : T; VAR tag : Tag.T) : BOOLEAN RAISES {E} =
  VAR pkgTagsOrRoot := self.pkgRoot; (* self.normalizedPkgFN(NIL); *)
      (* REVIEWME: We check _all_ files to ensure that this really
         is a change. This may be paranoid or just conservative. *)
  BEGIN
    TRY
      VAR
        stickyTags := self.cvs.currentStickyTags(pkgTagsOrRoot);
      BEGIN
        IF stickyTags.size() # 1 THEN
          (* To be on a consistent change branch, 
             there must be exactly one sticky tag. *)
          RETURN FALSE;
        END;
        tag := Tag.New(stickyTags.get(0));
        RETURN tag.isChangeBranchTag();
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END IsChangeBranch; 

(*---------------------------------------------------------------------------*)
PROCEDURE IsRelease(self : T; VAR tag : Tag.T) : BOOLEAN RAISES {E} =
  VAR pkgTagsOrRoot := self.pkgRoot; (* self.normalizedPkgFN(NIL); *)
      (* REVIEWME: We check _all_ files to ensure that this really
         is a release. This may be paranoid or just conservative. *)
  BEGIN
    TRY
      VAR
        stickyTags := self.cvs.currentStickyTags(pkgTagsOrRoot);
      BEGIN
        IF stickyTags.size() # 1 THEN
          (* To be on a consistent release, 
             there must be exactly one sticky tag. *)
          RETURN FALSE;
        END;
        tag := Tag.New(stickyTags.get(0));
        RETURN tag.kind() = Tag.Kind.Release;
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END IsRelease; 

(*---------------------------------------------------------------------------*)
PROCEDURE LatestReleaseBranch(self : T; VAR tag : Tag.T) : BOOLEAN RAISES {E} =
  VAR pkgTagsOrRoot := self.normalizedPkgFN(NIL);
  BEGIN
    TRY
      VAR
        releaseTags := self.cvs.tags(pkgTagsOrRoot, 
                                     Tag.KindToText(Tag.Kind.Release) & "_" & 
                                     Tag.CheckedName(self.pkgName));
      BEGIN
        tag := Tag.LatestTag(releaseTags, Tag.Kind.Release);
        IF tag # NIL THEN
          (* hack: we want to use the branch head here and not a static tag *)
          tag.t_attr := tag.t_attr + Tag.Attributes{Tag.Attribute.Stable};
          tag.t_attr := tag.t_attr + Tag.Attributes{Tag.Attribute.Branch};
          (* we also need to clear the original tag denotation *)
          tag.t_orig := NIL;
        END;
        RETURN tag # NIL;
      END
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END LatestReleaseBranch; 

(*---------------------------------------------------------------------------*)
PROCEDURE LatestChangeBranch(self : T; kind : Tag.Kind;
                             VAR tag : Tag.T) : BOOLEAN RAISES {E} =
  VAR pkgTagsOrRoot := self.normalizedPkgFN(NIL);
  BEGIN
    TRY
      VAR
        changeTags := self.cvs.tags(pkgTagsOrRoot, 
                                    Tag.KindToText(kind) & "_" & 
                                    Tag.CheckedName(self.pkgName));
      BEGIN
        tag := Tag.LatestTag(changeTags, Tag.Kind.Change);
        IF tag # NIL THEN
          (* hack: we want to use the branch head here and not a static tag *)
          tag.t_attr := tag.t_attr + Tag.Attributes{Tag.Attribute.Branch};
          (* we also need to clear the original tag denotation *)
          tag.t_orig := NIL;
        END;
        RETURN tag # NIL;
      END
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END LatestChangeBranch; 

(*---------------------------------------------------------------------------*)
PROCEDURE CurrentDevelopmentTag(self : T) : Tag.T RAISES {E} =
  VAR pkgTagsOrRoot := self.normalizedPkgFN(NIL);
  BEGIN
    TRY
      VAR
        develTags : TextSeq.T;
        latestTag : Tag.T;
      BEGIN
        IF UseDCVS(self) THEN
          develTags := self.cvs.tagsAndSnaps(
                                pkgTagsOrRoot,
                                Tag.KindToText(Tag.Kind.Devel) & "_" & 
                                Tag.CheckedName(self.pkgName));
        ELSE
          develTags := self.cvs.tags(pkgTagsOrRoot,
                                     Tag.KindToText(Tag.Kind.Devel) & "_" & 
                                     Tag.CheckedName(self.pkgName));
        END;
        latestTag := Tag.LatestTag(develTags, Tag.Kind.Devel);
        IF latestTag = NIL THEN
          RETURN Tag.Construct(Tag.Kind.Devel, self.pkgName, 0, 0, 0);
        END;
        RETURN latestTag;
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END CurrentDevelopmentTag;

(*---------------------------------------------------------------------------*)
PROCEDURE NextDevelopmentTag(self : T; ct : CommitType) : Tag.T RAISES {E} =
  BEGIN
    VAR
      latestTag := CurrentDevelopmentTag(self);
    BEGIN
      CASE ct OF
        CommitType.Major => latestTag.setNextMajorVersion();
      | CommitType.Minor => latestTag.setNextMinorVersion();
      | CommitType.Patch => latestTag.setNextPatchLevel();
      END;
      RETURN latestTag;
    END;
  END NextDevelopmentTag; 

(*---------------------------------------------------------------------------*)
PROCEDURE CurrentReleaseTag(self : T) : Tag.T RAISES {E} =
  BEGIN
    TRY
      VAR
        releaseTags : TextSeq.T;
        latestTag   : Tag.T;
        branchTag   : Tag.T;
        prefix      : TEXT;
      BEGIN
        IF IsReleaseBranch(self, branchTag) THEN
          (* hack: split of the last element *)
          prefix := branchTag.base(4);
        ELSIF IsRelease(self, branchTag) THEN
          RETURN branchTag;
        ELSE
          prefix := Tag.KindToText(Tag.Kind.Release) & "_" & 
                        Tag.CheckedName(self.pkgName);
        END;
        IF UseDCVS(self) THEN
          releaseTags := self.cvs.tagsAndSnaps(self.pkgRoot, prefix);
        ELSE
          releaseTags := self.cvs.tags(self.pkgRoot, prefix);
        END;
        latestTag := Tag.LatestTag(releaseTags, Tag.Kind.Release);
        IF latestTag = NIL THEN
          RETURN Tag.Construct(Tag.Kind.Release, self.pkgName, 0, 0, 0);
        END;
        RETURN latestTag;
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END CurrentReleaseTag;

(*---------------------------------------------------------------------------*)
PROCEDURE NextReleaseTag(self : T; ct : CommitType; 
                         branch : Tag.T := NIL) : Tag.T RAISES {E} =
  BEGIN
    TRY
      VAR
        releaseTags : TextSeq.T;
        latestTag   : Tag.T;
        prefix      : TEXT;
      BEGIN
        IF branch # NIL THEN
          (* hack: split of the last element *)
          prefix := branch.base(4);
        ELSE
          prefix := Tag.KindToText(Tag.Kind.Release) & "_" & 
                        Tag.CheckedName(self.pkgName);
        END;
        IF UseDCVS(self) THEN
          releaseTags := self.cvs.tagsAndSnaps(self.pkgRoot, prefix);
        ELSE
          releaseTags := self.cvs.tags(self.pkgRoot, prefix);
        END;
        latestTag := Tag.LatestTag(releaseTags, Tag.Kind.Release);
        IF latestTag = NIL THEN
          RETURN Tag.Construct(Tag.Kind.Release, self.pkgName, 0, 0, 0);
        END;
        CASE ct OF
          CommitType.Major => latestTag.setNextMajorVersion();
        | CommitType.Minor => latestTag.setNextMinorVersion();
        | CommitType.Patch => latestTag.setNextPatchLevel();
        END;
        RETURN latestTag;
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END NextReleaseTag; 

(*---------------------------------------------------------------------------*)
PROCEDURE NextChangeTag(self : T; changeName : TEXT; changeType : Tag.Kind;
                        ct : CommitType; branch : Tag.T := NIL) : Tag.T
  RAISES {E} =
  BEGIN
    TRY
      VAR
        changeTags : TextSeq.T;
        latestTag  : Tag.T;
        prefix     : TEXT;
      BEGIN
        IF branch # NIL THEN
          (* hack: split of the last element *)
          prefix := branch.base(3);
        ELSE
          prefix := Tag.KindToText(changeType) & "_" & 
                        Tag.CheckedName(changeName) & "_" &
                        Tag.CheckedName(self.pkgName);
        END;
        IF UseDCVS(self) THEN
          changeTags := self.cvs.tagsAndSnaps(self.pkgRoot, prefix);
        ELSE
          changeTags := self.cvs.tags(self.pkgRoot, prefix);
        END;
        latestTag := Tag.LatestTag(changeTags, changeType);
        IF latestTag = NIL THEN
          RETURN Tag.Construct(changeType, self.pkgName, 0, 0, 0,
                               change := changeName);
        END;
        CASE ct OF
          CommitType.Major => latestTag.setNextMajorVersion();
        | CommitType.Minor => latestTag.setNextMinorVersion();
        | CommitType.Patch => latestTag.setNextPatchLevel();
        END;
        RETURN latestTag;
      END;
    EXCEPT
      CVS.E(m) => RAISE E("Tag listing failed: " & m);
    END;
  END NextChangeTag; 

(*---------------------------------------------------------------------------*)
PROCEDURE CurrentLocalTag(self : T) : Tag.T RAISES {E} =
  VAR
    tag  : Tag.T;
    tag2 : Tag.T;
    tt   : TEXT;
  BEGIN
    IF self.isReleaseBranch(tag) THEN
      TRY
	tt := LastTagFileEntry(self);
      EXCEPT
	E(m) => 
        tt := "devel_dummy_0_0_0";
        m := "PkgTags file broken: " & m;
        MsgX.Warning2(self.msgif, "PkgVC.CurrentLocalTag()", m);
      END;
      tag2 := NEW(Tag.T).initFromText(tt);
      TRY
        VAR
          prefix := tag.base(4);
          releaseTags : TextSeq.T;
          latestTag : Tag.T;
        BEGIN
          IF UseDCVS(self) THEN
            releaseTags := self.cvs.tagsAndSnaps(self.pkgRoot, prefix);
          ELSE
            releaseTags := self.cvs.tags(self.pkgRoot, prefix);
          END;
          latestTag := Tag.LatestTag(releaseTags, Tag.Kind.Release);
          IF Text.Equal(prefix, tag2.base(4)) THEN
            (* the last entry in PkgTags is on the current release branch *)
            RETURN tag2;
          ELSE
            IF latestTag = NIL THEN
              RETURN Tag.Construct(Tag.Kind.Release, self.pkgName, 0, 0, 0);
            END;
            RETURN latestTag;
          END;
        END;
      EXCEPT
        CVS.E(m) => RAISE E("Tag listing failed: " & m);
      END;
    ELSIF self.isRelease(tag) THEN
      RETURN tag; 
    ELSIF self.isSticky(tag)  THEN
      RETURN tag;
    ELSIF self.upToDate() THEN
      tag := self.currentDevelopmentTag();
      RETURN tag;
    ELSE
      TRY
        tt := LastTagFileEntry(self);
      EXCEPT
        E(m) => 
        MsgX.Error2(self.msgif, "PkgVC.CurrentLocalTag()", m); RETURN Tag.Head;
      END;
      tag := NEW(Tag.T).initFromText(tt);
      RETURN tag;
    END;
  END CurrentLocalTag;

(*---------------------------------------------------------------------------*)
PROCEDURE PackageDirectory(<*UNUSED*> self : T) : APNSeq.T RAISES {E} =
  BEGIN
    RAISE E("Sorry, not yet implemented");
  END PackageDirectory;

(*---------------------------------------------------------------------------*)
PROCEDURE OverwritePackageDirectory(
    <*UNUSED*> self : T; <*UNUSED*> dir : APNSeq.T) RAISES {E} =
  BEGIN
    RAISE E("Sorry, not yet implemented");
  END OverwritePackageDirectory;

(*---------------------------------------------------------------------------*)
PROCEDURE UpdatePackageDirectory(
    <*UNUSED*> self : T;
    <*UNUSED*> VAR added : APNSeq.T;
    <*UNUSED*> VAR removed : APNSeq.T;
    <*UNUSED*> confirmRemoval := TRUE;
    <*UNUSED*> confirmAddition := FALSE) RAISES {E} =
  BEGIN
    RAISE E("Sorry, not yet implemented");
  END UpdatePackageDirectory;

(*---------------------------------------------------------------------------*)
PROCEDURE AddFiles(
    <*UNUSED*> self : T; <*UNUSED*> flist : APNSeq.T) RAISES {E} =
  BEGIN
    RAISE E("Sorry, not yet implemented");
  END AddFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE VersionControlledFiles(self : T; includePkgName := FALSE) 
  : APNSeq.T RAISES {E} =
  VAR 
    res  := NEW(APNSeq.T).init();
    frs  :  FileRevisionSeq.T;
    pn   :  TEXT;
    pkgRootLen := Text.Length(self.pkgRoot.denotation());
    pkgNameLen := Text.Length(self.pkgName);

  (*-------------------------------------------------------------------------*)
  (*
  PROCEDURE Gather(obj : APN.T) =
    VAR
      fn   :  TEXT;
    BEGIN
      IF NOT FileObj.Exists(obj) THEN 
	RETURN;
      ELSIF Text.Equal(APN.Last(obj).denotation(), "CVS") THEN
	RETURN; (* hack: never try to add CVS directories *)
      ELSIF FileObj.IsDir(obj) THEN
        IF NOT FileObj.IsDir(APN.Join(obj, APN.New("CVS"), NIL)) THEN
          RETURN;
        END;
	TRY
	  WITH it = FS.Iterate(obj.denotation()) DO
	    WHILE it.next(fn) DO
	      WITH fnbase = APN.New(fn) DO
		WITH fnfull = APN.Join(obj, fnbase, NIL) DO
		  Gather(fnfull);
		END;
	      END;
	    END;
	  END;
	EXCEPT ELSE
	  MsgX.Error(self.msgif, "cannot read directory " & obj.denotation());
	END;
	RETURN;
      ELSIF FileObj.IsFile(obj) THEN
        IF self.known(obj) THEN
          WITH pn = Text.Sub(obj.denotation(), plen + 1) DO
            res.addhi(APN.New(pn));
          END;
        END;
      END;
    END Gather;
   *)
  (*-------------------------------------------------------------------------*)
  BEGIN (* VersionControlledFiles *)
    (* Gather(self.pkgRoot); *)
    self.cvs.setCVSIgnore(self.ignorePatterns());
    frs := self.cvs.allFiles(self.pkgRoot);
    FOR i := 0 TO frs.size() - 1 DO
      WITH elem = frs.get(i) DO
        IF includePkgName THEN
          pn := Text.Sub(elem.file.denotation(), pkgRootLen - pkgNameLen);
          res.addhi(APN.New(pn));
        ELSE
          pn := Text.Sub(elem.file.denotation(), pkgRootLen + 1);
          res.addhi(APN.New(pn));
        END;
      END;
    END;
    RETURN res;
    IF FALSE THEN RAISE E("FIXME") END; <*NOWARN*>
  END VersionControlledFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE FilesAddedOrRemoved(self : T) : BOOLEAN RAISES {E} =
  VAR
    list  : TextSeq.T;
  BEGIN
    (* set ignore patterns *)
    self.cvs.setCVSIgnore(self.ignorePatterns());
    (* get state of files *)
    TRY
      list := self.cvs.stateList(self.pkgRoot);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
    FOR i := 0 TO list.size() - 1 DO
      WITH line = list.get(i), state = Text.GetChar(line, 0) DO
        IF state = 'A' OR state = 'R' THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END FilesAddedOrRemoved;

(*---------------------------------------------------------------------------*)
PROCEDURE GetFileStatus(self : T;
                        VAR added : APNSeq.T;
                        VAR removed : APNSeq.T;
                        VAR modified : APNSeq.T;
                        VAR conflicts : APNSeq.T;
                        VAR needingUpdate : APNSeq.T;
                        VAR unknown : APNSeq.T;
                        includePkgName := FALSE) RAISES {E} =
  VAR 
    list : TextSeq.T;
    pkgNameLen := Text.Length(self.pkgName);
  BEGIN
    (* initialize results *)
    IF added = NIL THEN added := NEW(APNSeq.T) END; EVAL added.init();
    IF removed = NIL THEN removed := NEW(APNSeq.T) END; EVAL removed.init();
    IF modified = NIL THEN modified := NEW(APNSeq.T) END; EVAL modified.init();
    IF conflicts = NIL THEN conflicts := NEW(APNSeq.T) END; 
    EVAL conflicts.init();
    IF needingUpdate = NIL THEN needingUpdate := NEW(APNSeq.T) END; 
    EVAL needingUpdate.init();
    IF unknown = NIL THEN unknown := NEW(APNSeq.T) END; EVAL unknown.init();
    (* set ignore patterns *)
    self.cvs.setCVSIgnore(self.ignorePatterns());
    (* get state of files and distribute names according to status *)
    TRY
      list := self.cvs.stateList(self.pkgRoot);
    EXCEPT
      CVS.E(m) => RAISE E("Status check failed: " & m);
    END;
    FOR i := 0 TO list.size() - 1 DO
      WITH line = list.get(i) DO
        VAR
          state := Text.GetChar(line, 0);
          name  :  TEXT;
        BEGIN
          IF includePkgName THEN
            name  := PathRepr.Native(Text.Sub(line, 2)); 
          ELSE
            name  := PathRepr.Native(Text.Sub(line, 3 + pkgNameLen)); 
          END;
          IF    state = 'A' THEN
            added.addhi(APN.New(name));
          ELSIF state = 'R' THEN
            removed.addhi(APN.New(name));
          ELSIF state = 'M' THEN
            modified.addhi(APN.New(name));
          ELSIF state = 'U' THEN
            needingUpdate.addhi(APN.New(name));
          ELSIF state = 'C' THEN
            conflicts.addhi(APN.New(name));
          ELSIF state = '?' THEN
            unknown.addhi(APN.New(name));
          END;
        END;
      END;
    END;
  END GetFileStatus;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckVersionControlStatus(self : T; fl     : APNSeq.T; 
                                    VAR vcFiles      : APNSeq.T;
                                    VAR unknownFiles : APNSeq.T) RAISES {E} =
  BEGIN
    self.cvs.setCVSIgnore(self.ignorePatterns());
    vcFiles := NEW(APNSeq.T).init();
    unknownFiles := NEW(APNSeq.T).init();
    FOR i := 0 TO fl.size() - 1 DO
      VAR f := fl.get(i); BEGIN
	IF NOT APN.Absolute(f) THEN
          f := APN.Join(self.pkgRoot, f, NIL);
        END;
        IF self.known(f) THEN
          vcFiles.addhi(f);
        ELSE
          unknownFiles.addhi(f);
        END;
      END;
    END;
    IF FALSE THEN RAISE E("FIXME") END; <*NOWARN*>
  END CheckVersionControlStatus;

(*---------------------------------------------------------------------------*)
PROCEDURE StateLabels(self : T) : TextTextTbl.T RAISES {E} =
  (* tag -> state label *)
  BEGIN
    TRY
      VAR
        atfn  := APN.New(TagFileName(self));
        taggedRevisions := self.cvs.taggedRevisions(atfn); 
        (* TextTextTbl.T: tag -> rev *)
        revAndLabels := self.cvs.revisionsAndLabels(self.cvs.logFile(atfn));
        (* TextTextTbl.T: rev -> state) *)
        iter : TextTextTbl.Iterator;
        rev, tag, state : TEXT;
        res : TextTextTbl.T;
        ttag : Tag.T;
      BEGIN
        IF taggedRevisions = NIL THEN
          RAISE E("cannot get tag -> revision map for " & atfn.denotation());
        END;
        iter := taggedRevisions.iterate();
        res := NEW(TextTextTbl.Default).init(taggedRevisions.size());
        WHILE iter.next(tag, rev) DO
          ttag := Tag.New(tag);
          IF NOT ttag.isStableBranchTag() AND 
            NOT CVS.IsCVSBranch(rev) THEN
            IF NOT revAndLabels.get(rev, state) THEN
              MsgX.Error2(self.msgif, "PkgVC.StateLabels", 
                          "undefined state for rev " & rev & "  " & tag);
              state := "undefined";
            END;
            EVAL res.put(tag, state);
          END;
        END;
        RETURN res;
        IF FALSE THEN RAISE E("FIXME") END; <*NOWARN*>
      END
    EXCEPT
      CVS.E(m) => RAISE E("cvs log failed: " & m);
    END;
  END StateLabels;

(*---------------------------------------------------------------------------*)
PROCEDURE StateLabel(self : T; tag : Tag.T := NIL) : TEXT RAISES {E} =
  VAR
    labels := StateLabels(self);
    res    :  TEXT;
  BEGIN
    IF tag = NIL THEN
      tag := CurrentLocalTag(self);
    ELSIF tag = Tag.Head THEN
      tag := CurrentDevelopmentTag(self);
    END;
    IF NOT labels.get(tag.denotation(), res) THEN
      RAISE E("no state label found for tag " & tag.denotation());
    END;
    RETURN res;
  END StateLabel;

(*---------------------------------------------------------------------------*)
PROCEDURE SetLabel(self : T; tag : Tag.T; label : TEXT; msg := NIL) 
  RAISES {E} =
  BEGIN
    TRY
      VAR
        atfn := APN.New(TagFileName(self));
        taggedRevisions := self.cvs.taggedRevisions(atfn); 
        (* TextTextTbl.T: rev -> tag *)
        rev : TEXT;
        log : TEXT;
      BEGIN
        IF tag = NIL THEN
          tag := CurrentLocalTag(self);
        ELSIF tag = Tag.Head THEN
          tag := CurrentDevelopmentTag(self);
        END;
        IF NOT taggedRevisions.get(tag.denotation(), rev) THEN
          RAISE E("cannot get revision from PkgTags for tag " & 
                tag.denotation());
        END;
        IF Text.Equal(self.cvs.getLabel(atfn, rev), "dead") THEN
          RAISE E("cannot change reserved label `dead' for PkgTags rev " &
                tag.denotation());
        END;
        IF NOT self.cvs.setLabel(atfn, rev, label) THEN
          RAISE E("cannot set label for PkgTags rev " & tag.denotation() &
                " to " & label);
        END;
        log := "Label of version " & tag.denotation() & " (PkgTags rev. " &
                   rev & ") set to `" & label & "'\n";
        IF msg # NIL THEN
          log := log & msg;
        END;
        ForceCommitTagFile(self, log);
      END;
    EXCEPT
      CVS.E(m) => RAISE E("cvs failed: " & m);
    END;
  END SetLabel;

(*---------------------------------------------------------------------------*)
PROCEDURE LabelLog(<*UNUSED*> self : T) : TEXT RAISES {E} =
  BEGIN
    RAISE E("Sorry, not yet implemented");
  END LabelLog;

(*---------------------------------------------------------------------------*)
VAR
  lb := OSSpecials.LineBreak;
BEGIN
  confirmation := NEW(Confirmation.StdIOClosure);
  VC := NEW(T).init();  
END PkgVC.

