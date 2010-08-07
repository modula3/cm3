(*---------------------------------------------------------------------------*)
MODULE PoolSet;

IMPORT Text, TextSeq, TextTextTbl, Pathname, Thread, Process, 
       Rd, FileRd, Fmt, TextConv, ASCII;
IMPORT PkgBase, System, TextUtils, PathRepr, FileInfo, Checkpoint,
       PkgVC, TextPkgVCTbl, Tag, VCUtils, SMsg AS Msg,
       APN AS APN, FSUtils, RegEx, MsgX, MsgIF;

(*---------------------------------------------------------------------------*)
CONST 
  Undefined = "%$#&!42?";

(*---------------------------------------------------------------------------*)
PROCEDURE NoVCIF(<*UNUSED*> self : PkgVCAccessor;
                 <*UNUSED*> dir  : Pathname.T) : PkgVC.T RAISES {} =
  BEGIN
    RETURN NIL;
  END NoVCIF;

(*---------------------------------------------------------------------------*)
PROCEDURE NewVCIF(self : PkgVCCreator; dir : Pathname.T) : PkgVC.T 
  RAISES {Error} =
  VAR res : PkgVC.T;
  BEGIN
    IF dir = NIL THEN
      RAISE Error("cannot create version control object: directory NIL");
    END;
    TRY
      dir := FSUtils.CanonicalPathname(dir);
    EXCEPT
      FSUtils.E(e) => RAISE Error(e);
    END;
    res := NEW(PkgVC.T).init(self.msgif);
    TRY
      res.setPackageRoot(APN.New(dir));
      IF self.env # NIL THEN
        res.setEnvironment(self.env);
      END;
    EXCEPT
      PkgVC.E(e) => RAISE Error("cannot create version control object: " & e);
    END;
    RETURN res;
  END NewVCIF;

(*---------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "PoolSet Type 0.0" OBJECT
    pools      : TextSeq.T;     (* list of pools *)
    location   : TextTextTbl.T; (* mapping from package name -> pool (path) *)
    type       : TextTextTbl.T; (* mapping from package name -> package type *)
    pkgvc      : TextPkgVCTbl.T;(* mapping from package name -> vc interface *)
    cfg        : PkgBase.T;
    prefkind   : PkgBase.Kind;
    fileCache  : FileInfo.T;
    stateCache : Checkpoint.T;
    useCache   : BOOLEAN;
    cacheEarly : BOOLEAN;
    msgif      : MsgIF.T;
    pkgvcAcc   : PkgVCAccessor;
    internalVC : BOOLEAN;
    verboseCache : BOOLEAN;
  METHODS
  OVERRIDES
    init := Init;
    prependPool := PrependPool;
    appendPool := AppendPool;
    setPreferredPkgKind := SetPreferredPkgKind;
    exists := Exists;
    pkgPath := PkgPath;
    pkgType := PkgType;
    pkgVCIF := PkgVCIF;
    checkAll := CheckAll;
    execAction := ExecAction;
    execCmdList := ExecCmdList;
    getCmdOutput := GetCmdOutput;
    getAndCacheVersionState := GetAndCacheVersionState;
    fileContents := FileContents;
    checkout := Checkout;
    getFileCache := GetFileCache;
    cachedState := CachedState;
    updateCache := UpdateCache;
    newCheckpoint := NewCheckpoint;
    replaceStateCache := ReplaceStateCache;
    setAttr := SetAttr;
    clearAttr := ClearAttr;
    attrIsSet := AttrIsSet;
    setVal := SetVal;
    getVal := GetVal;
    delVal := DelVal;
    updateStateCache := UpdateStateCache;
    actionProbablyNeeded := ActionProbablyNeeded;
    dumpStateCache := DumpStateCache;
  END;
    

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; cfg : PkgBase.T;
               fn  : TEXT := NIL;
               useCache := TRUE;
               p1  : Pathname.T := NIL;
               p2  : Pathname.T := NIL;
               p3  : Pathname.T := NIL;
               p4  : Pathname.T := NIL;
               p5  : Pathname.T := NIL;
               msgif  : MsgIF.T := NIL;
               pkgvcAcc : PkgVCAccessor := NIL;
               verboseCache := TRUE;
               prefkind : TEXT := NIL;
               cacheEarly := FALSE) : T RAISES {Error} =
    (* 
       Initialize the search list with the given paths. `p1' has the greatest
       priority. The package type and action configuration must be contained
       in `cfg'.
    *)
  BEGIN
    self.pkgvcAcc := pkgvcAcc;
    self.internalVC := pkgvcAcc # NIL;
    self.msgif := msgif;
    self.useCache := useCache;
    self.verboseCache := verboseCache;
    self.cfg := cfg;
    self.pools := NEW(TextSeq.T).init(5);
    self.location := NEW(TextTextTbl.Default).init(40);
    self.type := NEW(TextTextTbl.Default).init(40);
    self.cacheEarly := cacheEarly;
    IF self.internalVC THEN
      self.pkgvc := NEW(TextPkgVCTbl.Default).init(40);
    ELSE
      self.pkgvc := NEW(TextPkgVCTbl.Default).init(1);
    END;
    self.prefkind := prefkind;
    self.fileCache := NEW(FileInfo.T).init(1000, APN.New(PathRepr.RootDir),
                                           self.msgif);
    self.cfg.setCache(self.fileCache);
    self.stateCache := Checkpoint.New(self.fileCache, self.msgif);
    IF useCache AND fn # NIL AND FSUtils.IsFile(fn) THEN
      TRY
        self.stateCache.fromFile(fn);
      EXCEPT
        Checkpoint.Error(e) => RAISE Error(e);
      END;
    END;
    IF p1 # NIL THEN
      AppendPool(self, p1);
    END;
    IF p2 # NIL THEN
      AppendPool(self, p2);
    END;
    IF p3 # NIL THEN
      AppendPool(self, p3);
    END;
    IF p4 # NIL THEN
      AppendPool(self, p4);
    END;
    IF p5 # NIL THEN
      AppendPool(self, p5);
    END;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE PrependPool(self : T; p : Pathname.T) RAISES {Error} =
    (* Prepend `p' to the search list. *)
  BEGIN
    TRY
      WITH pn = FSUtils.CanonicalPathname(PathRepr.Native(p)) DO
        self.pools.addlo(pn);
        IF self.cacheEarly AND self.useCache THEN
          IF self.verboseCache AND NOT Msg.vFlag THEN
            MsgX.T(self.msgif, "caching " & pn);
          END;
          self.fileCache.updateRec(APN.New(pn), NIL, NIL, 
                                   Checkpoint.skipDirExpr, 
                                   Checkpoint.skipFileExpr);
        END;
      END;
    EXCEPT
      FSUtils.E(e) => RAISE Error(e);
    END;
  END PrependPool;

(*---------------------------------------------------------------------------*)
PROCEDURE AppendPool(self : T; p : Pathname.T) RAISES {Error} =
    (* Append `p' to the search list. *)
  BEGIN
    TRY
      WITH pn = FSUtils.CanonicalPathname(PathRepr.Native(p)) DO
        self.pools.addhi(pn);
        IF self.cacheEarly AND self.useCache THEN
          IF self.verboseCache AND NOT Msg.vFlag THEN
            MsgX.T(self.msgif, "caching " & pn);
          END;
          self.fileCache.updateRec(APN.New(pn), NIL, NIL, 
                                   Checkpoint.skipDirExpr, 
                                   Checkpoint.skipFileExpr);
        END;
      END;
    EXCEPT
      FSUtils.E(e) => RAISE Error(e);
    END;
  END AppendPool;

(*---------------------------------------------------------------------------*)
PROCEDURE SetPreferredPkgKind(self : T; k : PkgBase.Kind) =
  BEGIN
    self.prefkind := k;
  END SetPreferredPkgKind;

(*---------------------------------------------------------------------------*)
PROCEDURE Exists(self : T; pkg : PkgBase.Name; 
                 hint : Pathname.T := NIL) : BOOLEAN RAISES {Error} =
    (* 
       Check for the existence (and the type) of package `pkg' in the list
       of pools and remember the results in an internal cache. Return
       TRUE if the package was found in one of the pools.
    *)
  VAR
    cached :  BOOLEAN;
    path   :  TEXT;
    kind   :  PkgBase.Kind;
    found  := FALSE;
    hintt  :  TEXT;
  BEGIN
    IF hint = NIL THEN 
      hintt := "NIL";
    ELSE
      TRY
        hint := FSUtils.CanonicalPathname(hint);
      EXCEPT
        FSUtils.E(e) => RAISE Error(e);
      END;
      hintt := hint;
    END;
    IF Msg.dFlag THEN
      MsgX.D(self.msgif, "PoolSet.Exists(" & pkg & ", " & hintt & ")");
    END;
    cached := self.location.get(pkg, path);
    IF NOT cached THEN
      IF hint # NIL THEN
        path := Pathname.Join(PathRepr.Native(hint), pkg, NIL);
        kind := self.prefkind;
	found := self.cfg.kindFound(path, kind);
      ELSE
        FOR i := 0 TO self.pools.size() - 1 DO
          path := Pathname.Join(self.pools.get(i), pkg, NIL);
          kind := self.prefkind;
          found := self.cfg.kindFound(path, kind);
          IF found THEN EXIT END;
          IF Msg.dFlag THEN
            MsgX.D(self.msgif, "package " & pkg & " not found at " & path);
          END;
        END;
      END;
      IF found THEN
        EVAL self.location.put(pkg, path);
        EVAL self.type.put(pkg, kind);
        IF Msg.dFlag THEN
          MsgX.D(self.msgif, "package " & pkg & " kind " & kind & 
            " found at " & path);
          MsgX.D(self.msgif, "checkpointing package " & path);
        END;
        TRY
          IF self.useCache THEN
            IF self.verboseCache AND NOT Msg.vFlag THEN
              MsgX.T(self.msgif, "scanning " & path);
            END;
            self.stateCache.update(path, self.cacheEarly);
          END;
        EXCEPT
          Checkpoint.Error(e) => RAISE Error(e);
        END;
      ELSE
        EVAL self.location.put(pkg, Undefined);
        EVAL self.type.put(pkg, Undefined);
      END;
      RETURN found;
    END;
    RETURN NOT Text.Equal(path, Undefined);
  END Exists;

(*---------------------------------------------------------------------------*)
PROCEDURE PkgPath(self : T; name : PkgBase.Name) : Pathname.T =
    (*
      Return the path of the package with name `name' if cached,
      NIL else.
    *)
  VAR path : Pathname.T;
  BEGIN
    IF self.location.get(name, path) THEN
      RETURN path;
    ELSE
      RETURN NIL;
    END;
  END PkgPath;

(*---------------------------------------------------------------------------*)
PROCEDURE PkgVCIF(self : T; name : PkgBase.Name) : PkgVC.T RAISES {Error} =
  VAR 
    res : PkgVC.T;
    dir : TEXT;
  BEGIN
    IF NOT self.internalVC THEN RETURN NIL END;
    IF NOT self.pkgvc.get(name, res) THEN
      dir := PkgPath(self, name);
      IF dir = NIL THEN
        RAISE Error("cannot create version control object for missing " &
              "package: " & name);
      END;
      res := self.pkgvcAcc.getVCIF(dir);
      EVAL self.pkgvc.put(name, res);
    END;
    RETURN res;
  END PkgVCIF;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckAll(self : T; pkgList : TextSeq.T; VAR res : TEXT;
                   VAR missingPackages : TextSeq.T;
                   hints : TextTextTbl.T := NIL; checkHomogeneity := TRUE;
                   ignoreMissingPackages := FALSE) : BOOLEAN =
    (*
      Check for the existence and (type) homogeneity of all packages
      in `pkgList'. Return TRUE if all packages exist and are of the
      same type.
    *)
  VAR 
    pkg     : PkgBase.Name := "";
    actPkg  : PkgBase.Name;
    kind    : PkgBase.Kind := NIL;
    actKind : PkgBase.Kind;
    hint    : TEXT;
  BEGIN
    FOR i := 0 TO pkgList.size() - 1 DO
      actPkg := pkgList.get(i);
      IF hints # NIL THEN
        IF hints.get(actPkg, hint) THEN
          hint := PathRepr.Native(hint);
        ELSE
          hint := NIL;
        END;
      ELSE
        hint := NIL;
      END;
      TRY
        IF NOT self.exists(actPkg, hint) THEN
          IF missingPackages = NIL THEN
            missingPackages := NEW(TextSeq.T).init();
          END;
          missingPackages.addhi(actPkg);
          IF NOT ignoreMissingPackages THEN
            res := actPkg & " does not exist";
            RETURN FALSE;
          END;
        END;
      EXCEPT
        Error(e) => res := e; RETURN FALSE;
      END;
      (* package exists, location and kind cached *)
      IF kind = NIL THEN
        kind := self.pkgType(actPkg);
        pkg := actPkg;
      ELSE
        actKind := self.pkgType(actPkg);
        IF checkHomogeneity AND NOT Text.Equal(kind, actKind) THEN
          res := "different kinds of packages found:  " & 
                 pkg & "->" & kind & ", " & actPkg & "->" & actKind;
          RETURN FALSE;
        END;
      END;
    END;
    RETURN TRUE;
  END CheckAll;

(*---------------------------------------------------------------------------*)
PROCEDURE PkgType(self : T; name : PkgBase.Name) : PkgBase.Kind =
    (* Return the type of package `name'. *)
  VAR kind : PkgBase.Kind;
  BEGIN
    IF self.type.get(name, kind) THEN
      RETURN kind;
    ELSE
      RETURN NIL;
    END;
  END PkgType;
      
(*---------------------------------------------------------------------------*)
PROCEDURE UpdateStateCache(self : T; dir : Pathname.T; 
                           action : PkgBase.Action; ret : INTEGER;
                           rescan := TRUE) 
  RAISES {Error} =

  PROCEDURE SetOnSuccess(attr : Checkpoint.Attr) =
    BEGIN
      IF ret = 0 THEN
        attrs := attrs + Checkpoint.AttrSet{attr};
      ELSE
        attrs := attrs - Checkpoint.AttrSet{attr};
      END;
    END SetOnSuccess;

  PROCEDURE ClearOnSuccess(attr : Checkpoint.Attr) =
    BEGIN
      IF ret = 0 THEN
        attrs := attrs - Checkpoint.AttrSet{attr};
      END;
    END ClearOnSuccess;

  PROCEDURE SetOnFailure(attr : Checkpoint.Attr) =
    BEGIN
      IF ret = 0 THEN
        attrs := attrs - Checkpoint.AttrSet{attr};
      ELSE
        attrs := attrs + Checkpoint.AttrSet{attr};
      END;
    END SetOnFailure;

  PROCEDURE Clear(attr : Checkpoint.Attr) =
    BEGIN
      attrs := attrs - Checkpoint.AttrSet{attr};
    END Clear;

  PROCEDURE SetDefaultUnbuilt() =
    BEGIN
      attrs := attrs - Checkpoint.AttrSet{
                         Checkpoint.Attr.DepMade,
                         Checkpoint.Attr.BuildOk,
                         Checkpoint.Attr.BuildOkL,
                         Checkpoint.Attr.BuildFailed,
                         Checkpoint.Attr.ShippedToLP,
                         Checkpoint.Attr.ShippedToPP,
                         Checkpoint.Attr.ShippedToGP
                       };
    END SetDefaultUnbuilt;

  PROCEDURE SetDefaultRebuild() =
    BEGIN
      attrs := attrs - Checkpoint.AttrSet{
                         Checkpoint.Attr.BuildOk,
                         Checkpoint.Attr.BuildOkL,
                         Checkpoint.Attr.BuildFailed,
                         Checkpoint.Attr.ShippedToLP,
                         Checkpoint.Attr.ShippedToPP,
                         Checkpoint.Attr.ShippedToGP
                       };
    END SetDefaultRebuild; 

  PROCEDURE SetDefaultUnshipped() =
    BEGIN
      attrs := attrs - Checkpoint.AttrSet{
                         Checkpoint.Attr.ShippedToGP,
                         Checkpoint.Attr.ShippedToPP,
                         Checkpoint.Attr.ShippedToLP
                       };
    END SetDefaultUnshipped;

  PROCEDURE ClearPackageTags() RAISES {Error} =
    BEGIN
      TRY
        self.stateCache.delVal(dir, "sticky-tag");
        self.stateCache.delVal(dir, "release-tag");
        self.stateCache.delVal(dir, "current-tag");
        self.stateCache.delVal(dir, "current-release-tag");
        self.stateCache.delVal(dir, "current-devel-tag");
      EXCEPT
        Checkpoint.Error(e) => 
        RAISE Error("cannot delete checkpoint value " & pkg & ": " & e);
      END;
    END ClearPackageTags;

  PROCEDURE ClearPkgVCCache() RAISES {Error} = 
    BEGIN
      IF self.pkgvcAcc # NIL THEN
        WITH vc = self.pkgVCIF(pkg) DO
          vc.flushCache();
        END;
      END;
    END ClearPkgVCCache; 

  VAR
    pkg    := Pathname.Last(dir);
    attrs  :  Checkpoint.AttrSet;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    TRY
      attrs := self.stateCache.getAttr(dir);
    EXCEPT
      Checkpoint.Error => 
      TRY
        self.stateCache.update(dir);
      EXCEPT
        Checkpoint.Error(e) => 
        RAISE Error("cannot set attributes for package " & pkg & ": " & e);
      END;
    END;

    IF Text.Equal(action, "build") THEN
      SetOnSuccess(Checkpoint.Attr.BuildOk);
      ClearOnSuccess(Checkpoint.Attr.BuildOkL);
      SetOnFailure(Checkpoint.Attr.BuildFailed);
      SetDefaultUnshipped();
    ELSIF Text.Equal(action, "buildlocal") THEN
      SetOnSuccess(Checkpoint.Attr.BuildOkL);
      ClearOnSuccess(Checkpoint.Attr.BuildOk);
      SetOnFailure(Checkpoint.Attr.BuildFailed);
      SetDefaultUnshipped();
    ELSIF Text.Equal(action, "checkconflicts") THEN
      SetOnSuccess(Checkpoint.Attr.Conflicts);
      SetOnFailure(Checkpoint.Attr.NoConflicts);
      rescan := FALSE;
    ELSIF Text.Equal(action, "checkmodified") THEN
      SetOnSuccess(Checkpoint.Attr.Modified);
      SetOnFailure(Checkpoint.Attr.Unmodified);
      rescan := FALSE;
    ELSIF Text.Equal(action, "checkout") THEN
      SetDefaultUnbuilt();
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      ClearOnSuccess(Checkpoint.Attr.Modified);
      ClearOnSuccess(Checkpoint.Attr.OutOfDate);
      ClearOnSuccess(Checkpoint.Attr.IsRelease);
      ClearOnSuccess(Checkpoint.Attr.NoRelease);
      ClearPackageTags();
      ClearPkgVCCache();
    ELSIF Text.Equal(action, "checkrelease") THEN
      SetOnSuccess(Checkpoint.Attr.IsRelease);
      SetOnFailure(Checkpoint.Attr.NoRelease);
      rescan := FALSE;
    ELSIF Text.Equal(action, "checkuptodate") THEN
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      SetOnFailure(Checkpoint.Attr.OutOfDate);
      rescan := FALSE;
    ELSIF Text.Equal(action, "clean") THEN
      SetDefaultUnbuilt();
    ELSIF Text.Equal(action, "commitdevelmajor") THEN
      ClearOnSuccess(Checkpoint.Attr.Modified);
      ClearOnSuccess(Checkpoint.Attr.Conflicts);
      ClearOnSuccess(Checkpoint.Attr.IsRelease);
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      SetOnSuccess(Checkpoint.Attr.Unmodified);
      SetOnSuccess(Checkpoint.Attr.NoRelease);
      ClearPackageTags();
      ClearPkgVCCache();
    ELSIF Text.Equal(action, "commitdevelminor") THEN
      ClearOnSuccess(Checkpoint.Attr.Modified);
      ClearOnSuccess(Checkpoint.Attr.Conflicts);
      ClearOnSuccess(Checkpoint.Attr.IsRelease);
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      SetOnSuccess(Checkpoint.Attr.Unmodified);
      SetOnSuccess(Checkpoint.Attr.NoRelease);
      ClearPackageTags();
      ClearPkgVCCache();
    ELSIF Text.Equal(action, "commitdevelpatch") THEN
      ClearOnSuccess(Checkpoint.Attr.Modified);
      ClearOnSuccess(Checkpoint.Attr.Conflicts);
      ClearOnSuccess(Checkpoint.Attr.IsRelease);
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      SetOnSuccess(Checkpoint.Attr.Unmodified);
      SetOnSuccess(Checkpoint.Attr.NoRelease);
      ClearPackageTags();
      ClearPkgVCCache();
    ELSIF Text.Equal(action, "commitreleasemajor") THEN
      ClearOnSuccess(Checkpoint.Attr.Modified);
      ClearOnSuccess(Checkpoint.Attr.Conflicts);
      ClearOnSuccess(Checkpoint.Attr.NoRelease);
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      SetOnSuccess(Checkpoint.Attr.Unmodified);
      SetOnSuccess(Checkpoint.Attr.IsRelease);
      ClearPackageTags();
      ClearPkgVCCache();
      SetDefaultUnbuilt();
    ELSIF Text.Equal(action, "commitreleaseminor") THEN
      ClearOnSuccess(Checkpoint.Attr.Modified);
      ClearOnSuccess(Checkpoint.Attr.Conflicts);
      ClearOnSuccess(Checkpoint.Attr.NoRelease);
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      SetOnSuccess(Checkpoint.Attr.Unmodified);
      SetOnSuccess(Checkpoint.Attr.IsRelease);
      ClearPackageTags();
      ClearPkgVCCache();
      SetDefaultUnbuilt();
    ELSIF Text.Equal(action, "commitreleasepatch") THEN
      ClearOnSuccess(Checkpoint.Attr.Modified);
      ClearOnSuccess(Checkpoint.Attr.Conflicts);
      ClearOnSuccess(Checkpoint.Attr.NoRelease);
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      SetOnSuccess(Checkpoint.Attr.Unmodified);
      SetOnSuccess(Checkpoint.Attr.IsRelease);
      ClearPackageTags();
      ClearPkgVCCache();
      SetDefaultUnbuilt();
    ELSIF Text.Equal(action, "conflicts") THEN
      SetOnSuccess(Checkpoint.Attr.Conflicts);
      SetOnFailure(Checkpoint.Attr.NoConflicts);
      rescan := FALSE;
    ELSIF Text.Equal(action, "currentdeveltag") THEN
    ELSIF Text.Equal(action, "currentlabel") THEN
    ELSIF Text.Equal(action, "currentreleasetag") THEN
    ELSIF Text.Equal(action, "currenttag") THEN
    ELSIF Text.Equal(action, "externalshell") THEN
      rescan := FALSE;
    ELSIF Text.Equal(action, "getlabel") THEN
    ELSIF Text.Equal(action, "isrelease") THEN
      SetOnSuccess(Checkpoint.Attr.IsRelease);
      SetOnFailure(Checkpoint.Attr.NoRelease);
      rescan := FALSE;
    ELSIF Text.Equal(action, "listlabels") THEN
    ELSIF Text.Equal(action, "mkdep") THEN
      SetOnSuccess(Checkpoint.Attr.DepMade);
    ELSIF Text.Equal(action, "modified") THEN
      SetOnSuccess(Checkpoint.Attr.Modified);
      SetOnFailure(Checkpoint.Attr.Unmodified);
      rescan := FALSE;
    ELSIF Text.Equal(action, "realclean") THEN
      SetDefaultUnbuilt();
    ELSIF Text.Equal(action, "setlabel") THEN
      self.delVal(pkg, "current-label");
    ELSIF Text.Equal(action, "shipglobal") THEN
      SetOnSuccess(Checkpoint.Attr.ShippedToGP);
    ELSIF Text.Equal(action, "shiplocal") THEN
      SetOnSuccess(Checkpoint.Attr.ShippedToLP);
    ELSIF Text.Equal(action, "shipproject") THEN
      SetOnSuccess(Checkpoint.Attr.ShippedToPP);
    ELSIF Text.Equal(action, "update") THEN
      SetDefaultUnbuilt();
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      ClearOnSuccess(Checkpoint.Attr.OutOfDate);
      ClearOnSuccess(Checkpoint.Attr.IsRelease);
      ClearOnSuccess(Checkpoint.Attr.NoRelease);
      ClearPackageTags();
      ClearPkgVCCache();
    ELSIF Text.Equal(action, "merge") OR Text.Equal(action, "merge2") THEN
      SetDefaultUnbuilt();
      ClearPackageTags();
      ClearPkgVCCache();
      Clear(Checkpoint.Attr.Modified);
      Clear(Checkpoint.Attr.Unmodified);
      Clear(Checkpoint.Attr.UpToDate);
      Clear(Checkpoint.Attr.NoConflicts);
    ELSIF Text.Equal(action, "uptodate") THEN
      SetOnSuccess(Checkpoint.Attr.UpToDate);
      SetOnFailure(Checkpoint.Attr.OutOfDate);
      rescan := FALSE;
    ELSIF Text.Equal(action, "any-user-cmd") THEN
    ELSIF Text.Equal(action, "need-mkdep-build-ship") THEN
      IF Checkpoint.Attr.Changed IN attrs THEN
        SetDefaultUnbuilt();
        rescan := FALSE;
      END;
    ELSIF Text.Equal(action, "need-build-ship") THEN
      SetDefaultRebuild();
      rescan := FALSE;
    ELSIF Text.Equal(action, "clear-mod-unmod") THEN
      Clear(Checkpoint.Attr.Modified);
      Clear(Checkpoint.Attr.Unmodified);
      ClearPkgVCCache();
      rescan := FALSE;
    ELSIF Text.Equal(action, "clear-utd-nocfl") THEN
      Clear(Checkpoint.Attr.UpToDate);
      Clear(Checkpoint.Attr.NoConflicts);
      ClearPkgVCCache();
      self.delVal(pkg, "current-label");
      rescan := FALSE;
    ELSIF Text.Equal(action, "clear-tags") THEN
      ClearOnSuccess(Checkpoint.Attr.IsRelease);
      ClearOnSuccess(Checkpoint.Attr.NoRelease);
      ClearPackageTags();
      ClearPkgVCCache();
      rescan := FALSE;
    ELSIF Text.Equal(action, "diff") THEN
      rescan := FALSE;
    ELSIF Text.Equal(action, "diff1") THEN
      rescan := FALSE;
    ELSIF Text.Equal(action, "diff2") THEN
      rescan := FALSE;
    END;
    Clear(Checkpoint.Attr.Changed);
    TRY
      dir := FSUtils.CanonicalPathname(dir);
      self.stateCache.setAttr(dir, attrs);
    EXCEPT
      Checkpoint.Error(e) => 
      RAISE Error("cannot set attributes for package " & pkg & ": " & e);
    | FSUtils.E(e) => 
      RAISE Error("cannot set attributes for package " & pkg & ": " & e);
    END;
    IF rescan THEN
      TRY
        IF self.verboseCache AND NOT Msg.vFlag THEN
          MsgX.T(self.msgif, "rescanning " & dir);
        END;
        self.stateCache.update(dir);
      EXCEPT
        Checkpoint.Error(e) => 
        RAISE Error("cannot checkpoint package " & pkg & ": " & e);
      END;
    END;
  END UpdateStateCache;

(*---------------------------------------------------------------------------*)
PROCEDURE ActionProbablyNeeded(self    : T; 
                               pkg     : PkgBase.Name;  
                               dir     : Pathname.T; 
                               action  : PkgBase.Action;
                               VAR ret : INTEGER;
                               VAR res : TEXT) : BOOLEAN 
  RAISES {Error} =
  VAR
    attrs  :  Checkpoint.AttrSet;
  BEGIN
    res := NIL;
    ret := 0;
    (* `ret' is the fake return value of the action. `0' means
       `okay', `yes', and `true', everything else `failure',
       `no', and `false'. *)
    IF NOT self.useCache THEN RETURN TRUE END;
    TRY
      dir := FSUtils.CanonicalPathname(dir);
    EXCEPT
      FSUtils.E(e) => RAISE Error(e);
    END;
    TRY
      attrs := self.stateCache.getAttr(dir);
    EXCEPT
      Checkpoint.Error(e) => 
      RAISE Error("cannot set attributes for package " & pkg & ": " & e);
    END;

    IF Text.Equal(action, "build") THEN
      RETURN Checkpoint.Attr.BuildFailed IN attrs OR
             NOT Checkpoint.Attr.BuildOk IN attrs;
    ELSIF Text.Equal(action, "buildlocal") THEN
      RETURN Checkpoint.Attr.BuildFailed IN attrs OR
             NOT Checkpoint.Attr.BuildOkL IN attrs;
    ELSIF Text.Equal(action, "checkconflicts") THEN
      IF Checkpoint.Attr.Conflicts IN attrs THEN
        ret := 0; RETURN TRUE;
      ELSIF Checkpoint.Attr.NoConflicts IN attrs THEN
        ret := 1; RETURN TRUE;
      ELSE
        RETURN TRUE;
      END;
    ELSIF Text.Equal(action, "checkmodified") THEN 
      IF Checkpoint.Attr.Modified IN attrs THEN
        ret := 0; RETURN TRUE;
      ELSIF Checkpoint.Attr.Unmodified IN attrs THEN
        ret := 1; RETURN FALSE;
      ELSE
        RETURN TRUE;
      END;
    ELSIF Text.Equal(action, "checkout") THEN
    ELSIF Text.Equal(action, "checkrelease") THEN
      IF Checkpoint.Attr.IsRelease IN attrs THEN
        ret := 0; RETURN FALSE;
      ELSIF Checkpoint.Attr.NoRelease IN attrs THEN
        ret := 1; RETURN FALSE;
      ELSE
        RETURN TRUE;
      END;
    ELSIF Text.Equal(action, "checkuptodate") THEN
      IF Checkpoint.Attr.UpToDate IN attrs THEN
        ret := 0; RETURN TRUE;
      ELSIF Checkpoint.Attr.OutOfDate IN attrs THEN
        ret := 1; RETURN TRUE;
      ELSE
        RETURN TRUE;
      END;
    ELSIF Text.Equal(action, "clean") THEN
    ELSIF Text.Equal(action, "commitdevelmajor") THEN
    ELSIF Text.Equal(action, "commitdevelminor") THEN
    ELSIF Text.Equal(action, "commitdevelpatch") THEN
    ELSIF Text.Equal(action, "commitreleasemajor") THEN
    ELSIF Text.Equal(action, "commitreleaseminor") THEN
    ELSIF Text.Equal(action, "commitreleasepatch") THEN
    ELSIF Text.Equal(action, "conflicts") THEN
      IF Checkpoint.Attr.Conflicts IN attrs THEN
        ret := 0; RETURN FALSE;
      ELSIF Checkpoint.Attr.NoConflicts IN attrs THEN
        ret := 1; RETURN FALSE;
      ELSE
        RETURN TRUE;
      END;
    ELSIF Text.Equal(action, "currentdeveltag") THEN
      res := self.getVal(pkg, "current-devel-tag");
      RETURN res = NIL;
    ELSIF Text.Equal(action, "currentlabel") THEN
      res := self.getVal(pkg, "current-label");
      RETURN res = NIL;
    ELSIF Text.Equal(action, "currentreleasetag") THEN
      res := self.getVal(pkg, "current-release-tag");
      RETURN res = NIL;
    ELSIF Text.Equal(action, "currenttag") THEN
      res := self.getVal(pkg, "current-tag");
      RETURN res = NIL;
    ELSIF Text.Equal(action, "externalshell") THEN
    ELSIF Text.Equal(action, "getlabel") THEN
    ELSIF Text.Equal(action, "isrelease") THEN 
      res := self.getVal(pkg, "release-tag");
      IF Checkpoint.Attr.IsRelease IN attrs THEN
        ret := 0; RETURN FALSE;
      ELSIF Checkpoint.Attr.NoRelease IN attrs THEN
        ret := 1; RETURN FALSE;
      ELSE
        RETURN TRUE;
      END;
    ELSIF Text.Equal(action, "listlabels") THEN
    ELSIF Text.Equal(action, "mkdep") THEN
      RETURN NOT Checkpoint.Attr.DepMade IN attrs;
    ELSIF Text.Equal(action, "modified") THEN
      IF Checkpoint.Attr.Modified IN attrs THEN
        ret := 0; RETURN FALSE;
      ELSIF Checkpoint.Attr.Unmodified IN attrs THEN
        ret := 1; RETURN FALSE;
      ELSE
        RETURN TRUE;
      END;
    ELSIF Text.Equal(action, "realclean") THEN
    ELSIF Text.Equal(action, "setlabel") THEN
    ELSIF Text.Equal(action, "shipglobal") THEN
      RETURN NOT Checkpoint.Attr.ShippedToGP IN attrs;
    ELSIF Text.Equal(action, "shiplocal") THEN
      RETURN NOT Checkpoint.Attr.ShippedToLP IN attrs;
    ELSIF Text.Equal(action, "shipproject") THEN
      RETURN NOT Checkpoint.Attr.ShippedToPP IN attrs;
    ELSIF Text.Equal(action, "update") THEN
    ELSIF Text.Equal(action, "uptodate") THEN
      IF Checkpoint.Attr.UpToDate IN attrs THEN
        ret := 0; RETURN FALSE;
      ELSIF Checkpoint.Attr.OutOfDate IN attrs THEN
        ret := 1; RETURN FALSE;
      ELSE
        RETURN TRUE;
      END;
    ELSIF Text.Equal(action, "any-user-cmd") THEN
    ELSIF Text.Equal(action, "evaluate-changes") THEN
    END;

    RETURN TRUE;
  END ActionProbablyNeeded;

(*---------------------------------------------------------------------------*)
PROCEDURE ResultFromInternalVersionControl(
    self       : T;
    pkg        : PkgBase.Name; 
    action     : PkgBase.Action;
    parameters : TextTextTbl.T;
    VAR res    : TEXT;
    VAR ret    : INTEGER) : BOOLEAN RAISES {Error} =

  PROCEDURE Ret(b : BOOLEAN) : INTEGER =
    BEGIN
      IF b THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END;
    END Ret;

  PROCEDURE TagText(tag : Tag.T) : TEXT =
    BEGIN
      IF tag = NIL THEN
        RETURN "no appropriate tag found";
      END;
      RETURN tag.denotation();
    END TagText;

  CONST DoubleQuote = '\"';
  CONST Quote = '\'';
        
  PROCEDURE EvalMsgOptions() = 
    VAR opts : TEXT;

    PROCEDURE EvalOpts() =
      VAR args, res : TEXT;

      PROCEDURE EvalOpt(name : TEXT; VAR res : TEXT) : BOOLEAN =
        VAR
          start, end : INTEGER;
          c : CHAR;

          PROCEDURE FindNext(c : CHAR) =
            BEGIN
              WHILE end < Text.Length(args) AND Text.GetChar(args, end) # c DO
                INC(end);
              END;
              IF end = Text.Length(args) THEN
                INC(end);
              END;
            END FindNext;

        BEGIN
          start := TextUtils.Pos(args, name);
          IF start > -1 THEN
            INC(start, Text.Length(name));
            WHILE start < Text.Length(args) AND 
              Text.GetChar(args, start) IN ASCII.Spaces DO
              INC(start);
            END;
            IF start < Text.Length(args) THEN
              c := Text.GetChar(args, start);
              IF c = DoubleQuote THEN
                INC(start);
                end := start;
                FindNext(c);
              ELSIF c = Quote THEN
                INC(start);
                end := start;
                FindNext(c);
              ELSE
                end := start;
                FindNext(' ');
              END;
              res := Text.Sub(args, start, end - start);
              RETURN TRUE;
            END;
          END;
          RETURN FALSE;
        END EvalOpt;

      BEGIN
        TRY
          args := TextConv.Decode(opts, FALSE);
        EXCEPT ELSE
          MsgX.Error(self.msgif, "cannot un-escape options line: " & opts);
          args := opts;
        END;
        IF Msg.dFlag THEN
          MsgX.D(self.msgif, " args:  `" & args & "'");
        END;
        IF EvalOpt("-message", res) THEN
          IF Msg.dFlag THEN
            MsgX.D(self.msgif, " argument -message `" & res & "'");
          END;
          msg := res;
        END;
        IF msg = NIL AND EvalOpt("-msg", res) THEN
          IF Msg.dFlag THEN
            MsgX.D(self.msgif, " argument -msg `" & res & "'");
          END;
          msg := res;
        END;
        IF  msg = NIL AND EvalOpt("-m", res) THEN
          IF Msg.dFlag THEN
            MsgX.D(self.msgif, " argument -m `" & res & "'");
          END;
          msg := res;
        END;
        IF EvalOpt("-file", res) THEN
          IF Msg.dFlag THEN
            MsgX.D(self.msgif, " argument -file `" & res & "'");
          END;
          msgFile := APN.New(res);
        END;
        IF  msgFile = NIL AND EvalOpt("-f", res) THEN
          IF Msg.dFlag THEN
            MsgX.D(self.msgif, " argument -f `" & res & "'");
          END;
          msgFile := APN.New(res);
        END;
      END EvalOpts;

    BEGIN
      IF parameters.get("PKGVMOPT", opts) THEN
        EvalOpts();
      END;
      IF parameters.get("PKGMOPT", opts) THEN
        EvalOpts();
      END;
    END EvalMsgOptions;

  VAR
    done    := FALSE;
    tag     :  Tag.T;
    tagtext :  TEXT;
    vc, vcn :  PkgVC.T;
    msg     :  TEXT := NIL;
    msgFile :  APN.T := NIL;
  BEGIN
    TRY
      IF self.pkgvcAcc # NIL THEN
        vc := self.pkgVCIF(pkg);
        IF vc # NIL THEN
          IF    Text.Equal(action, "checkconflicts") THEN
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "checking package " & pkg & 
                " for conflicts", level := 2);
            END;
            ret := Ret(vc.conflicts());
            res := vc.lastVCMsg;
            (* MsgX.T(self.msgif, res); *)
            done := TRUE;
          ELSIF Text.Equal(action, "checkmodified") THEN 
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "checking package " & pkg & 
                " for modifications", level := 2);
            END;
            ret := Ret(vc.modified());
            res := vc.lastVCMsg;
            (* MsgX.T(self.msgif, res); *)
            done := TRUE;
          ELSIF Text.Equal(action, "checkout") THEN
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "checking out package " & pkg, level := 2);
            END;
            VAR loc, tagtext, pkg : TEXT; pkgs : TextSeq.T; BEGIN
              IF parameters.get("TAG", tagtext) AND
                 parameters.get("LOCATION", loc) AND
                 parameters.get("PKG", pkg) THEN
                vcn := NEW(PkgVC.T).init(self.msgif);
                vcn.setEnvironment(vc.getEnvironment());
                TRY
                  res := "unknown checkout error";
                  pkgs := NEW(TextSeq.T).init();
                  pkgs.addhi(pkg);
                  VCUtils.CheckoutDirect(vcn, NIL, loc, tagtext, pkgs);
                  res := vcn.lastVCMsg;
                  (* MsgX.T(self.msgif, res); *)
                  ret := 0;
                EXCEPT
                  PkgVC.E(t) => ret := 1; res := vcn.lastVCMsg & "\n" & t;
                END;
                done := TRUE;
              ELSE
                RAISE Error("checkout missing one of TAG, LOCATION, PKG");
              END;
            END;
          ELSIF Text.Equal(action, "checkrelease") THEN
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "checking if package " & pkg & 
                " is a release", level := 2);
            END;
            ret := Ret(vc.isRelease(tag));
            res := TagText(tag);
            done := TRUE;
          ELSIF Text.Equal(action, "checkuptodate") THEN
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "checking if package " & pkg & 
                " is up-to-date", level := 2);
            END;
            ret := Ret(vc.upToDate());
            res := vc.lastVCMsg;
            (* MsgX.T(self.msgif, res); *)
            done := TRUE;
          ELSIF Text.Equal(action, "commitdevelmajor") THEN
            TRY
              EvalMsgOptions();
              vc.commitChanges(PkgVC.CommitType.Major, msg, msgFile);
              res := ""; (* commit will only leave the tag in lastVCMsg *)
              ret := 0;
            EXCEPT 
              PkgVC.E(t) => res := vc.lastVCMsg & "\n" & t; ret := 1;
            END;
            done := TRUE;
          ELSIF Text.Equal(action, "commitdevelminor") THEN
            TRY
              EvalMsgOptions();
              vc.commitChanges(PkgVC.CommitType.Minor, msg, msgFile);
              res := ""; (* commit will only leave the tag in lastVCMsg *)
              ret := 0;
            EXCEPT 
              PkgVC.E(t) => res := vc.lastVCMsg & "\n" & t; ret := 1;
            END;
            done := TRUE;
          ELSIF Text.Equal(action, "commitdevelpatch") THEN
            TRY
              EvalMsgOptions();
              vc.commitChanges(PkgVC.CommitType.Patch, msg, msgFile);
              res := ""; (* commit will only leave the tag in lastVCMsg *)
              ret := 0;
            EXCEPT 
              PkgVC.E(t) => res := vc.lastVCMsg & "\n" & t; ret := 1;
            END;
            done := TRUE;
          ELSIF Text.Equal(action, "commitreleasemajor") THEN
            TRY
              EvalMsgOptions();
              vc.commitRelease(PkgVC.CommitType.Major, msg, msgFile);
              res := ""; (* commit will only leave the tag in lastVCMsg *)
              ret := 0;
            EXCEPT 
              PkgVC.E(t) => res := vc.lastVCMsg & "\n" & t; ret := 1;
            END;
            done := TRUE;
          ELSIF Text.Equal(action, "commitreleaseminor") THEN
            TRY
              EvalMsgOptions();
              vc.commitRelease(PkgVC.CommitType.Minor, msg, msgFile);
              res := ""; (* commit will only leave the tag in lastVCMsg *)
              ret := 0;
            EXCEPT 
              PkgVC.E(t) => res := vc.lastVCMsg & "\n" & t; ret := 1;
            END;
            done := TRUE;
          ELSIF Text.Equal(action, "commitreleasepatch") THEN
            TRY
              EvalMsgOptions();
              vc.commitRelease(PkgVC.CommitType.Patch, msg, msgFile);
              res := ""; (* commit will only leave the tag in lastVCMsg *)
              ret := 0;
            EXCEPT 
              PkgVC.E(t) => res := vc.lastVCMsg & "\n" & t; ret := 1;
            END;
            done := TRUE;
          ELSIF Text.Equal(action, "conflicts") THEN
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "checking package " & pkg & 
                " for conflicts", level := 2);
            END;
            ret := Ret(vc.conflicts());
            res := "";
            done := TRUE;
          ELSIF Text.Equal(action, "currentdeveltag") THEN
            tag := vc.currentDevelopmentTag(); ret := 0;
            res := TagText(tag);
            done := TRUE;
          ELSIF Text.Equal(action, "currentlabel") THEN
          ELSIF Text.Equal(action, "currentreleasetag") THEN
            tag := vc.currentReleaseTag(); ret := 0;
            res := TagText(tag);
            done := TRUE;
          ELSIF Text.Equal(action, "currenttag") THEN
            tag := vc.currentLocalTag(); ret := 0;
            res := TagText(tag);
            done := TRUE;
          ELSIF Text.Equal(action, "getlabel") THEN
          ELSIF Text.Equal(action, "isrelease") THEN 
            ret := Ret(vc.isRelease(tag));
            res := "";
            done := TRUE;
          ELSIF Text.Equal(action, "listlabels") THEN
          ELSIF Text.Equal(action, "modified") THEN
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "checking package " & pkg & 
                " for modifications", level := 2);
            END;
            ret := Ret(vc.modified());
            res := "";
            done := TRUE;
          ELSIF Text.Equal(action, "setlabel") THEN
          ELSIF Text.Equal(action, "update") THEN
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "updating package " & pkg, level := 2);
            END;
            IF parameters.get("TAG", tagtext) THEN
              tag := Tag.New(tagtext);
              IF VCUtils.TagExists(vc, tag) THEN
                TRY
                  vc.update(tag);
                  res := vc.lastVCMsg;
                  (* MsgX.T(self.msgif, res); *)
                  ret := 0;
                EXCEPT
                  PkgVC.E(t) => res := t; ret := 1;
                END;
              ELSE
                res := "tag does not exist: " & tag.originalText();
                ret := 1;
              END;
            ELSE
              res := "no tag defined for update";
              ret := 1;
            END;
            done := TRUE;
          ELSIF Text.Equal(action, "uptodate") THEN
            IF Msg.vFlag THEN
              MsgX.V(self.msgif, "checking if package " & pkg & 
                " is up-to-date", level := 2);
            END;
            ret := Ret(vc.upToDate());
            res := "";
            done := TRUE;
          ELSIF Text.Equal(action, "diff") THEN
            (* FIXME: provide internal implementation for diff action *)
          ELSIF Text.Equal(action, "diff1") THEN
            (* FIXME: provide internal implementation for diff1 action *)
          ELSIF Text.Equal(action, "diff2") THEN
            (* FIXME: provide internal implementation for diff2 action *)
          END;
          IF done THEN
            UpdateStateCache(self, self.pkgPath(pkg), action, ret);
            RETURN TRUE;
          END;
        END;
      END;
    EXCEPT
      PkgVC.E(m) => RAISE Error("version control backend failed: " & m);
    END;
    RETURN FALSE;
  END ResultFromInternalVersionControl;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecAction(self : T; pkg : PkgBase.Name; action : PkgBase.Action;
                     VAR res  :  TEXT;
                     externalShell : TEXT := NIL;
                     parameters : TextTextTbl.T := NIL) : INTEGER 
  RAISES {Error} =
    (*
      Execute the action associated with `action' in the
      package root directory of `pkg' and return the exit code of
      the process (shell).
    *)
  VAR 
    ret  :  INTEGER;
    cmd  :  PkgBase.CmdSeq;
    nwd  :  Pathname.T;
    kind :  PkgBase.Kind;
    errt :  TEXT := "";
    done := FALSE;
  BEGIN
    IF NOT self.exists(pkg) THEN
      RAISE Error("package " & pkg & " does not exist");
    END;

    nwd  := self.pkgPath(pkg);
    kind := self.pkgType(pkg);

    IF self.useCache AND 
       NOT ActionProbablyNeeded(self, pkg, nwd, action, ret, res) THEN
      IF Msg.vFlag THEN
        MsgX.V(self.msgif, "omitting action " & action & 
          " based on cached state", level := 2);
      END;
      RETURN ret;
    END;

    IF self.useCache AND 
      (Text.Equal(action, "modified") OR Text.Equal(action, "conflicts") OR
       Text.Equal(action, "uptodate") OR Text.Equal(action, "release")) THEN
      TRY
        WITH res = self.getAndCacheVersionState(pkg) DO
          IF Msg.vFlag THEN
            MsgX.V(self.msgif, "--- status package " & 
              TextUtils.Compress(res));
          END;
        END;
        IF Text.Equal(action, "modified") THEN
          IF self.attrIsSet(pkg, Checkpoint.Attr.Modified) THEN
            ret := 0;
          ELSE
            ret := 1;
          END;
        ELSIF Text.Equal(action, "conflicts") THEN
          IF self.attrIsSet(pkg, Checkpoint.Attr.Conflicts) THEN
            ret := 0;
          ELSE
            ret := 1;
          END;
        ELSIF Text.Equal(action, "uptodate") THEN 
          IF self.attrIsSet(pkg, Checkpoint.Attr.UpToDate) THEN
            ret := 0;
          ELSE
            ret := 1;
          END;
        ELSIF Text.Equal(action, "release") THEN
          IF self.attrIsSet(pkg, Checkpoint.Attr.IsRelease) THEN
            ret := 0;
          ELSE
            ret := 1;
          END;
        END;
        done := TRUE;
      EXCEPT 
      END;
    END;
    IF done THEN
      RETURN ret;
    END;
    IF ResultFromInternalVersionControl(self, pkg, action, parameters,
                                        res, ret) THEN
      RETURN ret;
    END;
    res := "external command failure";
    TRY
      TRY
	cmd := self.cfg.getAction(kind, action);
        IF cmd = NIL THEN 
          RAISE Error("no commands for action " & action);
        END;
        TRY
          cmd := TextUtils.SubstituteVariables(cmd, parameters);
        EXCEPT
          TextUtils.Error(e) => RAISE Error("parameter error in " & cmd &
            ": " & e);
        END;
	IF externalShell = NIL THEN
          externalShell := self.cfg.getAction(kind, "externalshell");
        END;
        MsgX.T(self.msgif, "[" & nwd & "] " & cmd); 
        IF NOT Text.Empty(cmd) AND NOT Text.Equal(cmd, "-") THEN
          IF externalShell = NIL THEN
            errt := " failed";
            ret := System.ExecuteList(cmd, msgif := self.msgif, wd := nwd);
          ELSE
            errt := " via shell " & externalShell & " failed";
            ret := System.ExecuteShell(cmd, externalShell, msgif := self.msgif,
                                       wd := nwd);
          END;
          (* caller is expected to print exit code when appropriate *)
          res := "";
        ELSE
          ret := 0;
        END;
        UpdateStateCache(self, nwd, action, ret);
      EXCEPT
	System.ExecuteError => RAISE Error("execution of " & cmd & errt);
      | Thread.Alerted      => RAISE Error("execution of " & cmd & 
        "interrupted");
      | Error(e)            => RAISE Error(e);
      END;
    FINALLY
      (* skip *)
    END;
    RETURN ret;
  END ExecAction;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecCmdList(self : T; pkg : PkgBase.Name; cmd : TEXT;
                      externalShell : TEXT := NIL) : INTEGER RAISES {Error} =
  VAR 
    ret  :  INTEGER;
    nwd  :  Pathname.T;
    kind := self.pkgType(pkg);
  BEGIN
    IF NOT self.exists(pkg) THEN
      RAISE Error("package " & pkg & " does not exist");
    END;

    nwd := self.pkgPath(pkg);

    TRY
      TRY
	IF externalShell = NIL THEN
          externalShell := self.cfg.getAction(kind, "externalshell");
        END;
        MsgX.T(self.msgif, "[" & nwd & "] " & cmd); 
	IF externalShell = NIL THEN
	  ret := System.ExecuteList(cmd, msgif := self.msgif, wd := nwd);
	ELSE
	  ret := System.ExecuteShell(cmd, externalShell, msgif := self.msgif,
                                     wd := nwd);
	END;
        UpdateStateCache(self, nwd, "any-user-cmd", ret);
      EXCEPT
	System.ExecuteError => RAISE Error("execution of " & cmd & " failed");
      | Thread.Alerted      => RAISE Error("execution of " & cmd & 
	"interrupted");
      END;
    FINALLY
      (* skip *)
    END;
    RETURN ret;
  END ExecCmdList;

(*---------------------------------------------------------------------------*)
PROCEDURE GetCmdOutput(self : T; pkg : PkgBase.Name; 
                       cmd : TEXT; VAR ret : INTEGER) : TEXT RAISES {Error} =
  VAR
    proc :  Process.T;
    res  :  TEXT := NIL;
    rd   :  Rd.T;
    nwd  :  Pathname.T;
  BEGIN
    IF NOT self.exists(pkg) THEN
      RAISE Error("package " & pkg & " does not exist");
    END;

    nwd := self.pkgPath(pkg);

    TRY
      TRY
        MsgX.T(self.msgif, "[" & nwd & "] " & cmd); 
        proc := System.RdExecute(cmd, rd, nwd, msgif := self.msgif);
        res  := Rd.GetText(rd, LAST(INTEGER));
        ret := Process.Wait(proc);
        UpdateStateCache(self, nwd, "any-user-cmd", ret);
      EXCEPT
        Rd.Failure  => RAISE Error("error reading from command " & cmd);
      | Thread.Alerted  => RAISE Error("execution of " & cmd & "interrupted");
      | System.ExecuteError => RAISE Error("execution of " & cmd & " failed");
      END;
    FINALLY
      (* skip *)
    END;
    RETURN res;
  END GetCmdOutput;

(*---------------------------------------------------------------------------*)
PROCEDURE GetAndCacheVersionState(self : T; pkg : PkgBase.Name) : TEXT 
  RAISES {Error} =
  VAR
    ret  :  INTEGER;
    res  :  TEXT;
    val  :  TEXT;
    cmd  :  TEXT;
    kind := self.pkgType(pkg);
    dir  := self.pkgPath(pkg);
    seq  :  TextSeq.T;
    vc   :  PkgVC.T;
    tag  :  Tag.T;
  BEGIN
    IF NOT self.useCache THEN RETURN NIL END;
    IF self.pkgvcAcc # NIL THEN
      vc := self.pkgVCIF(pkg);
      TRY
        IF vc # NIL THEN
          IF Msg.vFlag THEN
            MsgX.V(self.msgif, "getting short status for package " & pkg,
                   level := 2);
          END;
          IF vc.modified() THEN
            UpdateStateCache(self, dir, "modified", 0, FALSE);
            res := pkg & ": modified";
          ELSE
            UpdateStateCache(self, dir, "modified", 1, FALSE);
            res := pkg & ":";
          END;
          IF vc.upToDate() THEN
            UpdateStateCache(self, dir, "uptodate", 0, FALSE);
            res := res & " up-to-date";
          ELSE
            UpdateStateCache(self, dir, "uptodate", 1, FALSE);
          END;
          IF vc.conflicts() THEN
            UpdateStateCache(self, dir, "conflicts", 0, FALSE);
            res := res & " conflicts";
          ELSE
            UpdateStateCache(self, dir, "conflicts", 1, FALSE);
          END;
          TRY
            IF vc.isRelease(tag) THEN
              UpdateStateCache(self, dir, "isrelease", 0, FALSE);
              self.stateCache.setVal(dir, "release-tag", tag.denotation());
            ELSE
              UpdateStateCache(self, dir, "isrelease", 1, FALSE);
              self.stateCache.delVal(dir, "release-tag");
            END;
            IF vc.isSticky(tag) THEN
              self.stateCache.setVal(dir, "sticky-tag", tag.denotation());
            ELSE
              self.stateCache.delVal(dir, "sticky-tag");
            END;
            tag := vc.currentLocalTag();
            self.stateCache.setVal(dir, "current-tag", tag.denotation());
          EXCEPT
            Checkpoint.Error(e) => 
            RAISE Error("checkpoint error for " & pkg & ": " & e);
          END;
          RETURN res;
        END;
      EXCEPT
        PkgVC.E(m) => RAISE Error("version control backend failed: " & m);
      END;
    END;
    cmd := self.cfg.getAction(kind, "shortstatus");
    IF cmd = NIL THEN 
      cmd := "pkgvm -sstat";
    END;
    res := GetCmdOutput(self, pkg, cmd, ret);
    IF ret # 0 THEN
      RAISE Error("command `" & cmd & "' failed in package " & pkg &
            " with status " & Fmt.Int(ret));
    END;
    seq := TextUtils.Split(res, " ");
    IF TextUtils.MemberOfTextSeq(seq, "modified") THEN
      UpdateStateCache(self, dir, "modified", 0, FALSE);
    ELSE
      UpdateStateCache(self, dir, "modified", 1, FALSE);
    END;
    IF TextUtils.MemberOfTextSeq(seq, "up-to-date") THEN
      UpdateStateCache(self, dir, "uptodate", 0, FALSE);
    ELSE
      UpdateStateCache(self, dir, "uptodate", 1, FALSE);
    END;
    IF TextUtils.MemberOfTextSeq(seq, "conflicts") THEN
      UpdateStateCache(self, dir, "conflicts", 0, FALSE);
    ELSE
      UpdateStateCache(self, dir, "conflicts", 1, FALSE);
    END;
    TRY
      IF MatchesTextSeq(seq, "^release:", val) THEN
        UpdateStateCache(self, dir, "isrelease", 0, FALSE);
        self.stateCache.setVal(dir, "release-tag", TextAfterChar(val, ':'));
      ELSE
        UpdateStateCache(self, dir, "isrelease", 1, FALSE);
        self.stateCache.delVal(dir, "release-tag");
      END;
      IF MatchesTextSeq(seq, "^current:", val) THEN
        self.stateCache.setVal(dir, "current-tag", TextAfterChar(val, ':'));
      ELSE
        self.stateCache.delVal(dir, "current-tag");
      END;
      IF MatchesTextSeq(seq, "^sticky:", val) THEN
        self.stateCache.setVal(dir, "sticky-tag", TextAfterChar(val, ':'));
      ELSE
        self.stateCache.delVal(dir, "sticky-tag");
      END;
    EXCEPT
      Checkpoint.Error(e) => 
      RAISE Error("checkpoint error for " & pkg & ": " & e);
    END;
    RETURN res;
  END GetAndCacheVersionState;

(*---------------------------------------------------------------------------*)
PROCEDURE MatchesTextSeq(ts : TextSeq.T; pattern : TEXT; 
                         VAR res : TEXT) : BOOLEAN =
  VAR pat : RegEx.Pattern;
  BEGIN
    TRY pat := RegEx.Compile(pattern); EXCEPT ELSE END;
    FOR i := 0 TO ts.size() - 1 DO
      WITH elem = ts.get(i) DO
        IF RegEx.Execute(pat, elem) > -1 THEN
          res := elem;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END MatchesTextSeq;

(*---------------------------------------------------------------------------*)
PROCEDURE TextAfterChar(t : TEXT; c : CHAR) : TEXT =
  VAR i := Text.FindChar(t, c);
  BEGIN
    IF i < 0 THEN
      RETURN "";
    END;
    RETURN Text.Sub(t, i + 1);
  END TextAfterChar;

(*---------------------------------------------------------------------------*)
PROCEDURE FileContents(self : T; pkg : PkgBase.Name; fn : TEXT) : TEXT 
  RAISES {Error} =
  VAR
    path : Pathname.T;
    rd   : Rd.T;
    res  : TEXT;
  BEGIN
    IF NOT self.exists(pkg) THEN
      RAISE Error("package " & pkg & " does not exist");
    END;
    IF Pathname.Absolute(PathRepr.Native(fn)) THEN
      RAISE Error("pathname " & fn & " must not be absolute");
    END;
    path := Pathname.Join(self.pkgPath(pkg), PathRepr.Native(fn), NIL);
    TRY
      rd := FileRd.Open(path);
    EXCEPT ELSE
      RAISE Error("cannot open file " & fn);
    END;
    TRY
      TRY
        res := Rd.GetText(rd, LAST(CARDINAL));
      EXCEPT ELSE
        RAISE Error("cannot read file " & fn);
      END;
    FINALLY
      TRY Rd.Close(rd) EXCEPT ELSE END;
    END;
    RETURN res;
  END FileContents;

(*---------------------------------------------------------------------------*)
PROCEDURE Checkout(self : T; pkg : PkgBase.Name; 
                   checkoutCmd : PkgBase.Action;
                   externalShell : TEXT := NIL;
                   rootDir : Pathname.T := NIL;
                   parameters : TextTextTbl.T := NIL) : INTEGER 
  RAISES {Error} =
  VAR 
    ret  :  INTEGER;
    loc  :  TEXT;
    cmd  :  PkgBase.CmdSeq;
    kind := self.pkgType(pkg);
  BEGIN
    IF self.exists(pkg) THEN
      RAISE Error("package " & pkg & " does already exist");
    END;

    TRY
      TRY
	cmd := self.cfg.getAction(self.pkgType(pkg), checkoutCmd);
	IF cmd = NIL THEN (* undefined package type *)
	  cmd := self.cfg.getAction("DEFAULT", checkoutCmd);
	END;
        TRY
          cmd := TextUtils.SubstituteVariables(cmd, parameters);
        EXCEPT
          TextUtils.Error(e) => RAISE Error("parameter error in " & cmd &
            ": " & e);
        END;
	IF externalShell = NIL THEN
          externalShell := self.cfg.getAction(kind, "externalshell");
        END;
        MsgX.T(self.msgif, "[" & rootDir & "] " & cmd); 
	IF externalShell = NIL THEN
	  ret := System.ExecuteList(cmd, msgif := self.msgif, wd := rootDir);
	ELSE
	  ret := System.ExecuteShell(cmd, externalShell, msgif := self.msgif,
                                     wd := rootDir);
	END;
        IF parameters # NIL AND parameters.get("LOCATION", loc) THEN
          rootDir := Pathname.Join(rootDir, loc, NIL);
        ELSIF self.location.get(pkg, loc) THEN
          IF NOT Text.Equal(loc, Undefined) THEN
            rootDir := Pathname.Join(rootDir, loc, NIL);
          END;
        END;
        EVAL self.location.delete(pkg, loc);
        loc := Pathname.Join(rootDir, pkg, NIL);
        EVAL self.exists(pkg, rootDir);
        UpdateStateCache(self, loc, checkoutCmd, ret);
      EXCEPT
	System.ExecuteError => RAISE Error("execution of " & cmd & " failed");
      | Thread.Alerted      => RAISE Error("execution of " & cmd & 
	"interrupted");
      END;
    FINALLY
      (* skip *)
    END;
    RETURN ret;
  END Checkout;

(*---------------------------------------------------------------------------*)
PROCEDURE GetFileCache(self : T) : FileInfo.T =
  BEGIN
    RETURN self.fileCache;
  END GetFileCache;

(*---------------------------------------------------------------------------*)
PROCEDURE NewCheckpoint(self : T; update := FALSE) : Checkpoint.T 
  RAISES {Error} =
  VAR 
    cp   := Checkpoint.New(self.fileCache, self.msgif);
    iter := self.location.iterate();
    pkg  :  TEXT;
    path :  TEXT;
  BEGIN
    TRY
      WHILE iter.next(pkg, path) DO
        IF NOT Text.Equal(path, Undefined) THEN
          cp.addDir(path);
        END;
      END;
      IF update THEN
        IF self.verboseCache AND NOT Msg.vFlag THEN
          MsgX.T(self.msgif, "scanning all packages (new checkpoint)...");
        END;
        cp.update();
      END;
    EXCEPT
      Checkpoint.Error(e) => RAISE Error("NewCheckpoint: " & e);
    END;
    RETURN cp;
  END NewCheckpoint;

(*---------------------------------------------------------------------------*)
PROCEDURE ReplaceStateCache(self : T; sc : Checkpoint.T) =
  BEGIN
    self.stateCache := sc;
  END ReplaceStateCache;

(*---------------------------------------------------------------------------*)
PROCEDURE CachedState(self : T) : Checkpoint.T =
  BEGIN
    RETURN self.stateCache;
  END CachedState;

(*---------------------------------------------------------------------------*)
PROCEDURE UpdateCache(self : T) RAISES {Error} =
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    TRY
      IF self.verboseCache AND NOT Msg.vFlag THEN
        MsgX.T(self.msgif, "scanning all packages (cache update)...");
      END;
      self.stateCache.update();
    EXCEPT
      Checkpoint.Error(e) => RAISE Error(e);
    END;
  END UpdateCache;

(*---------------------------------------------------------------------------*)
PROCEDURE DumpStateCache(self : T; header : TEXT) =
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    MsgX.T(self.msgif, header);
    TRY MsgX.T(self.msgif, self.stateCache.toText()); EXCEPT ELSE END;
  END DumpStateCache;

(*---------------------------------------------------------------------------*)
PROCEDURE SetAttr(self : T; pkg : PkgBase.Name; attr : Checkpoint.Attr) 
  RAISES {Error} =
  VAR
    path  : TEXT;
    attrs : Checkpoint.AttrSet;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    IF self.location.get(pkg, path) THEN
      TRY
        attrs := self.stateCache.getAttr(path) + Checkpoint.AttrSet{attr};
        self.stateCache.setAttr(path, attrs);
      EXCEPT
        Checkpoint.Error(e) => 
        RAISE Error("cannot set attributes for package " & pkg & ": " & e);
      END;
    ELSE
      RAISE Error("SetAttr: no location for package " & pkg);
    END;
  END SetAttr;

(*---------------------------------------------------------------------------*)
PROCEDURE ClearAttr(self : T; pkg : PkgBase.Name; attr : Checkpoint.Attr) 
  RAISES {Error} =
  VAR
    path  : TEXT;
    attrs : Checkpoint.AttrSet;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    IF self.location.get(pkg, path) THEN
      TRY
        attrs := self.stateCache.getAttr(path) - Checkpoint.AttrSet{attr};
        self.stateCache.setAttr(path, attrs);
      EXCEPT
        Checkpoint.Error(e) => 
        RAISE Error("cannot set attributes for package " & pkg & ": " & e);
      END;
    ELSE
      RAISE Error("SetAttr: no location for package " & pkg);
    END;
  END ClearAttr;

(*---------------------------------------------------------------------------*)
PROCEDURE AttrIsSet(self : T; pkg : PkgBase.Name; attr : Checkpoint.Attr) 
  : BOOLEAN RAISES {Error} =
  VAR
    path  : TEXT;
    attrs : Checkpoint.AttrSet;
  BEGIN
    IF NOT self.useCache THEN RETURN FALSE END;
    IF self.location.get(pkg, path) THEN
      TRY
        attrs := self.stateCache.getAttr(path);
      EXCEPT
        Checkpoint.Error(e) => 
        RAISE Error("cannot get attributes for package " & pkg & ": " & e);
      END;
      RETURN attr IN attrs;
    ELSE
      RAISE Error("AttrIsSet: no location for package " & pkg);
    END;
  END AttrIsSet;

(*---------------------------------------------------------------------------*)
PROCEDURE SetVal(self : T; pkg : PkgBase.Name; name, val : TEXT) 
  RAISES {Error} =
  VAR
    path  : TEXT;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    IF self.location.get(pkg, path) THEN
      TRY
        self.stateCache.setVal(path, name, val);
      EXCEPT
        Checkpoint.Error(e) => 
        RAISE Error("cannot set value for package " & pkg & ": " & e);
      END;
    ELSE
      RAISE Error("SetVal: no location for package " & pkg);
    END;
  END SetVal;

(*---------------------------------------------------------------------------*)
PROCEDURE GetVal(self : T; pkg : PkgBase.Name; name : TEXT) : TEXT 
  RAISES {Error} =
  VAR
    path  : TEXT;
  BEGIN
    IF NOT self.useCache THEN RETURN NIL END;
    IF self.location.get(pkg, path) THEN
      TRY
        RETURN self.stateCache.getVal(path, name);
      EXCEPT
        Checkpoint.Error(e) => 
        RAISE Error("cannot get value for package " & pkg & ": " & e);
      END;
    ELSE
      RAISE Error("GetVal: no location for package " & pkg);
    END;
  END GetVal;

(*---------------------------------------------------------------------------*)
PROCEDURE DelVal(self : T; pkg : PkgBase.Name; name : TEXT) RAISES {Error} =
  VAR
    path  : TEXT;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    IF self.location.get(pkg, path) THEN
      TRY
        self.stateCache.delVal(path, name);
      EXCEPT
        Checkpoint.Error(e) => 
        RAISE Error("cannot delete value for package " & pkg & ": " & e);
      END;
    ELSE
      RAISE Error("GetVal: no location for package " & pkg);
    END;
  END DelVal;

(*---------------------------------------------------------------------------*)
BEGIN (* PoolSet MAIN *)
END PoolSet.
