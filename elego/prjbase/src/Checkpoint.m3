(*---------------------------------------------------------------------------*)
MODULE Checkpoint;

IMPORT Pathname, Fingerprint, TextRefTbl, TextTextTbl, Text, TextRd, Rd, 
       FileRd, Wr, FileWr, TextSeq, Thread, OSError;
IMPORT FileInfo, APN AS APN, 
       FingerprintFmt, TextUtils, TextReadingUtils, PathRepr, 
       FindExpr, FSFindError, EnvUtils, MsgX, MsgIF;

(*---------------------------------------------------------------------------*)
TYPE
  CacheElem = OBJECT
    fp    : REF Fingerprint.T;
    attr  : AttrSet;
    env   : TextTextTbl.T
  METHODS
    init(fp : REF Fingerprint.T; attr : AttrSet) : CacheElem := InitCacheElem;
    scan(rd : Rd.T) : CacheElem RAISES {Error} := CacheElemScan;
    toText() : TEXT RAISES {Error} := CacheElemToText;
    set(a : Attr) := CacheElemSet;
    isSet(a : Attr) : BOOLEAN := CacheElemIsSet;
    clear(a : Attr) := CacheElemClear;
    clearAll() := CacheElemClearAll;
    scanEnv(rd : Rd.T) : TEXT RAISES {Error} := CacheElemScanEnv;
    setVal(name, val : TEXT) := CacheElemSetVal;
    getVal(name : TEXT) : TEXT := CacheElemGetVal;
    delVal(name : TEXT) := CacheElemDelVal;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE InitCacheElem(self : CacheElem; 
                        fp : REF Fingerprint.T; attr : AttrSet) : CacheElem =
  BEGIN
    self.fp := fp;
    self.attr := attr;
    self.env := NIL;
    RETURN self;
  END InitCacheElem;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemScan(self : CacheElem; rd : Rd.T) : CacheElem 
  RAISES {Error} =
  VAR
    fpt, opt : TEXT;
  BEGIN
    TRY
      fpt := TextReadingUtils.GetTokenOrString(rd);
      opt := TextReadingUtils.GetTokenOrString(rd);
      self.fp := NEW(REF Fingerprint.T);
      IF NOT FingerprintFmt.Scan(fpt, self.fp^) THEN
        RAISE Error("invalid fingerprint: " & fpt);
      END;
      AttrFromText(opt, self.attr);
    EXCEPT
      Rd.EndOfFile => RAISE Error("unexpected end of file");
    | Rd.Failure => RAISE Error("text reader failure");
    | Thread.Alerted => RAISE Error("interrupted");
    END;
    RETURN self;
  END CacheElemScan;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemScanEnv(self : CacheElem; rd : Rd.T) : TEXT
  RAISES {Error} =
  VAR
    tok : TEXT;
  BEGIN
    TRY
      tok := TextReadingUtils.GetTokenOrString(rd);
      IF Text.Equal(tok, "(@env-start") THEN
        self.env := EnvUtils.FromRd(rd, skipStart := TRUE);
        tok := NIL;
      END;
    EXCEPT
      Rd.EndOfFile => tok := NIL; (* skip, may be correct *)
    | Rd.Failure => RAISE Error("text reader failure");
    | Thread.Alerted => RAISE Error("interrupted");
    | EnvUtils.Error(e) => RAISE Error(e);
    END;
    RETURN tok;
  END CacheElemScanEnv;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemToText(self : CacheElem) : TEXT RAISES {Error} =
  VAR env := "";
  BEGIN
    IF self.env # NIL THEN
      TRY
        env := B & EnvUtils.ToText(self.env);
      EXCEPT ELSE 
        RAISE Error("cannot convert checkpoint environment");
      END;
    END;
    RETURN FingerprintFmt.Hex(self.fp^) & B & AttrToText(self.attr) & env;
  END CacheElemToText;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemSet(self : CacheElem; a : Attr) =
  BEGIN
    self.attr := self.attr + AttrSet{a};
  END CacheElemSet;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemIsSet(self : CacheElem; a : Attr) : BOOLEAN =
  BEGIN
    RETURN a IN self.attr;
  END CacheElemIsSet;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemClear(self : CacheElem; a : Attr) =
  BEGIN
    self.attr := self.attr - AttrSet{a};
  END CacheElemClear;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemClearAll(self : CacheElem) =
  BEGIN
    self.attr := AttrSet{};
  END CacheElemClearAll;

(*---------------------------------------------------------------------------*)
PROCEDURE NewCacheElem(fp : REF Fingerprint.T; attr : AttrSet) : CacheElem =
  BEGIN
    RETURN NEW(CacheElem).init(fp, attr);
  END NewCacheElem;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemSetVal(self : CacheElem; name, val : TEXT) =
  BEGIN
    IF self.env = NIL THEN
      self.env := NEW(TextTextTbl.Default).init();
    END;
    EVAL self.env.put(name, val);
  END CacheElemSetVal;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemGetVal(self : CacheElem; name : TEXT) : TEXT =
  VAR val : TEXT;
  BEGIN
    IF self.env = NIL THEN
      RETURN NIL;
    END;
    IF self.env.get(name, val) THEN
      RETURN val;
    ELSE
      RETURN NIL;
    END;
  END CacheElemGetVal;

(*---------------------------------------------------------------------------*)
PROCEDURE CacheElemDelVal(self : CacheElem; name : TEXT) =
  VAR val : TEXT;
  BEGIN
    IF self.env = NIL THEN
      RETURN;
    END;
    EVAL self.env.delete(name, val);
  END CacheElemDelVal;

(*---------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED " Checkpoint 0.0" OBJECT
    cache : FileInfo.T;
    roots : TextRefTbl.T;
    msgif : MsgIF.T;
  OVERRIDES
    init := Init;
    addDir := AddDir;
    delDir := DelDir;
    dirs := Dirs;
    fingerprint := GetFingerprint;
    update := Update;
    diff := Diff;
    selectByAttr := SelectByAttr;
    toText := ToText;
    fromRd := FromRd;
    fromText := FromText;
    toFile := ToFile;
    fromFile := FromFile;
    getAttr := GetAttr;
    setAttr := SetAttr;
    getVal := GetVal;
    setVal := SetVal;
    delVal := DelVal;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; cache : FileInfo.T; msgif : MsgIF.T := NIL) : T =
  BEGIN
    <* ASSERT cache # NIL *>
    self.msgif := msgif;
    self.cache := cache;
    self.roots := NEW(TextRefTbl.Default).init();
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE AddDir(self : T; dir : Pathname.T; fp : REF Fingerprint.T := NIL) 
  RAISES {Error} =
  BEGIN
    <* ASSERT self.cache # NIL *>
    dir := PathRepr.Native(dir);
    IF fp = NIL THEN
      WITH dirp = APN.New(dir) DO
        fp := self.cache.fingerprint(dirp, ignoreDirExpr, ignoreFileExpr);
        IF fp = NIL THEN
          MsgX.V(self.msgif, "scanning directory " & dir);
          self.cache.updateRec(dirp, NIL, NIL,
                               skipDirExpr, skipFileExpr); 
          fp := self.cache.fingerprint(dirp, ignoreDirExpr, ignoreFileExpr);
          IF fp = NIL THEN
            RAISE Error("cannot compute fingerprint for directory " & dir);
          END;
        END;
      END;
    END;
    <* ASSERT fp # NIL *>
    WITH ce = NewCacheElem(fp, AttrSet{}) DO
      EVAL self.roots.put(dir, ce);
    END;
  END AddDir;

(*---------------------------------------------------------------------------*)
PROCEDURE DelDir(self : T; dir : Pathname.T) =
  VAR ce : REFANY;
  BEGIN
    <* ASSERT self.roots # NIL *>
    dir := PathRepr.Native(dir);
    EVAL self.roots.delete(dir, ce);
  END DelDir;

(*---------------------------------------------------------------------------*)
PROCEDURE Dirs(self : T) : TextSeq.T =
  VAR 
    iter := self.roots.iterate();
    fn   :  TEXT;
    ref  :  REFANY;
    res  := NEW(TextSeq.T).init();
  BEGIN
    WHILE iter.next(fn, ref) DO
      res.addhi(fn);
    END;
    RETURN res;
  END Dirs;

(*---------------------------------------------------------------------------*)
PROCEDURE GetFingerprint(self : T; dir : Pathname.T) : REF Fingerprint.T =
  VAR ref : REFANY;
  BEGIN
    <* ASSERT self.roots # NIL *>
    dir := PathRepr.Native(dir);
    IF self.roots.get(dir, ref) THEN
      RETURN NARROW(ref, CacheElem).fp;
    ELSE
      RETURN NIL;
    END;
  END GetFingerprint;

(*---------------------------------------------------------------------------*)
PROCEDURE Update(self : T; dir : Pathname.T := NIL; missingOnly := FALSE)
  RAISES {Error} =
  VAR 
    iter := self.roots.iterate();
    fn   :  TEXT;
    ref  :  REFANY;
    errs := "";

  PROCEDURE UpdateOne(dir : Pathname.T; ref : REFANY) =
    VAR fp : REF Fingerprint.T;
    BEGIN
      dir := PathRepr.Native(dir);
      WITH dirp = APN.New(dir) DO
        MsgX.V(self.msgif, "scanning directory " & dir);
        IF missingOnly THEN
          fp := self.cache.fingerprint(dirp, ignoreDirExpr, ignoreFileExpr);
        END;
        IF NOT missingOnly OR fp = NIL THEN
          self.cache.updateRec(dirp, NIL, NIL,
                               skipDirExpr, skipFileExpr); 
          fp := self.cache.fingerprint(dirp, ignoreDirExpr, ignoreFileExpr);
        END;
        IF fp = NIL THEN
          errs := errs & B & dir;
        ELSE
          NARROW(ref, CacheElem).fp := fp;
          EVAL self.roots.put(dir, ref);
        END;
      END;
    END UpdateOne;

  BEGIN (* Update *)
    IF dir = NIL THEN
      WHILE iter.next(fn, ref) DO
        UpdateOne(fn, ref);
      END;
    ELSE
      IF self.roots.get(dir, ref) THEN
        UpdateOne(dir, ref);
      ELSE
        AddDir(self, dir);
      END;
    END;
    IF NOT Text.Empty(errs) THEN
      RAISE Error("cannot compute fingerprint for the following " &
            "directories: " & errs);
    END;
  END Update;

(*---------------------------------------------------------------------------*)
PROCEDURE Diff(self : T; cp : T) : TextSeq.T =
  VAR
    iter := self.roots.iterate();
    fn   :  TEXT;
    ref  :  REFANY;
    fp   :  REF Fingerprint.T;
    ce   :  CacheElem;
    res  := NEW(TextSeq.T).init();
  BEGIN
    WHILE iter.next(fn, ref) DO
      fp := cp.fingerprint(fn);
      ce := NARROW(ref, CacheElem);
      IF fp = NIL OR fp^ # ce.fp^ THEN
        res.addhi(fn);
        ce.attr := ce.attr + AttrSet{Attr.Changed};
      ELSE
        ce.attr := ce.attr - AttrSet{Attr.Changed};
      END;
    END;
    RETURN res;
  END Diff;

(*---------------------------------------------------------------------------*)
PROCEDURE SelectByAttr(self : T; attr : AttrSet) : TextSeq.T =
  VAR
    iter := self.roots.iterate();
    fn   :  TEXT;
    ref  :  REFANY;
    ce   :  CacheElem;
    res  := NEW(TextSeq.T).init();
  BEGIN
    WHILE iter.next(fn, ref) DO
      ce := NARROW(ref, CacheElem);
      IF ce.attr * attr # AttrSet{} THEN
        res.addhi(fn);
      END;
    END;
    RETURN res;
  END SelectByAttr;

(*---------------------------------------------------------------------------*)
PROCEDURE ToText(self : T; ) : TEXT RAISES {Error} =
  VAR
    iter := self.roots.iterate();
    fn   :  TEXT;
    ref  :  REFANY;
    ce   :  CacheElem;
    res  := "";
  BEGIN
    WHILE iter.next(fn, ref) DO
      IF ref # NIL THEN
        ce := NARROW(ref, CacheElem);
        WITH cetext = ce.toText() DO
          res := res & Q & fn & Q & B & cetext & "\n";
        END;
      END;
    END;
    RETURN res;
  END ToText;

(*---------------------------------------------------------------------------*)
PROCEDURE FromRd(self : T; rd : Rd.T) RAISES {Error} =
  VAR
    fn  :  TEXT := NIL;
    ce  :  CacheElem;
  BEGIN
    <* ASSERT self.cache # NIL *>
    <* ASSERT self.roots # NIL *>
    TRY
      TRY
        WHILE NOT Rd.EOF(rd) DO
          IF fn = NIL THEN
            fn := TextReadingUtils.GetTokenOrString(rd);
          END;
          fn := PathRepr.Native(fn);
          ce := NEW(CacheElem).scan(rd);
          EVAL self.roots.put(fn, ce);
          fn := ce.scanEnv(rd);
        END;
      EXCEPT
        Rd.EndOfFile => (* skip *)
      | Rd.Failure => RAISE Error("text reader failure");
      | Thread.Alerted => RAISE Error("interrupted");
      END;
    FINALLY
      IF rd # NIL THEN
        TRY Rd.Close(rd) EXCEPT ELSE END;
      END;
    END;
  END FromRd;

(*---------------------------------------------------------------------------*)
PROCEDURE FromText(self : T; t : TEXT) RAISES {Error} =
  VAR
    rd  := TextRd.New(t);
  BEGIN
    <* ASSERT self.cache # NIL *>
    <* ASSERT self.roots # NIL *>
    FromRd(self, rd);
  END FromText;

(*---------------------------------------------------------------------------*)
PROCEDURE ToFile(self : T; fn : Pathname.T) RAISES {Error} =
  VAR
    iter := self.roots.iterate();
    dir  :  TEXT;
    ref  :  REFANY;
    ce   :  CacheElem;
    line := "";
    wr   :  Wr.T := NIL;
  BEGIN
    TRY
      wr := FileWr.Open(fn);
    EXCEPT
      OSError.E => RAISE Error("cannot open file " & fn);
    END;
    TRY
      TRY
        WHILE iter.next(dir, ref) DO
          IF ref # NIL THEN
            ce := NARROW(ref, CacheElem);
            WITH cetext = ce.toText() DO
              line := Q & dir & Q & B & cetext & "\n";
              Wr.PutText(wr, line);
            END;
          END;
        END;
      EXCEPT
        Wr.Failure => RAISE Error("write failed on file " & fn);
      | Thread.Alerted => RAISE Error("interrupted writing file " & fn);
      END;
    FINALLY
      IF wr # NIL THEN
        TRY Wr.Close(wr) EXCEPT ELSE END;
      END;
    END;
  END ToFile;

(*---------------------------------------------------------------------------*)
PROCEDURE FromFile(self : T; fn : Pathname.T) RAISES {Error} =
  VAR
    rd  :  Rd.T := NIL;
  BEGIN
    <* ASSERT self.cache # NIL *>
    <* ASSERT self.roots # NIL *>
    TRY
      rd := FileRd.Open(fn);
    EXCEPT
      OSError.E => RAISE Error("cannot open file " & fn);
    END;
    FromRd(self, rd);
  END FromFile;

(*---------------------------------------------------------------------------*)
PROCEDURE GetAttr(self : T; dir : Pathname.T) : AttrSet  RAISES {Error} =
  VAR
    ref  :  REFANY;
    ce   :  CacheElem;
  BEGIN
    <* ASSERT self.roots # NIL *>
    dir := PathRepr.Native(dir);
    IF self.roots.get(dir, ref) THEN
      ce := NARROW(ref, CacheElem);
      RETURN ce.attr;
    ELSE
      RAISE Error("cache miss: " & dir);
    END;
  END GetAttr;

(*---------------------------------------------------------------------------*)
PROCEDURE SetAttr(self : T; dir : Pathname.T; attr : AttrSet)  RAISES {Error} =
  VAR
    ref  :  REFANY;
    ce   :  CacheElem;
  BEGIN
    <* ASSERT self.roots # NIL *>
    dir := PathRepr.Native(dir);
    IF self.roots.get(dir, ref) THEN
      ce := NARROW(ref, CacheElem);
      ce.attr := attr;
    ELSE
      RAISE Error("cache miss: " & dir);
    END;
  END SetAttr;

(*---------------------------------------------------------------------------*)
PROCEDURE SetVal(self : T; dir : Pathname.T; name, val : TEXT) RAISES {Error} =
  VAR
    ref  :  REFANY;
    ce   :  CacheElem;
  BEGIN
    <* ASSERT self.roots # NIL *>
    dir := PathRepr.Native(dir);
    IF self.roots.get(dir, ref) THEN
      ce := NARROW(ref, CacheElem);
      ce.setVal(name, val);
    ELSE
      RAISE Error("cache miss: " & dir);
    END;
  END SetVal;

(*---------------------------------------------------------------------------*)
PROCEDURE GetVal(self : T; dir : Pathname.T; name : TEXT) : TEXT
  RAISES {Error} = 
  VAR
    ref  :  REFANY;
    ce   :  CacheElem;
  BEGIN
    <* ASSERT self.roots # NIL *>
    dir := PathRepr.Native(dir);
    IF self.roots.get(dir, ref) THEN
      ce := NARROW(ref, CacheElem);
      RETURN ce.getVal(name);
    ELSE
      RAISE Error("cache miss: " & dir);
    END;
  END GetVal;

(*---------------------------------------------------------------------------*)
PROCEDURE DelVal(self : T; dir : Pathname.T; name : TEXT) RAISES {Error} =
  VAR
    ref  :  REFANY;
    ce   :  CacheElem;
  BEGIN
    <* ASSERT self.roots # NIL *>
    dir := PathRepr.Native(dir);
    IF self.roots.get(dir, ref) THEN
      ce := NARROW(ref, CacheElem);
      ce.delVal(name);
    ELSE
      RAISE Error("cache miss: " & dir);
    END;
  END DelVal;

(*---------------------------------------------------------------------------*)
PROCEDURE AttrToText(attr : AttrSet) : TEXT =
  VAR
    res   := Q;
    first := TRUE;
  BEGIN
    FOR a := FIRST(Attr) TO LAST(Attr) DO
      IF a IN attr THEN
        IF first THEN
          res := res & AttrRepr[a];
          first := FALSE;
        ELSE
          res := res & B & AttrRepr[a];
        END;
      END;
    END;
    RETURN res & Q;
  END AttrToText;

(*---------------------------------------------------------------------------*)
PROCEDURE AttrFromText(t : TEXT; VAR attr : AttrSet) RAISES {Error} =
  
  PROCEDURE OneFromText(one : TEXT) : Attr =
    BEGIN
      FOR a := FIRST(Attr) TO LAST(Attr) DO
        IF Text.Equal(one, AttrRepr[a]) THEN
          RETURN a;
        END;
      END;
      RETURN Attr.None;
    END OneFromText;

  VAR
    seq := TextUtils.Split(t, B);
  BEGIN
    attr := AttrSet{};
    FOR i := 0 TO seq.size() - 1 DO
      WITH elem = TextUtils.Compress(seq.get(i)) DO
        IF NOT Text.Empty(elem) THEN
          WITH a = OneFromText(elem) DO
            IF a # Attr.None THEN
              attr := attr + AttrSet{a};
            ELSE
              RAISE Error("unknown attribute: " & elem);
            END;
          END;
        END;
      END;
    END;
  END AttrFromText;

(*---------------------------------------------------------------------------*)
PROCEDURE New(c : FileInfo.T; msgif : MsgIF.T := NIL) : T =
  BEGIN
    RETURN NEW(T).init(c, msgif);
  END New;

(*---------------------------------------------------------------------------*)
PROCEDURE DefineIgnorePatterns(
    cacheIgnoreDirs : TEXT := NIL; 
    cacheIgnoreFiles : TEXT := NIL;
    fingerprintIgnoreDirs : TEXT := NIL;
    fingerprintIgnoreFiles : TEXT := NIL) RAISES {Error} =
  BEGIN
    TRY
      IF cacheIgnoreDirs # NIL THEN
        skipDirExpr := FindExpr.New(cacheIgnoreDirs);
      END;
    EXCEPT
      FSFindError.E(e) => 
      RAISE Error("error in directory ignore pattern for file cache: " & e);
    END;
    TRY
      IF cacheIgnoreFiles # NIL THEN
        skipFileExpr := FindExpr.New(cacheIgnoreFiles);
      END;
    EXCEPT
      FSFindError.E(e) => 
      RAISE Error("error in file ignore pattern for file cache: " & e);
    END;
    TRY
      IF fingerprintIgnoreDirs # NIL THEN
        ignoreDirExpr := FindExpr.New(fingerprintIgnoreDirs);
      END;
    EXCEPT
      FSFindError.E(e) => 
      RAISE Error("error in directory ignore pattern for fingerprints: " & e);
    END;
    TRY
      IF fingerprintIgnoreFiles # NIL THEN
        ignoreFileExpr := FindExpr.New(fingerprintIgnoreFiles);
      END;
    EXCEPT
      FSFindError.E(e) => 
      RAISE Error("error in file ignore pattern for fingerprints: " & e);
    END;
  END DefineIgnorePatterns;

(*---------------------------------------------------------------------------*)
CONST
  B = " ";
  Q = "\"";
  IgnDir = "tmp or temp";
  IgnFile = "\"PkgCDT\" or \"PkgCRT\" or \"PkgCT\" or \"*~\" or " &
    "\"*.bak\" or \"*.tmp\" or \"*.temp\" or \"PkgErr\"";
  SkipDir = IgnDir;
  SkipFile = IgnFile;
BEGIN
  ignoreDirExpr := FindExpr.New(IgnDir); <* NOWARN *>
  ignoreFileExpr := FindExpr.New(IgnFile); <* NOWARN *>
  skipDirExpr := FindExpr.New(SkipDir); <* NOWARN *>
  skipFileExpr := FindExpr.New(SkipFile); <* NOWARN *>
END Checkpoint.
