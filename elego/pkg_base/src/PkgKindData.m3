(*---------------------------------------------------------------------------*)
MODULE PkgKindData;

IMPORT TextTextTbl, TextSeq, Pathname, File, OSError, RegEx;
IMPORT System, FSUtils, PkgError, TextUtils, MsgX, MsgIF, FileInfo,
       FileStatus, APN AS APN, APNSeq AS APNSeq;
IMPORT (* FSFixed AS *) FS, SMsg AS Msg;

(*---------------------------------------------------------------------------*)
TYPE
  PredElem = OBJECT
    kind : PredKind;
    arg  : TEXT;
    next : PredElem;
  END;

(*---------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "PkgKindData.T rel 0.0" OBJECT
    action : TextTextTbl.T;
    expr   : PredElem;
    myname : TEXT;
    cache  : FileInfo.T;
    msgif  : MsgIF.T;
  OVERRIDES
    init := Init;
    init2 := Init2;
    setCache := SetCache;
    setName := SetName;
    name := Name;
    putAction := PutAction;
    getAction := GetAction;
    addCondition := AddCondition;
    evalCondition := EvalCondition;
    createStructure := CreateStructure;
    ensureStructureExists := EnsureStructureExists;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; fc : FileInfo.T := NIL; msgif : MsgIF.T := NIL) : T =
  BEGIN
    self.msgif := msgif;
    self.action := NEW(TextTextTbl.Default).init();
    self.expr := NIL;
    self.myname := "";
    self.cache := fc;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE Init2(self : T; n : TEXT; fc : FileInfo.T := NIL; 
                msgif : MsgIF.T := NIL) : T =
  BEGIN
    self.msgif := msgif; 
    self.action := NEW(TextTextTbl.Default).init();
    self.expr := NIL;
    self.myname := n;
    self.cache := fc;
    RETURN self;
  END Init2;

(*---------------------------------------------------------------------------*)
PROCEDURE SetCache(self : T; fc : FileInfo.T) =
  BEGIN
    self.cache := fc;
  END SetCache;

(*---------------------------------------------------------------------------*)
PROCEDURE SetName(self : T; n : TEXT) =
  BEGIN
    self.myname := n;
  END SetName;

(*---------------------------------------------------------------------------*)
PROCEDURE Name(self : T) : TEXT =
  BEGIN
    RETURN self.myname;
  END Name;

(*---------------------------------------------------------------------------*)
PROCEDURE PutAction(self : T; name, cmds : TEXT) : BOOLEAN =
  BEGIN
    RETURN self.action.put(name, cmds);
  END PutAction;

(*---------------------------------------------------------------------------*)
PROCEDURE GetAction(self : T; name : TEXT; VAR cmds : TEXT) : BOOLEAN =
  BEGIN
    RETURN self.action.get(name, cmds);
  END GetAction;

(*---------------------------------------------------------------------------*)
PROCEDURE AddCondition(self : T; p : PredKind; arg : TEXT) =
  VAR cond := NEW(PredElem);
  BEGIN
    cond.kind := p;
    cond.arg := arg;
    cond.next := self.expr;
    self.expr := cond;
  END AddCondition;

(*---------------------------------------------------------------------------*)
PROCEDURE EvalCondition(self : T; path : TEXT;
                        hosttype := "unknown"; 
                        ostype   := "unknown") : BOOLEAN =
  VAR 
    res      := TRUE;
    act      := self.expr;
    pn       :  Pathname.T;
    plist    :  TextSeq.T;
    platform :  TEXT;

  (*-------------------------------------------------------------------------*)
  PROCEDURE RegExMatches(pattern, t : TEXT) : BOOLEAN = 
    VAR
      p     :  RegEx.Pattern;
      res   :  BOOLEAN;
      plist := TextUtils.Split(pattern, "|");
    BEGIN
      IF Msg.dFlag THEN
        MsgX.D(self.msgif, "PkgKindData.RegExMatches(" & pattern & ", " & t & 
          ")", level := 2);
      END;
      FOR i := 0 TO plist.size() - 1 DO
        TRY
          p   := RegEx.Compile(plist.get(i));
          res := RegEx.Execute(p, t) > -1;
        EXCEPT ELSE
          res := FALSE;
        END;
        IF res THEN 
          IF Msg.dFlag THEN
            MsgX.D(self.msgif, "  returning TRUE", level := 2);
          END;
          RETURN TRUE;
        END;
      END;
      IF Msg.dFlag THEN
        MsgX.D(self.msgif, "  returning FALSE", level := 2);
      END;
      RETURN FALSE;
    END RegExMatches;

  (*-------------------------------------------------------------------------*)
  PROCEDURE Matches(path : TEXT; plist : TextSeq.T) : BOOLEAN = 
    VAR
      p0  :  TEXT;
      res := FALSE;
    BEGIN
      IF Msg.dFlag THEN
        MsgX.D(self.msgif, "PkgKindData.EvalCondition.Matches(" & path & ", ["
               & TextUtils.TextSeqToText(plist, ", ") & "])", level := 2);
      END;
      IF plist.size() < 1 THEN RETURN TRUE END;
      IF NOT IsDir(self, path) THEN RETURN FALSE END;
      p0 := plist.remlo();
      TRY
	VAR 
	  it := FS.Iterate(path); 
	  pattern := RegEx.Compile(p0);
	  fn  : TEXT;
	  dir : TEXT;
	BEGIN
	  TRY
	    TRY <* NOWARN *> (* no exceptions currently, but... *)
	      WHILE NOT res AND it.next(fn) DO
		IF RegEx.Execute(pattern, fn) > -1 THEN
		  dir := Pathname.Join(path, fn, NIL);
                  IF IsDir(self, dir) THEN
                    IF Matches(dir, plist) THEN
                      res := TRUE;
                    END;
                  ELSE
                    res := plist.size() = 0;
                  END;
		END;
	      END;
	    EXCEPT ELSE
	      MsgX.Error(self.msgif, "cannot read directory " & path);
	    END;
	  FINALLY
	    it.close();
	  END;
	END;
      EXCEPT
        OSError.E => MsgX.Error(self.msgif, "cannot open directory " & path);
      | RegEx.Error => MsgX.Error(self.msgif,
                                  "invalid regular expression: " & p0);
      END;
      IF Msg.dFlag THEN
        IF res THEN
          MsgX.D(self.msgif, "  returning TRUE", level := 2);
        ELSE
          MsgX.D(self.msgif, "  returning FALSE", level := 2);
        END;
      END;
      RETURN res;
    END Matches;

  (*-------------------------------------------------------------------------*)
  BEGIN (* EvalCondition *)
    IF Msg.dFlag THEN
      MsgX.D(self.msgif, "PkgKindData.EvalCondition.Matches(" & path & ")", 
             level := 2);
    END;

    <* ASSERT(hosttype # NIL) *>
    <* ASSERT(ostype # NIL) *>

    platform := hosttype & "-" & ostype;
    WHILE res AND act # NIL DO
      IF Pathname.Absolute(act.arg) THEN
        pn := act.arg;
      ELSE
        pn := Pathname.Join(path, act.arg, NIL);
      END;
      CASE act.kind OF
        PredKind.Dir => res := IsDir(self, pn);
        IF Msg.dFlag THEN
          MsgX.D(self.msgif, "IsDir(" & pn & ")", level := 2);
        END;
      | PredKind.File => res := IsFile(self, pn);
        IF Msg.dFlag THEN
          MsgX.D(self.msgif, "IsFile(" & pn & ")", level := 2);
        END;
      | PredKind.Match =>
        plist := TextUtils.Split(act.arg, "/");
        res   := Matches(path, plist);
      | PredKind.NoDir => res := NOT IsDir(self, pn);
        IF Msg.dFlag THEN
          MsgX.D(self.msgif, "NoDir(" & pn & ")", level := 2);
        END;
      | PredKind.NoFile => res := NOT IsFile(self, pn);
        IF Msg.dFlag THEN
          MsgX.D(self.msgif, "NoFile(" & pn & ")", level := 2);
        END;
      | PredKind.NoMatch =>
        plist := TextUtils.Split(act.arg, "/");
        res   := NOT Matches(path, plist);
      | PredKind.HostType => res := RegExMatches(act.arg, hosttype);
      | PredKind.OSType   => res := RegExMatches(act.arg, ostype);
      | PredKind.Platform => res := RegExMatches(act.arg, platform);
      END;
      act := act.next;
    END;
    IF Msg.dFlag THEN
      IF res THEN
        MsgX.D(self.msgif, "  returning TRUE", level := 2);
      ELSE
        MsgX.D(self.msgif, "  returning FALSE", level := 2);
      END;
    END;
    RETURN res;
  END EvalCondition;

(*---------------------------------------------------------------------------*)
PROCEDURE SortByLength(self : T; pnlist : TextSeq.T) : TextSeq.T =
  VAR
    res  := NEW(TextSeq.T).init();
    rest :  TextSeq.T;
    len  := 1;
    pn   :  TEXT;
  BEGIN
    (* very inefficient, but should be acceptable here *)
    WHILE pnlist.size() > 0 DO
      rest := NEW(TextSeq.T).init();
      WHILE pnlist.size() > 0 DO
        pn := pnlist.remlo();
        TRY
          IF Pathname.Decompose(pn).size() = len THEN
            res.addhi(pn);
          ELSE
            rest.addhi(pn);
          END;
        EXCEPT
          Pathname.Invalid => 
          MsgX.Error(self.msgif, 
                     "invalid pathname in package structure skipped: " & pn);
        END;
      END;
      pnlist := rest;
      INC(len);
    END;
    RETURN res;
  END SortByLength;

(*---------------------------------------------------------------------------*)
PROCEDURE CreateStructure(self : T; path : TEXT) RAISES {PkgError.E} =
  VAR
    act   := self.expr;
    pn    :  Pathname.T;
    f     :  File.T;
    dirs  := NEW(TextSeq.T).init();
    files := NEW(TextSeq.T).init();
  BEGIN
    IF NOT FSUtils.Exists(path) THEN
      TRY
        FS.CreateDirectory(path);
      EXCEPT
        OSError.E(e) => RAISE PkgError.E("cannot create directory " &
          path & ": " & System.AtomListToText(e));
      END;
    END;
    (* traverse and check conditions *)
    WHILE act # NIL DO
      IF Pathname.Absolute(act.arg) THEN
        pn := act.arg;
      ELSE
        pn := Pathname.Join(path, act.arg, NIL);
      END;
      CASE act.kind OF
        PredKind.Dir => 
        IF FSUtils.Exists(pn) THEN
          RAISE PkgError.E("component " & pn & " already exists")
        END;
        dirs.addhi(pn);
      | PredKind.File => 
        IF FSUtils.Exists(pn) THEN
          RAISE PkgError.E("component " & pn & " already exists")
        END;
        files.addhi(pn);
      | PredKind.NoDir => 
        IF FSUtils.Exists(pn) THEN
          RAISE PkgError.E("component " & pn & " exists but mustn't")
        END;
      | PredKind.NoFile => 
        IF FSUtils.Exists(pn) THEN
          RAISE PkgError.E("component " & pn & " exists but mustn't")
        END;
      ELSE
        (* skip *)
      END;
      act := act.next;
    END;
    (* sort files and directories *)
    dirs := SortByLength(self, dirs);
    files := SortByLength(self, files);
    (* create directories and files *)
    FOR i := 0 TO dirs.size() - 1 DO
      WITH dir = dirs.get(i) DO
	TRY
	  FS.CreateDirectory(dir);
	EXCEPT
	  OSError.E(e) => RAISE PkgError.E("cannot create directory " &
	    dir & ": " & System.AtomListToText(e));
	END;
      END;
    END;
    FOR i := 0 TO files.size() - 1 DO
      WITH file = files.get(i) DO
        TRY
          f := FS.OpenFile(p := file, create := FS.CreateOption.Always);
          f.close();
        EXCEPT
          OSError.E(e) => RAISE PkgError.E("cannot create empty file " &
            file & ": " & System.AtomListToText(e));
        END;
      END;
    END;
  END CreateStructure;

(*---------------------------------------------------------------------------*)
PROCEDURE EnsureStructureExists(self : T; path : TEXT) RAISES {PkgError.E} =
  VAR
    act    := self.expr;
    pn     :  Pathname.T;
    f      :  File.T;
    dirs   := NEW(TextSeq.T).init();
    files  := NEW(TextSeq.T).init();
    exists := FALSE;
  BEGIN
    IF FSUtils.Exists(path) THEN
      IF FSUtils.IsDir(path) THEN
        exists := TRUE;
      ELSE
        RAISE PkgError.E("component " & path & 
              " already exists, but is no directory")
      END;
    END;
    IF NOT exists THEN
      TRY
	FS.CreateDirectory(path);
      EXCEPT
	OSError.E(e) => RAISE PkgError.E("cannot create directory " &
	  path & ": " & System.AtomListToText(e));
      END;
    END;
    (* traverse and check conditions *)
    WHILE act # NIL DO
      IF Pathname.Absolute(act.arg) THEN
        pn := act.arg;
      ELSE
        pn := Pathname.Join(path, act.arg, NIL);
      END;
      CASE act.kind OF
        PredKind.Dir => 
        IF FSUtils.Exists(pn) THEN
          IF NOT FSUtils.IsDir(pn) THEN
            RAISE PkgError.E("component " & pn & 
                  " already exists, but is no directory")
          END;
        ELSE
          dirs.addhi(pn);
        END;
      | PredKind.File => 
        IF FSUtils.Exists(pn) THEN
          IF NOT FSUtils.IsFile(pn) THEN
            RAISE PkgError.E("component " & pn & 
                  " already exists, but is no ordinary file")
          END;
        ELSE
          files.addhi(pn);
        END;
      | PredKind.NoDir => 
        IF FSUtils.Exists(pn) THEN
          RAISE PkgError.E("component " & pn & " exists but mustn't")
        END;
      | PredKind.NoFile => 
        IF FSUtils.Exists(pn) THEN
          RAISE PkgError.E("component " & pn & " exists but mustn't")
        END;
      ELSE
        (* skip *)
      END;
      act := act.next;
    END;
    (* sort files and directories *)
    dirs := SortByLength(self, dirs);
    files := SortByLength(self, files);
    (* create directories and files *)
    FOR i := 0 TO dirs.size() - 1 DO
      WITH dir = dirs.get(i) DO
	TRY
	  FS.CreateDirectory(dir);
	EXCEPT
	  OSError.E(e) => RAISE PkgError.E("cannot create directory " &
	    dir & ": " & System.AtomListToText(e));
	END;
      END;
    END;
    FOR i := 0 TO files.size() - 1 DO
      WITH file = files.get(i) DO
        TRY
          f := FS.OpenFile(p := file, create := FS.CreateOption.Always);
          f.close();
        EXCEPT
          OSError.E(e) => RAISE PkgError.E("cannot create empty file " &
            file & ": " & System.AtomListToText(e));
        END;
      END;
    END;
  END EnsureStructureExists; 

(*---------------------------------------------------------------------------*)
PROCEDURE IsDir(self : T; pn : Pathname.T) : BOOLEAN =
  VAR 
    s   : FileStatus.T;
    apn : APN.T;
  BEGIN
    IF self.cache = NIL THEN
      RETURN FSUtils.IsDir(pn);
    END;
    apn := APN.New(pn);
    s := self.cache.getStatus(apn);
    IF NOT s.exists THEN
      IF FSUtils.IsDir(pn) THEN
        self.cache.updateRec(apn, extensions, ignoreDirs); 
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END;
    RETURN s.isDir;
  END IsDir;

(*---------------------------------------------------------------------------*)
PROCEDURE IsFile(self : T; pn : Pathname.T) : BOOLEAN =
  VAR 
    s   : FileStatus.T;
    apn : APN.T;
  BEGIN
    IF self.cache = NIL THEN
      RETURN FSUtils.IsFile(pn);
    END;
    apn := APN.New(pn);
    s := self.cache.getStatus(apn);
    IF NOT s.exists THEN
      IF FSUtils.IsFile(pn) THEN
        s := self.cache.update(apn); 
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END;
    RETURN s.isFile;
  END IsFile; 

(*--- currently not used ---
(*---------------------------------------------------------------------------*)
PROCEDURE Exists(self : T; pn : Pathname.T) : BOOLEAN =
  VAR 
    s   : FileStatus.T;
    apn : APN.T;
  BEGIN
    IF self.cache = NIL THEN
      RETURN FSUtils.Exists(pn);
    END;
    apn := APN.New(pn);
    s := self.cache.getStatus(apn);
    RETURN s.exists;
  END Exists; 
*)

VAR
  extensions := NEW(TextSeq.T).init();
  ignoreDirs := NEW(APNSeq.T).init();
BEGIN
END PkgKindData.
