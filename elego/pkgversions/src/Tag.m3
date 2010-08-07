(*---------------------------------------------------------------------------*)
MODULE Tag;

IMPORT TextSeq, Text, TextExtras AS TextEx, TextUtils, Version, SMsg AS Msg;

(*---------------------------------------------------------------------------*)
PROCEDURE Decompose(tag : TEXT) : TextSeq.T =
  VAR
    i    : CARDINAL;
    res  := NEW(TextSeq.T).init(5);
    elem : TEXT;
  BEGIN
    i := 0;
    WHILE TextEx.FindSub(tag, "_", i) DO
      elem := Text.Sub(tag, 0, i);
      tag := Text.Sub(tag, i + 1);
      i := 0;
      res.addhi(TextUtils.SubstChar(elem, '=', '_'));
    END;
    IF NOT Text.Empty(tag) THEN
      res.addhi(TextUtils.SubstChar(tag, '=', '_'));
    END;
    RETURN res;
  END Decompose; 

(*---------------------------------------------------------------------------*)
PROCEDURE CheckedName(t : TEXT) : TEXT =
  VAR
    from := ARRAY [0..12] OF CHAR  
        {'_', ' ', '\t', '#', ',', '.', '/', '$', '%', '@', '^', '&', '*'};
    to   := ARRAY [0..12] OF CHAR 
        {'=', '=', '=',  '=', '=', '=', '=', '=', '=', '=', '=', '=', '='};
  BEGIN
    RETURN TextUtils.SubstChars(t, from, to);
  END CheckedName;

(*---------------------------------------------------------------------------*)
PROCEDURE Compose(seq : TextSeq.T) : TEXT =
  VAR
    res  : TEXT;
  BEGIN
    IF seq.size() = 0 THEN RETURN "" END;
    res := CheckedName(seq.get(0));
    FOR i := 1 TO seq.size() - 1 DO
      WITH elem = seq.get(i) DO
        res := res & "_" & CheckedName(elem);
      END;
    END;
    RETURN res;
  END Compose; 

(*---------------------------------------------------------------------------*)
PROCEDURE New(tag : TEXT) : T =
  BEGIN
    IF tag = NIL THEN
      Msg.Error("Tag.New(NIL)");
      RETURN NIL;
    ELSE
      Msg.D("Tag.New(" & tag & ")");
    END;
    IF Text.Equal(tag, "head") OR 
       Text.Equal(tag, "headoftrunk") OR 
       Text.Equal(tag, "topoftrunk") OR 
       Text.Equal(tag, "head-of-trunk") OR 
       Text.Equal(tag, "top-of-trunk") OR 
       Text.Equal(tag, "top") THEN
      RETURN Head;
    ELSIF Text.Equal(tag, "headofbranch") OR 
          Text.Equal(tag, "tipofbranch") OR 
          Text.Equal(tag, "head-of-branch") OR 
          Text.Equal(tag, "tip-of-branch") OR 
          Text.Equal(tag, "tip") THEN
      RETURN Tip;
    ELSIF Text.Equal(tag, "last-release-branch") OR 
          Text.Equal(tag, "lastreleasebranch") OR 
          Text.Equal(tag, "latest-release-branch") OR 
          Text.Equal(tag, "latestreleasebranch") OR 
          Text.Equal(tag, "latest-tip") OR 
          Text.Equal(tag, "latesttip") OR 
          Text.Equal(tag, "last-tip") OR 
          Text.Equal(tag, "lasttip") THEN
      RETURN LastTip;
    END;
    RETURN InitFromText(NEW(T), tag);
  END New;

(*---------------------------------------------------------------------------*)
PROCEDURE NewCopy(tag : T) : T =
  BEGIN
    RETURN InitFromTag(NEW(T), tag);
  END NewCopy;

(*---------------------------------------------------------------------------*)
PROCEDURE Construct(k : Kind; pkgname : TEXT; 
                    major, minor, pl : CARDINAL;
                    stable := FALSE; change : TEXT := NIL;
                    branch := FALSE; suffix : TEXT := NIL;
                    start := FALSE) : T =
  BEGIN
    RETURN InitFromElements(NEW(T), k, pkgname, major, minor, pl, stable,
                            change, branch, suffix, start);
  END Construct;

(*---------------------------------------------------------------------------*)
PROCEDURE NewStableBranch(tag : T) : T =
  VAR t := NewCopy(tag);
  BEGIN
    t.t_attr := Attributes{Attribute.Stable, Attribute.Branch};
    t.t_orig := NIL;
    RETURN t;
  END NewStableBranch;

(*---------------------------------------------------------------------------*)
PROCEDURE NewBranchStartTag(tag : T) : T =
  VAR t := NewCopy(tag);
  BEGIN
    t.t_attr := Attributes{Attribute.BranchStart};
    t.t_orig := NIL;
    RETURN t;
  END NewBranchStartTag;

(*---------------------------------------------------------------------------*)
PROCEDURE NewBranch(tag : T; kind : Kind := Kind.Undefined) : T =
  VAR t := NewCopy(tag);
  BEGIN
    t.t_attr := Attributes{Attribute.Branch};
    t.t_orig := NIL;
    IF kind # Kind.Undefined THEN
      t.t_kind := kind;
    END;
    t.t_seq := Decompose(Denotation(t, FALSE));
    RETURN t;
  END NewBranch;
     
(*---------------------------------------------------------------------------*)
PROCEDURE NewBranchStart(tag : T; kind : Kind := Kind.Undefined) : T =
  VAR t := NewCopy(tag);
  BEGIN
    t.t_attr := Attributes{Attribute.BranchStart};
    t.t_orig := NIL;
    IF kind # Kind.Undefined THEN
      t.t_kind := kind;
    END;
    t.t_seq := Decompose(Denotation(t, FALSE));
    RETURN t;
  END NewBranchStart;

(*---------------------------------------------------------------------------*)
PROCEDURE NewMergeTag(tag : T; mversion : Version.T := NIL) : T =
  VAR t := NewCopy(tag);
  BEGIN
    t.t_attr := Attributes{Attribute.MergeMarker};
    t.t_kind := Kind.Merge;
    t.t_orig := NIL;
    IF mversion # NIL THEN
      t.t_mversion := mversion;
    END;
    (* for debugging...
    WITH d = Denotation(t, checkIt := FALSE) DO
      WITH r = t.initFromText(d) DO
        RETURN r;
      END;
    END;
    *)
    RETURN t.initFromText(Denotation(t, checkIt := FALSE));
  END NewMergeTag;

(*---------------------------------------------------------------------------*)
PROCEDURE KindFromText(t : TEXT) : Kind =
  BEGIN
    IF    Text.Equal(t, "alpha") THEN
      RETURN Kind.Alpha;
    ELSIF Text.Equal(t, "beta") THEN
      RETURN Kind.Beta;
    ELSIF Text.Equal(t, "gamma") THEN
      RETURN Kind.Gamma;
    ELSIF Text.Equal(t, "merge") THEN
      RETURN Kind.Merge;
    ELSIF Text.Equal(t, "devel") THEN
      RETURN Kind.Devel;
    ELSIF Text.Equal(t, "release") THEN
      RETURN Kind.Release;
    ELSIF Text.Equal(t, "latest") THEN
      RETURN Kind.Latest;
    ELSIF Text.Equal(t, "change") THEN
      RETURN Kind.Change;
    ELSIF Text.Equal(t, "feature") THEN
      RETURN Kind.Feature;
    ELSIF Text.Equal(t, "fix") THEN
      RETURN Kind.Fix;
    ELSE
      RETURN Kind.Undefined;
    END;
  END KindFromText;  

(*---------------------------------------------------------------------------*)
PROCEDURE KindToText(k : Kind) : TEXT =
  BEGIN
    CASE k OF
      Kind.Alpha => RETURN "alpha";
    | Kind.Beta  => RETURN "beta";
    | Kind.Gamma => RETURN "gamma";
    | Kind.Devel => RETURN "devel";
    | Kind.Merge => RETURN "merge";
    | Kind.Change => RETURN "change";
    | Kind.Feature => RETURN "feature";
    | Kind.Fix => RETURN "fix";
    | Kind.Release => RETURN "release";
    | Kind.Latest => RETURN "latest";
    | Kind.Undefined => RETURN "undefined";
    END;
  END KindToText;

(*---------------------------------------------------------------------------*)
PROCEDURE KindToVersionKind(k : Kind) : CHAR =
  BEGIN
    CASE k OF
      Kind.Alpha => RETURN 'a';
    | Kind.Beta  => RETURN 'b';
    | Kind.Gamma => RETURN 'g';
    | Kind.Devel => RETURN 'd';
    | Kind.Merge => RETURN 'm';
    | Kind.Change => RETURN 'c';
    | Kind.Feature => RETURN 'f';
    | Kind.Fix => RETURN 'F';
    | Kind.Release => RETURN 'r';
    | Kind.Latest => RETURN 'l';
    | Kind.Undefined => RETURN 'u';
    END;
  END KindToVersionKind; 

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T) : T =
  BEGIN
    self.t_attr := Attributes{};
    self.t_seq := NIL;
    self.t_kind := Kind.Undefined;
    self.t_version := NEW(Version.T).init();
    self.t_mversion := NEW(Version.T).init();
    self.t_pkgname := "";
    self.t_orig := NIL;
    self.t_change := NIL;
    self.t_location := NIL;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE InitFromTag(self : T; tag : T) : T =
  BEGIN
    self.t_orig := tag.t_orig;
    self.t_seq := tag.t_seq;
    self.t_pkgname := tag.t_pkgname;
    self.t_change := tag.t_change;
    self.t_location := tag.t_location;
    self.t_kind := tag.t_kind;
    self.t_attr := tag.t_attr;
    self.t_version := NEW(Version.T).init();
    self.t_version.major := tag.t_version.major;
    self.t_version.minor := tag.t_version.minor;
    self.t_version.patchlevel := tag.t_version.patchlevel;
    self.t_version.kind := tag.t_version.kind;
    self.t_mversion := NEW(Version.T).init();
    self.t_mversion.major := tag.t_mversion.major;
    self.t_mversion.minor := tag.t_mversion.minor;
    self.t_mversion.patchlevel := tag.t_mversion.patchlevel;
    self.t_mversion.kind := tag.t_mversion.kind;
    RETURN self;
  END InitFromTag;


(*---------------------------------------------------------------------------*)
PROCEDURE InitFromText(self : T; tag : TEXT) : T =
  VAR
    v, w : TEXT;
    vstart : CARDINAL;
    i,n : CARDINAL := 0;
  BEGIN
    self.t_orig := tag;
    self.t_seq := Decompose(tag);
    self.t_attr := Attributes{};
    n := self.t_seq.size();
    IF i < n THEN
      self.t_kind := KindFromText(self.t_seq.get(i));
      INC(i);
    ELSE
      self.t_kind := Kind.Undefined;
    END;
    IF self.t_kind = Kind.Change OR
       self.t_kind = Kind.Feature OR
       self.t_kind = Kind.Fix THEN
      IF i < n  THEN
        self.t_change := self.t_seq.get(i);
        INC(i);
      ELSE
        self.t_change := "";
      END;
    END;
    IF i < n  THEN
      self.t_pkgname := self.t_seq.get(i);
      INC(i);
    ELSE
      self.t_pkgname := "";
    END;
    IF i < n  THEN
      v := self.t_seq.get(i);
      IF Text.Equal(v, "head") OR Text.Equal(v, "stable") OR
         Text.Equal(v, "branch") THEN
        self.t_attr := self.t_attr + Attributes{Attribute.Branch};
        IF Text.Equal(v, "stable") THEN
          self.t_attr := self.t_attr + Attributes{Attribute.Stable};
        END;
      END;
      IF Text.Equal(v, "start") THEN
        self.t_attr := self.t_attr + Attributes{Attribute.BranchStart};
        self.t_attr := self.t_attr - Attributes{Attribute.Branch};
        self.t_attr := self.t_attr - Attributes{Attribute.Stable};
      END;
      INC(i);
      vstart := i;
      w := NIL;
      WHILE i < n DO
        WITH elem = self.t_seq.get(i) DO
          INC(i);
          IF self.t_location = NIL THEN
            IF Text.Equal(elem, "at") THEN
              self.t_location := elem;
            ELSIF Text.Equal(elem, "stable") THEN
              self.t_attr := self.t_attr + Attributes{Attribute.Stable};
              self.t_attr := self.t_attr + Attributes{Attribute.Branch};
            ELSIF Text.Equal(elem, "head") THEN
              self.t_attr := self.t_attr + Attributes{Attribute.Branch};
            ELSIF Text.Equal(elem, "start") THEN
              self.t_attr := self.t_attr + Attributes{Attribute.BranchStart};
            ELSIF Text.Equal(elem, "into") THEN
              self.t_attr := self.t_attr + Attributes{Attribute.MergeMarker};
            ELSIF Attribute.MergeMarker IN self.t_attr THEN
              IF w = NIL THEN
                w := elem;
              ELSE
                (* FIXME: This is somewhat wanting... *)
                w := w & "_" & elem;
              END;
            ELSE
              (* FIXME: This is somewhat wanting... *)
              v := v & "_" & elem;
            END;
          ELSE
            self.t_location := self.t_location & "_" & elem;
          END;
        END;
      END;
      self.t_version := NEW(Version.T).fromDir(v);
      IF self.t_kind = Kind.Merge AND w # NIL THEN
        self.t_mversion := NEW(Version.T).fromDir(w);
      ELSE
        (* We must not override the kind of version we merged into. *)
        self.t_version.kind := KindToVersionKind(self.t_kind);
        self.t_mversion := NEW(Version.T).init();
      END;
    ELSE
      self.t_version := NEW(Version.T).init();
    END;
    RETURN self;
  END InitFromText;

(*---------------------------------------------------------------------------*)
PROCEDURE InitFromElements(self : T; k : Kind; pkgname : TEXT; 
                           major, minor, pl : CARDINAL;
                           stable := FALSE; change : TEXT := NIL;
                           branch := FALSE; suffix : TEXT := NIL;
                           start  := FALSE) : T =
  BEGIN
    self.t_kind := k;
    self.t_pkgname := pkgname;
    self.t_version := NEW(Version.T).init();
    self.t_version.major := major;
    self.t_version.minor := minor;
    self.t_version.patchlevel := pl;
    self.t_version.kind := KindToVersionKind(k);
    self.t_mversion := NEW(Version.T).init();
    self.t_change := change;
    self.t_location := suffix;
    self.t_attr := Attributes{};
    IF start THEN
      self.t_attr := self.t_attr + Attributes{Attribute.BranchStart};
    ELSIF branch THEN
      self.t_attr := self.t_attr + Attributes{Attribute.Branch};
    ELSIF stable THEN
      self.t_attr := self.t_attr + Attributes{Attribute.Stable};
      self.t_attr := self.t_attr + Attributes{Attribute.Branch};
    ELSE
    END;
    IF k = Kind.Change OR k = Kind.Feature OR k = Kind.Fix THEN
      IF self.t_change = NIL THEN
        self.t_change := "noname";
      END;
    END;
    self.t_seq := Decompose(Denotation(self, FALSE));
    RETURN self;
  END InitFromElements;

(*---------------------------------------------------------------------------*)
PROCEDURE OriginalText(self : T) : TEXT =
  BEGIN
    IF self.t_orig = NIL THEN
      RETURN "";
    ELSE
      RETURN self.t_orig;
    END;
  END OriginalText;

(*---------------------------------------------------------------------------*)
PROCEDURE Denotation(self : T; checkIt := TRUE) : TEXT =
  VAR t : TEXT;
  BEGIN
    IF self = Head THEN RETURN "head" END; (* Hack for special head value *)
    IF self = Tip THEN RETURN "tip" END; 
    IF self = LastTip THEN RETURN "lasttip" END;
    IF checkIt AND NOT self.okay() THEN
      Msg.D(self.debugInfo());
      IF self.t_orig # NIL THEN
        RETURN self.t_orig & "[undefined_format]";
      ELSE
        RETURN "undefined_tag";
      END;
    END;
    IF NOT checkIt AND self.t_orig # NIL THEN
      RETURN self.t_orig;
    END;
    IF self.t_kind = Kind.Undefined THEN
      IF self.t_orig # NIL THEN
        RETURN self.t_orig & "[undefined_format]";
      ELSE
        RETURN "undefined_tag";
      END;
    END;
    IF self.t_kind = Kind.Merge THEN
      t := KindToText(self.t_kind) & "_" & CheckedName(self.t_pkgname) & "_" & 
               self.t_version.toDir() & "_into_" &
               self.t_mversion.toDir();
    ELSIF self.t_kind = Kind.Change OR
      self.t_kind = Kind.Feature OR
      self.t_kind = Kind.Fix THEN
      t := KindToText(self.t_kind) & "_" & CheckedName(self.t_change) & "_" &
               CheckedName(self.t_pkgname) & "_";
      IF Attribute.BranchStart IN self.t_attr THEN
        t := t & "start";
      ELSIF Attribute.Branch IN self.t_attr THEN
        t := t & "head";
      ELSE
        t := t & self.t_version.toDir();
      END;
      IF self.t_location # NIL THEN
        (* Msg.T("XXX1: " & self.debugInfo()); *)
        WHILE TextUtils.Pos(self.t_location, "at_") = 0 DO
          self.t_location := Text.Sub(self.t_location, 3);
        END;
        t := t & "_at_" & self.t_location;
      END;
      RETURN t;
    ELSE
      t := KindToText(self.t_kind) & "_" & CheckedName(self.t_pkgname) & "_" & 
               self.t_version.toDir();
    END;
    IF Attribute.Stable IN self.t_attr THEN
      VAR i := Text.FindCharR(t, '_'); BEGIN
        t := Text.Sub(t, 0, i + 1) & "stable";
      END;
    ELSIF Attribute.BranchStart IN self.t_attr THEN
      VAR i := Text.FindCharR(t, '_'); BEGIN
        t := Text.Sub(t, 0, i + 1) & "start";
      END;
    END;
    IF self.t_location # NIL THEN
      (* Msg.T("XXX2: " & self.debugInfo()); *)
      WHILE TextUtils.Pos(self.t_location, "at_") = 0 DO
        self.t_location := Text.Sub(self.t_location, 3);
      END;
      t := t & "_at_" & self.t_location;
    END;
    RETURN t;
  END Denotation;

(*---------------------------------------------------------------------------*)
PROCEDURE Base(self : T; level := 5) : TEXT =
  VAR
    seq : TextSeq.T;
  BEGIN
    self.t_seq := Decompose(Denotation(self, FALSE));
    seq := NEW(TextSeq.T).init(level);
    FOR i := 0 TO level - 1 DO
      seq.addhi(self.t_seq.get(i));
    END;
    RETURN Compose(seq);
  END Base;

(*---------------------------------------------------------------------------*)
PROCEDURE Okay(self : T) : BOOLEAN =
  VAR n := self.t_seq.size();

  PROCEDURE def(t: TEXT) : BOOLEAN = 
    BEGIN
      RETURN t # NIL AND Text.Length(t) > 0;
    END def;

  BEGIN
    IF NOT def(self.t_pkgname) THEN RETURN FALSE END;
    IF self.t_kind = Kind.Merge THEN
      RETURN n >= 9 AND self.t_version.defined();
    ELSIF self.t_kind = Kind.Change OR
      self.t_kind = Kind.Feature OR
      self.t_kind = Kind.Fix THEN
      IF NOT def(self.t_change) THEN RETURN FALSE END;
      RETURN (n = 4 AND NOT def(self.t_location) OR
              n > 4 AND def(self.t_location)) AND 
             (Attribute.BranchStart IN self.t_attr OR
              Attribute.Branch IN self.t_attr)
      OR
             (n = 6 AND NOT def(self.t_location) OR
              n > 6 AND def(self.t_location)) AND
             NOT Attribute.BranchStart IN self.t_attr AND
             NOT Attribute.Branch IN self.t_attr AND 
             self.t_version.defined();
    END;
    RETURN (n = 5 AND NOT def(self.t_location) OR
            n > 5 AND def(self.t_location)) AND
           self.t_version.defined(); 
    (* FIXME: check some more *)
  END Okay; 

(*---------------------------------------------------------------------------*)
PROCEDURE Predefined(self : T) : BOOLEAN =
  BEGIN
    RETURN self = Head OR self = Tip OR self = LastTip;
  END Predefined;

(*---------------------------------------------------------------------------*)
PROCEDURE IsKind(tag : TEXT; k : Kind) : BOOLEAN =
  VAR 
    seq   := Decompose(tag);
    elem1 : TEXT;
  BEGIN
    IF seq.size() < 1 THEN RETURN FALSE END;
    elem1 := seq.get(0);
    CASE k OF
      Kind.Alpha => RETURN Text.Equal(elem1, "alpha");
    | Kind.Beta  => RETURN Text.Equal(elem1, "beta");
    | Kind.Gamma => RETURN Text.Equal(elem1, "gamma");
    | Kind.Devel => RETURN Text.Equal(elem1, "devel");
    | Kind.Merge => RETURN Text.Equal(elem1, "merge");
    | Kind.Release => RETURN Text.Equal(elem1, "release");
    | Kind.Latest => RETURN Text.Equal(elem1, "latest");
    | Kind.Change => RETURN Text.Equal(elem1, "change");
    | Kind.Feature => RETURN Text.Equal(elem1, "feature");
    | Kind.Fix => RETURN Text.Equal(elem1, "fix");
    | Kind.Undefined =>
      RETURN NOT Text.Equal(elem1, "alpha") AND 
             NOT Text.Equal(elem1, "beta")  AND
             NOT Text.Equal(elem1, "gamma") AND
             NOT Text.Equal(elem1, "devel") AND
             NOT Text.Equal(elem1, "merge") AND
             NOT Text.Equal(elem1, "change") AND
             NOT Text.Equal(elem1, "feature") AND
             NOT Text.Equal(elem1, "fix") AND
             NOT Text.Equal(elem1, "latest") AND
             NOT Text.Equal(elem1, "release");
    END;
  END IsKind; 

(*---------------------------------------------------------------------------*)
PROCEDURE IsStableBranchTag(self : T) : BOOLEAN =
  BEGIN
    IF self.t_seq.size() # 5 THEN 
      self.t_attr := self.t_attr - Attributes{Attribute.Stable};
      RETURN FALSE;
    END;
    IF Text.Equal(self.t_seq.get(4), "stable") THEN
      self.t_attr := self.t_attr + Attributes{Attribute.Stable};
      self.t_attr := self.t_attr + Attributes{Attribute.Branch};
      RETURN TRUE;
    ELSE
      self.t_attr := self.t_attr - Attributes{Attribute.Stable};
      RETURN FALSE;
    END;
  END IsStableBranchTag; 

(*---------------------------------------------------------------------------*)
PROCEDURE IsChangeBranchTag(self : T) : BOOLEAN =
  BEGIN
    self.t_kind := KindFromText(self.t_seq.get(0));
    IF self.t_kind # Kind.Change AND self.t_kind # Kind.Fix AND
      self.t_kind # Kind.Feature THEN
      RETURN FALSE;
    END;
    IF Text.Equal(self.t_seq.get(3), "head") THEN
      self.t_attr := self.t_attr + Attributes{Attribute.Branch};
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END IsChangeBranchTag; 

(*---------------------------------------------------------------------------*)
PROCEDURE IsChangeFeatureOrFixTag(self : T) : BOOLEAN =
  BEGIN
    self.t_kind := KindFromText(self.t_seq.get(0));
    RETURN self.t_kind = Kind.Change OR self.t_kind = Kind.Fix OR
      self.t_kind = Kind.Feature;
  END IsChangeFeatureOrFixTag;

(*---------------------------------------------------------------------------*)
PROCEDURE GetKind(self : T) : Kind =
  BEGIN
    RETURN self.t_kind;
  END GetKind;

(*---------------------------------------------------------------------------*)
PROCEDURE GetKindAsText(self : T) : TEXT =
  BEGIN
    RETURN KindToText(self.t_kind);
  END GetKindAsText;

(*---------------------------------------------------------------------------*)
PROCEDURE PackageName(self : T) : TEXT =
  BEGIN
    RETURN self.t_pkgname;
  END PackageName; 

(*---------------------------------------------------------------------------*)
PROCEDURE GetChangeName(self : T) : TEXT =
  BEGIN
    RETURN self.t_change;
  END GetChangeName;

(*---------------------------------------------------------------------------*)
PROCEDURE GetLocationSuffix(self : T) : TEXT =
  BEGIN
    RETURN self.t_location;
  END GetLocationSuffix; 

(*---------------------------------------------------------------------------*)
PROCEDURE MergeDestVersion(self : T) : Version.T =
  BEGIN
    RETURN self.t_mversion;
  END MergeDestVersion; 

(*---------------------------------------------------------------------------*)
PROCEDURE GetVersion(self: T) : Version.T =
  BEGIN
    RETURN self.t_version;
  END GetVersion; 

(*---------------------------------------------------------------------------*)
PROCEDURE VersionAsText(self: T) : TEXT =
  BEGIN
    RETURN self.t_version.toDir();
  END VersionAsText;  

(*---------------------------------------------------------------------------*)
PROCEDURE SetNextMajorVersion(self : T) =
  BEGIN
    self.t_version.nextMajor();
    self.t_orig := NIL;
  END SetNextMajorVersion; 

(*---------------------------------------------------------------------------*)
PROCEDURE SetNextMinorVersion(self : T) =
  BEGIN
    self.t_version.nextMinor();
    self.t_orig := NIL;
  END SetNextMinorVersion; 

(*---------------------------------------------------------------------------*)
PROCEDURE SetNextPatchLevel(self : T) =
  BEGIN
    self.t_version.nextPatchLevel();
    self.t_orig := NIL;
  END SetNextPatchLevel; 

(*---------------------------------------------------------------------------*)
PROCEDURE LatestTag(list : TextSeq.T; k : Kind) : T =
  VAR
    max, act :T;
  BEGIN
    max := NIL; 
    FOR i := 0 TO list.size() - 1 DO
      WITH elem = list.get(i) DO
        act := New(elem);
        (* Msg.D("LatestTag: elem " & elem); *)
        IF act.isStableBranchTag() THEN
          act.t_version.patchlevel := 0;
          act.t_attr := act.t_attr - Attributes{Attribute.Stable};
          act.t_attr := act.t_attr - Attributes{Attribute.Branch};
          act.t_orig := NIL;
        END;
        (* Msg.D("act: " & act.debugInfo()); *)
        IF act.t_version.exactDefined() THEN
          IF act.kind() = k AND act.okay() THEN
            IF max = NIL THEN
              (* Msg.D("new max (1)"); *)
              max := act;
            ELSIF max.t_version.less(act.t_version) THEN
              (* Msg.D("new max (2)"); *)
              max := act;
            END;
          END;
        END;
      END;
    END;
    IF max = NIL THEN
      Msg.D("LatestTag --> NIL");
    ELSE
      Msg.D("LatestTag --> " & max.denotation(FALSE));
    END;
    RETURN max;
  END LatestTag; 

(*---------------------------------------------------------------------------*)
PROCEDURE ContainedInList(list : TextSeq.T; tag : T) : BOOLEAN =
  VAR
    act :T;
  BEGIN
    FOR i := 0 TO list.size() - 1 DO
      WITH elem = list.get(i) DO
        act := New(elem);
        IF Equal(act, tag) THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END ContainedInList;  

(*---------------------------------------------------------------------------*)
PROCEDURE Equal(self, tag : T) : BOOLEAN =
  BEGIN
    IF (self.t_pkgname # NIL AND tag.t_pkgname = NIL) OR
       (self.t_pkgname = NIL AND tag.t_pkgname # NIL) OR
       ((self.t_pkgname # NIL AND tag.t_pkgname # NIL) AND
         NOT Text.Equal(self.t_pkgname, tag.t_pkgname)) OR
       (self.t_change # NIL AND tag.t_change = NIL) OR
       (self.t_change = NIL AND tag.t_change # NIL) OR
       ((self.t_change # NIL AND tag.t_change # NIL) AND
         NOT Text.Equal(self.t_change, tag.t_change)) OR
       NOT self.t_kind = tag.t_kind OR 
       NOT self.t_attr = tag.t_attr THEN
      RETURN FALSE;
    END;
    IF self.t_kind = Kind.Merge THEN
      RETURN Version.Equal(self.t_version,  tag.t_version) AND
             Version.Equal(self.t_mversion, tag.t_mversion);
    ELSE
      RETURN Version.Equal(self.t_version, tag.t_version);
    END;
  END Equal;

(*---------------------------------------------------------------------------*)
PROCEDURE Compare(self, tag : T) : [-1..1] =
  VAR rtn : [-1..1];
  BEGIN 
    IF self.t_location = NIL AND tag.t_location # NIL THEN
      RETURN -1;
    ELSIF self.t_location # NIL AND tag.t_location = NIL THEN
      RETURN 1;
    ELSIF self.t_location = NIL AND tag.t_location = NIL THEN
      rtn := 0;
    ELSE
      rtn := Text.Compare(self.t_location, tag.t_location);
    END;
    IF rtn # 0 THEN RETURN rtn END;

    IF self.t_change = NIL AND tag.t_change # NIL THEN
      RETURN -1;
    ELSIF self.t_change # NIL AND tag.t_change = NIL THEN
      RETURN 1;
    ELSIF self.t_change = NIL AND tag.t_change = NIL THEN
      rtn := 0;
    ELSE
      rtn := Text.Compare(self.t_change, tag.t_change);
    END;
    IF rtn # 0 THEN RETURN rtn END;

    IF self.t_pkgname = NIL AND tag.t_pkgname # NIL THEN
      RETURN -1;
    ELSIF self.t_pkgname # NIL AND tag.t_pkgname = NIL THEN
      RETURN 1;
    ELSIF self.t_pkgname = NIL AND tag.t_pkgname = NIL THEN
      rtn := 0;
    ELSE
      rtn := Text.Compare(self.t_pkgname, tag.t_pkgname);
    END;

    IF rtn # 0 THEN
      RETURN rtn;
    ELSIF self.t_kind < tag.t_kind THEN
      RETURN -1;
    ELSIF self.t_kind > tag.t_kind THEN
      RETURN 1;
    ELSIF self.t_attr < tag.t_attr THEN
      RETURN -1;
    ELSIF self.t_attr > tag.t_attr THEN
      RETURN 1;
    ELSIF Version.Less(self.t_version, tag.t_version) THEN
      RETURN -1;
    ELSIF NOT Version.Equal(self.t_version, tag.t_version) THEN
      RETURN 1;
    ELSIF self.t_kind = Kind.Merge THEN
      IF Version.Less(self.t_mversion, tag.t_mversion) THEN
        RETURN -1;
      ELSIF NOT Version.Equal(self.t_mversion, tag.t_mversion) THEN
        RETURN 1;
      END;
    END;
    RETURN 0;
  END Compare;

(*---------------------------------------------------------------------------*)
PROCEDURE CompareFromText(tag1, tag2 : TEXT) : [-1..1] =
  VAR
    t1 := NEW(T).initFromText(tag1);
    t2 := NEW(T).initFromText(tag2);
  BEGIN
    RETURN t1.compare(t2);
  END CompareFromText;

(*---------------------------------------------------------------------------*)
PROCEDURE DebugInfo(self : T) : TEXT =

  PROCEDURE NonNil(t : TEXT) : TEXT =
    BEGIN
      IF t = NIL THEN RETURN "NIL" END;
      RETURN t;
    END NonNil;

  VAR res : TEXT;
  BEGIN
    IF self = NIL THEN
      RETURN "NIL";
    END;
    res := "kind: " & KindToText(self.t_kind);
    res := res & ", pkgname: " & NonNil(self.t_pkgname);
    res := res & ", change: " & NonNil(self.t_change);
    res := res & ", location: " & NonNil(self.t_location);
    res := res & "\nversion: " & self.t_version.toDir();
    IF self.t_version.defined() THEN
      res := res & " (defined)";
    ELSE
      res := res & " (undefined)";
    END;
    IF self.t_mversion # NIL THEN
      res := res & ", mversion: " & self.t_mversion.toDir();
      IF self.t_mversion.defined() THEN
        res := res & " (defined)";
      ELSE
        res := res & " (undefined)";
      END;
    END;
    IF Attribute.Branch IN self.t_attr THEN
      res := res & ", is branch";
    END;
    IF Attribute.Stable IN self.t_attr THEN
      res := res & ", is stable";
    END;
    IF Attribute.BranchStart IN self.t_attr THEN
      res := res & ", is start";
    END;
    res := res & "\norig: " & NonNil(self.t_orig);
    IF self.t_seq = NIL THEN
      res := res & ", seq: NIL";
    ELSE
      res := res & ", seq: ";
      FOR i := 0 TO self.t_seq.size() -1 DO
        res := res & self.t_seq.get(i) & "|";
      END;
    END;
    IF self.okay() THEN
      res := res & " OK";
    ELSE
      res := res & " NOT OK";
    END;
    RETURN res;
  END DebugInfo;

BEGIN (* Tag *)
  Head    := NEW(T).initFromText("head_any_0_0_0");
  Tip     := NEW(T).initFromText("tip_any_0_0_0");
  LastTip := NEW(T).initFromText("lasttip_any_0_0_0");
END Tag.
