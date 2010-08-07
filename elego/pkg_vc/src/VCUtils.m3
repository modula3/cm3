(*--------------------------------------------------------------------------*)
MODULE VCUtils;

IMPORT Text, Process, Pathname, TextSeq, OSError;
IMPORT FileObj, Tag, TagSeq, PkgVC, FSUtils;
IMPORT RegEx, Glob;
IMPORT APN AS APN;

(*--------------------------------------------------------------------------*)
PROCEDURE DetectPackageRootDir(VAR packageDir : TEXT; VAR pkgName : TEXT) =
  (* post: packageDir, pkgName defined *)

  PROCEDURE FileExists(p : Pathname.T) : BOOLEAN =
    VAR
      pn := APN.New(p);
    BEGIN
      RETURN FileObj.Exists(pn) AND FileObj.IsFile(pn);
    END FileExists;

  VAR
    i := 4; 
    found := FALSE;
    dir, wd : Pathname.T;
    PkgDescFile, PkgDirFile, PkgKindFile, PkgTagsFile, PrjDescFile,
    m3makefile : Pathname.T;
  BEGIN
    TRY
      wd := Process.GetWorkingDirectory();
      dir := wd;
    EXCEPT 
      OSError.E => Process.Crash("cannot get working directory");
    END;

    WHILE i > 0 AND NOT found DO
      PkgDescFile := Pathname.Join(dir, "PkgDesc", NIL);
      PrjDescFile := Pathname.Join(dir, "PrjDesc", NIL);
      PkgKindFile := Pathname.Join(dir, "PkgKind", NIL);
      PkgTagsFile := Pathname.Join(dir, "PkgTags", NIL);
      PkgDirFile := Pathname.Join(dir, "PkgDirectory", NIL);
      m3makefile  := Pathname.Join(dir, 
                                   Pathname.Join("src", "m3makefile", NIL),
                                   NIL);
      IF FileExists(PkgDescFile) THEN
        found := TRUE;
      ELSIF FileExists(PrjDescFile) THEN
        found := TRUE;
      ELSIF FileExists(PkgKindFile) THEN
        found := TRUE;
      ELSIF FileExists(PkgDirFile) THEN
        found := TRUE;
      ELSIF FileExists(PkgTagsFile) THEN
        found := TRUE;
      ELSIF FileExists(m3makefile) THEN
        found := TRUE;
      ELSE
        DEC(i);
        dir := Pathname.Prefix(dir);
      END;
    END;

    IF found THEN
      packageDir := dir;
      pkgName := Pathname.Last(packageDir);
    ELSE
      packageDir := wd;
      pkgName := "LonelyPackage";
    END;
  END DetectPackageRootDir;

(*--------------------------------------------------------------------------*)
PROCEDURE ChangeToPackageRoot(packageDir : TEXT) RAISES {PkgVC.E} =
  (* post: current working directory is root of the package *)
  BEGIN
    TRY
      Process.SetWorkingDirectory(packageDir);
    EXCEPT
      OSError.E => RAISE PkgVC.E("cannot chdir to " & packageDir);
    END;
  END ChangeToPackageRoot;

(*--------------------------------------------------------------------------*)
PROCEDURE CommitTypeFromText(t : TEXT) : PkgVC.CommitType RAISES {PkgVC.E} =
  VAR res : PkgVC.CommitType;
  BEGIN
    IF Text.Equal(t, "major") OR Text.Equal(t, "maj") THEN
      res := PkgVC.CommitType.Major;
    ELSIF Text.Equal(t, "minor") OR Text.Equal(t, "min") THEN
      res := PkgVC.CommitType.Minor;
    ELSIF Text.Equal(t, "patch") OR Text.Equal(t, "pat") OR
          Text.Equal(t, "p") THEN
      res := PkgVC.CommitType.Patch;
    ELSE
      RAISE PkgVC.E("unknown commit type: " & t);
    END;
    RETURN res;
  END CommitTypeFromText;

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutDir(pkgVC : PkgVC.T; name : TEXT; tag : Tag.T)
  RAISES {PkgVC.E} =
  BEGIN
    IF FSUtils.Exists(name) THEN
      IF NOT FSUtils.IsDir(name) THEN
        RAISE PkgVC.E(name & " exists, but is no directory");
      ELSE
        TRY
          pkgVC.setPackageRoot(APN.New(FSUtils.CanonicalPathname(name)));
        EXCEPT 
          FSUtils.E(t) => RAISE PkgVC.E(t);
        | PkgVC.E(t)   => RAISE PkgVC.E(t);
        END;
        pkgVC.update(tag);
      END;
    ELSE
      pkgVC.checkout(name, tag);
    END;
  END CheckoutDir;

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutPackage(pkgVC : PkgVC.T; name : TEXT; tag : Tag.T) 
  RAISES {PkgVC.E} =
  BEGIN
    CheckoutDir(pkgVC, name, tag);
  END CheckoutPackage;

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutCollection(pkgVC : PkgVC.T; name : TEXT; tag : Tag.T)
  RAISES {PkgVC.E} =
  BEGIN
    CheckoutDir(pkgVC, name, tag);
  END CheckoutCollection; 

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutDirect(pkgVC : PkgVC.T; prjRoot, collection, rev : TEXT;
                         packages : TextSeq.T) RAISES {PkgVC.E} =
  VAR
    okay := TRUE;
    errs := "";
    tag  :  Tag.T;
  BEGIN
    tag := Tag.New(rev);
    VAR prefix, name : TEXT; 
    BEGIN
      IF collection # NIL THEN
        IF packages.size() > 1 THEN
          prefix := collection;
        ELSE
          (* no package, checkout complete colletcion *)
          CheckoutCollection(pkgVC,
                             APN.New(collection).denotation(APN.Type.Posix),
                             tag);
        END;
      ELSIF prjRoot # NIL THEN
        prefix := prjRoot;
      ELSE
	prefix := NIL;
      END;
      (* checkout all packages given on the command line *)
      FOR i := 0 TO packages.size() - 1 DO
	name := packages.get(i);
	IF prefix # NIL THEN
	  name := Pathname.Join(prefix, name, NIL);
	END;
        TRY
          CheckoutPackage(pkgVC, 
                          APN.New(name).denotation(APN.Type.Posix), tag);
        EXCEPT
          PkgVC.E(t) => okay := FALSE; errs := errs & t & "\n";
        END;
      END;
    END;
    IF NOT okay THEN
      RAISE PkgVC.E(errs);
    END;
  END CheckoutDirect;

(*--------------------------------------------------------------------------*)
PROCEDURE FindTagnameInList(tagname : TEXT; tags : TagSeq.T;) : Tag.T =
  VAR
    ct   :  Tag.T;
    ctt  :  TEXT;
  BEGIN
    IF tagname = NIL THEN RETURN NIL END;
    FOR i := 0 TO tags.size() - 1 DO
      ct := tags.get(i);
      IF NOT ct.predefined() THEN
        ctt := ct.originalText();
        IF Text.Empty(ctt) AND ct.okay() THEN
          ctt := ct.denotation();
        END;
        IF Text.Equal(tagname, ctt) THEN
          RETURN ct;
        END;
      END;
    END;
    RETURN NIL;
  END FindTagnameInList;

(*--------------------------------------------------------------------------*)
PROCEDURE FindTagInList(tag : Tag.T; tags : TagSeq.T;) : Tag.T =
  VAR
    tagname : TEXT;
  BEGIN
    IF tag = NIL THEN RETURN NIL END;
    tagname := tag.originalText();
    IF Text.Empty(tagname) AND tag.okay() THEN
      tagname := tag.denotation();
    END;
    RETURN FindTagnameInList(tagname, tags);
  END FindTagInList;

(*--------------------------------------------------------------------------*)
PROCEDURE TagExists(pkgVC : PkgVC.T; t : Tag.T) : BOOLEAN RAISES {PkgVC.E} =
  BEGIN
    IF t = NIL THEN RETURN FALSE END;
    IF t.predefined() THEN
      RETURN TRUE;
    END;
    RETURN FindTagInList(t, pkgVC.tags(prefix := "")) # NIL;
  END TagExists;

(*--------------------------------------------------------------------------*)
PROCEDURE NewTagFromPattern(pkgVC : PkgVC.T; pat: TEXT) : Tag.T
  RAISES {PkgVC.E} =
  VAR
    tags :  TagSeq.T;
    ct   :  Tag.T;
    res  :  Tag.T := NIL;
    ctt  :  TEXT;
    pat2 :  TEXT;
  BEGIN
    IF pat = NIL THEN RETURN NIL END;
    IF Text.Equal(pat, "head") THEN
      RETURN Tag.Head;
    END;
    (*
    IF Text.FindChar(pat, '*' ) < 0 THEN
      RETURN Tag.New(pat);
    END;
    *)
    tags := pkgVC.tags(prefix := "");
    (* look for an exact match *)
    ct := FindTagnameInList(pat, tags);
    IF ct # NIL THEN
      RETURN ct;
    END;
    (* look for a pattern match *)
    FOR i := 0 TO tags.size() - 1 DO
      ct := tags.get(i);
      ctt := ct.originalText();
      IF Text.Empty(ctt) AND ct.okay() THEN
        ctt := ct.denotation();
      END;
      TRY
        IF Glob.Match(pat, ctt) THEN
          IF res # NIL THEN
            RAISE PkgVC.E("pattern match result is not unique: " & 
                  res.denotation() & ", " & ct.denotation());
          END;
          res := ct;
        END;
      EXCEPT
        RegEx.Error(e) => RAISE PkgVC.E("pattern syntax error: " & e);
      END;
    END;
    IF res = NIL THEN
      pat2 := "*" & pat & "*";
      (* look for a weaker pattern match *)
      FOR i := 0 TO tags.size() - 1 DO
        ct := tags.get(i);
        IF ct.okay() THEN
          ctt := ct.denotation();
        ELSE
          ctt := ct.originalText();
        END;
        TRY
          IF Glob.Match(pat2, ctt) THEN
            IF res # NIL THEN
              RAISE PkgVC.E("pattern match result is not unique: " & 
                    res.denotation() & ", " & ct.denotation());
            END;
            res := ct;
          END;
        EXCEPT
          RegEx.Error(e) => RAISE PkgVC.E("pattern syntax error: " & e);
        END;
      END;
    END;
    IF res = NIL THEN
      RAISE PkgVC.E("tag does not exist: " & pat);
    END;
    RETURN res;
  END NewTagFromPattern; 

BEGIN
END VCUtils.
