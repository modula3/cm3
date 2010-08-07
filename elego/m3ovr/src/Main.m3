(*---------------------------------------------------------------------------*)
MODULE Main;

IMPORT Text, TextSeq, ParseParams, Pathname, OSError, Stdio, Process;
IMPORT SMsg AS Msg, FSUtils, M3makefile, TextUtils, PathRepr, Creation;
IMPORT (* FSFixed AS *) FS;

(*---------------------------------------------------------------------------*)
CONST maxLevel = 5;

(*---------------------------------------------------------------------------*)
PROCEDURE EvalParams() =
  VAR pp := NEW(ParseParams.T).init(Stdio.stderr);
  BEGIN
    pdir := Pathname.Current;
    root := Pathname.Current;
    Msg.tFlag := TRUE;
    TRY
      Msg.vFlag := pp.keywordPresent("-v"); (* be verbose *)
      Msg.dFlag := pp.keywordPresent("-d"); (* debug messages *)

      IF pp.keywordPresent("-r") OR pp.keywordPresent("-root") THEN
        root := pp.getNext();
        pdir := root;
      END;
      (* creation date option *)
      IF pp.keywordPresent("-created") THEN
        Msg.T(Creation.Date & " on " & Creation.System);
        Process.Exit(0);
      END;
      npkgs := NUMBER(pp.arg^) - pp.next;
      pkglocs := NEW(TextSeq.T).init(npkgs);
      FOR i := 1 TO npkgs DO
	VAR t := pp.getNext(); BEGIN
	  IF Text.GetChar(t, 0) = '-' THEN
	    Msg.Fatal("unrecognized option: " & t, 2);
	  ELSE
	    pkglocs.addhi(t);
	  END;
	END;
      END;
      pp.finish();
    EXCEPT
      ParseParams.Error => 
      Msg.Fatal("unknown command line arguments; usage: " &
        "m3ovr [-v] [-d] [-r dir] pkg-location-1 .. pkg-location-n");
    END;
  END EvalParams;

(*---------------------------------------------------------------------------*)
PROCEDURE FindMakefileAndPkgName() =
  VAR
    rmkfname := Pathname.Join("src", "m3makefile", NIL);
    mkfname := Pathname.Join(root, rmkfname, NIL);
  BEGIN
    TRY
      pdir := FS.GetAbsolutePathname(pdir);
      root := FS.GetAbsolutePathname(root);
    EXCEPT
      OSError.E => Msg.Fatal("cannot get working directory");
    END;
    WHILE (level < maxLevel) AND NOT FSUtils.IsFile(mkfname) DO
      INC(level);
      root := Pathname.Prefix(root);
      pdir := Pathname.Prefix(pdir);
      mkfname := Pathname.Join(root, rmkfname, NIL);
    END;
    IF level = maxLevel THEN
      Msg.Fatal("cannot find m3makefile")
    END;
    pname := Pathname.Last(pdir);
    TRY
      fn := FS.GetAbsolutePathname(mkfname);
    EXCEPT
      OSError.E => Msg.Fatal("cannot get absolute pathname");
    END;
  END FindMakefileAndPkgName;

(*---------------------------------------------------------------------------*)
PROCEDURE ParseMakefile() =
  BEGIN
    TRY
      mkf := M3makefile.New(fn);
    EXCEPT
      OSError.E => Msg.Fatal("error reading m3makefile");
    | M3makefile.ParseError(e) => Msg.Fatal("error parsing m3makefile: " & e);
    END;
  END ParseMakefile;

(*---------------------------------------------------------------------------*)
PROCEDURE C(m : TEXT) =
  BEGIN
    Msg.V("% " & m);
  END C;

(*---------------------------------------------------------------------------*)
PROCEDURE M(m : TEXT) =
  BEGIN
    Msg.T( m);
  END M;

(*---------------------------------------------------------------------------*)
VAR
  mkf      : M3makefile.T;
  fn       : TEXT;
  root     : TEXT;
  level    := 0;
  pdir     : Pathname.T;
  pname    := "undefined";
  imps     : TextSeq.T;
  decls    : M3makefile.M3DeclarationList;
  pkgs     : TextSeq.T;
  pkglocs  : TextSeq.T;
  npkgs    : CARDINAL;
BEGIN (* Main *)
  EvalParams();

  FindMakefileAndPkgName();

  ParseMakefile();

  IF mkf.unknownElements() THEN
    Msg.Warning("m3makefile " & fn & 
      "\n     contains elements unknown to m3dep");
    C("m3makefile contains elements unknown to m3dep");
  END;

  IF Msg.vFlag THEN
    IF mkf.targetType() = M3makefile.TargetType.Library THEN
      IF mkf.targetName() = NIL THEN
        C("undefined main target");
      ELSE
        C("main target is library " & mkf.targetName());
      END;
    ELSIF mkf.targetType() = M3makefile.TargetType.Program THEN
      IF mkf.targetName() = NIL THEN
        C("undefined main target");
      ELSE
        C("main target is program " & mkf.targetName());
      END;
    ELSE
      C("unknown main target");
    END;

    decls := mkf.elements();
    WHILE decls # NIL DO
      IF decls.name = NIL THEN
        C("procedure/option " & decls.type);
      ELSE
        C(decls.type & " declaration " & decls.name & " (args = " &
          TextUtils.TextSeqToText(decls.args) & ")");
      END;
      decls := decls.next;
    END;
  END;

  imps := mkf.imports();
  pkgs := NEW(TextSeq.T).init(npkgs);
  FOR i := 0 TO npkgs - 1 DO
    WITH pkgloc = pkglocs.get(i) DO
      WITH pkg = Pathname.Last(pkgloc) DO
        pkgs.addhi(pkg);
        WITH loc = PathRepr.Posix(Pathname.Prefix(pkgloc)) DO
          IF TextUtils.MemberOfTextSeq(imps, pkg) THEN
            M("override(\"" & pkg & "\", \"" & loc & "\")");
          ELSE
            C("package " & pkg & " not used");
          END;
        END;
      END;
    END;
  END;
  IF mkf.targetType() = M3makefile.TargetType.Program THEN
    M("if not defined(\"PM3\")");
    M("  % The procedure \"build_standalone\" exists only in SRC Modula-3.");
    M("  if not defined(\"build_standalone\")");
    M("    PM3 = \"T\"");
    M("  end");
    M("end");
    M("if defined(\"PM3\")");
    M("  proc build_standalone() is");
    M("    option(\"standalone\", \"T\")");
    M("  end");
    M("end");
    M("if not defined(\"DYNAMIC\")");
    M("  build_standalone()");
    M("end");
    Msg.Warning("program " & mkf.targetName() & " will be linked statically");
  END;
  FOR i := 0 TO imps.size() - 1 DO
    WITH imp = imps.get(i) DO
      IF NOT TextUtils.MemberOfTextSeq(pkgs, imp) THEN
        C("location of imported package " & imp & " not overriden");
        Msg.Warning("location of imported package " & imp & " not overriden");
      END;
    END;
  END;
END Main.
