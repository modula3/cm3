(*---------------------------------------------------------------------------*)
MODULE Main;

IMPORT Text, TextSeq, Params, Pathname, OSError, Process;
IMPORT SMsg AS Msg, FSUtils, M3makefile, Creation;
IMPORT (* FSFixed AS *) FS;
FROM TextUtils IMPORT TextSeqToText;

(*---------------------------------------------------------------------------*)
CONST maxLevel = 5;

(*---------------------------------------------------------------------------*)
PROCEDURE EvalParams() =
  BEGIN
    pdir := Pathname.Current;
    root := Pathname.Current;
    Msg.tFlag := TRUE;
    Msg.vFlag := FALSE;
    FOR i := 1 TO Params.Count - 1 DO
      par1 := Params.Get(i);
      IF Text.Equal(par1, "-created") THEN
        Msg.T(Creation.Date & " on " & Creation.System);
        Process.Exit(0);
      ELSIF Text.Equal(par1, "-v") THEN
	Msg.vFlag := TRUE;
      ELSIF Text.Equal(par1, "-d") THEN
	Msg.dFlag := TRUE;
      ELSE
	root := par1;
        pdir := root;
        IF i >= Params.Count THEN
          Msg.Error("unknown command line arguments; usage: " &
            "m3dep [-v] [-d] [dir]");
        END;
        EXIT;
      END;
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
    Msg.V("# " & m);
  END C;

(*---------------------------------------------------------------------------*)
PROCEDURE M(m : TEXT) =
  BEGIN
    Msg.T( m);
  END M;

(*---------------------------------------------------------------------------*)
VAR
  mkf   : M3makefile.T;
  fn    : TEXT;
  root  : TEXT;
  level := 0;
  pdir  : Pathname.T;
  pname := "undefined";
  imps  : TextSeq.T;
  par1  : TEXT;
  decls : M3makefile.M3DeclarationList;
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
      C("main target is library " & mkf.targetName());
    ELSIF mkf.targetType() = M3makefile.TargetType.Program THEN
      C("main target is program " & mkf.targetName());
    ELSE
      C("unknown main target");
    END;

    decls := mkf.elements();
    WHILE decls # NIL DO
      IF decls.name = NIL THEN
        C("procedure/option " & decls.type);
      ELSE
        C(decls.type & " declaration " & decls.name & " (args = " &
          TextSeqToText(decls.args) & ")");
      END;
      decls := decls.next;
    END;
  END;

  imps := mkf.imports();
  M(pname & ": " & TextSeqToText(imps));
END Main.
