(*---------------------------------------------------------------------------*)
MODULE Main;

IMPORT Rd, FileRd, OSError, ParseParams, Process, Stdio, Text, TextSeq;
IMPORT PkgBase, PkgError, Msg;

(*---------------------------------------------------------------------------*)
PROCEDURE Usage() =
  CONST
    t =
      "\nusage:\n" &
      "  t_base (-h|-help)\n" &
      "  t_base [ -i <pkg kind file> ] ( <pkg pathname> )* \n" &
      "  t_base [ -i <pkg kind file> ] [ (-n|-new) <pkg kind> ] ( <pkg pathname> )*\n" &
      " \n" &
      "Check the kind of packages according to a given package kind\n" &
      "definition file or create empty new packages. \n" &
      " \n";
  BEGIN
    Msg.T(t);
  END Usage;

(*---------------------------------------------------------------------------*)
TYPE
  Action = {Check, List, Build};

(*---------------------------------------------------------------------------*)
PROCEDURE Init() =
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
	(* help option *)
	IF pp.keywordPresent("-h") OR pp.keywordPresent("-help") THEN
          Usage();
          Process.Exit(0);
        END;
        IF pp.keywordPresent("-i") THEN
          initFile := pp.getNext();
        END;
        IF pp.keywordPresent("-n") OR pp.keywordPresent("-new") THEN
          pkgKind := pp.getNext();
          action := Action.Build;
        END;
	pp.skipParsed();
	nTargets := NUMBER(pp.arg^) - pp.next;
	targets := NEW(TextSeq.T).init(nTargets);
	FOR i := 1 TO nTargets DO
	  VAR t := pp.getNext(); BEGIN
	    IF Text.GetChar(t, 0) = '-' THEN
	      Msg.Fatal("unrecognized option: " & t);
	    ELSE
	      targets.addhi(t);
	    END;
	  END;
	END;
	pp.finish();
      EXCEPT
	ParseParams.Error => Msg.Fatal("parameter error");
      END;
    END;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadKindFile() =
  VAR
    rd : FileRd.T;
  BEGIN
    TRY
      rd := FileRd.Open(initFile);
      IF NOT pkgBase.addDefs(rd) THEN
        Msg.Fatal("syntax error in package kind file");
      END;
    EXCEPT
      OSError.E => Msg.Fatal("cannot read file " & initFile);
    END;
    TRY Rd.Close(rd) EXCEPT ELSE END;
  END ReadKindFile;

(*---------------------------------------------------------------------------*)
PROCEDURE TestStructures() =
  BEGIN
    IF nTargets = 0 THEN
      Msg.Error("no packages specified");
    END;
    FOR i := 0 TO nTargets - 1 DO
      WITH pkg = targets.get(i) DO
        VAR kind : TEXT := NIL; BEGIN
          IF pkgBase.kindFound(pkg, kind) THEN
            Msg.T("  " & pkg & ": " & kind);
          ELSE
            Msg.T("  " & pkg & ": undefined kind");
          END;
        END;
      END;
    END;
  END TestStructures;

(*---------------------------------------------------------------------------*)
PROCEDURE BuildStructures() =
  BEGIN
    IF pkgKind = NIL THEN
      Msg.Fatal("kind of new package(s) must be specified");
    END;
    IF nTargets = 0 THEN
      Msg.Error("no packages specified");
    END;
    FOR i := 0 TO nTargets - 1 DO
      WITH pkg = targets.get(i) DO
        TRY
          pkgBase.createEmptyPkg(pkg, pkgKind);
          Msg.T("  empty package " & pkg & " created");
        EXCEPT
          PkgError.E(e) => Msg.Error("cannot create package: " & e);
        END;
      END;
    END;
  END BuildStructures;

(*---------------------------------------------------------------------------*)
VAR
  initFile :  TEXT := "../../src/PkgBase.DefaultData";
  pkgKind  :  TEXT := NIL;
  action   := Action.Check;
  nTargets :  INTEGER;
  targets  :  TextSeq.T;
  pkgBase  := NEW(PkgBase.T).init();
BEGIN
  Msg.tFlag := TRUE;
  Init();
  ReadKindFile();
  IF action = Action.Check THEN
    TestStructures()
  ELSIF action = Action.Build THEN
    BuildStructures();
  ELSE
    Msg.Fatal("command not implemented");
  END;
END Main.
