(*---------------------------------------------------------------------------*)
MODULE Main;

IMPORT Text, TextSeq, Fmt;
IMPORT Msg, FilePool, FSUtils, TextUtils;

(*---------------------------------------------------------------------------*)
CONST
  l = "-------------------------------------------------------------------" &
      "--------";

(*---------------------------------------------------------------------------*)
PROCEDURE Update() =
  BEGIN
    Msg.T(l);
    Msg.T("updating cache...");
    fp.update();
  END Update;

(*---------------------------------------------------------------------------*)
PROCEDURE List(p : TEXT := NIL) =
  BEGIN
    Msg.T(l);
    IF p = NIL THEN
      Msg.T("listing of all files: ");
    ELSE
      Msg.T("listing of files matching pattern " & p);
    END;
    Msg.T(TextUtils.TextSeqToText(fp.list(p, ordinaryOnly := FALSE)));
  END List;

(*---------------------------------------------------------------------------*)
PROCEDURE ListFiles(p : TEXT := NIL) =
  VAR list := fp.list(p, ordinaryOnly := TRUE);
  BEGIN
    FOR i := 0 TO list.size() - 1 DO
      WITH fn = list.get(i) DO
        Msg.T(l);
        Msg.T(fn & ":");
        Msg.T(fp.content(fn));
      END;
    END;
  END ListFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE CreateFile(fn : TEXT := NIL) =
  VAR name : TEXT;
  BEGIN
    Msg.T(l);
    IF fn = NIL THEN
      Msg.T("creating file...");
    ELSE
      Msg.T("creating file " & fn);
    END;
    name := fp.createNewFile(fn);
    fns.addhi(name);
    Msg.T("new file " & name & " created");
  END CreateFile;

(*---------------------------------------------------------------------------*)
PROCEDURE CreateSome() =
  BEGIN
    Msg.T(l);
    Msg.T("creating some files...");
    FOR i := 1 TO 20 DO
      CreateFile();
    END;
  END CreateSome;

(*---------------------------------------------------------------------------*)
PROCEDURE DeleteFile(fn : TEXT) =
  BEGIN
    Msg.T(l);
    Msg.T("deleting file " & fn);
    fp.delete(fn);
    Msg.T("file " & fn & " deleted");
  END DeleteFile;

(*---------------------------------------------------------------------------*)
PROCEDURE DeleteAllNew() =
  BEGIN
    Msg.T(l);
    Msg.T("deleting all new files...");
    FOR i := 0 TO fns.size() - 1 DO
      DeleteFile(fns.get(i));
    END;
  END DeleteAllNew;

(*---------------------------------------------------------------------------*)
PROCEDURE CreateDeletePerformance() =
  VAR 
    n := 1000;
    m := NEW(TextSeq.T).init(n);
  BEGIN
    Msg.T(l);
    Msg.T("creating " & Fmt.Int(n) & " files...");
    FOR i := 1 TO n DO
      m.addhi(fp.createNewFile());
    END;
    List("^[A-Z]*$");
    List("^[a-z]*$");
    List();
    Msg.T(l);
    Msg.T("deleting " & Fmt.Int(n) & " files...");
    FOR i := 0 TO m.size() - 1 DO
      fp.delete(m.get(i));
    END;
    Msg.T("done");
  END CreateDeletePerformance;

(*---------------------------------------------------------------------------*)
TYPE
  SelectE = FilePool.PredClosure OBJECT
  OVERRIDES
    pred := SelectFilesWithE;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE SelectFilesWithE(self : SelectE; fn : TEXT) : BOOLEAN
  RAISES {FilePool.Error} =
  BEGIN
    RETURN Text.FindChar(fn, 'e', 0) > -1;
  END SelectFilesWithE;

(*---------------------------------------------------------------------------*)
PROCEDURE SelectEs() =
  VAR list : TextSeq.T;
  BEGIN
    Msg.T(l);
    Msg.T("selecting all file names containing an `e'...");
    list := fp.select(NEW(SelectE), ordinaryOnly := FALSE);
    Msg.T(TextUtils.TextSeqToText(list));
  END SelectEs;

(*---------------------------------------------------------------------------*)
PROCEDURE SuiteA() =
  BEGIN
    Msg.T("directory " & dir & " exists");
    fp := NEW(FilePool.T).init(dir);
    List();
    List("[A-Z]");
    List("^[a-z]*$");
    SelectEs();
    ListFiles();
  END SuiteA;

(*---------------------------------------------------------------------------*)
PROCEDURE SuiteB() =
  BEGIN
    Msg.T("creating directory " & dir);
    FSUtils.MakeDir(dir);
    fp := NEW(FilePool.T).init(dir);
    List();
    CreateSome();
    List();
    Update();
    List();
    List("[A-Z]");
    List("^[a-z]*$");
    SelectEs();
    CreateFile("susi");
    CreateFile("hugo");
    CreateFile("otto");
    List();
    DeleteAllNew();
    List();
    CreateDeletePerformance();
  END SuiteB;

(*---------------------------------------------------------------------------*)
VAR (* Main *)
  fp  :  FilePool.T;
  dir := "dummy";
  fns := NEW(TextSeq.T).init();
BEGIN
  Msg.tFlag := TRUE;
  IF FSUtils.IsDir(dir) THEN
    SuiteA();
  ELSE
    SuiteB();
  END;
END Main.
