(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Main.m3,v 1.2 2001-09-19 15:14:22 wagner Exp $ *)

MODULE Main;
IMPORT Pathname;
IMPORT ParseParams;
IMPORT Text;
IMPORT TextSubs;
IMPORT TextList;
IMPORT LoadSpec;
IMPORT FileWr;
IMPORT FileRdErr;
IMPORT ExtHeader;
IMPORT ExtBody;
IMPORT ExtSection;
FROM Stdio IMPORT stderr;
IMPORT OSError;
IMPORT Wr;
IMPORT Thread;
IMPORT Process;
<* FATAL Wr.Failure, Thread.Alerted, OSError.E *>
VAR
  searchDirs := TextList.List2("", "../src/");
  spec := NEW(LoadSpec.T).init();

PROCEDURE LoadSpecFile(fileName, mn: TEXT) =
  VAR
    rd := FileRdErr.Open(fileName, searchDirs);
  BEGIN
    spec.readSpec(rd, mn, Text.GetChar(Pathname.LastExt(fileName),0));
  END LoadSpecFile;

PROCEDURE GetParam(): TEXT =
  VAR
    pp := NEW(ParseParams.T).init(stderr);
    result: TEXT;
  BEGIN
    TRY
      result := pp.getNext();
      pp.finish();
    EXCEPT
      ParseParams.Error =>
      Wr.PutText(stderr, "ext file");
      Process.Exit(1);
    END;
    RETURN result;
  END GetParam;

VAR
  inFileName := GetParam();
  input := FileRdErr.Open(inFileName, searchDirs);
  header := ExtHeader.Parse(input);

PROCEDURE InputHeader() =
  VAR
    curS := header.sources;
    curI := header.imports;
  BEGIN
    IF TextList.Length(curS) # TextList.Length(curI) THEN
      FileRdErr.E(input, "num(%source) # num(%input)");
    END;
    WHILE curS # NIL DO
      LoadSpecFile(curS.head, curI.head);
      curS := curS.tail;
      curI := curI.tail;
    END;
  END InputHeader;

VAR
  outBase := Pathname.LastBase(inFileName);
  specInfo: LoadSpec.Info;
  subs: TextSubs.T;
PROCEDURE WriteFile(bodyKind: ExtSection.T; ext: TEXT) =
  VAR
    bodyText := ExtSection.GetText(specInfo.kind, bodyKind);
    wr := FileWr.Open(outBase & "." & ext);
  BEGIN
    Wr.PutText(wr, subs.apply(bodyText));
    Wr.Close(wr);
  END WriteFile;
BEGIN
  InputHeader();
  spec.setTarget(outBase);
  specInfo := spec.get();
  subs := ExtBody.Parse(input, specInfo);
  WriteFile(ExtSection.T.Interface, "i3");
  WriteFile(ExtSection.T.Module, "m3");
END Main.
