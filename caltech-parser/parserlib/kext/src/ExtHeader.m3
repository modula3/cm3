(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ExtHeader.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE ExtHeader;
IMPORT TextList;
IMPORT TextReader;
IMPORT FileRdErr;
IMPORT Text, Rd;
IMPORT Thread;
<* FATAL Rd.EndOfFile, Rd.Failure, Thread.Alerted *>
PROCEDURE ShatterLine(rd: Rd.T; keyword: TEXT): TextList.T =
  VAR
    line: TEXT;
    all: TextList.T;
  BEGIN
    REPEAT
      line := Rd.GetLine(rd);
    UNTIL Text.Length(line) # 0;
    all := NEW(TextReader.T).init(line).shatter("\t ","",TRUE);
    IF NOT Text.Equal(all.head, keyword) THEN
      FileRdErr.E(rd, "expected " & keyword);
      RETURN NIL;
    ELSE
      RETURN all.tail;
    END;
  END ShatterLine;

PROCEDURE Parse(from: Rd.T): T =
  BEGIN
    RETURN T{sources := ShatterLine(from, "%source"),
             imports := ShatterLine(from, "%import")};
  END Parse;
BEGIN
END ExtHeader.
