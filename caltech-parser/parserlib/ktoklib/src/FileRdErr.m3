(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE FileRdErr;
IMPORT FileRd;
IMPORT TextList;
IMPORT Pathname;
IMPORT Stdio;
IMPORT Process;
IMPORT OSError;
IMPORT Rd, Wr;
IMPORT Fmt;
IMPORT Thread;
IMPORT FS;
FROM CharCodes IMPORT Q;
<* FATAL Thread.Alerted, Wr.Failure, Rd.Failure *>

TYPE
  NamedRd = FileRd.T BRANDED OBJECT
    p: Pathname.T;
  END;

PROCEDURE Error(message: TEXT; fatal := TRUE) =
  BEGIN
    Wr.PutText(Stdio.stderr, message & "\n");
    IF fatal THEN
      Process.Exit(1);
    END;
  END Error;

PROCEDURE Open(p: Pathname.T; searchDirs: TextList.T := NIL): T =
  VAR
    cur := searchDirs;
    full: TEXT;
  BEGIN
    IF cur = NIL THEN
      TRY
        RETURN NEW(NamedRd, p := p).init(FS.OpenFileReadonly(p));
      EXCEPT
      | OSError.E => Error("Cannot open " & Q(p)); RETURN NIL;
      END;
    ELSE
      REPEAT
        full := cur.head & p;
        TRY
          RETURN NEW(NamedRd, p := full).init(FS.OpenFileReadonly(full));
        EXCEPT
        | OSError.E =>
        END;
        cur := cur.tail;
      UNTIL cur = NIL;
      Error("Cannot find " & Q(p)); RETURN NIL;
    END;
  END Open;

PROCEDURE LineNo(rd: Rd.T): INTEGER =
  VAR
    pos := Rd.Index(rd);
    result: INTEGER := 0;
  BEGIN
    TRY
      Rd.Seek(rd, 0);
      WHILE Rd.Index(rd) <= pos DO
        EVAL Rd.GetLine(rd);
        INC(result);
      END;
      Rd.Seek(rd, pos);
    EXCEPT
    | Rd.EndOfFile =>
    END;
    RETURN result;
  END LineNo;

PROCEDURE E(rd: T; message: TEXT; fatal := TRUE) =
  VAR
    acc := "";
    pos: INTEGER;
  BEGIN
    IF NOT fatal THEN
      acc := "Warning: ";
    END;
    IF rd # NIL THEN
      IF ISTYPE(rd, NamedRd) THEN
        acc := acc & NARROW(rd, NamedRd).p & " ";
      ELSIF Rd.Seekable(rd) THEN
        TRY
          pos := Rd.Index(rd);
          Rd.Seek(rd, MAX(Rd.Index(rd)-256,0));
          REPEAT
            acc := Rd.GetLine(rd);
          UNTIL Rd.Index(rd) >= pos;
          acc := "source: " & acc & "\n";
        EXCEPT
          Rd.EndOfFile => acc := "??\n";
        END;
      END;
      IF Rd.Seekable(rd) THEN
        acc := acc & "line " & Fmt.Int(LineNo(rd)) & ": ";
      END;
    END;
    acc := acc & message;
    Error(acc, fatal);
  END E;

BEGIN
END FileRdErr.
