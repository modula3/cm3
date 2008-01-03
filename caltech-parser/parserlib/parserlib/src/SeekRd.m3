MODULE SeekRd;
IMPORT TextList;
IMPORT Pathname;
IMPORT Stdio;
IMPORT Process;
IMPORT OSError;
IMPORT Rd, Wr;
IMPORT Fmt;
IMPORT Thread;
IMPORT FS;
IMPORT FileRd;
IMPORT File;
IMPORT SeekRdClass;
<* FATAL Thread.Alerted, Wr.Failure, Rd.Failure, OSError.E *>

TYPE
  NamedRd = FileRd.T BRANDED OBJECT
    p: Pathname.T;
  END;

PROCEDURE Error(message: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, message & "\n");
    Process.Exit(1);
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
      | OSError.E => Error("Cannot open " & p); RETURN NIL;
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
      Error("Cannot find " & p); RETURN NIL;
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

PROCEDURE E(rd: T; message: TEXT) =
  VAR
    acc := "";
    lineNo: INTEGER;
  BEGIN
    IF rd # NIL THEN
      TYPECASE rd OF
      | NamedRd(nrd) =>
        acc := acc & nrd.p & " ";
        lineNo := LineNo(rd);
      | SeekRdClass.T(srd) =>
        lineNo := srd.lineNo();
      ELSE      
        IF Rd.Seekable(rd) THEN
          lineNo := LineNo(rd);
        ELSE
          lineNo := -1;
        END;
      END;
      IF lineNo # -1 THEN
        acc := acc & "line " & Fmt.Int(lineNo) & ": ";
      END;
    END;
    acc := acc & message;
    Error(acc);
  END E;

PROCEDURE Stdin(): T =
  VAR
    hIn, hOut, hErr: File.T;
  BEGIN
    Process.GetStandardFileHandles(stdin:=hIn, stdout:=hOut, stderr:=hErr);
    RETURN NEW(SeekRdClass.T).init(hIn);
  END Stdin;

PROCEDURE DiscardPrevious(rd: T) =
  BEGIN
    TYPECASE rd OF SeekRdClass.T(r) =>
      r.discardPrevious();
    ELSE
    END;
  END DiscardPrevious;

BEGIN
END SeekRd. 
