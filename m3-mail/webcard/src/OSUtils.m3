(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* OSUtils.mod                                                 *)
(* Last modified on Wed Apr 27 14:58:54 PDT 1994 by birrell    *)
(*      modified on Thu Mar  3 10:19:10 PST 1994 by mcjones    *)
(*      modified on Thu Jun 11 12:05:19 PDT 1992 by meehan     *)

(* Much of the implementation was taken from Ted Wobber's FileSys.m3 *)

UNSAFE MODULE OSUtils EXPORTS OSUtils, UtimeExtra;

IMPORT Atom, FileRd, FileWr, Fmt, OSError, OSErrorPosix, Pipe, Process, Rd,
       TextList, Text, Thread, Time, Word, Wr;
IMPORT M3toC, Uerror, Unix, Ustat, Utime;

FROM Ctypes IMPORT char_star, int, long;

(* *)
(* Internal subroutines *)
(* *)

TYPE
  EC = CARDINAL;

TYPE
  ErrorClass = {LookupError, ProtectionError, OtherError};

PROCEDURE ClassifyError (ec: EC): ErrorClass =
  BEGIN
    CASE ec OF
    | Uerror.ENOENT, Uerror.ENOTDIR => RETURN ErrorClass.LookupError;
    | Uerror.EACCES, Uerror.EEXIST => RETURN ErrorClass.ProtectionError;
    ELSE RETURN ErrorClass.OtherError;
    END;
  END ClassifyError;

PROCEDURE ErrorMessage (ec: EC): Text.T =
  VAR
    p: char_star;
  BEGIN
    IF ec <= Uerror.Max THEN
      p := Uerror.GetFrom_sys_errlist(ec);
      RETURN M3toC.StoT(p);  (* assumes sys err list is static *)
    ELSE
      RETURN "Error code " & Fmt.Int(ec);
    END;
  END ErrorMessage;

PROCEDURE ConvertOSError(a: Atom.T): TEXT =
  BEGIN
    FOR i := 0 TO Uerror.Max DO
      IF a = OSErrorPosix.ErrnoAtom(i) THEN RETURN ErrorMessage(i) END;
    END;
    RETURN Atom.ToText(a)
  END ConvertOSError;

PROCEDURE ConvertPath(t: Text.T) : char_star =
  BEGIN
    IF t = NIL THEN
      RETURN M3toC.TtoS("");
    ELSE
      RETURN M3toC.TtoS(t);
    END;
  END ConvertPath;

(* *)
(* File system operations *)
(* *)

PROCEDURE GetInfo(path: TEXT; VAR (*OUT*) mtime: Time.T): FileType
                  RAISES { FileNotFound, FileError } =
  VAR
    p := ConvertPath(path);
    statBuf: Ustat.struct_stat;
    status: int;
    micro: INTEGER;
  BEGIN
    status := Ustat.stat(p, ADR(statBuf));
    IF status = -1 THEN
      IF ClassifyError(Uerror.errno) = ErrorClass.LookupError THEN
        RAISE FileNotFound
      ELSE
        RAISE FileError(ErrorMessage(Uerror.errno));
      END
    END;
    micro := statBuf.st_spare2;
    IF micro > 999999 THEN micro := micro MOD 1000000 END;
    mtime := FLOAT(statBuf.st_mtime, LONGREAL) + FLOAT(micro, LONGREAL) / 1.0d6;
    CASE Word.And(statBuf.st_mode, Ustat.S_IFMT) OF
      | Ustat.S_IFDIR => RETURN FileType.Dir;
      | Ustat.S_IFREG => RETURN FileType.Normal;
      | Ustat.S_IFLNK => RETURN FileType.SLink;
    ELSE
      RETURN FileType.Other;
    END;
  END GetInfo;

PROCEDURE CheckFile(path: TEXT) RAISES { FileNotFound, FileError } =
    VAR mtime: Time.T;
  BEGIN
    EVAL GetInfo(path, mtime)
  END CheckFile;

PROCEDURE OpenRead(path: TEXT): Rd.T RAISES { FileNotFound, FileError } =
  BEGIN
    CheckFile(path);
    TRY
      RETURN FileRd.Open(path)
    EXCEPT OSError.E(ec) =>
      RAISE FileError(ConvertOSError(ec.head))
    END;
  END OpenRead;

PROCEDURE OpenWrite(path: TEXT; append: BOOLEAN): Wr.T RAISES { FileError } =
  BEGIN
    TRY
      IF append THEN
        RETURN FileWr.OpenAppend(path)
      ELSE
        RETURN FileWr.Open(path)
      END;
    EXCEPT OSError.E(ec) =>
      RAISE FileError(ConvertOSError(ec.head))
    END;
  END OpenWrite;

PROCEDURE Delete(path: TEXT) RAISES { FileError } =
  VAR
    p: char_star := ConvertPath(path);
    status: int;
  BEGIN
    status := Unix.unlink(p);
    IF status = -1 THEN
      RAISE FileError(ErrorMessage(Uerror.errno));
    END;
  END Delete;
  
PROCEDURE Rename(srce, dest: TEXT) RAISES { FileError } =
  VAR
    pSrce: char_star := ConvertPath(srce);
    pDest: char_star := ConvertPath(dest);
    status: int;
  BEGIN
    status := Unix.rename(pSrce, pDest);
    IF status = -1 THEN
      RAISE FileError(ErrorMessage(Uerror.errno));
    END;
  END Rename;

PROCEDURE MakeDir(path: TEXT) RAISES { FileError } =
  VAR
    status: int;
    p := ConvertPath(path);
  BEGIN
    status := Unix.mkdir(p, 8_0777); (* masked by process's mode mask *)
    IF status = -1 THEN
      RAISE FileError(ErrorMessage(Uerror.errno));
    END;
  END MakeDir;

PROCEDURE Enumerate(path: TEXT): TextList.T RAISES { FileError } =
  VAR
    head, tail, this: TextList.T := NIL;
    filter: Filter;
  BEGIN
    filter := RunProcess(ARRAY OF TEXT { "/bin/ls", path });
    TRY
      TRY
        WHILE NOT Rd.EOF(filter.stdout) DO
          WITH t = Rd.GetLine(filter.stdout) DO
            IF NOT Text.Equal(t, ".") AND NOT Text.Equal(t, "..") THEN
              this := TextList.Cons(t, NIL);
              IF head = NIL THEN head := this ELSE tail.tail := this END;
              tail := this;
            END;
          END;
        END;
      EXCEPT Rd.Failure, Rd.EndOfFile, Thread.Alerted =>
        RAISE FileError("Enumeration failed (1)");
      END;
    FINALLY
      TRY
        Rd.Close(filter.stdout);
        Wr.Close(filter.stdin);
      EXCEPT
        Rd.Failure, Wr.Failure, Thread.Alerted =>
      END;
      IF WaitProcess(filter.handle) # 0 THEN
        RAISE FileError("Enumeration failed (2)");
      END;
    END;
    RETURN head
  END Enumerate;


(* *)
(* Time conversions *)
(* *)

VAR timeLock := NEW(MUTEX);

PROCEDURE TimeLocalToText(time: Time.T): TEXT =
  VAR
    secs: long;
    c: ARRAY [0..25] OF CHAR;
  BEGIN
    secs := ROUND(time);
    LOCK timeLock DO
      c := LOOPHOLE(Utime.ctime(ADR(secs)),
                    UNTRACED REF ARRAY [0..25] OF CHAR)^
    END;
    RETURN Text.FromChars(SUBARRAY(c, 0, 24))
  END TimeLocalToText;

PROCEDURE FromTimeLocal(time: Time.T; VAR (*OUT*) t: CalendarTime) =
  VAR
    tm: struct_tm;
    secs: long;
  BEGIN
    secs := ROUND(time);
    LOCK timeLock DO
      tm := localtime(ADR(secs))^
    END;
    t := CalendarTime{
      second := tm.tm_sec,
      minute := tm.tm_min,
      hour := tm.tm_hour,
      day := tm.tm_mday,
      month := tm.tm_mon,
      year := tm.tm_year,
      dayOfWeek := tm.tm_wday,
      dayOfYear := tm.tm_yday,
      daylightTime := (tm.tm_isdst # 0),
      gmtOffset := tm.tm_gmtoff
      };
  END FromTimeLocal;

PROCEDURE ToTime (READONLY t: CalendarTime): Time.T =
  VAR
    tm := struct_tm{
      tm_sec := t.second,
      tm_min := t.minute,
      tm_hour := t.hour,
      tm_mday := t.day,
      tm_mon := t.month,
      tm_year := t.year,
      tm_wday := t.dayOfWeek,
      tm_yday := t.dayOfYear,
      tm_isdst := ORD(t.daylightTime),
      tm_gmtoff := t.gmtOffset,
      tm_zone := NIL
      };
  BEGIN
    RETURN FLOAT(mktime(tm), LONGREAL)
  END ToTime;


(* *)
(* Sub-processes *)
(* *)

REVEAL SubProcess = BRANDED REF Process.T;

PROCEDURE RunProcess(READONLY argv: ARRAY OF TEXT): Filter RAISES {FileError} =
  VAR
    hrChild, hwChild, hrSelf, hwSelf: Pipe.T;
    res: Filter;
  BEGIN
    TRY
      Pipe.Open(hr := hrChild, hw := hwSelf);
      res.stdin := NEW(FileWr.T).init(hwSelf);
      Pipe.Open(hr := hrSelf, hw := hwChild);
      res.stdout := NEW(FileRd.T).init(hrSelf);
      res.handle := NEW(SubProcess);
      res.handle^ := Process.Create(argv[0],
                                    SUBARRAY(argv, 1, NUMBER(argv)-1),
                                    NIL, NIL,
                                    hrChild, hwChild, hwChild);
      hrChild.close();
      hwChild.close();
      RETURN res
    EXCEPT OSError.E(c) =>
      RAISE FileError(ConvertOSError(c.head));
    END;
  END RunProcess;

PROCEDURE WaitProcess(child: SubProcess): INTEGER =
  BEGIN
    RETURN Process.Wait(child^)
  END WaitProcess;

BEGIN
END OSUtils.
