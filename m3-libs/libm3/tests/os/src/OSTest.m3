(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Mar 30 13:54:20 PST 1995 by mcjones *)

UNSAFE (* LOOPHOLE *) MODULE OSTest EXPORTS Main;

IMPORT Atom, Env, File, Fmt, FmtTime, FS, Pathname, PathnameTests,
  Pipe, OSError, OSErrorPosix, Rd, RefList, RegularFile, Subr, Stdio,
  Sx, Process, Text, TextSubrTbl, Thread, Wr;

VAR
  QFile := Atom.FromText("file");
  QOk := Atom.FromText("ok");
  QQuote := Atom.FromText("quote");

VAR subrs := NEW(TextSubrTbl.Default).init();

PROCEDURE Register(name: TEXT; subr: Subr.T) =
  BEGIN
    WITH present = subrs.put(name, subr) DO <* ASSERT NOT present *> END
  END Register;  

VAR
  r := Stdio.stdin;
  w := Stdio.stderr;
  stdin, stdout, stderr: File.T;

<* FATAL Rd.Failure, Sx.PrintError, Wr.Failure, Thread.Alerted *>

PROCEDURE Eval(expr: Sx.T)
  : Sx.T RAISES {Subr.Usage, OSError.E, Pathname.Invalid} =
  BEGIN
    TYPECASE expr OF
    | Atom.T, TEXT, REF INTEGER, REF REAL, REF LONGREAL, REF EXTENDED =>
        RETURN expr
    | RefList.T(list) =>
      TYPECASE list.head OF
      | NULL => RAISE Subr.Usage("Unexpected NIL as function")
      | Atom.T(atom) =>
          IF atom = QQuote THEN RETURN list.tail.head END;
          RETURN Apply(Atom.ToText(atom), EvList(list.tail))
      ELSE RAISE Subr.Usage("Unexpected expression type as function")
      END
    ELSE RAISE Subr.Usage("Unknown expression type to eval")
    END
  END Eval;

PROCEDURE Apply(name: TEXT; args: RefList.T)
  : Sx.T RAISES {Subr.Usage, OSError.E, Pathname.Invalid} =
  VAR subr: Subr.T;
  BEGIN
    IF subrs.get(name, subr) THEN RETURN subr(args) END;
    RAISE Subr.Usage("Unknown procedure: " & name)
  END Apply;

PROCEDURE EvList(list: RefList.T)
  : RefList.T RAISES {Subr.Usage, OSError.E, Pathname.Invalid} =
  VAR l: RefList.T := NIL;
  BEGIN
    WHILE list # NIL DO
      l := RefList.Cons(Eval(list.head), l);
      list := list.tail
    END;
    RETURN RefList.ReverseD(l)
  END EvList;

PROCEDURE Print(w: Wr.T; value: Sx.T) =
  BEGIN
    TYPECASE value OF
    | NULL => Wr.PutText(w, "NIL")
    | Atom.T(atom) => IF atom = QOk THEN (*SKIP*) ELSE Sx.Print(w, value) END
    | File.T(file) => Sx.Print(w, NewFile(file))
    | Pathname.Arcs(arcs) =>
        Wr.PutText(w, "[");
        WITH n = arcs.size() DO
          FOR i := 0 TO n - 1 DO
            IF arcs.get(i) = NIL THEN Wr.PutText(w, "NIL")
            ELSE Wr.PutText(w, "\"" & arcs.get(i) & "\"")
            END;
            IF i # n - 1 THEN Wr.PutText(w, ", ") END
          END
        END;
        Wr.PutText(w, "]")
    ELSE (*RefList.T, ... *) Sx.Print(w, value)
    END
  END Print;

PROCEDURE GetValue(VAR args: RefList.T; default: Sx.T := NIL)
  : Sx.T RAISES {Subr.Usage} =
  VAR v: Sx.T;
  BEGIN
    IF args = NIL THEN
      IF default # NIL THEN RETURN default ELSE RAISE Subr.Usage("") END
    END;
    v := args.head;
    args := args.tail;
    RETURN v
  END GetValue;

PROCEDURE GetBool(
    VAR args: RefList.T; defaultOk: BOOLEAN := FALSE; default := FALSE)
  : BOOLEAN RAISES {Subr.Usage} =
  VAR b: BOOLEAN;
  BEGIN
    IF args = NIL THEN
      IF defaultOk THEN RETURN default
      ELSE RAISE Subr.Usage("expected atom TRUE|FALSE")
      END
    END;
    TYPECASE args.head OF
    | NULL => RAISE Subr.Usage("")
    | Atom.T(atom) =>
        IF atom = Sx.True THEN b := TRUE
        ELSIF atom = Sx.False THEN b := FALSE
        ELSE RAISE Subr.Usage("expected TRUE|FALSE")
        END
    ELSE RAISE Subr.Usage("expected atom TRUE|FALSE")
    END;
    args := args.tail;
    RETURN b
  END GetBool;

PROCEDURE GetInt(VAR args: RefList.T): INTEGER
  RAISES {Subr.Usage} =
  VAR i: INTEGER;
  BEGIN
    IF args = NIL THEN
      RAISE Subr.Usage("")
    END;
    TYPECASE args.head OF
    | NULL => RAISE Subr.Usage("")
    | REF INTEGER(ri) => i := ri^
    ELSE RAISE Subr.Usage("")
    END;
    args := args.tail;
    RETURN i
  END GetInt;

PROCEDURE GetText(
  VAR args: RefList.T; default: TEXT := NIL; usage: TEXT := NIL)
  : TEXT RAISES {Subr.Usage} =
  VAR t: TEXT;
  BEGIN
    IF args = NIL THEN
      IF default # NIL THEN RETURN default ELSE RAISE Subr.Usage(usage) END
    END;
    TYPECASE args.head OF
    | NULL => RAISE Subr.Usage(usage)
    | TEXT(text) => t := text
    | Atom.T(atom) => t := Atom.ToText(atom)
    ELSE RAISE Subr.Usage(usage)
    END;
    args := args.tail;
    RETURN t
  END GetText;

PROCEDURE GetList(VAR args: RefList.T; default: RefList.T := NIL): RefList.T
  RAISES {Subr.Usage} =
  VAR l: RefList.T;
  BEGIN
    IF args = NIL THEN RETURN default END;
    TYPECASE args.head OF
    | RefList.T(list) => l := list
    ELSE RAISE Subr.Usage("")
    END;
    args := args.tail;
    RETURN l
  END GetList;

PROCEDURE ToTextArray(l: RefList.T): REF ARRAY OF TEXT RAISES {Subr.Usage} =
  VAR a := NEW(REF ARRAY OF TEXT, RefList.Length(l));
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      TYPECASE l.head OF
      | NULL => RAISE Subr.Usage("Unexpected NIL in text array")
      | Atom.T(atom) => a[i] := Atom.ToText(atom)
      | TEXT(text) => a[i] := text
      ELSE RAISE Subr.Usage("Unexpected type in text array")
      END;
      l := l.tail
    END;
    RETURN a
  END ToTextArray;

PROCEDURE ToArcs(l: RefList.T): Pathname.Arcs RAISES {Subr.Usage} =
  VAR a := NEW(Pathname.Arcs).init();
  BEGIN
    WHILE l # NIL DO
      TYPECASE l.head OF
      | NULL => a.addhi(NIL)
      | Atom.T(atom) => a.addhi(Atom.ToText(atom))
      | TEXT(text) => a.addhi(text)
      ELSE RAISE Subr.Usage("unexpected type in Pathname.Arcs")
      END;
      l := l.tail
    END;
    RETURN a
  END ToArcs;

PROCEDURE Help(<*UNUSED*>args: RefList.T): Sx.T =
  VAR it := subrs.iterate(); name: TEXT; subr: Subr.T;
  BEGIN
    Wr.PutText(w, "Commands:\n");
    WHILE it.next(name, subr) DO Wr.PutText(w, "  " & name & "\n") END;
    RETURN QOk
  END Help;

VAR fileCount := 0; files: RefList.T := NIL;

PROCEDURE NewFile(h: File.T): RefList.T =
  VAR list := files; tag: INTEGER;
  BEGIN
    LOOP
      IF list = NIL THEN EXIT END;
      WITH pair = NARROW(list.head, RefList.T) DO
        IF h = pair.head THEN
          tag := NARROW(pair.tail.head, REF INTEGER)^;
          EXIT
        END
      END;
      list := list.tail
    END;
    IF list = NIL THEN
      tag := fileCount;
      INC(fileCount);
      files := RefList.Cons(RefList.List2(h, Sx.FromInt(tag)), files)
    END;
    RETURN RefList.List2(QFile, Sx.FromInt(tag))
  END NewFile;

PROCEDURE OldFile(args: RefList.T): Sx.T RAISES {Subr.Usage} =
  VAR i := GetInt(args); list := files;
  BEGIN
    WHILE list # NIL DO
      WITH pair = NARROW(list.head, RefList.T) DO
        IF i = NARROW(pair.tail.head, REF INTEGER)^ THEN
          RETURN pair.head
        END
      END;
      list := list.tail
    END;
    RAISE Subr.Usage("unknown file #" & Fmt.Int(i))
  END OldFile;

PROCEDURE OSError_AtomToErrno(args: RefList.T): Sx.T RAISES {Subr.Usage} =
  CONST Usage = "(oserror_atomtoerrno text)";
  VAR t := GetText(args, usage := Usage);
  BEGIN
    RETURN Sx.FromInt(OSErrorPosix.AtomToErrno(Atom.FromText(t)))
  END OSError_AtomToErrno;

PROCEDURE FileRead(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  VAR
    expr := GetValue(args);
    n := GetInt(args);
    buf: ARRAY [0..999] OF CHAR;
    m: INTEGER;
  BEGIN
    TYPECASE expr OF
    | NULL => RAISE Subr.Usage("(file_read file nbytes)")
    | File.T(file) => m := file.read(
        SUBARRAY(LOOPHOLE(buf, ARRAY [0..999] OF File.Byte), 0, n))
    ELSE RAISE Subr.Usage("(file_read file nbytes)")
    END;
    RETURN Text.FromChars(SUBARRAY(buf, 0, m))
  END FileRead;

PROCEDURE FileWrite(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  VAR
    expr := GetValue(args);
    t := GetText(args);
    buf: ARRAY [0..999] OF CHAR;
    m: INTEGER;
  BEGIN
    TYPECASE expr OF
    | NULL => RAISE Subr.Usage("(file_write file text)")
    | File.T(file) =>
        Text.SetChars(buf, t);
        m := MIN(NUMBER(buf), Text.Length(t));
        file.write(
          SUBARRAY(LOOPHOLE(buf, ARRAY [0..999] OF File.Byte), 0, m))
    ELSE RAISE Subr.Usage("(file_write text)")
    END;
    RETURN QOk
  END FileWrite;

PROCEDURE FileStatus(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  VAR expr := GetValue(args); status: File.Status;
  BEGIN
    TYPECASE expr OF
    | NULL => RAISE Subr.Usage("(file_status file)")
    | File.T(file) => status := file.status()
    ELSE RAISE Subr.Usage("(file_status file)")
    END;
    RETURN RefList.List3(
      status.type,
      FmtTime.Long(status.modificationTime),
      Sx.FromInt(status.size))
  END FileStatus;

PROCEDURE FileLock(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  VAR expr := GetValue(args);
  BEGIN
    TYPECASE expr OF
    | NULL => RAISE Subr.Usage("(file_lock regularFile)")
    | RegularFile.T(file) => RETURN Sx.FromBool(file.lock())
    ELSE RAISE Subr.Usage("(file_lock regularFile)")
    END
  END FileLock;

PROCEDURE FileUnlock(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  VAR expr := GetValue(args);
  BEGIN
    TYPECASE expr OF
    | NULL => RAISE Subr.Usage("(file_unlock file)")
    | RegularFile.T(file) => file.unlock()
    ELSE RAISE Subr.Usage("(file_unlock regularFile)")
    END;
    RETURN QOk
  END FileUnlock;

PROCEDURE FileClose(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  VAR expr := GetValue(args);
  BEGIN
    TYPECASE expr OF
    | NULL => RAISE Subr.Usage("(file_close file)")
    | File.T(file) => file.close()
    ELSE RAISE Subr.Usage("(file_close file)")
    END;
    RETURN QOk
  END FileClose;

PROCEDURE PnVal(args: RefList.T): Sx.T RAISES {Subr.Usage} =
  BEGIN
    TRY
      pn := GetText(args);
      RETURN Sx.FromBool(Pathname.Valid(pn))
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_val pn)")
    END
  END PnVal;

PROCEDURE PnDecomp(args: RefList.T)
  : Sx.T RAISES {Subr.Usage, Pathname.Invalid} =
  BEGIN
    TRY
      pn := GetText(args, pn);
      RETURN Pathname.Decompose(pn)
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_decomp pn)")
    END
  END PnDecomp;

PROCEDURE PnComp(args: RefList.T)
  : Sx.T RAISES {Subr.Usage, Pathname.Invalid} =
  VAR arcs: Pathname.Arcs;
  BEGIN
    TRY
      arcs := ToArcs(GetList(args));
      pn := Pathname.Compose(arcs);
      RETURN pn
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_comp arclist)")
    END
  END PnComp;

PROCEDURE PnAbs(args: RefList.T): Sx.T RAISES {Subr.Usage} =
   BEGIN
    TRY
      pn := GetText(args);
      RETURN Sx.FromBool(Pathname.Absolute(pn))
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_abs pn)")
    END
  END PnAbs;

PROCEDURE PnPrefix(args: RefList.T): Sx.T RAISES {Subr.Usage} =
   BEGIN
    TRY
      pn := GetText(args);
      RETURN Pathname.Prefix(pn)
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_prefix pn)")
    END
  END PnPrefix;

PROCEDURE PnLast(args: RefList.T): Sx.T RAISES {Subr.Usage} =
   BEGIN
    TRY
      pn := GetText(args);
      RETURN Pathname.Last(pn)
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_last pn)")
    END
  END PnLast;

PROCEDURE PnBase(args: RefList.T): Sx.T RAISES {Subr.Usage} =
   BEGIN
    TRY
      pn := GetText(args);
      RETURN Pathname.Base(pn)
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_base pn)")
    END
  END PnBase;

PROCEDURE PnJoin(args: RefList.T): Sx.T RAISES {Subr.Usage} =
   VAR base, ext: TEXT;
   BEGIN
    TRY
      pn := GetText(args);
      base := GetText(args);
      TYPECASE GetValue(args) OF
      | NULL => ext := NIL
      | TEXT(text) => ext := text
      | Atom.T(atom) => ext := Atom.ToText(atom)
      ELSE RAISE Subr.Usage("")
      END;
      RETURN Pathname.Join(pn, base, ext)
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_join pn base ext)")
    END
  END PnJoin;

PROCEDURE PnLastBase(args: RefList.T): Sx.T RAISES {Subr.Usage} =
   BEGIN
    TRY
      pn := GetText(args);
      RETURN Pathname.LastBase(pn)
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_lastbase pn)")
    END
  END PnLastBase;

PROCEDURE PnLastExt(args: RefList.T): Sx.T RAISES {Subr.Usage} =
   BEGIN
    TRY
      pn := GetText(args);
      RETURN Pathname.LastExt(pn)
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_lastext pn)")
    END
  END PnLastExt;

PROCEDURE PnReplaceExt(args: RefList.T): Sx.T RAISES {Subr.Usage} =
   VAR ext: TEXT;
   BEGIN
    TRY
      pn := GetText(args);
      ext := GetText(args);
      RETURN Pathname.ReplaceExt(pn, ext)
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pn_replaceext pn ext)")
    END
  END PnReplaceExt;

PROCEDURE FSGetAbs(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  BEGIN
    TRY
      pn := GetText(args, pn);
      pn := FS.GetAbsolutePathname(pn);
      RETURN pn
    EXCEPT Subr.Usage => RAISE Subr.Usage("(fs_getabs pathname)")
    END
  END FSGetAbs;

TYPE
  W = Thread.Closure OBJECT h: Pipe.T; k: INTEGER
    METHODS init(h: Pipe.T; k: INTEGER): W := WInit
    OVERRIDES apply := WApply
  END;

VAR big: ARRAY [0..20000] OF File.Byte;

PROCEDURE WInit(t: W; h: Pipe.T; k: INTEGER): W =
  BEGIN
    t.h := h;
    t.k := k;
    RETURN t
  END WInit;

PROCEDURE WApply(t: W): REFANY =
  <* FATAL OSError.E *>
  BEGIN
    t.h.write(SUBARRAY(big, 0, t.k));
    RETURN NIL
  END WApply;

PROCEDURE PipeTest(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  VAR
    hW, hR: Pipe.T;
    w: Thread.T;
    k, n, i: INTEGER;
  BEGIN
    TRY
      k := MIN(GetInt(args), NUMBER(big));
      Pipe.Open(hr := hR, hw := hW);
      w := Thread.Fork(NEW(W).init(hW, k));
      n := 0;
      WHILE n < k DO
	i := hR.read(SUBARRAY(big, n, k - n));
	<* ASSERT i > 0 *>
	n := n + i
      END;
      <* ASSERT n = k *>
      RETURN QOk
    EXCEPT Subr.Usage => RAISE Subr.Usage("(pipe_test <nbytes>)")
    END
  END PipeTest;

PROCEDURE FSOpen(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  VAR
    trunc: BOOLEAN;
    t: TEXT;
    create: FS.CreateOption;
    template: File.T := NIL;
    access: FS.AccessOption;
  BEGIN
    pn := GetText(args);
    trunc := GetBool(args, TRUE, TRUE);
    t := GetText(args, "Ok");
    IF Text.Equal(t, "Never") THEN create := FS.CreateOption.Never
    ELSIF Text.Equal(t, "Ok") THEN create := FS.CreateOption.Ok
    ELSIF Text.Equal(t, "Always") THEN create := FS.CreateOption.Always
    ELSE RAISE Subr.Usage("(fs_open pn [Never|Ok|Always] ...)")
    END;
    (* template ??? *)
    t := GetText(args, "Default");
    IF Text.Equal(t, "OnlyOwnerCanRead") THEN
      access := FS.AccessOption.OnlyOwnerCanRead
    ELSIF Text.Equal(t, "ReadOnly") THEN access := FS.AccessOption.ReadOnly
    ELSIF Text.Equal(t, "Default") THEN access := FS.AccessOption.Default
    ELSE RAISE Subr.Usage(
      "(fs_open pn access [OnlyOwnerCanRead|ReadOnly|Default]...)")
    END;
    WITH h = FS.OpenFile(pn, trunc, create, template, access) DO
      EVAL NewFile(h);
      RETURN h
    END
  END FSOpen;

PROCEDURE FSOpenRo(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  CONST Usage = "(fs_openro path)";
  BEGIN
    pn := GetText(args, usage := Usage);
    WITH h = FS.OpenFileReadonly(pn) DO
      EVAL NewFile(h);
      RETURN h
    END
  END FSOpenRo;

PROCEDURE FSIterate(args: RefList.T)
  : Sx.T RAISES {Subr.Usage, OSError.E} =
  CONST Usage = "(fs_iterate path)";
  VAR
    pn := GetText(args, usage := Usage);
    i := FS.Iterate(pn);
    name: TEXT;
    l: RefList.T := NIL;
  BEGIN
    WHILE i.next(name) DO
      l := RefList.AppendD(l, RefList.List1(name))
    END;
    RETURN l
  END FSIterate;

PROCEDURE FSIterateWithStatus(args: RefList.T)
  : Sx.T RAISES {Subr.Usage, OSError.E} =
  CONST Usage = "(fs_iterate_status path)";
  VAR
    pn := GetText(args, usage := Usage);
    i := FS.Iterate(pn);
    name: TEXT; status: File.Status;
    l: RefList.T := NIL;
  BEGIN
    WHILE i.nextWithStatus(name, status) DO
      l := RefList.AppendD(l, RefList.List1(RefList.List2(name, status.type)))
    END;
    RETURN l
  END FSIterateWithStatus;

PROCEDURE PrCr(args: RefList.T): Sx.T RAISES {Subr.Usage, OSError.E} =
  VAR
    cmd, wd: TEXT;
    argv, env: REF ARRAY OF TEXT;
    in, out, err: File.T;
    p: Process.T;
  BEGIN
    TRY
      cmd := GetText(args);
      argv := ToTextArray(GetList(args, NIL));
      WITH l = GetList(args, NIL) DO
        IF l = NIL THEN env := NIL
        ELSE env := ToTextArray(l)
        END
      END;
      wd := GetText(args, "");
      in := GetValue(args, stdin);
      out := GetValue(args, stdout);
      err := GetValue(args, stderr);
      p := Process.Create(cmd, argv^, env, wd, in, out, err);
      RETURN Sx.FromInt(Process.Wait(p))
    EXCEPT Subr.Usage => RAISE Subr.Usage(
      "(pr_cr cmd [arglist] [envlist] [wd] [stdin] [stdout] [stderr])")
    END
  END PrCr;

PROCEDURE PrGetMyID(<*UNUSED*>args: RefList.T): Sx.T =
  BEGIN
    RETURN Sx.FromInt(Process.GetMyID())
  END PrGetMyID;

PROCEDURE PrGetWD(<*UNUSED*>args: RefList.T): Sx.T RAISES {OSError.E} =
  BEGIN
    RETURN Process.GetWorkingDirectory()
  END PrGetWD;

VAR pn := "";

PROCEDURE PrSetWD(args: RefList.T): Sx.T RAISES {OSError.E} =
  <* FATAL Subr.Usage *>
  BEGIN
    pn := GetText(args, pn);
    Process.SetWorkingDirectory(pn);
    RETURN pn
  END PrSetWD;

PROCEDURE EnvGet(args: RefList.T): Sx.T = <*FATAL Subr.Usage *>
  BEGIN
    RETURN Env.Get(GetText(args))
  END EnvGet;

PROCEDURE EnvCount(<*UNUSED*> args: RefList.T): Sx.T =
  BEGIN
    RETURN Sx.FromInt(Env.Count)
  END EnvCount;

PROCEDURE EnvGetAll(<*UNUSED*> args: RefList.T): Sx.T =
  VAR l: RefList.T := NIL; nm, val: TEXT;
  BEGIN
    FOR i := 0 TO Env.Count - 1 DO
      Env.GetNth(i, nm, val);
      l := RefList.Cons(nm & "=" & val, l)
    END;
    RETURN RefList.ReverseD(l)
  END EnvGetAll;

BEGIN
  Process.GetStandardFileHandles(stdin, stdout, stderr);
  EVAL NewFile(stdin); EVAL NewFile(stdout); EVAL NewFile(stderr);
  Register("help", Help);
  Register("oserror_atomtoerrno", OSError_AtomToErrno);
  Register("file", OldFile);
  Register("file_read", FileRead);
  Register("file_write", FileWrite);
  Register("file_status", FileStatus);
  Register("file_lock", FileLock);
  Register("file_unlock", FileUnlock);
  Register("file_close", FileClose);
  Register("pn_posixtests", PathnameTests.PosixTests);
  Register("pn_decomp", PnDecomp);
  Register("pn_comp", PnComp);
  Register("pn_val", PnVal);
  Register("pn_abs", PnAbs);
  Register("pn_prefix", PnPrefix);
  Register("pn_last", PnLast);
  Register("pn_base", PnBase);
  Register("pn_join", PnJoin);
  Register("pn_lastbase", PnLastBase);
  Register("pn_lastext", PnLastExt);
  Register("pn_replaceext", PnReplaceExt);
  Register("pipe_test", PipeTest);
  Register("fs_getabs", FSGetAbs);
  Register("fs_open", FSOpen);
  Register("fs_openro", FSOpenRo);
  Register("fs_iterate", FSIterate);
  Register("fs_iterate_status", FSIterateWithStatus);
  Register("pr_cr", PrCr);
  Register("pr_getmyid", PrGetMyID);
  Register("pr_getwd", PrGetWD);
  Register("pr_setwd", PrSetWD);
  Register("env_get", EnvGet);
  Register("env_count", EnvCount);
  Register("env_getall", EnvGetAll);

  Wr.PutText(w, "OS test shell\n");
  WHILE NOT Rd.EOF(r) DO
    TRY
      WITH e = Sx.Read(r) DO
        TRY
          Print(w, Eval(e)); Wr.PutText(w, "\n")
        EXCEPT
        | Subr.Usage(msg) => Wr.PutText(w, "Usage: " & msg & "\n")
        | OSError.E(code) =>
            Wr.PutText(w, "OSError.E(" & Atom.ToText(code.head) & ")\n")
        | Pathname.Invalid => Wr.PutText(w, "Pathname.Invalid\n")
        END
      END
    EXCEPT
    | Sx.ReadError(t) => Wr.PutText(w, "Read error (" & t & ")\n")
    | Rd.EndOfFile => Wr.PutText(w, "Unexpected EOF\n")
    END
  END
END OSTest.
