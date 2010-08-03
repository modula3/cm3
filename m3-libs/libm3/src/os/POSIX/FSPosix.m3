(* Copyright (C) 1992, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Jan 26 13:51:09 PST 1995 by kalsow  *)
(*      modified on Thu Jul 14 11:37:50 PDT 1994 by mcjones *)
(*      modified on Wed May 12 17:21:07 PDT 1993 by meehan  *)
(*      modified on Fri May  7 14:53:27 PDT 1993 by mjordan *)

(* Version for Unix *)

UNSAFE MODULE FSPosix EXPORTS FS;

IMPORT Atom, AtomList, Cerrno, Ctypes, File, FilePosix, M3toC, OSError,
       OSErrorPosix, Pathname, Process, Time, Text, TextSeq, Unix,
       Udir, Uerror, Ustat, Word, Scheduler, FSPosixC;

FROM Unix IMPORT O_RDWR, O_CREAT, O_TRUNC, O_EXCL;

PROCEDURE GetAbsolutePathname(pn: Pathname.T): Pathname.T RAISES {OSError.E} =
  VAR arcs, prefix: Pathname.Arcs;
  BEGIN
    TRY
      arcs := Pathname.Decompose(pn);
      WITH rootDir = arcs.remlo() DO
        IF rootDir = NIL THEN (* "pn" is relative *)
          prefix := Pathname.Decompose(Process.GetWorkingDirectory())
        ELSE (* "pn" is absolute *)
          prefix := Seq1(rootDir)
       END
      END;
      arcs := Expand(prefix, arcs); (* expand symbolic links *)
      Contract(arcs); (* eliminate relative arcs *)
      RETURN Pathname.Compose(arcs)
    EXCEPT Pathname.Invalid => RAISE OSError.E(AtomList.List1(Invalid))
    END;
  END GetAbsolutePathname;

VAR Invalid := Atom.FromText("Invalid Pathname"); (* ??? *)

PROCEDURE Expand(prefix: Pathname.Arcs; tail: TextSeq.T): Pathname.Arcs
  RAISES {OSError.E, Pathname.Invalid} =
  (* "tail" is a sequence of arcs relative to "prefix". "Expand"
     returns an absolute pathname with no "", ".", or ".." arcs that
     names the same object as "TextSeq.Cat(prefix, tail)". *)
  VAR arc, rootDir: TEXT;
  BEGIN
    WHILE tail.size() > 0 DO
      arc := tail.remlo();
      prefix.addhi(arc);
      WITH link = CheckLink(prefix) DO
        IF link # NIL THEN
          tail := TextSeq.Cat(link, tail);
          rootDir := tail.remlo();
          IF rootDir = NIL THEN (* "link" is relative *)
            EVAL prefix.remhi() (* use previous "prefix" *)
          ELSE (* "link" is absolute *)
            prefix := Seq1(rootDir)
          END
        END
      END
    END;
    RETURN prefix
  END Expand;

PROCEDURE Contract(arcs: Pathname.Arcs) =
(* Eliminate relative arcs ("..", ".", or ""). *)
  VAR i := 1;
  BEGIN
    (* Invariant: Sub(arcs, 0, i) contains no relative arc. *)
    WHILE i < arcs.size() DO
      WITH arc = arcs.get(i) DO
        IF Text.Equal(arc, Pathname.Current) OR Text.Equal(arc, "") THEN
          (* a/./b => a/b; a//b => a/./b => a/b *)
          Rem(arcs, i)
        ELSIF Text.Equal(arc, Pathname.Parent) THEN
          IF i = 1 AND Text.Equal(arcs.get(0), "/") THEN
            (* Special case: /../a => /a *)
            Rem(arcs, 1)
          ELSE
            (* a/b/../c => a/c *)
            Rem(arcs, i); DEC(i); Rem(arcs, i)
          END
        ELSE
          INC(i)
        END
      END
    END
  END Contract;

(* TextSeq utility procedures: *)
PROCEDURE Seq1(t: TEXT): TextSeq.T =
  (* Return a new sequence whose only element is "t". *)
  BEGIN RETURN NEW(TextSeq.T).fromArray(ARRAY OF TEXT{t}) END Seq1;

PROCEDURE Rem(s: TextSeq.T; i: CARDINAL) =
  (* Remove the "i"th element of "s". *)
  BEGIN
    FOR j := i TO s.size()-2 DO s.put(j, s.get(j+1)) END;
    EVAL s.remhi()
  END Rem;

PROCEDURE CheckLink(arcs: Pathname.Arcs): Pathname.Arcs
  RAISES {OSError.E, Pathname.Invalid} =
  VAR
    path  := Pathname.Compose(arcs);
    fname := M3toC.SharedTtoS(path);
    cc    := Unix.readlink(fname, ADR(buf [0]), NUMBER(buf));
    p_buf : ADDRESS := ADR (buf[0]);
    buf   : ARRAY [0 .. 1023] OF CHAR;
  BEGIN
    IF cc > 0 THEN
      M3toC.FreeSharedS(path, fname);
      buf[cc] := '\000'; (* terminate the string *)
      RETURN Pathname.Decompose(M3toC.CopyStoT(p_buf))
    ELSIF Cerrno.GetErrno() = Uerror.EINVAL THEN (* not a symbolic link *)
      M3toC.FreeSharedS(path, fname);
      RETURN NIL;
    ELSE
      (* Some component is not a directory, or the file doesn't exist, or too
         many links (shouldn't happen, since we're expanding them one by one),
         or timeout, or ... *)
      Fail(path, fname);
      RETURN NIL;
    END;
  END CheckLink;

TYPE ABW = ARRAY BOOLEAN OF Word.T;

(* CONST *)
VAR OpenFlags := ARRAY CreateOption OF ABW{
  (* truncate =    FALSE                  TRUE                 *)
  (* Never  *) ABW{O_RDWR,                O_RDWR+O_TRUNC        },
  (* Ok     *) ABW{O_RDWR+O_CREAT,        O_RDWR+O_CREAT+O_TRUNC},
  (* Always *) ABW{O_RDWR+O_CREAT+O_EXCL, O_RDWR+O_CREAT+O_EXCL }
  };

(* CONST *)
VAR AllAccessModes :=
  Unix.MSETUID + Unix.MSETGID + Unix.MSTICKY +
  Unix.MROWNER + Unix.MWOWNER + Unix.MXOWNER +
  Unix.MRGROUP + Unix.MWGROUP + Unix.MXGROUP +
  Unix.MROTHER + Unix.MWOTHER + Unix.MXOTHER;

(* CONST *)
VAR OpenMode := ARRAY AccessOption OF Ctypes.int{
  (*OnlyOwnerCanRead*) Unix.MROWNER+Unix.MWOWNER,
  (*ReadOnly*)         Unix.MROWNER+Unix.MRGROUP+Unix.MROTHER,
  (*Default*)          Unix.Mrwrwrw (* should this be AllAccessModes? *)
  };

PROCEDURE OpenFile(pn: Pathname.T; truncate: BOOLEAN := TRUE; 
    create: CreateOption := CreateOption.Ok; template: File.T := NIL; 
    access: AccessOption := AccessOption.Default
    ): File.T RAISES {OSError.E}=
  VAR
    fd      : INTEGER;
    statBuf : Ustat.struct_stat;
    mode    : Ctypes.int;
    fname   := M3toC.SharedTtoS(pn);
  BEGIN
    IF template # NIL THEN
      IF Ustat.fstat(template.fd, ADR(statBuf)) < 0 THEN
         Fail(pn, fname);
      END;
      mode := Word.And(statBuf.st_mode, AllAccessModes)
    ELSE
      mode := OpenMode[access]
    END;
    fd := Unix.open(fname, OpenFlags[create, truncate], mode);
    IF fd < 0 THEN Fail(pn, fname); END;
    M3toC.FreeSharedS(pn, fname);
    RETURN FilePosix.New(fd, FilePosix.ReadWrite)
  END OpenFile;

PROCEDURE OpenFileReadonly(pn: Pathname.T): File.T RAISES {OSError.E}=
  VAR
    fname := M3toC.SharedTtoS(pn);
    fd    := Unix.open(fname, Unix.O_RDONLY, Unix.Mrwrwrw);
  BEGIN
    IF fd < 0 THEN Fail(pn, fname); END;
    M3toC.FreeSharedS(pn, fname);
    RETURN FilePosix.New(fd, FilePosix.Read)
  END OpenFileReadonly;

PROCEDURE CreateDirectory(pn: Pathname.T) RAISES {OSError.E}=
  VAR
    (* Default access is rwxrwxrwx. The umask is applied by Unix *)
    (*CONST*)
    RWXRWXRWX := Ustat.S_IREAD + Ustat.S_IWRITE + Ustat.S_IEXEC +
        Ustat.S_GREAD + Ustat.S_GWRITE + Ustat.S_GEXEC +
        Ustat.S_OREAD + Ustat.S_OWRITE + Ustat.S_OEXEC;
    fname := M3toC.SharedTtoS(pn);
  BEGIN
    IF Unix.mkdir(fname, RWXRWXRWX) < 0 THEN Fail(pn, fname); END;
    M3toC.FreeSharedS(pn, fname);
  END CreateDirectory;

PROCEDURE DeleteDirectory(pn: Pathname.T) RAISES {OSError.E}=
  VAR fname := M3toC.SharedTtoS(pn);
  BEGIN
    IF Unix.rmdir(fname) < 0 THEN Fail(pn, fname); END;
    M3toC.FreeSharedS(pn, fname);
  END DeleteDirectory;

PROCEDURE DeleteFile(pn: Pathname.T) RAISES {OSError.E}=
  VAR fname := M3toC.SharedTtoS(pn);
  BEGIN
    IF Unix.unlink(fname) < 0 THEN Fail(pn, fname); END;
    M3toC.FreeSharedS(pn, fname);
  END DeleteFile;

PROCEDURE Rename(pn0, pn1: Pathname.T) RAISES {OSError.E}=
  VAR
    err : INTEGER;
    f0  := M3toC.SharedTtoS(pn0);
    f1  := M3toC.SharedTtoS(pn1);
  BEGIN
    IF Unix.rename(f0, f1) < 0 THEN
      err := Cerrno.GetErrno();
      M3toC.FreeSharedS(pn0, f0);
      M3toC.FreeSharedS(pn1, f1);
      OSErrorPosix.Raise0(err)
    END;
    M3toC.FreeSharedS(pn0, f0);
    M3toC.FreeSharedS(pn1, f1);
  END Rename;

REVEAL Iterator = PublicIterator BRANDED OBJECT
    pn: Pathname.T; (* pathname of directory being iterated over *)
    d: Udir.DIR_star;
    closed := FALSE; (* has close() been called? *)
  OVERRIDES
    next := IterNext;
    nextWithStatus := IterNextWithStatus;
    close := IterClose
  END;

EXCEPTION IterClosed; <* FATAL IterClosed *>

PROCEDURE Iterate(pn: Pathname.T): Iterator RAISES {OSError.E} =
  VAR d: Udir.DIR_star;  fname: Ctypes.char_star;
  BEGIN
    IF NOT Pathname.Absolute(pn) THEN
      pn := Pathname.Join(Process.GetWorkingDirectory(), pn, NIL)
    END;
    fname := M3toC.SharedTtoS(pn);
    Scheduler.DisableSwitching();
      (* opendir() calls malloc() => not user-thread safe *)
      d := Udir.opendir(fname);
    Scheduler.EnableSwitching();
    IF d = NIL THEN Fail(pn, fname); END;
    M3toC.FreeSharedS(pn, fname);
    RETURN NEW(Iterator, d := d, pn := pn)
  END Iterate;

PROCEDURE IterNext(iter: Iterator; VAR (*OUT*) name: TEXT): BOOLEAN =
  BEGIN
    IF IterRaw(iter, name) THEN RETURN TRUE END;
    RETURN FALSE
  END IterNext;

PROCEDURE IterNextWithStatus(
    iter: Iterator; VAR (*OUT*) name: TEXT; VAR (*OUT*) status: File.Status)
  : BOOLEAN RAISES {OSError.E} =
  VAR p: TEXT;  fname: Ctypes.char_star;
  BEGIN
    IF IterRaw(iter, name) THEN
      p := Pathname.Join(iter.pn, name, NIL);
      fname := M3toC.SharedTtoS(p);
      IF CStatus(fname, status) < 0 THEN Fail(p, fname); END;
      M3toC.FreeSharedS(p, fname);
      RETURN TRUE
    END;
    RETURN FALSE
  END IterNextWithStatus;

TYPE NamePrefix = UNTRACED REF ARRAY [0..2] OF Ctypes.char;

PROCEDURE IterRaw(iter: Iterator; VAR (*OUT*) name: TEXT): BOOLEAN =
  VAR n: Ctypes.const_char_star;
  BEGIN
    IF iter.closed THEN RAISE IterClosed END;
    LOOP (* to ignore "." and ".." *)
      IF iter.d = NIL THEN RETURN FALSE
      ELSE
      (* cvsup source says readdir needs EnableSwitching/DisableSwitching *)
        Scheduler.DisableSwitching ();
          n := FSPosixC.readdir_name(iter.d);
        Scheduler.EnableSwitching ();
          IF n = NIL THEN
            Scheduler.DisableSwitching ();
              (* closedir() calls free() => not user-thread safe *)
              EVAL Udir.closedir(iter.d);
            Scheduler.EnableSwitching ();
            iter.d := NIL;
            RETURN FALSE
          ELSE
            IF NOT DotOrDotDot(LOOPHOLE(n, NamePrefix)) THEN
              name := M3toC.CopyStoT(n);
              RETURN TRUE
            END
          END
      END
    END
  END IterRaw;

PROCEDURE DotOrDotDot(n: NamePrefix): BOOLEAN =
  CONST Dot = ORD('.'); Nul = ORD('\000');
  BEGIN
    RETURN n[0] = Dot AND (n[1] = Nul OR n[1] = Dot AND n[2] = Nul)
  END DotOrDotDot;
    
PROCEDURE IterClose(iter: Iterator) =
  BEGIN
    IF iter.d # NIL THEN
      Scheduler.DisableSwitching ();
        (* closedir() calls free() => not user-thread safe *)
        EVAL Udir.closedir(iter.d);
      Scheduler.EnableSwitching ();
      iter.d := NIL;
    END;
    iter.closed := TRUE
  END IterClose;

PROCEDURE Status(pn: Pathname.T): File.Status RAISES {OSError.E} =
  VAR status: File.Status;  fname := M3toC.SharedTtoS(pn);
  BEGIN
    IF CStatus(fname, status) < 0 THEN Fail(pn, fname); END;
    M3toC.FreeSharedS(pn, fname);
    RETURN status
  END Status;

PROCEDURE CStatus(s: Ctypes.char_star; VAR status: File.Status): INTEGER =
  VAR statBuf: Ustat.struct_stat;
  BEGIN
    IF Ustat.lstat(s, ADR(statBuf)) < 0 THEN RETURN -1 END;
    status.type := FilePosix.FileTypeFromStatbuf(statBuf);
    (* Could make following assignments conditional on type: *)
    status.modificationTime := FLOAT(statBuf.st_mtime, LONGREAL);
    status.size := statBuf.st_size;
    IF status.size < 0L THEN RETURN -1 END;
    RETURN 0
  END CStatus;

PROCEDURE SetModificationTime(pn: Pathname.T; READONLY t: Time.T)
  RAISES {OSError.E}=
  VAR fname := M3toC.SharedTtoS(pn);
  BEGIN
    IF FSPosixC.SetModificationTime(fname, ROUND(t), ROUND(Time.Now())) < 0 THEN Fail(pn, fname); END;
    M3toC.FreeSharedS(pn, fname);
  END SetModificationTime;

PROCEDURE Fail(p: Pathname.T;  f: Ctypes.char_star) RAISES {OSError.E} =
  VAR err := Cerrno.GetErrno();
  BEGIN
    M3toC.FreeSharedS(p, f);
    OSErrorPosix.Raise0(err);
  END Fail;

BEGIN
END FSPosix.
