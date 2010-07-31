(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(*--------------------------------------------------------------------------*)
MODULE FSUtils;

IMPORT Pathname, File, RegularFile, Process, OSError, Rd, FileRd,
       Wr, FileWr, Thread, Text, TextSeq;
IMPORT SMsg AS Msg, PathRepr;
IMPORT (* FSFixed AS *) FS;
FROM System IMPORT AtomListToText;

(*--------------------------------------------------------------------------*)
PROCEDURE Exists(fn : Pathname.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(PathRepr.Native(fn));
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN TRUE;
  END Exists;

(*--------------------------------------------------------------------------*)
PROCEDURE IsDir(fn : Pathname.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(PathRepr.Native(fn));
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN s.type = FS.DirectoryFileType;
  END IsDir;

(*--------------------------------------------------------------------------*)
PROCEDURE IsFile(fn : Pathname.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(PathRepr.Native(fn));
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN s.type = RegularFile.FileType;
  END IsFile;

(*--------------------------------------------------------------------------*)
PROCEDURE MakeDir(path : Pathname.T) =
  VAR
    arcs  : Pathname.Arcs;
    iarcs : Pathname.Arcs;
    ipath : Pathname.T;
  BEGIN
    TRY
      arcs  := Pathname.Decompose(PathRepr.Native(path));
      iarcs := NEW(Pathname.Arcs).init(arcs.size());
    EXCEPT
      Pathname.Invalid => Process.Crash("internal error: invalid pathname");
    END;
    FOR i := 0 TO arcs.size() - 1 DO
      iarcs.addhi(arcs.get(i));
      TRY
        ipath := Pathname.Compose(iarcs);
      EXCEPT
        Pathname.Invalid => Process.Crash("internal error: invalid pathname");
      END;
      IF arcs.get(i) # NIL THEN
        IF NOT IsDir(ipath) THEN
          Msg.D("MakeDir: component `" & ipath & "'");
          IF Exists(ipath) THEN
            Msg.Fatal("cannot create directory, file exists " & ipath);
          END;
          TRY
            FS.CreateDirectory(ipath);
          EXCEPT
            OSError.E(l) => 
            Process.Crash("cannot create directory " & ipath & 
              ": " & AtomListToText(l));
          END;
        END;
      END;
    END;
  END MakeDir;

(*--------------------------------------------------------------------------*)
PROCEDURE SubDirs(path : Pathname.T; relative := FALSE) : TextSeq.T
  RAISES {E} =
  VAR
    iter : FS.Iterator;
    name : TEXT;
    stat : File.Status;
    res  : TextSeq.T := NEW(TextSeq.T).init();
  BEGIN
    TRY
      iter := FS.Iterate(path);
      TRY
        WHILE iter.nextWithStatus(name, stat) DO
          IF stat.type = FS.DirectoryFileType THEN
            IF relative THEN
              res.addhi(name);
            ELSE
              res.addhi(Pathname.Join(path, name, NIL));
            END;
          END;
        END;
      FINALLY
        iter.close();
      END;
    EXCEPT
      OSError.E(l) => RAISE E("error traversing directory " & path & 
        ": " & AtomListToText(l));
    END;
    RETURN res;
  END SubDirs;

(*--------------------------------------------------------------------------*)
PROCEDURE SubFiles(path : Pathname.T; relative := FALSE) : TextSeq.T
  RAISES {E} =
  VAR
    iter : FS.Iterator;
    name : TEXT;
    stat : File.Status;
    res  : TextSeq.T := NEW(TextSeq.T).init();
  BEGIN
    TRY
      iter := FS.Iterate(path);
      TRY
        WHILE iter.nextWithStatus(name, stat) DO
          IF stat.type = RegularFile.FileType THEN
            IF relative THEN
              res.addhi(name);
            ELSE
              res.addhi(Pathname.Join(path, name, NIL));
            END;
          END;
        END;
      FINALLY
        iter.close();
      END;
    EXCEPT
      OSError.E(l) => RAISE E("error traversing directory " & path & 
        ": " & AtomListToText(l));
    END;
    RETURN res;
  END SubFiles;

(*--------------------------------------------------------------------------*)
PROCEDURE RemoveFile(fn : Pathname.T) =
  BEGIN
    IF NOT Exists(fn) THEN RETURN END;
    IF IsFile(fn) THEN
      TRY
        FS.DeleteFile(PathRepr.Native(fn));
      EXCEPT
        OSError.E(l) => Process.Crash("cannot remove file " & fn & 
          ": " & AtomListToText(l));
      END;
    ELSE
      Msg.Fatal("internal error: cannot remove non-regular file " & fn);
    END;
  END RemoveFile;

(*--------------------------------------------------------------------------*)
PROCEDURE RemoveDir(fn : Pathname.T) =
  BEGIN
    IF NOT Exists(fn) THEN RETURN END;
    IF IsDir(fn) THEN
      TRY
        FS.DeleteDirectory(PathRepr.Native(fn));
      EXCEPT
        OSError.E(l) => Process.Crash("cannot remove directory " & fn & 
          ": " & AtomListToText(l));
      END;
    ELSE
      Msg.Fatal("internal error: " & fn & " is no directory");
    END;
  END RemoveDir;

(*--------------------------------------------------------------------------*)
PROCEDURE TouchFile(fn : Pathname.T) =
  VAR f : File.T;
  BEGIN
    TRY
      f := FS.OpenFile(PathRepr.Native(fn), truncate := FALSE,
                       create := FS.CreateOption.Ok);
      f.close();
    EXCEPT
      OSError.E(l) => Process.Crash("cannot touch file " & fn & 
        ": " & AtomListToText(l));
    END;
  END TouchFile;

(*--------------------------------------------------------------------------*)
PROCEDURE Mkdir(path : Pathname.T) RAISES {E} =
  VAR
    arcs  : Pathname.Arcs;
    iarcs : Pathname.Arcs;
    ipath : Pathname.T;
  BEGIN
    TRY
      arcs  := Pathname.Decompose(PathRepr.Native(path));
      iarcs := NEW(Pathname.Arcs).init(arcs.size());
    EXCEPT
      Pathname.Invalid => RAISE E("internal error: invalid pathname");
    END;
    FOR i := 0 TO arcs.size() - 1 DO
      iarcs.addhi(arcs.get(i));
      TRY
        ipath := Pathname.Compose(iarcs);
      EXCEPT
        Pathname.Invalid => RAISE E("internal error: invalid pathname");
      END;
      IF arcs.get(i) # NIL THEN
        IF NOT IsDir(ipath) THEN
          Msg.D("MakeDir: component `" & ipath & "'");
          IF Exists(ipath) THEN
            RAISE E("cannot create directory, file exists " & ipath);
          END;
          TRY
            FS.CreateDirectory(ipath);
          EXCEPT
            OSError.E(l) => RAISE E("cannot create directory " & ipath & 
              ": " & AtomListToText(l));
          END;
        END;
      END;
    END;
  END Mkdir;

(*--------------------------------------------------------------------------*)
PROCEDURE Rm(fn : Pathname.T) RAISES {E} =
  BEGIN
    IF NOT Exists(fn) THEN RETURN END;
    IF IsFile(fn) THEN
      TRY
        FS.DeleteFile(PathRepr.Native(fn));
      EXCEPT
        OSError.E(l) => RAISE E("cannot remove file " & fn & 
          ": " & AtomListToText(l));
      END;
    ELSE
      RAISE E("internal error: cannot remove non-regular file " & fn);
    END;
  END Rm;

(*--------------------------------------------------------------------------*)
PROCEDURE Rmdir(fn : Pathname.T) RAISES {E} =
  BEGIN
    IF NOT Exists(fn) THEN RETURN END;
    IF IsDir(fn) THEN
      TRY
        FS.DeleteDirectory(PathRepr.Native(fn));
      EXCEPT
        OSError.E(l) => RAISE E("cannot remove directory " & fn & 
          ": " & AtomListToText(l));
      END;
    ELSE
      RAISE E("internal error: " & fn & " is no directory");
    END;
  END Rmdir;

(*--------------------------------------------------------------------------*)
PROCEDURE OldRmRec(fn : Pathname.T) RAISES {E} =
  VAR
    iter : FS.Iterator;
    name : TEXT;
    stat : File.Status;
  BEGIN
    IF NOT Exists(fn) THEN
      RETURN
    END;
    IF IsFile(fn) THEN
      Rm(fn);
    ELSIF IsDir(fn) THEN
      TRY
        iter := FS.Iterate(fn);
        TRY
          WHILE iter.nextWithStatus(name, stat) DO
            WITH fnext = Pathname.Join(fn, name, NIL) DO
              IF stat.type = FS.DirectoryFileType THEN
                OldRmRec(fnext);
              ELSIF stat.type = RegularFile.FileType THEN
                Rm(fnext);
              END;
            END;
          END;
        FINALLY
          iter.close();
        END;
      EXCEPT
        OSError.E(l) => RAISE E("error traversing directory " & fn & 
          ": " & AtomListToText(l));
      END;
      Rmdir(fn);
    ELSE
      RAISE E("error: " & fn & " is no directory or ordinary file");
    END;
  END OldRmRec;

(*--------------------------------------------------------------------------*)

PROCEDURE xRmRec(fn : Pathname.T) RAISES {E} =
  VAR sub := SubFiles(fn);
  BEGIN
    FOR i := 0 TO sub.size() - 1 DO
      Rm(sub.get(i));
    END;
    sub := SubDirs(fn);
    FOR i := 0 TO sub.size() - 1 DO
      xRmRec(sub.get(i));
    END;
    Rmdir(fn);
  END xRmRec;

PROCEDURE RmRec(fn : Pathname.T) RAISES {E} =
  BEGIN
    IF NOT Exists(fn) THEN
      RETURN
    END;
    IF IsFile(fn) THEN
      Rm(fn);
    ELSIF IsDir(fn) THEN
      xRmRec(fn);
    ELSE
      RAISE E("error: " & fn & " is no directory or ordinary file");
    END;
  END RmRec;

(*--------------------------------------------------------------------------*)
PROCEDURE Touch(fn : Pathname.T) RAISES {E} =
  VAR f : File.T;
  BEGIN
    TRY
      f := FS.OpenFile(PathRepr.Native(fn), truncate := FALSE,
                       create := FS.CreateOption.Ok);
      f.close();
    EXCEPT
      OSError.E(l) => RAISE E("cannot touch file " & fn & 
        ": " & AtomListToText(l));
    END;
  END Touch;

(*--------------------------------------------------------------------------*)
PROCEDURE LongestExistingPrefix(path     : Pathname.T;
                                VAR rest : Pathname.T) : Pathname.T
  RAISES {E} =
  VAR
    arcs  : Pathname.Arcs;
    iarcs : Pathname.Arcs;
    rarcs : Pathname.Arcs;
    ipath : Pathname.T;
  BEGIN
    TRY
      arcs  := Pathname.Decompose(PathRepr.Native(path));
      iarcs := NEW(Pathname.Arcs).init(arcs.size());
      FOR i := 0 TO arcs.size() - 1 DO
        iarcs.addhi(arcs.get(i));
        ipath := Pathname.Compose(iarcs);
        IF arcs.get(i) # NIL THEN
          IF NOT Exists(ipath) THEN
            rarcs := TextSeq.Sub(arcs, i);
            rarcs.addlo(NIL);
            rest := Pathname.Compose(rarcs);
            IF i < 2 THEN
              RETURN "";
            ELSE
              RETURN Pathname.Compose(TextSeq.Sub(iarcs, 0, i));
            END;
          END;
        END;
      END;
      rest := "";
      RETURN ipath;
    EXCEPT
      Pathname.Invalid => RAISE E("internal error: invalid pathname: " & path);
    END;
  END LongestExistingPrefix;

(*--------------------------------------------------------------------------*)
PROCEDURE CanonicalPathname(fn : Pathname.T) : Pathname.T RAISES {E} =
  VAR
    wd, existingPath, rest, res : Pathname.T;
  BEGIN
    fn := PathRepr.Native(fn);
    existingPath := LongestExistingPrefix(fn, rest);
    IF Text.Empty(existingPath) THEN
      IF Pathname.Absolute(fn) THEN
        res := fn;
      ELSE
        TRY
          wd := Process.GetWorkingDirectory();
        EXCEPT
          OSError.E(l) => RAISE E("cannot get working directory" & 
            ": " & AtomListToText(l));
        END;
        IF Text.Empty(fn) THEN
          res := wd;
        ELSE
          res := Pathname.Join(wd, fn, NIL);
        END;
      END;
    ELSE
      TRY
        IF Text.Empty(rest) THEN
          res := FS.GetAbsolutePathname(existingPath);
        ELSE
          res := Pathname.Join(FS.GetAbsolutePathname(existingPath),
                               rest, NIL);
        END;
      EXCEPT ELSE
        RAISE E("pathname " & fn & " is invalid");
      END;
    END;
    RETURN res;
  END CanonicalPathname;

(*--------------------------------------------------------------------------*)
PROCEDURE Cp(src, dest : Pathname.T) RAISES {E} =
  VAR
    rd  : File.T;
    wr  : File.T;
    buf : ARRAY [0..4095] OF File.Byte;
    n   : INTEGER;
  BEGIN
    IF NOT Exists(src) THEN
      RAISE E("file not found: " & src);
    END;
    TRY
      rd := FS.OpenFileReadonly(PathRepr.Native(src));
    EXCEPT
      OSError.E(l) => RAISE E("cannot open file " & src & 
        ": " & AtomListToText(l));
    END;
    TRY
      TRY
        wr := FS.OpenFile(PathRepr.Native(dest));
      EXCEPT
        OSError.E(l) => RAISE E("cannot open file " & dest & 
          ": " & AtomListToText(l));
      END;
      TRY
        TRY
          n := rd.read(buf);
          WHILE n > 0 DO
            wr.write(SUBARRAY(buf, 0, n));
            n := rd.read(buf);
          END;
        EXCEPT
          OSError.E(l) => RAISE E("error copying file " & src & 
            ": " & AtomListToText(l));
        END;
      FINALLY
        TRY
          wr.close();
        EXCEPT
          OSError.E(l) => RAISE E("cannot close file " & dest & 
            ": " & AtomListToText(l));
        END;
      END;
    FINALLY
      TRY rd.close() EXCEPT ELSE END;
    END;
  END Cp;

(*---------------------------------------------------------------------------*)
PROCEDURE FileContents(fn : Pathname.T) : TEXT RAISES {E} =
  VAR
    rd   : FileRd.T;
    data : TEXT;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
    EXCEPT
      OSError.E(l) => RAISE E("cannot open file " & fn & 
        ": " & AtomListToText(l));
    END;
    TRY
      TRY
        data := Rd.GetText(rd, LAST(CARDINAL));
      EXCEPT
        Rd.Failure,
        Thread.Alerted => RAISE E("cannot read file " & fn);
      END;
    FINALLY
      TRY
        Rd.Close(rd);
      EXCEPT
        Rd.Failure,
        Thread.Alerted => RAISE E("cannot close file " & fn);
      END;
    END;
    RETURN data;
  END FileContents;

(*---------------------------------------------------------------------------*)
PROCEDURE PutFile(fn : Pathname.T; data : TEXT) RAISES {E} =
  VAR
    wr : Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open(fn);
    EXCEPT
      OSError.E(l) => RAISE E("cannot open file " & fn & " for writing" & 
        ": " & AtomListToText(l));
    END;
    TRY
      TRY
        Wr.PutText(wr, data);
        Wr.Close(wr);
      EXCEPT
        Wr.Failure,
        Thread.Alerted => RAISE E("cannot write to file " & fn);
      END;
    FINALLY
      TRY
        Wr.Close(wr);
      EXCEPT
        Wr.Failure,
        Thread.Alerted => RAISE E("cannot close file " & fn);
      END;
    END;
  END PutFile;

(*--------------------------------------------------------------------------*)
BEGIN
END FSUtils.
