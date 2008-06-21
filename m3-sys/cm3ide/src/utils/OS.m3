(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OS.m3                                                 *)
(* Last modified on Fri Mar 10 09:46:19 PST 1995 by kalsow     *)
(*      modified on Tue Mar 24 16:04:38 PST 1992 by muller     *)

MODULE OS;

IMPORT Atom, AtomList, Env, File, FileRd, FmtTime, FS, Pathname;
IMPORT M3File, OSError, Rd, RegularFile, Text, Thread, Time, Wr;
IMPORT Default, ErrLog, Text2;

PROCEDURE IsDirectory (file: TEXT): BOOLEAN =
  BEGIN
    TRY
      WITH stat = FS.Status (file) DO
        RETURN stat.type = FS.DirectoryFileType;
      END
    EXCEPT
    | OSError.E => RETURN FALSE;
    END
  END IsDirectory;

PROCEDURE IsExecutable (file: TEXT): BOOLEAN =
  BEGIN
    TRY
      WITH stat = FS.Status (file) DO
        RETURN stat.type = RegularFile.FileType;
      END
    EXCEPT
    | OSError.E => RETURN FALSE;
    END
  END IsExecutable;

PROCEDURE FileNameEq (a, b: TEXT): BOOLEAN =
  BEGIN
    IF (Default.on_unix)
      THEN  RETURN Text.Equal (a, b);     (* POSIX *)
      ELSE  RETURN Text2.CIEqual (a, b);  (* WIN32 *)
    END;
  END FileNameEq;

PROCEDURE FindExecutable (file: TEXT): TEXT =
  CONST UnixExts = ARRAY OF TEXT { NIL };
  CONST WinExts = ARRAY OF TEXT { NIL, "exe", "com", "cmd", "bat" };
  VAR path := Env.Get ("PATH");
  BEGIN
    IF Default.on_unix
      THEN RETURN SearchPath (file, path, ':', UnixExts);
      ELSE RETURN SearchPath (file, path, ';', WinExts);
    END;
  END FindExecutable;

PROCEDURE SearchPath (file, path: TEXT;   sep: CHAR;
                      READONLY exts: ARRAY OF  TEXT): TEXT =
  VAR dir, fn: TEXT;  s0, s1, len: INTEGER;  no_ext: BOOLEAN;
  BEGIN
    IF IsExecutable (file) THEN RETURN file; END;

    no_ext := Text.Equal (file, Pathname.Base (file));

    (* first try the file without looking at the path *)
    IF no_ext THEN
      FOR i := FIRST (exts) TO LAST (exts) DO
        fn := Pathname.Join (NIL, file, exts[i]);
        IF IsExecutable (fn) THEN RETURN fn; END;
      END;
    END;

    IF path = NIL THEN RETURN NIL; END;
    IF Pathname.Absolute (file) THEN RETURN NIL; END;

    (* try the search path *)
    len := Text.Length (path);  s0 := 0;
    WHILE (s0 < len) DO
      s1 := Text.FindChar (path, sep, s0);
      IF (s1 < 0) THEN s1 := len; END;
      IF (s0 < s1) THEN
        dir := Text.Sub (path, s0, s1 - s0);
        IF no_ext THEN
          FOR i := FIRST (exts) TO LAST (exts) DO
            fn := Pathname.Join (dir, file, exts[i]);
            IF IsExecutable (fn) THEN RETURN fn; END;
          END;
        ELSE
          fn := Pathname.Join (dir, file, NIL);
          IF IsExecutable (fn) THEN RETURN fn; END;
        END;
      END;
      s0 := s1 + 1;
    END;

    (* failed *)
    RETURN NIL;
  END SearchPath;

PROCEDURE CopyDirectory (src, dest: TEXT) =
  VAR nm, src_path, dest_path : TEXT;  iter: FS.Iterator;
  BEGIN
    TRY
      IF NOT IsDirectory (dest) THEN  FS.CreateDirectory (dest);  END;
      iter := FS.Iterate (src);
      TRY
        WHILE iter.next (nm) DO
          src_path  := MakePath (src, nm);
          dest_path := MakePath (dest, nm);
          IF IsDirectory (src_path)
            THEN CopyDirectory (src_path, dest_path);
            ELSE M3File.Copy (src_path, dest_path);
          END;
        END;
      FINALLY
        iter.close ();
      END;
    EXCEPT OSError.E (ec) =>
      ErrLog.Msg ("trouble copying directory: ",
                  src, " -> ", dest & Err (ec));
    END;
  END CopyDirectory;

(*---------------------------------------------------------- times ---*)

VAR epoch: Time.T := Time.Now ();
(* Internally, we measure time in seconds since we started. *)

PROCEDURE Now (): FileTime =
  BEGIN
    RETURN M3ToFileTime (Time.Now ());
  END Now;

PROCEDURE M3ToFileTime (t: Time.T): FileTime =
  BEGIN
    RETURN ROUND (t - epoch);
  END M3ToFileTime;

PROCEDURE FileToM3Time (t: FileTime): Time.T =
  BEGIN
    RETURN FLOAT (t, LONGREAL) + epoch;
  END FileToM3Time;

PROCEDURE LastModified (file: TEXT): FileTime =
  BEGIN
    TRY
      WITH status = FS.Status (file) DO
        RETURN M3ToFileTime (status.modificationTime);
      END
    EXCEPT
    | OSError.E => RETURN NO_TIME;
    END
  END LastModified;

PROCEDURE FmtFileTime (t: FileTime): TEXT =
  BEGIN
    RETURN Text2.ConvertNBSP (FmtTime.Short (FileToM3Time (t)));
  END FmtFileTime;

(*------------------------------------------------------------------*)

VAR (* we keep a small cache of allocated file readers... *)
  spare_mu := NEW (MUTEX);
  n_spares := 0;
  spares   := ARRAY [0..4] OF FileRd.T { NIL, .. };

PROCEDURE OpenRd (file: TEXT): Rd.T =
  VAR rd: FileRd.T := NIL;
  BEGIN
    LOCK spare_mu DO
      IF (n_spares > 0) THEN
        DEC (n_spares);
        rd := spares [n_spares];
        spares [n_spares] := NIL;
      END;
    END;
    IF (rd = NIL) THEN rd := NEW (FileRd.T); END;

    TRY
      EVAL rd.init (FS.OpenFileReadonly (file));
    EXCEPT OSError.E (ec) =>
      ErrLog.Msg ("Unable to open \"", file, "\" for reading", Err (ec));
      ReleaseRd (rd);
      RETURN NIL;
    END;
    
    RETURN rd;
  END OpenRd;

PROCEDURE ReleaseRd (rd: Rd.T) =
  BEGIN
    TYPECASE rd OF
    | NULL => (* skip *)
    | FileRd.T (frd) =>
        LOCK spare_mu DO
          IF (n_spares < NUMBER (spares)) THEN
            spares [n_spares] := frd;  INC (n_spares);
          END;
        END;
    ELSE (* skip *)
    END;
  END ReleaseRd;

PROCEDURE CloseRd (rd: Rd.T) =
  BEGIN
    TRY Rd.Close (rd)
    EXCEPT Rd.Failure, Thread.Alerted => (*SKIP*) 
    END;
    ReleaseRd (rd);
  END CloseRd;

PROCEDURE CloseWr (wr: Wr.T) =
  BEGIN
    TRY Wr.Close (wr)
    EXCEPT Wr.Failure, Thread.Alerted => (*SKIP*)
    END;
  END CloseWr;

PROCEDURE CloseFile (f: File.T) =
  BEGIN
    TRY f.close ();
    EXCEPT OSError.E => (*SKIP*)
    END;
  END CloseFile;

(***
PROCEDURE DrainFile (f: File.T) =
  VAR buf: ARRAY [0..1023] OF File.Byte;
  BEGIN
    TRY WHILE (f.read (buf) > 0) DO (*skip*) END;
    EXCEPT OSError.E => (* ignore *)
    END;
  END DrainFile;
***)

PROCEDURE MakePath (a, b, c, d: TEXT := NIL): TEXT =
  VAR path := a;
  BEGIN
    IF (b # NIL) THEN path := Pathname.Join (path, b, NIL); END;
    IF (c # NIL) THEN path := Pathname.Join (path, c, NIL); END;
    IF (d # NIL) THEN path := Pathname.Join (path, d, NIL); END;
    RETURN path;
  END MakePath;

PROCEDURE Err (args: AtomList.T): TEXT =
  VAR msg : TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL) THEN  msg := ": ";  ELSE  msg := msg & "  ***  ";  END;
      msg  := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    IF (msg = NIL) THEN msg := ": ** NO INFO **"; END;
    RETURN msg;
  END Err;

BEGIN
END OS.

