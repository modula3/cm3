(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

MODULE OS;

IMPORT Atom, AtomList, Env, File, FileWr, FS, Msg, Pathname;
IMPORT OSError, RegularFile, Text2, Text, Thread, Wr;

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
    IF (on_unix)
      THEN  RETURN Text.Equal (a, b);     (* POSIX *)
      ELSE  RETURN Text2.CIEqual (a, b);  (* WIN32 *)
    END;
  END FileNameEq;

PROCEDURE CleanDirName (dir: TEXT): TEXT =
  CONST Slash = ARRAY BOOLEAN OF CHAR { '\134', '/' }[on_unix];
  VAR len: INTEGER;
  BEGIN
    IF (dir # NIL) AND NOT FileNameEq (Pathname.Prefix (dir), dir) THEN
      len := Text.Length (dir);
      IF (len > 1) AND Text.GetChar (dir, len-1) = Slash THEN
        dir := Text.Sub (dir, 0, len-1);
      END;
    END;
    RETURN dir;
  END CleanDirName;

PROCEDURE FindExecutable (file: TEXT): TEXT =
  CONST UnixExts = ARRAY OF TEXT { NIL };
  CONST WinExts = ARRAY OF TEXT { NIL, "exe", "com", "cmd", "bat" };
  VAR path := Env.Get ("PATH");
  BEGIN
    IF on_unix
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
        dir := CleanDirName (dir);
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

PROCEDURE GetAbsolutePath (a, b: TEXT := NIL): TEXT =
  VAR path := MakePath (a, b);
  BEGIN
    IF Pathname.Absolute (path) THEN RETURN path; END;
    TRY
      RETURN FS.GetAbsolutePathname (path);
    EXCEPT OSError.E =>
      RETURN path;
    END;
  END GetAbsolutePath;

PROCEDURE MakePath (a, b, c, d: TEXT := NIL): TEXT =
  VAR path := a;
  BEGIN
    IF (b # NIL) THEN path := Pathname.Join (path, b, NIL); END;
    IF (c # NIL) THEN path := Pathname.Join (path, c, NIL); END;
    IF (d # NIL) THEN path := Pathname.Join (path, d, NIL); END;
    RETURN path;
  END MakePath;

PROCEDURE MakeDir (dir: TEXT): BOOLEAN =
  VAR parent: TEXT;
  BEGIN
    IF dir = NIL OR Text.Length (dir) = 0 THEN dir := "."; END;
    IF IsDirectory (dir) THEN RETURN TRUE; END;

    parent := Pathname.Prefix (dir);
    IF (parent # NIL) AND NOT FileNameEq (parent, dir) THEN
      IF NOT MakeDir (parent) THEN RETURN FALSE; END;
    END;

    TRY
      FS.CreateDirectory (dir);
      RETURN TRUE;
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;
  END MakeDir;

PROCEDURE WriteFile (name, contents: TEXT) =
  VAR wr: Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open (name);
      Wr.PutText (wr, contents);
      Wr.Close (wr);
    EXCEPT
    | OSError.E (ec) =>
        Msg.Error (ec, "Unable to open the file: ", name);
    | Wr.Failure (ec) =>
        Msg.Error (ec, "Unable to write the file: ", name);
    | Thread.Alerted =>
        Msg.Error (NIL, "Interrupted while writing the file: ", name);
    END;
  END WriteFile;

PROCEDURE RemoveFile (file: TEXT) =
  BEGIN
    TRY
      FS.DeleteFile (file);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to remove file: ", file);
    END;
  END RemoveFile;

PROCEDURE MoveFile (src, dest: TEXT) =
  BEGIN
    TRY
      CopyFile (src, dest);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to copy file: ", src, " -> ", dest);
    END;
    RemoveFile (src);
  END MoveFile;

PROCEDURE CopyFile (src, dest: TEXT) RAISES {OSError.E} =
  VAR
    rd, wr : File.T := NIL;
    len    : INTEGER;
    buf    : ARRAY [0..4095] OF File.Byte;
  BEGIN
    TRY
      rd := FS.OpenFileReadonly (src);
      wr := OpenDestination (dest, rd);
      LOOP
        len := rd.read (buf);
        IF (len <= 0) THEN EXIT; END;
        wr.write (SUBARRAY (buf, 0, len));
      END;
    FINALLY
      IF (wr # NIL) THEN wr.close (); END;
      IF (rd # NIL) THEN rd.close (); END;
    END;
  END CopyFile;

PROCEDURE OpenDestination (dest: TEXT;  src: File.T): File.T RAISES {OSError.E} =
  BEGIN
    (* We need to preserve permission bits on Unix.  On Win32 it's too hard
       (Win95 doesn't have security, and WinNT w/o NTFS is broken too!),
       so we don't bother.  *)
    IF NOT on_unix THEN
      (* File permissions on Windows are broken... *)
      src := NIL;
    END;

    TRY RETURN FS.OpenFile (dest, template := src);
    EXCEPT OSError.E => (* nope. *)
    END;

    (* If we can't open the file for writing, try deleting it first
       and then opening it, sometimes that'll work instead... *)

    TRY FS.DeleteFile (dest);
    EXCEPT OSError.E => (* doesn't look very hopeful *)
    END;

    RETURN FS.OpenFile (dest, template := src);
  END OpenDestination;

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

