(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3File.m3                                             *)
(* Last modified on Mon Apr 17 09:01:53 PDT 1995 by kalsow     *)

UNSAFE MODULE M3File;

IMPORT Compiler, FS, File, OSError;

TYPE
  BufPtr = UNTRACED REF ARRAY BufferLength OF File.Byte;

CONST
  OnUnix = (Compiler.ThisOS = Compiler.OS.POSIX);
  (* We need to preserve permission bits on Unix.  On Win32 it's
     too hard (Win95 doesn't have security, and WinNT w/o NTFS
     is broken too!), so we don't bother.  *)

PROCEDURE Read (f: File.T; VAR(*OUT*)buf: Buffer; len: BufferLength): INTEGER
  RAISES {OSError.E} =
  VAR ptr: BufPtr;
  BEGIN
    IF (NUMBER (buf) <= 0) THEN RETURN 0 END;
    ptr := LOOPHOLE (ADR (buf[0]), BufPtr);
    len := MIN (len, NUMBER (buf));
    IF (len <= 0) THEN RETURN 0; END;
    RETURN f.read (SUBARRAY (ptr^, 0, len), mayBlock := TRUE);
  END Read;

PROCEDURE Copy (src, dest: TEXT) RAISES {OSError.E} =
  VAR
    rd, wr : File.T := NIL;
    len    : INTEGER;
    buf    : ARRAY [0..4095] OF File.Byte;
    status  : File.Status;
  BEGIN
    TRY
      rd := FS.OpenFileReadonly (src);
      (* delete in case someone else is reading *)
      TRY FS.DeleteFile (dest);
      EXCEPT OSError.E => (* nope *)
      END;
      wr := OpenDestination (dest, rd);
      status := FS.Status(src);
      LOOP
        len := rd.read (buf);
        IF (len <= 0) THEN EXIT; END;
        wr.write (SUBARRAY (buf, 0, len));
      END;
    FINALLY
      IF (wr # NIL) THEN wr.close (); END;
      IF (rd # NIL) THEN rd.close (); END;
      TRY 
        FS.SetModificationTime(dest, status.modificationTime) 
      EXCEPT ELSE END;
    END;
  END Copy;

PROCEDURE OpenDestination (dest: TEXT;  src: File.T): File.T RAISES {OSError.E} =
  BEGIN
    IF NOT OnUnix THEN
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

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN RAISES {OSError.E} =
  VAR
    f1, f2     : File.T := NIL;
    buf1, buf2 : ARRAY [0..1023] OF File.Byte;
    len1, len2 : INTEGER;
  BEGIN
    TRY
      f1 := FS.OpenFileReadonly (a);
      f2 := FS.OpenFileReadonly (b);
      IF (f1 = NIL) OR (f2 = NIL) THEN RETURN FALSE; END;
      IF (f1.status().size # f2.status().size) THEN RETURN FALSE; END;
      LOOP
        len1 := f1.read (buf1);
        len2 := f2.read (buf2);
        IF (len1 # len2) THEN RETURN FALSE; END;
        IF (len1 <= 0)   THEN RETURN TRUE;  END;
        FOR i := 0 TO len1-1 DO
          IF buf1[i] # buf2[i] THEN RETURN FALSE END;
        END;
      END;
    FINALLY
      IF (f1 # NIL) THEN f1.close (); END;
      IF (f2 # NIL) THEN f2.close (); END;
    END;
  END IsEqual;

PROCEDURE IsDirectory (path: TEXT): BOOLEAN =
  VAR s: File.Status;
  BEGIN
    TRY
      s := FS.Status (path);
      RETURN (s.type = FS.DirectoryFileType);
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;
  END IsDirectory;

PROCEDURE IsReadable (path: TEXT): BOOLEAN =
  (* We don't really check for readablitiy, just for existence *)
  BEGIN
    TRY
      EVAL FS.Status (path);
      RETURN TRUE;
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;
  END IsReadable;

BEGIN
END M3File.

