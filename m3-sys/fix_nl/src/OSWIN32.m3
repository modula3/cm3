(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE OSWIN32 EXPORTS OS;

IMPORT FS, File, FileWin32, OSError, OSErrorWin32, TimeWin32, WinBase;

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

PROCEDURE Close (f: File.T;  modTime: LONGREAL; <*UNUSED*> path: TEXT)
  RAISES {OSError.E} =
  BEGIN
    TRY
      SetModifiedTime (f, modTime);
    FINALLY
      f.close ();
    END;
  END Close;

PROCEDURE SetModifiedTime (f: File.T;  time: LONGREAL) RAISES {OSError.E} =
  VAR modTime: WinBase.FILETIME;
  BEGIN
    TimeWin32.ToFileTime (time, modTime);
    IF WinBase.SetFileTime (f.handle, NIL, NIL, ADR (modTime)) = 0 THEN
      OSErrorWin32.Raise ();
    END;
  END SetModifiedTime;

BEGIN
END OSWIN32.

