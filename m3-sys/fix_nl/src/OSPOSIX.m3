(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE OSPOSIX EXPORTS OS;
IMPORT FS, File, OSError, OSErrorPosix;

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

PROCEDURE Close (f: File.T;  modTime: LONGREAL;  path: TEXT)
  RAISES {OSError.E} =
  BEGIN
    f.close ();
    SetModifiedTime (path, modTime);
  END Close;

PROCEDURE SetModifiedTime (path: TEXT;  time: LONGREAL) RAISES {OSError.E} =
  BEGIN
    IF UTimes (path, time) < 0 THEN
      OSErrorPosix.Raise ();
    END;
  END SetModifiedTime;

BEGIN
END OSPOSIX.
