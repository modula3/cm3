(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE OSPOSIX EXPORTS OS;

IMPORT FS, File, FilePosix, M3toC, OSError, OSErrorPosix, TimePosix;
IMPORT Unix, Utime;

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
  VAR tv: ARRAY [0..1] OF Utime.struct_timeval;
  BEGIN
    tv[0] := TimePosix.ToUtime (time);  (* last accessed time *)
    tv[1] := tv[0];                     (* last modified time *)
    IF Unix.utimes (M3toC.TtoS (path), ADR (tv[0])) < 0 THEN
      OSErrorPosix.Raise ();
    END;
  END SetModifiedTime;

BEGIN
END OSPOSIX.

