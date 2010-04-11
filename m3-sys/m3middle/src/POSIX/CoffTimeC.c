(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Fri Nov 18 07:39:18 PST 1994 by kalsow   *)

MODULE CoffTime;

IMPORT Time, TimePosix, Utime, File, FS, OSError;

PROCEDURE Now (): INTEGER =
  VAR x := TimePosix.ToUtime (Time.Now ());
  BEGIN
    RETURN x.tv_sec;
  END Now;

PROCEDURE OfFile (file: TEXT): INTEGER =
  VAR s: File.Status;  x: Utime.struct_timeval;
  BEGIN
    TRY
      s := FS.Status (file);
    EXCEPT OSError.E =>
      RETURN 0;
    END;
    x := TimePosix.ToUtime (s.modificationTime);
    RETURN x.tv_sec;
  END OfFile;

BEGIN
END CoffTime.
