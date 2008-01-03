(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OS.m3                                                 *)
(* Last modified on Wed Jun 21 09:36:57 PDT 1995 by kalsow     *)
(*      modified on Tue Mar 24 16:04:38 PST 1992 by muller     *)

UNSAFE MODULE OS;

IMPORT Time;
IMPORT FS, OSError;

(* old unix specific function
PROCEDURE CreateTime (file: TEXT): Time.T =
  VAR s: Ustat.struct_stat; ret: INTEGER;
  BEGIN
    WITH str = M3toC.SharedTtoS (file) DO
      ret := Ustat.stat (str, ADR (s));
      M3toC.FreeSharedS (file, str);
    END;
    IF ret = 0
      THEN RETURN FLOAT (s.st_mtime, LONGREAL);
      ELSE RETURN NO_TIME;
    END;
  END CreateTime;
*)

PROCEDURE ModTime (file: TEXT): Time.T =
  BEGIN
    TRY
      WITH stat = FS.Status(file) DO
        RETURN stat.modificationTime;
      END;
    EXCEPT
      OSError.E => RETURN NO_TIME;
    END;
  END ModTime;

BEGIN
END OS.

