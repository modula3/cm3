(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PathnameUtils.m3,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

MODULE PathnameUtils;
IMPORT FS;
IMPORT OSError;
IMPORT Text;
IMPORT Pathname;

PROCEDURE CompleteE(t: T): T RAISES {OSError.E} =
  VAR
    base := Pathname.Prefix(t);
    iter := FS.Iterate(base);
    begin := Pathname.Last(t);
    name: TEXT;
  BEGIN
    WHILE iter.next(name) DO
      IF Text.Equal(Text.Sub(name, 0, Text.Length(begin)), begin) THEN
        RETURN SlashedPrefix(t) & name;
      END;
    END;
    RETURN t;
  END CompleteE;

PROCEDURE Complete(t: T): T =
  BEGIN
    TRY
      RETURN CompleteE(t);
    EXCEPT
      OSError.E => RETURN t;
    END;
  END Complete;

PROCEDURE SlashedPrefix(t: T): T =
  VAR
    path := Pathname.Prefix(t);
  BEGIN
    IF Text.Length(path)#0 AND
      Text.GetChar(path,Text.Length(path)-1) # '/' THEN
      path := path & "/";
    END;
    RETURN path;
  END SlashedPrefix;

BEGIN
END PathnameUtils.
