(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Oct 30              1995 by preschern      *)

MODULE Unix;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT off_t;

PROCEDURE lseek (d: int; offset: off_t; whence: int): int =
  BEGIN
    IF (d = 0) OR (d = 1) OR (d = 2) THEN
      RETURN 0;
    END;
    RETURN ulseek (d, offset, whence);
  END lseek;

BEGIN
END Unix.

