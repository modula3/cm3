(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Sat Jun 27 18:03:45 PDT 1992 by muller        *)

MODULE Unetdb;

FROM Ctypes IMPORT int;

PROCEDURE VALID_SVC (a: int): BOOLEAN =
  BEGIN
    RETURN 0 < a AND a < NSVCS;
  END VALID_SVC;

BEGIN
END Unetdb.

