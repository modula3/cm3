(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Mar  4 11:46:50 PST 1992 by muller        *)

MODULE Unetdb;

FROM Ctypes IMPORT int;

PROCEDURE VALID_SVC (a: int): BOOLEAN =
  BEGIN
    RETURN 0 < a AND a < NSVCS;
  END VALID_SVC;

BEGIN
END Unetdb.

