(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Dec 17 11:26:18 PST 1993 by kalsow    *)
(*      modified on Thu Nov 21 11:23:26 PST 1991 by muller    *)

MODULE Unetdb;

FROM Ctypes IMPORT int;

PROCEDURE VALID_SVC (a: int): BOOLEAN =
  BEGIN
    RETURN 0 < a AND a < NSVCS;
  END VALID_SVC;

BEGIN
END Unetdb.

