(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Sep 24 09:51:47 PDT 1993 by kalsow    *)
(*      modified on Tue Nov 19 11:20:29 PST 1991 by muller    *)

MODULE Unetdb;

IMPORT Ctypes;

PROCEDURE VALID_SVC (a: Ctypes.int): BOOLEAN =
  BEGIN
    RETURN 0 < a AND a < NSVCS;
  END VALID_SVC;

BEGIN
END Unetdb.

