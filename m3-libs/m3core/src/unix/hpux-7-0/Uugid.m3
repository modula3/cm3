(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Jan  6 09:04:36 PST 1995 by kalsow    *)

MODULE Uugid;

FROM Ctypes IMPORT int;

PROCEDURE setreuid (ruid, euid: int): int =
  BEGIN
    RETURN setresuid (ruid, euid, -1);
  END setreuid;

BEGIN
END Uugid.
