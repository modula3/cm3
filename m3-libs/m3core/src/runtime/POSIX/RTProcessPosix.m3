(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE RTProcessPosix EXPORTS RTProcess;
IMPORT RTProcessPosixC;

PROCEDURE TimeUsed (): REAL =
  BEGIN
    RETURN RTProcessPosixC.TimeUsed ();
  END TimeUsed;

BEGIN
END RTProcessPosix.
