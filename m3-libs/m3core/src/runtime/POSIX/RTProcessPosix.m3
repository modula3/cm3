(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE RTProcessPosix EXPORTS RTProcess;
IMPORT Utime, Uresource;

PROCEDURE TimevalSecs(READONLY t: Utime.struct_timeval): REAL =
(* Return the number of seconds represented by "t" as a floating-
   point number. *)
  BEGIN
    RETURN FLOAT(t.tv_sec) + (FLOAT(t.tv_usec) / 1.0e6)
  END TimevalSecs;

PROCEDURE TimeUsed (): REAL =
  VAR
    usage: Uresource.struct_rusage;
    ret := Uresource.getrusage(Uresource.RUSAGE_SELF, usage);
  BEGIN
    <* ASSERT ret = 0 *>
    RETURN TimevalSecs(usage.ru_utime) + TimevalSecs(usage.ru_stime);
  END TimeUsed;

BEGIN
END RTProcessPosix.
