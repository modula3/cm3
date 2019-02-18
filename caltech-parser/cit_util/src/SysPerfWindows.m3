(* $Id$ *)

MODULE SysPerfWindows EXPORTS SysPerf;
IMPORT Pathname;
(* these things might actually be implemented under Cygwin.  Shouldn't
   give up so easy! *)

PROCEDURE GetLoadAvg(VAR loadavg : ARRAY [0..2] OF LONGREAL) : 
  [-1..LAST(CARDINAL)] =
  BEGIN RETURN -1 END GetLoadAvg;

PROCEDURE DiskAvail(mp : Pathname.T) : LONGREAL RAISES { Error } = 
  BEGIN RAISE Error END DiskAvail;

BEGIN END SysPerfWindows.
