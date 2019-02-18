(* $Id$ *)

INTERFACE SysPerf;
IMPORT Pathname;

PROCEDURE GetLoadAvg(VAR loadavg : ARRAY [0..2] OF LONGREAL) : [-1..LAST(CARDINAL)];
  (* see getloadavg(3) -- will return -1 on Windows *)

EXCEPTION Error;

PROCEDURE DiskAvail(mountPoint : Pathname.T) : LONGREAL RAISES { Error };

END SysPerf.
