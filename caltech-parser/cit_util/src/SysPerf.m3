(* $Id$ *)

UNSAFE MODULE SysPerf;
IMPORT c_SysPerf;
IMPORT Pathname, M3toC;

PROCEDURE GetLoadAvg(VAR loadavg : ARRAY [0..2] OF LONGREAL) : 
  [-1..LAST(CARDINAL)] =
  VAR
    fail := NUMBER(loadavg);
  BEGIN
    FOR i := FIRST(loadavg) TO LAST(loadavg) DO
      IF c_SysPerf.getloadavg_glue(loadavg[i],i)#i THEN fail := i END;
    END;
    RETURN fail
  END GetLoadAvg;

PROCEDURE DiskAvail(mountPoint : Pathname.T) : LONGREAL RAISES { Error } =
  VAR
    bsize : INTEGER;
    total,avail,availNonSuperUser : LONGREAL;
  BEGIN
    WITH s = M3toC.CopyTtoS(mountPoint),
         res = c_SysPerf.diskAvail(s,
                                   bsize,total,avail,availNonSuperUser) DO
      IF res < 0 THEN RAISE Error END;
      M3toC.FreeCopiedS(s)
    END;
    WITH superPart = avail - availNonSuperUser,
         nonSuperTotal = total - superPart DO
      RETURN availNonSuperUser/nonSuperTotal
    END
  END DiskAvail;

BEGIN END SysPerf.
