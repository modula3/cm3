(* $Id: Main.m3,v 1.1 2007/11/09 03:09:25 mika Exp $ *)

MODULE Main;
IMPORT SysPerf;
IMPORT Fmt, IO;

PROCEDURE FmtLA() : TEXT =
  VAR res := ""; BEGIN
    FOR i := FIRST(la) TO LAST(la) DO
      res := res & Fmt.LongReal(la[i], 
                                prec := 2) & " " 
    END;
    RETURN res
  END FmtLA;

VAR
  la : ARRAY[0..2] OF LONGREAL;
BEGIN
  EVAL SysPerf.GetLoadAvg(la);

  IO.Put(FmtLA() & "\n");

  IO.Put("Disk avail on /: " & Fmt.LongReal(SysPerf.DiskAvail("/")) & "\n")
END Main.

