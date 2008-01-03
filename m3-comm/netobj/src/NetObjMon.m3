(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjMonLocal.m3 *)
(* Last modified on Tue Jan 31 08:48:28 PST 1995 by kalsow *)
(*      modified on Mon Aug 30 10:31:24 PDT 1993 by wobber *)
(*      modified on Thu Sep 17 16:24:24 PDT 1992 by evers  *)

MODULE NetObjMon;
   
IMPORT NetObj, NGCMonitor, Fmt, Params, Process, RefList, Thread;

TYPE
  TT = T OBJECT OVERRIDES
    dump      := Dump;
    dumpNames := DumpNames;
  END;

PROCEDURE Register () =
  VAR reg: Registry;
  BEGIN
    TRY
      reg := NetObj.Import(RegistryServiceName);
      IF reg # NIL THEN
        reg.register(NEW(TT),
	   Fmt.F("%s(%s)", Params.Get(0), Fmt.Int(Process.GetMyID())));
      END;
    EXCEPT
    | NetObj.Error, Thread.Alerted => (* skip *)
    END;
  END Register;

PROCEDURE Dump(<*UNUSED*>m: TT) : REFANY =
  BEGIN
    RETURN NGCMonitor.MonitorDump();
  END Dump;

PROCEDURE DumpNames(<*UNUSED*>m: TT) : RefList.T =
  BEGIN
    RETURN NGCMonitor.MonitorDumpNames();
  END DumpNames;

BEGIN
END NetObjMon.
