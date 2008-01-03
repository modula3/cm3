(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjMonLocal.m3 *)
(* Last modified on Wed Apr 14 11:41:06 PDT 1993 by wobber *)
(*      modified on Thu Sep 17 16:24:24 PDT 1992 by evers  *)

MODULE NetObjMonLocal;
   
IMPORT NetObj, NGCMonitor, NetObjMon, Fmt, Params, Process, RefList, Thread;

TYPE
  T = NetObjMon.T OBJECT OVERRIDES
    dump      := Dump;
    dumpNames := DumpNames;
  END;

PROCEDURE Register () =
  VAR reg: NetObjMon.Registry;
  BEGIN
    TRY
      reg := NetObj.Import(RegistryServiceName);
      IF reg # NIL THEN
        reg.register(NEW(T),
	   Fmt.F("%s(%s)", Params.Get(0), Fmt.Int(Process.GetMyID())));
      END;
    EXCEPT
    | NetObj.Invalid, NetObj.Error, Thread.Alerted => (* skip *)
    END;
  END Register;

PROCEDURE Dump(<*UNUSED*>m: T) : NGCMonitor.Dump
    RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN NGCMonitor.MonitorDump();
  END Dump;

PROCEDURE DumpNames(<*UNUSED*>m: T) : RefList.T
    RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN NGCMonitor.MonitorDumpNames();
  END DumpNames;

BEGIN
END NetObjMonLocal.
