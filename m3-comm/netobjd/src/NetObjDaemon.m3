(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjDaemon.m3 *)
(* Last modified on Wed Sep  8 15:08:35 PDT 1993 by wobber *)
(*      modified on Mon Sep 14 13:50:01 PDT 1992 by evers  *)

MODULE NetObjDaemon EXPORTS Main;
   
IMPORT IP, NetObj, TCPNetObj;
IMPORT NetObjMon, NMonRegistrySvr;
IMPORT Wr, Stdio, Params, Thread, Process, RTCollectorSRC;

VAR r: NetObjMon.Registry;
BEGIN
  TRY
    EVAL TCPNetObj.Listen(IP.NullPort);

    TRY
      (* export a NetObjMon.Registry service in our own name table *)
      r := NMonRegistrySvr.New();
      NetObj.Export(NetObjMon.RegistryServiceName, r);

      (* register our own monitor *)
      NetObjMon.Register();
    EXCEPT
    | NetObj.Error, Thread.Alerted => (* skip *)
    END;

    RTCollectorSRC.StartBackgroundCollection();
    LOOP
      RTCollectorSRC.StartCollection();
      (* Thread.Pause (1.8D2); *)
      Thread.Pause (5.0D0);
      RTCollectorSRC.FinishCollection();
    END;
  EXCEPT
  | TCPNetObj.Failed =>
      <* FATAL Wr.Failure, Thread.Alerted *>
      BEGIN
        Wr.PutText(Stdio.stderr,
      	  Params.Get(0) & ": listen failed.  Is a netobjd already running?\n");
        Wr.Flush(Stdio.stderr);
        Process.Exit(1);
      END;
  END;
END NetObjDaemon.
