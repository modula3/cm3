(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObj.m3 *)
(* Last modified on Tue Jan 31 08:49:10 PST 1995 by kalsow *)
(*      modified on Fri Jun  4 11:59:57 PDT 1993 by wobber *)
(*      modified on Thu Sep 17 16:24:24 PDT 1992 by evers  *)

MODULE NetObj EXPORTS NetObj, TransportRegistry;
   
IMPORT NetObjMon, RTParams;

IMPORT Atom, AtomList, IP, SpecialObj, Transport, NetObjInit, NetObjMonInit,
       TCPNetObj, TCPTransport, Thread;

<*NOWARN*> IMPORT ListPkl;   (* remove when the special is added to List pkg *)
<*NOWARN*> IMPORT AtomPkl;   (* remove when the special is added to Atom pkg *)

<* PRAGMA LL *>

PROCEDURE Locate(host: TEXT) : Address
    RAISES {Invalid, Error} =
  VAR ep: IP.Endpoint;
  BEGIN
    TRY
      IF NOT IP.GetHostByName(host, ep.addr) THEN RAISE Invalid; END;
      ep.port := IP.NullPort;
    EXCEPT
    | IP.Error(ec) => RAISE Error(AtomList.Cons(CommFailure, ec));
    END;
    RETURN TCPNetObj.Locate(ep);
  END Locate;

PROCEDURE Export(name: TEXT; obj: T; where: Address := NIL)
    RAISES {Error, Thread.Alerted} =
  BEGIN
    CheckMonitorExport();
    GetSpecial(where).put(name, obj);
  END Export;

PROCEDURE Import(name: TEXT; where: Address := NIL) : T
    RAISES {Error, Thread.Alerted} =
  BEGIN
    CheckMonitorExport();
    RETURN GetSpecial(where).get(name);
  END Import;


(* assume this is initialized in main body *)
(* no mutex is needed *)

CONST MaxTransports = 10;
VAR transports: ARRAY [0..MaxTransports-1] OF Transport.T;
VAR lastTransport: CARDINAL;

PROCEDURE LocationFromAdr(where: Address) : Transport.Location =
  VAR loc: Transport.Location;
  BEGIN
    IF where # NIL THEN
      FOR i := 0 TO lastTransport DO
        FOR j := 0 TO LAST(where^) DO
          loc := transports[i].fromEndpoint(where[j]);
          IF loc # NIL THEN RETURN loc; END;
        END;
      END;
    END;
    RETURN NIL;
  END LocationFromAdr;
  
PROCEDURE LocalAdr() : Address =
  VAR
    adr := NEW(Address, lastTransport+1);
  BEGIN
    FOR i := 0 TO lastTransport DO
      adr[i] := transports[i].toEndpoint();
    END;
    RETURN adr;
  END LocalAdr;
  
REVEAL
  Iterator = IteratorPublic BRANDED OBJECT
    i: CARDINAL := 0;
  OVERRIDES
    next := IteratorNext;
  END;

EXCEPTION FatalError; <* FATAL FatalError *>

PROCEDURE Iterate (): Iterator =
  BEGIN
    RETURN NEW (Iterator);
  END Iterate;

PROCEDURE IteratorNext (it: Iterator; VAR (*OUT*) tr: Transport.T): BOOLEAN =
  BEGIN
    IF it.i > lastTransport + 1 THEN
      RAISE FatalError
    ELSIF it.i = lastTransport + 1 THEN
      INC (it.i);
      RETURN FALSE;
    ELSE
      tr := transports[it.i];
      INC (it.i);
      RETURN TRUE;
    END;
  END IteratorNext;

VAR
  mu := NEW(MUTEX);
  localSpecialObj: SpecialObj.ST := NIL;
  
PROCEDURE GetSpecial(where: Address) : SpecialObj.ST
    RAISES {Error} =
  VAR loc: Transport.Location;
  BEGIN
    IF where = NIL THEN
      LOCK mu DO
        IF localSpecialObj # NIL THEN RETURN localSpecialObj; END;
        loc := LocationFromAdr(TCPNetObj.Locate(IP.NullEndPoint));
        <* ASSERT loc # NIL *>
        localSpecialObj := SpecialObj.New(loc);
        RETURN localSpecialObj;
      END;
    ELSE
      loc := LocationFromAdr(where);
      IF loc = NIL THEN RAISE Error(AtomList.List1(NoTransport)); END;
      RETURN SpecialObj.New(loc);
    END;
  END GetSpecial;
  
VAR <* LL >= {mu} *>
  exportMonitor   := RTParams.IsPresent("ngcmonitor");
  monitorExported := FALSE;

PROCEDURE CheckMonitorExport () =
  BEGIN
    LOCK mu DO
      IF exportMonitor AND NOT monitorExported THEN
      	EVAL Thread.Fork(NEW (Thread.Closure, apply := ExportMonitor));
        monitorExported := TRUE;
      END;      
    END;
  END CheckMonitorExport;

PROCEDURE ExportMonitor (<*UNUSED*> cl: Thread.Closure): REFANY =
  BEGIN
    NetObjMon.Register();
      (* this export will fail silently if there is no listener on
      	 the daemon port yet *)
    RETURN NIL;
  END ExportMonitor;

BEGIN
  CommFailure := Atom.FromText("NetObj.CommFailure");
  MissingObject := Atom.FromText("NetObj.MissingObject");
  NoResources := Atom.FromText("NetObj.NoResources");
  NoTransport := Atom.FromText("NetObj.NoTransport");
  UnsupportedDataRep := Atom.FromText("NetObj.UnsupportedDataRep");
  Alerted := Atom.FromText("NetObj.Alerted");

  transports[0] := TCPTransport.New();
  lastTransport := 0;

  NetObjInit.InitAgentStubs();
  NetObjInit.InitVoucherStubs();
  NetObjMonInit.InitMonitorStubs();
  NetObjMonInit.InitRegistryStubs();

END NetObj.
